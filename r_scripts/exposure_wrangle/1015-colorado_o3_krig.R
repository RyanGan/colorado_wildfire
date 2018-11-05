# ------------------------------------------------------------------------------
# Title: Krig of ozone from EPA monitors to Colorado front range grid
# Author: Ryan Gan
# Date Created: 2018-10-31
# ------------------------------------------------------------------------------

# Introduction: This code is adapted from Sheena's code
# https://github.com/smartenies/AQS_Data_Scrape/blob/master/R/03_Kriging_AQS_Data.R
# I am using prepared 8 hour max ozone estimates from AQS since the API for raw
# AQS was down at the time.

# Libraries
library(tidyverse) # basic data management
library(gstat) # used for ordinary krig
library(sp) # spatial objects
library(sf)

# Vector of files to read
files <- paste0('./data/smoke/', seq(2010,2015), '-co_daily_ozone.csv')

# Reading in 2010 to 2015 ozone data
ozone <- map_dfr(files, function(x) {
  read_csv(x) %>% 
  rename(o3_8hr_max = `Daily Max 8-hour Ozone Concentration`)
})

# Find Colorado monitor locations; taking only first obs of each monitor
monitor_locs <- ozone %>% 
  group_by(AQS_SITE_ID) %>% 
  filter(row_number()==1) %>% 
  filter(POC == 1) %>% 
  # filter grid coords to front range bounding box
  filter(SITE_LONGITUDE >= -105.42 & SITE_LONGITUDE <= -104.452) %>% 
  filter(SITE_LATITUDE >= 38 & SITE_LATITUDE <= 41)

# read in 15x15 grid
study_grid <- read_csv('./data/smoke/krig_grid_id_coords.csv') %>% 
  # filter grid coords to front range bounding box
  filter(lon >= -105.42 & lon <= -104.452) %>% 
  filter(lat >= 38.5 & lat <= 40.75) %>% 
  filter(!(GRID_ID %in% c(23715, 23716, 23717)))

# saving study grid
#write_csv(study_grid, path = './data/smoke/front_range_grid.csv')

# Note: I believe the Krig grid should be larger
  ggplot() +
  geom_point(data = monitor_locs, aes(x=SITE_LONGITUDE, y=SITE_LATITUDE), 
             color = 'red', size = 0.5) +
  # grid points
  geom_point(data=study_grid, aes(x=lon, y=lat), color = 'black', size = 0.5) 


# limiting to front range ozone monitors
fr_o3 <- ozone %>% 
  filter(AQS_SITE_ID %in% monitor_locs$AQS_SITE_ID) %>% 
  mutate(Date = as.Date(Date, format = '%m/%d/%Y')) %>% 
  rename(lat = SITE_LATITUDE, lon = SITE_LONGITUDE)

# converting front range grid to spatial points dataframe
coordinates(study_grid) = ~lon+lat

# vector of grid ids
grid_id_vector <- study_grid@data$GRID_ID

# create vector of dates to map through
date_vector <- unique(fr_o3$Date)

# define function to perform krig on varying monitors for each day
# returns vector of predictions for each grid cell 
krige_fun <- function(filter_date){
  # subset ozone data
  o3_subset <- filter(fr_o3, Date == filter_date) 
  
  # convert o3_subset to a spatial points dataframe
  coordinates(o3_subset) = ~lon+lat
  
  # set up variogram
  all_models <- c("Exp", "Sph", "Gau", "Cir", "Lin", "Log")

  # cutoff distance is 40 km based on what sheena used
  c_dist = 40000

  # define variogram
  vgm <- variogram(o3_8hr_max ~ 1, data = o3_subset)

  # fit variogram
  vgm_fit <- fit.variogram(vgm, model=vgm(all_models),
                           fit.kappa = seq(0.3,5,0.1))

  # fit model
  model <- as.character(vgm_fit$model)[nrow(vgm_fit)]
  
  # ordinary kriging model  
  krige_result <- krige(o3_8hr_max ~ 1, o3_subset, study_grid, vgm_fit)

  # leave-one out crossvalidation for validation metrics
  # cross validated
  cv_krige_result <- krige.cv(o3_8hr_max ~ 1, o3_subset, vgm_fit, nfold = 10)
  # cross validated
  cv_correlation <- cor(cv_krige_result$var1.pred, 
                        cv_krige_result$observed, 
                        method = 'pearson')
  # return kriging predictions for each grid cell
  krige_pred <- data.frame(krige_result$var1.pred) 
  colnames(krige_pred) <- paste0('d_', filter_date)
  return(krige_pred)
}

# start time
start <- Sys.time()

# map kriging over date vector
kriged_ozone <- map_dfc(date_vector, ~ krige_fun(.x)) 

# end time
end <-  Sys.time() - start
end

# bind grid ids
kriged_ozone <- cbind(grid_id_vector, kriged_ozone)
summary(kriged_ozone[,1:20])

# write kriged estimates; I should probably consider some performance metrics
write_csv(kriged_ozone, path = './data/smoke/1015-frontrange_kriged_o3')
