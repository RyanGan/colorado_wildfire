# ------------------------------------------------------------------------------
# Title: Proportion Intersection Zip WRFGrid CO
# Author: Jingyang Liu
# Date Created: 1/30/17
# R version: 3.3.2
# ------------------------------------------------------------------------------

library(rgdal) # package for shape files
library(sp)
library(rgeos) # rgeos package contains the intersect and area commands I need
library(tidyverse)
library(maptools)
library(tidyverse)
library(data.table)
library(dplyr)
library(readr)
library(lubridate)

### previous data cod paste from case_crossover---------------------------------
read_path <- paste0('./data/co_hosp_w_outcome_df.csv')
disease <- read_csv(read_path) 
read_path2 <- paste0('./st08_co_cou.txt')
co_geo <- read_csv(read_path2, col_names = F)

# changing variable name, 
summary(co_geo)
names(co_geo)[1:5] <- c("state","st_code","county_code","county_name","FIPS")

# convert to vector
county <- as.vector(as.matrix(co_geo$county_code))

disease <- disease %>%
  # filter for Colorado State
  filter(WRFGRID_ID!=0) %>%
  filter(county_final %in% county) %>%
  # add new transverted admit date
  mutate(dates = as.Date(admit, "%m/%d/%Y"))
###-----------------------------------------------------------------------------


# Import Shapefiles  -----------------------------------------------------------
# WRF Grid
grid_dir <- paste0('./instructions/co_grid_shp/co_grid.shp')

smoke_grid <- readOGR(dsn = grid_dir, layer = 'co_grid') 

summary(smoke_grid) 
plot(smoke_grid) 

# Zipcode shapefile
shp_dir <- paste0('./instructions/tl_2012_us_zcta510/tl_2012_us_zcta510.shp')

us_zip_2012 <- readOGR(dsn = shp_dir, layer = 'tl_2012_us_zcta510')
summary(us_zip_2012)
plot(us_zip_2012)

# read in CHARS zip code file so I can subset the larger US shapefile to just
# Colorado state
co_zip_2012 <- disease %>%
  # filter date in July 1, 2012 and Oct 31, 2012
  # filter(dates >= '2012-07-01' & 
  #          dates <= '2012-10-31') %>%
  # arrange with dates
  arrange(dates) %>%
  mutate(id = seq(1, nrow(.), by = 1)) %>% # create subject id
  # select 11 cols, the 4th is disease name
  select(ZIP, county_final) %>% 
  unique()

# convert to character vector
co_zip_2012$ZIP <- as.character(co_zip_2012$ZIP)

# check on zip code in colorado range
# removing anyways
co_zip_2012 <- filter(co_zip_2012, ZIP>=80001&ZIP<=81658)

# limit to just Colorado state zipcodes
co_zip_map <- us_zip_2012[us_zip_2012$ZCTA5CE10 %in% co_zip_2012$ZIP,]

# output zipcodes from washington zipcode map to bind values to
# plot map to check
plot(co_zip_map)

# saving wash shapefile ----
# save shapefile to use in future work
summary(co_zip_map)

# save the Colorado zips to a shapefile to use later
# create save path
save_path <- paste0('./instructions/co_zip_2012_shape_files')

writeOGR(co_zip_map, layer = 'co_zip_2012_shape_files', save_path, driver = "ESRI Shapefile")


# Set coordinate reference system for smoke gird
nad83 <- '+proj=longlat +datum=NAD83 +no_defs +ellps=GRS80 +towgs84=0,0,0'
proj4string(smoke_grid) <- nad83

# Plot overlay
plot(smoke_grid)
plot(co_zip_map, add=T)
# looks like they overlay pretty well, same projections

# Test code to figure out proportion calculations in each WRF-Grid -------------
# Trying 'over' function in sp package
# limit to a specific zip code
test_zip <- c(80521)
test_zip_map <- co_zip_map[co_zip_map$ZCTA5CE10 %in% test_zip,]

plot(test_zip_map)
plot(smoke_grid, add = T)
invisible(text(getSpPPolygonsLabptSlots(smoke_grid), 
               labels=as.character(smoke_grid$WRFGRID_ID)))

### end by now. 02/07/2017





