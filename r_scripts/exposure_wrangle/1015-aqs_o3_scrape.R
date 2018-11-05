# ------------------------------------------------------------------------------
# Title: Scrape of ozone 
# Author: Ryan Gan
# Date Created: 2018-10-31
# ------------------------------------------------------------------------------

# This script is adapted from Sheena Martenies script for scraping the EPA AQS
# for hourly ozone data using the AQS API

# Library 
library(tidyverse)

# read user name and password from text file
creds <- read.delim('./r_scripts/exposure_wrangle/epa_aqs_api_creds.txt', 
                    header = F, colClasses = c('character'))

# I made a txt file that contains my API credentials; remember to put this file
# on the ozone server to run this script

# extract user name
user_name <- creds[1,]
# extract password
pw <- creds[2,]

# base url that sets up daily ozone downloads for colorado
base_url <- paste0('https://aqs.epa.gov/api/rawData?user=', user_name, '&pw=',
                   pw, '&format=DMCSV&param=44201&state=08')

# vector of colorado counties to pull
all_counties <- c("001", "003", "005", "007", "009", "011", "013", "014", "015",
                  "017", "019", "021", "023", "025", "027", "029", "031", "033",
                  "035", "037", "039", "041", "043", "045", "047", "049", "051",
                  "053", "055", "057", "059", "061", "063", "065", "067", "069",
                  "071", "072", "073", "075", "077", "079", "081", "083", "085",
                  "087", "089", "091", "093", "095", "097", "099", "103", "105",
                  "107", "109", "111", "113", "115", "117", "119", "121", "123",
                  "125") 

# vector of date ranges
date_range <- c("&bdate=20100101&edate=20101231", 
                "&bdate=20110101&edate=20111231",
                "&bdate=20120101&edate=20121231",
                "&bdate=20130101&edate=20131231",
                "&bdate=20140101&edate=20141231",
                "&bdate=20150101&edate=20151231")


# Create dataframe of urls to pull for stations in each county from 2010 to 2015
# URLs are in line with API requests
url_list <- expand.grid(base_url, paste0("&county=", all_counties), 
                        date_range) %>% 
  mutate(url = paste0(Var1, Var2, Var3)) %>% 
  select(url)

# apply over each element of the url list
test_list <- as.data.frame(url_list[5, ])


apply(test_list, 1, function(x){
  # set up error and warning catch
  error_catch <- F
  warn_catch <- F
  tryCatch(read.csv(url(x)), error = function(e) error_catch <- T,
           warning = function(e) warn_catch <- T)
  # if no error, proceed
  if(!error_catch) {
    data <- read.csv(url(x), colClasses = rep('character', 25))
    # remove the END OF FILE row
    if(length(which(data$Latitude == 'END OF FILE')) > 0){
      data <- data[-which(data$Latitude == 'END OF FILE'),]
      }
  
    if(nrow(data) > 0) {
      if(nrow(output) > 0) {
        output <- r
        
      }
    }
  }
    }
  )

rep('c', 25)
read.csv('https://aqs.epa.gov/api/rawData?user=ryan.william.gan@gmail.com&pw=aquamouse29&format=DMCSV&param=44201&state=08&county=001&bdate=20100101&edate=20100101')

rm(data)

# move study map chunk to other script -----------------------------------------
# study map so I can visualize where to make this cut
# clip by bbox function ------
bbox_clip <- function(sf, bbox) {
  # find the CRS of the sf object
  crs <- sf::st_crs(sf)$proj4string
  # create matrix
  x <- c(bbox[1], bbox[1], bbox[3], bbox[3], bbox[1])
  y <- c(bbox[2], bbox[4], bbox[4], bbox[2], bbox[2])
  coords <- matrix(cbind(x, y), ncol=2)
  # create polygon and assign same coord crs as sf object
  coords_poly <- sp::Polygon(coords)
  bbox_poly <- sp:: SpatialPolygons(list(sp::Polygons(list(coords_poly),
                                                      ID = "bbox")), proj4string = sp::CRS(crs))
  # convert to sf feature
  bbox_sf <- st_as_sf(bbox_poly)
  # clip sf object
  clipped_sf <- sf[bbox_sf,]
  return(clipped_sf)
}

# clipping bounding box
study_bbox <- st_bbox(c(xmin=-105.3, xmax=-104.5, ymax=41, ymin = 38))

# county subset
county_sub_name <- c("Larimer", "Weld", "Boulder", "Broomfield", "Adams", 
                     "Denver", "Jefferson", "Arapahoe", "Douglas", "El Paso",
                     "Pueblo")

# read in county shapefile and subset to only colorado fips
co_county_sf <- st_read("../meta_wildfire/data/shapefile/us_county", 
                        layer = "us_county") %>%
  # limit to colorado
  filter(STATEFP == "08") %>% 
  # create fips variable
  mutate(fips = paste0(STATEFP, COUNTYFP))

# save wgs84 crs
wgs <- st_crs(co_county_sf)

# county clip
county_clip <- bbox_clip(co_county_sf, study_bbox)

# extract county names
county_text <- county_clip %>% 
  st_transform(wgs) %>% 
  st_centroid() %>% 
  st_coordinates() %>% 
  cbind(., as.character(county_clip$NAME)) %>%
  as_data_frame() %>% 
  rename(lon = X, lat = Y, county = V1) %>% 
  mutate(lon = as.numeric(lon), lat = as.numeric(lat))


# read in city polygons 2010
city <- st_read("./data/shapefiles/Colorado_City_Point_Locations/", 
                layer = "Colorado_City_Point_Locations") %>% 
  st_transform(crs = wgs)

# limit cities 
city_points <- city %>% 
  filter(NAME %in% c("FORT COLLINS", "PUEBLO", "GREELEY", "BOULDER", "DENVER",
                     "COLORADO SPRINGS")) %>% 
  mutate(city = stringr::str_to_title(NAME))


# read colorado 2015 population raster
co_pop_2015 <- raster("./data/shapefiles/2015-ColoradoPopDensity.tif")
# extract bbox of clipped county subset
fr_extent <- extent(st_bbox(county_clip)[c(1,3,2,4)])
# raster
fr_pop_2015 <- crop(co_pop_2015, fr_extent)

# i'm going to create a shapefile/simple features; easier to plot
front_range_pop_sf <- st_as_sf(rasterToPolygons(fr_pop_2015)) %>% 
  rename(popden = X2015.ColoradoPopDensity) %>% 
  # filtering to cells > 100 
  filter(popden > 100)

# cut once more to county shapefile so that populations outside the counties are not present
pop_clip <- front_range_pop_sf[county_clip,]

# plot map
study_map <-ggplot(pop_clip) +
  # start with plot of simple features of populations
  geom_sf(aes(fill=popden), color = NA) +
  # define aesthetics of poipulation
  scale_fill_gradient(name = expression("Population per km"^2), 
                      low = "#26d0ce", high = "#1a2980") +
  # plot lines of counties
  geom_sf(data = county_clip, color = "black", 
          fill = "transparent", size = 0.5) +
  geom_point(data = city_points, aes(x = LONG, y = LAT), color = "#3f2b96") +
  geom_text(data = city_points, aes(x = LONG, y = LAT, label = city), 
            color = "#3f2b96", size = 4, hjust = 1, vjust = -0.6) +
  # stations
  geom_point(data = monitor_locs, aes(x=SITE_LONGITUDE, y=SITE_LATITUDE), 
             color = 'red', size = 0.5) +
  # grid points
  geom_point(data=grid, aes(x=lon, y=lat), color = 'black', size = 0.5) +
  # theme
  theme(panel.background = element_blank(),
        panel.grid.major = element_line(colour = 'transparent'),
        panel.grid.minor = element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank(),
        axis.title = element_blank(),
        legend.key = element_rect(fill = NA, colour = NA, size = 0.25))

study_map

# --------
