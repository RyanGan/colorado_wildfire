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

grid_dir <- paste0('./instructions/tl_2012_us_zcta510/tl_2012_us_zcta510.shp')

smoke_grid <- readOGR(dsn = grid_dir) # ?layer
summary(smoke_grid) 
plot(smoke_grid) 

getwd()


library(maptools)


