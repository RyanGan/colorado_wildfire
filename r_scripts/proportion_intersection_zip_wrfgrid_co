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

grid_dir <- paste0('./instructions/co_grid_shp_nad83')

smoke_grid <- readOGR(dsn = grid_dir, layer = "co_grid") # ? layer
summary(smoke_grid) # 
plot(smoke_grid) 

getwd()




