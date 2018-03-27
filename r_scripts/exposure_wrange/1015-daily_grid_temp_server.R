# ------------------------------------------------------------------------------
# Title: Regridding of NCEP North American Regional Reanalysis Temperature to 
#         Colorado Grid Corridnates
# Author: Ryan Gan
# Date Created: 2018-02-21
# ------------------------------------------------------------------------------

# This script regrids temperature estimates from NCEP NARR to Kate's PM grid
# for colorado

# libraries ----
library(tidyverse) # all purpose tidy data
library(ncdf4) # working with netcdf files
library(raster) # creating rasters
library(parallel) # for parallel computing

# read in colorado pm coordinates ----
grid_coords <- read_csv("./data/smoke/colo_krig_grid_coords.csv")


#define air_temp file path
temp_path <- paste0("../../../meta_wildfire/data/smoke/air_temp_2m/")
# create list of temp files
temp_list <- list.files(temp_path)
# print temp files
temp_list



# parallel sapply nc read and write function over list of .nc files
sapply(temp_list, function(meow){
meow <- temp_list[[1]]
# open nc connection
temp_nc <- nc_open(paste0(temp_path, meow))
# get temp_matrix
temp_mat <- ncvar_get(temp_nc, varid = "air")
# extract date values to assign to column header
date <- paste0("d",
gsub("-", "", as.Date(ncvar_get(temp_nc, varid = "time")/24, 
origin = "1800-01-01")))
# extract year from name of list
year <- substring(meow, 8, 11)

start_time <- Sys.time()
# regrid apply for each day
co_temp_mat <- apply(temp_mat[,,1:5], 3, function(x){
# regrid temp raster 
rast_temp <- rasterize(temp_coords[,1:2], r, as.vector(x), fun=mean)
# extract values from raster to grid_id coordinates
temp_k_regrid <- extract(rast_temp, grid_coords[,2:3])
}) # end innter apply function
stop_time <- Sys.time() - start_time
stop_time

}) # end sapply