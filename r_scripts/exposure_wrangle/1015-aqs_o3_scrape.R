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
