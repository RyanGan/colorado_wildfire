# ------------------------------------------------------------------------------
# Title: Creating county-level time series for cardiopulmonary outcomes in 
#        Colorado from 2010 to 2015
# Author: Ryan Gan
# Date Created: 5/4/2017
# R version: 3.3.3
# ------------------------------------------------------------------------------

# Purpose of this code is to create time series health outcomes data for each 
# Colorado county for the time periods available. I will limit that counts to 
# emergency room visits that are for a primary diagnosis of the health outcome
# of interest.

# load libraries ----
library(tidyverse)

# read cleaned CHA data ----
read_path <- paste0('./data/co_hosp_w_outcome_df.csv')
# limit to ER visits
co_hosp_df <- read_csv(read_path) 

# limit to ER visits in the state of colorado 
co_er_df <- co_hosp_df %>% 
  filter(ADMTNO == 1) %>% 
  # limit to state residents; best to link to county_geo
  filter(county_geo != "999") %>%
  filter(!is.na(county_geo)) %>% 
  filter(WRFGRID_ID != 0)

# check county codes
xtabs(~ county_geo, co_er_df)

# import Colorado county fips codes
county_fips <- read_csv('./data/st08_co_cou.csv', col_names = F) %>% 
  select(1:4) %>% 
  mutate(fips = paste0(X2, X3)) %>%
  # split out "County" from county variable
  separate(X4, into = c("county", "del"), sep = " County") %>% 
  rename(state = X1, co_fips = X3) %>% 
  select(state, county, co_fips, fips)




