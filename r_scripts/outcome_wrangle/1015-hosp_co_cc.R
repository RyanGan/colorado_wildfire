# ------------------------------------------------------------------------------
# Title: Creation of hospitalization case-crossover dataframe for Colorado 2010-2015
# Author: Ryan Gan
# Date Created: 2018-03-29
# ------------------------------------------------------------------------------

# This script creates monthly time-stratified case-crossover dataframes for 
# colorado morbidity outcomes from 2010 to 2016.
# Note: I may move this to the Colorado wildfire repo.

# libraries -----
library(tidyverse)
library(lubridate)
library(case.crossover)


# load Rdata vector of outcomes ----
load("./data/health/icd9_outcome_vectors.RData")

# read morbidity data -----
# define path where hosp data is
co_path <- paste0("./data/health/co_hosp_1015.csv")
# import path
co_hosp <- read_csv(co_path, col_types = cols(.default = "c")) %>% 
  # filter to ER admission
  filter(ADMTNO == 1) %>% 
  # filter to study date
  mutate(date = as.Date(admit, format = "%m/%d/%Y")) %>% 
  # code outcomes
  mutate(resp = if_else(dx1 %in% icd9_outcomes$resp, 1, 0),
         asthma = if_else(dx1 %in% icd9_outcomes$asthma, 1, 0),
         copd = if_else(dx1 %in% icd9_outcomes$copd, 1, 0),
         acute_bronch = if_else(dx1 %in% icd9_outcomes$acute_bronch, 1, 0),
         pneum = if_else(dx1 %in% icd9_outcomes$pneumonia, 1, 0),
         cvd = if_else(dx1 %in% icd9_outcomes$cvd, 1, 0),
         arrhythmia = if_else(dx1 %in% icd9_outcomes$arrhythmia, 1, 0),
         cereb_vas = if_else(dx1 %in% icd9_outcomes$cereb_vas, 1, 0),
         hf = if_else(dx1 %in% icd9_outcomes$hf, 1, 0), 
         ihd = if_else(dx1 %in% icd9_outcomes$ihd, 1, 0),
         mi = if_else(dx1 %in% icd9_outcomes$mi, 1, 0),
         sex  = case_when(SEX == "1" ~ "M",
                          SEX == "2" ~ "F"),
         age = as.numeric(AGEYRS),
         age_cat = case_when(AGEYRS < 15 ~ "age_under_15",
                             AGEYRS >= 15 & AGEYRS < 65 ~ "age_15_to_65",
                             AGEYRS >= 65 ~ "age_over_65"),
         zip = as.character(ZIP),
         id = paste0(cdpheid,"c"),
         state = "colorado",
         fips = paste0("08", county_final),
         stay_length = as.numeric(LOS)) %>% 
  select(id, date, ADMTNO, stay_length, dx1, age, age_cat, sex, zip, fips, WRFGRID_ID, 
         state)

# check variables
glimpse(co_hosp)

# apply time stratified case-crossover function ----
# time-stratified case-crossover ----
start_time <- Sys.time()
co_morbidity_cc_list <- icd9_outcomes %>% 
  # create lists of outcome dataframes
  map(~filter(co_hosp, dx1 %in% .)) %>% 
  # apply ts casecross over function and join with pm data
  map(~casecross(data = ., id = "id", date = "date", period = "month", 
                 covariate = c("stay_length", "dx1", "age", "age_cat", "sex", 
                               "zip", "fips", "WRFGRID_ID", "state"))) 
stop_time <- Sys.time()
time <- stop_time - start_time
print(time)

# head casecross list
glimpse(co_morbidity_cc_list[1])

# saving casecross over list as Rdata
save(co_morbidity_cc_list, file = paste0("./data/health/",
                                   "1015-co_morbidity_casecross_list.RData"))
