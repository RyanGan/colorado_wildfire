# ------------------------------------------------------------------------------
# Title: Case crossover for diseases
# Author: Jingyang Liu
# Date Created: 1/30/17
# R version: 3.3.2
# ------------------------------------------------------------------------------

#Creating case crossover dataframes
library(tidyverse)
library(data.table)
library(dplyr)
library(readr)
library(lubridate)


read_path <- paste0('./co_hosp_w_outcome_df.csv')
disease <- read_csv(read_path) 
read_path2 <- paste0('./st08_co_cou.txt')
co_geo <- read_csv(read_path2, col_names = F)

# changing variable name, 
summary(co_geo)
names(co_geo)[1:5] <- c("state","st_code","county_code","county_name","FIPS")

# convert to vector
county <- as.vector(as.matrix(co_geo$county_code))

disease_ashtma1 <- disease %>%
  # filter for Colorado State
  filter(WRFGRID_ID!=0) %>%
  filter(county_final %in% county) %>%
  # add new transverted admit date
  mutate(date_admit = as.Date(admit, "%m/%d/%Y"))

ggplot(disease_ashtma1, aes(x = disease_ashtma1$date_admit)) + geom_density()

summary(disease_ashtma1$WRFGRID_ID)
xtabs(~county_final, disease_ashtma1)


# Asthma1 ----------------------------------------------------------------------
disease_asthma1_try <- disease_ashtma1 %>%
  # filter asthma1
  filter(asthma1==1) %>%
  # filter ZIP=80526
  filter(ZIP==80526) %>%
  # filter date in July 1, 2012 and Oct 31, 2012
  filter(date_admit >= '2012-07-01' & 
           date_admit <= '2012-10-31') %>%
  # add ID
  mutate(id = seq(1, nrow(.), by = 1),
         # add outcome and new admit date
        outcome_asthma1 = 1)

one_asthma_case <- sample_n(disease_asthma1_try, 1) %>% 
  select(cdpheid, admit, date_admit, dx1, asthma1, ZIP, county_final, WRFGRID_ID,
         RACE, sex_ind, age_ind, id)

one_asthma_case

date <- seq(as.Date(one_asthma_case[[1,3]]) - 
              (as.Date("2012-07-01", "%Y-%m-%d")),
            as.Date(one_asthma_case[[1,3]] + 
              (as.Date("2012-10-31", "%Y-%m-%d")), by='1 week'))

date
  
# bind unique id and date of the year with covariates
  id_date <- data_frame(date) %>% bind_cols(cov_df)
  # iteration which binds rows of unique ids
  id_date_df <- bind_rows(id_date_df, id_date)


# replicate covariates length of counterfactual dates
cov_df <- do.call("bind_rows", replicate(3, disease_asthma1_try,simplify = F))

# Make the counterfactual data for before and after dates of one week
date_before <- disease_asthma1_try$date_admit-7
date_after <-  disease_asthma1_try$date_admit+7
cov_df$date_admit <- c(disease_asthma1_try$date_admit, date_before, date_after)

# make outcome variable, 1 means asthma1 and 0 means no asthma1
cov_df <- cov_df %>%
  mutate(outcome = ifelse(date_admit == disease_asthma1_try$date_admit, 1, 0)) %>%
  arrange(id, date_admit) # order by id and date





