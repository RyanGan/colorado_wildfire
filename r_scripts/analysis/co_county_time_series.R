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

# removing co_hosp_df to save some memory
rm(co_hosp_df)

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

# merge in county names and create time series dataframe
colorado_co_ts_df <- co_er_df %>% 
  rename(co_fips = county_geo) %>% 
  right_join(county_fips, by = "co_fips") %>% 
  # create date variable
  mutate(date = admit) %>% 
  # group by county and date and count up event outcomes
  group_by(county, date) %>% 
  # sum up each primary diagnosis for each outcome for each day for each county
  summarise(n_obs = n(), resp_n = sum(resp1), asthma_n = sum(asthma1), 
            pneum_n = sum(pneum1), acute_bronch_n = sum(acute_bronch1), 
            copd_n = sum(copd1), cvd_n = sum(cvd1), ihd_n = sum(ihd1), 
            arrythmia_n = sum(arrhythmia1), hf_n = sum(hf1), 
            cereb_vas_n = sum(cereb_vas1), mi_n = sum(mi1), 
            broken_arm_n = sum(broken_arm1)) %>% 
  # cvd and resp er visits
  mutate(cvd_resp_n = resp_n + cvd_n) %>% 
  ungroup() %>%  # remove grouping element
  # joining back in the fips and state name
  right_join(county_fips, by = "county") %>% 
  select(state, county, co_fips, fips, n_obs:cvd_resp_n)
  
# note n_obs might mean something different here as we only received 
# cardiopulmonary dx
summary(colorado_co_ts_df)

# write permanent colorado ts dataframe
write_path <- paste0("./data/colorado_county_timeseries_2010_2015.csv")
write_csv(colorado_co_ts_df, write_path)

# data checks (i will probably move this to another script) -----
# plotting time series of outcomes
# aggregating to state level
colorado_aggregate_ts <- colorado_co_ts_df %>% 
  select(-county, -n_obs) %>% 
  group_by(date) %>% 
  summarise_each(funs(sum(., na.rm = T))) %>% 
  gather(key = outcome, value = n, -date)

ggplot(colorado_aggregate_ts, aes(x = date, y = n)) +
  geom_point() +
  facet_wrap(~outcome)

# I want to look at just respiratory outcomes
resp_ts <- colorado_aggregate_ts %>% 
  filter(outcome %in% c("asthma_n", "copd_n", "acute_bronch_n",
                        "pneum_n")) %>% 
  # filter to 2014
  filter(date >= "2014-01-01" & date <= "2014-12-31")

ggplot(resp_ts, aes(x = date, y = n)) +
  geom_point() +
  facet_wrap(~outcome)

# Larimer county
larimer_june_2012 <- colorado_co_ts_df  %>% 
  filter(county == "Larimer") %>% 
  filter(date >= "2012-05-01" & date <= "2012-07-30") %>% 
  select(-county, -n_obs) %>% 
  gather(key = outcome, value = n, -date)

ggplot(larimer_june_2012, aes(x = date, y = n)) +
  geom_point() +
  facet_wrap(~outcome)
