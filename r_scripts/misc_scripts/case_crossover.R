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

read_path <- paste0('./data/co_hosp_w_outcome_df.csv')
disease <- read_csv(read_path) 
read_path2 <- paste0('./st08_co_cou.txt')
co_geo <- read_csv(read_path2, col_names = F)

## check unique person and unique claim, yes
length(unique(disease$cdpheid)) 

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

ggplot(disease, aes(x = disease$dates)) + geom_density()

summary(disease$WRFGRID_ID)
xtabs(~county_final, disease)


# Asthma1 ----------------------------------------------------------------------
disease_asthma1 <- disease %>%
  # filter asthma1
  filter(asthma1==1) %>%
  # filter ZIP=80526
  filter(ZIP==80526) %>%
  # filter date in July 1, 2012 and Oct 31, 2012
  filter(dates >= '2012-07-01' & 
           dates <= '2012-10-31') %>%
  # add ID
  mutate(id = seq(1, nrow(.), by = 1),
         # add outcome and new admit date
        outcome_asthma1 = 1)

one_asthma1_case <- sample_n(disease_asthma1, 1) %>% 
  select(cdpheid, admit, dates, dx1, asthma1, ZIP, county_final, WRFGRID_ID,
         RACE, sex_ind, age_ind, id)

one_asthma1_case

# find the replicate times of weeks
dates <- one_asthma1_case[[1,3]] 
n1 <- 0
d=as.Date("2012-07-01")
i=1
while (dates >= "2012-07-01"){
  dates <- dates - 7
  d[i] = dates
  i = i+1
  n1 = n1+1
}
d[1:n1-1] # shows character(0) when the first week
n1-1

dates <- one_asthma1_case[[1,3]] 
n2=0
e=as.Date("2012-10-31")
j=1
while (dates <= "2012-10-31"){
  dates <- dates + 7
  e[j]=dates
  j=j+1
  n2 = n2 + 1
}
e[1:n2-1] # shows character(0) when the last week
n2-1

# replicate covariates length of counterfactual dates
# and make conuterfactual dates
if (n1==1){
  cov_df <- do.call("bind_rows", replicate(n1+n2, one_asthma1_case,simplify = F))
  cov_df$dates <- c(one_asthma1_case[[1,3]], e[1:n2-1])
} else if (n2==1){
  cov_df <- do.call("bind_rows", replicate(n1+n2, one_asthma1_case,simplify = F))
  cov_df$dates <- c(one_asthma1_case[[1,3]], d[1:(n1-1)])
}else{
  cov_df <- do.call("bind_rows", replicate(n1+n2-1, one_asthma1_case,simplify = F))
  cov_df$dates <- c(one_asthma1_case[[1,3]], d[1:(n1-1)], e[1:n2-1])
}

# make outcome variable, 1 means asthma1 and 0 means no asthma1
cov_df <- cov_df %>%
  mutate(outcome = ifelse(dates == one_asthma1_case[[1,3]], 1, 0)) %>%
  arrange(id, dates) # order by id and date



### Every diseases--------------------------------------------------------------
# Make 12 separate data frames (excel) for each disease for dx1
# 01/31/2017-02/01/2017

var_list <- c('resp1', 'asthma1', 'pneum1',  'acute_bronch1', 'copd1', 'cvd1', 
              'ihd1', 'arrhythmia1', 'hf1', 'cereb_vas1', 'mi1', 'broken_arm1')

start <- Sys.time()
for(m in var_list){ # begin first loop of variable names (outcomes)
  
  # Case-Crossover loop --------------------------------------------------------
  outcome_col <- which(colnames(disease) == m) # use to keep outcome var
  
  outcome_id <- disease %>%
    # filter total outcome for jth disease
    filter(disease[[m]] == 1) %>% 
    # filter date in July 1, 2012 and Oct 31, 2012
    filter(dates >= '2012-07-01' & 
             dates <= '2012-10-31') %>%
    # arrange with dates
    arrange(dates) %>%
    mutate(id = seq(1, nrow(.), by = 1)) %>% # create subject id
    # select 11 cols, the 4th is disease name
    select(cdpheid, id, admit, dates, 
           (outcome_col), # keep in bracket for outcome var num
           dx1, ZIP, county_final, WRFGRID_ID, RACE, sex_ind, age_ind)
  
  outcome_col2 <- which(colnames(outcome_id) == m) # use to keep outcome var
  
  # create dataset to populate
  id_date_df <- data_frame(cdpheid = NA, id =NA, admit = NA, dates = NA, 
                           dx1 = NA, ZIP = NA, county_final = NA, WRFGRID_ID = NA, 
                           RACE = NA, sex_ind = NA, age_ind = NA)
  
  
  # begin second loop to create counterfactual observations for each case subject
  for (k in 1:nrow(outcome_id)){
    
    # find the replicate times of weeks
    dates_l <- outcome_id[[k,4]] 
    n1 <- 0
    d=as.Date("2012-07-01")
    i=1
    while (dates_l >= "2012-07-01"){
      dates_l <- dates_l - 7
      d[i] = dates_l
      i = i+1
      n1 = n1+1
    }
    d[1:n1-1] # shows character(0) when the first week
    n1-1
    
    dates_l <- outcome_id[[k,4]] 
    n2=0
    e=as.Date("2012-10-31")
    j=1
    while (dates_l <= "2012-10-31"){
      dates_l <- dates_l + 7
      e[j]=dates_l
      j=j+1
      n2 = n2 + 1
    }
    e[1:n2-1] # shows character(0) when the last week
    n2-1
    
    # replicate covariates length of counterfactual dates
    # and make conuterfactual dates
    if (n1==1){
      cov_df <- do.call("bind_rows", replicate(n1+n2-1, outcome_id[k,],simplify = F))
      cov_df$dates <- c(outcome_id[[k,4]], e[1:n2-1])
    } else if (n2==1){
      cov_df <- do.call("bind_rows", replicate(n1+n2-1, outcome_id[k,],simplify = F))
      cov_df$dates <- c(outcome_id[[k,4]], d[1:(n1-1)])
    }else{
      cov_df <- do.call("bind_rows", replicate(n1+n2-1, outcome_id[k,],simplify = F))
      cov_df$dates <- c(outcome_id[[k,4]], d[1:(n1-1)], e[1:n2-1])
    }
    
    # bind unique id and date of the year with covariates
    id_date <- bind_cols(cov_df)
    # iteration which binds rows of unique ids
    id_date_df <- na.omit(bind_rows(id_date_df, id_date))
    
  } # End of inner loop
  
  # make outcome variable, 1 means asthma1 and 0 means no asthma1
  outcome_casecross <- id_date_df %>%
    mutate(outcome = ifelse(dates == as.Date(admit, "%m/%d/%Y"), 1, 0)) %>%
    arrange(id, dates) # order by id and date
  
  # Create a permanent case-cross over dataset
  file_name <- paste(m, 'jul_to_oct_casecross.csv', sep = '_')
  
  # write permanent dataset
  write_csv(outcome_casecross, paste0("./data/case_cross_data/", file_name))
} # End of the overall loop

# sweet this works
total_time <- Sys.time() - start
total_time # Time difference of 16.00895 mins


getwd()
# check
aaa <- disease %>% filter(dates >= '2012-07-01' & 
                            dates <= '2012-10-31')
check_broken <- sum(aaa[[166]] == 1) # 107
file_path <- paste0("./data/case_cross_data/mi1_jul_to_oct_casecross.csv")


# read in dataframe (Warning: 7802 parsing failures)
check <- read_csv(file_path)
