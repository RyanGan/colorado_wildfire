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
  mutate(date_admit = as.Date(admit, "%m/%d/%Y"))

ggplot(disease, aes(x = disease$date_admit)) + geom_density()

summary(disease$WRFGRID_ID)
xtabs(~county_final, disease)


# Asthma1 ----------------------------------------------------------------------
disease_asthma1 <- disease %>%
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

one_asthma1_case <- sample_n(disease_asthma1, 1) %>% 
  select(cdpheid, admit, date_admit, dx1, asthma1, ZIP, county_final, WRFGRID_ID,
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
  cov_df$date_admit <- c(one_asthma1_case[[1,3]], e[1:n2-1])
} else if (n2==1){
  cov_df <- do.call("bind_rows", replicate(n1+n2, one_asthma1_case,simplify = F))
  cov_df$date_admit <- c(one_asthma1_case[[1,3]], d[1:(n1-1)])
}else{
  cov_df <- do.call("bind_rows", replicate(n1+n2-1, one_asthma1_case,simplify = F))
  cov_df$date_admit <- c(one_asthma1_case[[1,3]], d[1:(n1-1)], e[1:n2-1])
}

# make outcome variable, 1 means asthma1 and 0 means no asthma1
cov_df <- cov_df %>%
  mutate(outcome = ifelse(date_admit == one_asthma1_case[[1,3]], 1, 0)) %>%
  arrange(id, date_admit) # order by id and date



### Every diseases
# choose a small sample
disease1 <- disease[1:100,]

var_list <- c('arrhythmia1', 'cereb_vas1', 'ihd1', 'hf1')



# make outcome variable, 1 means asthma1 and 0 means no asthma1
cov_df <- cov_df %>%
  mutate(outcome = ifelse(date_admit == one_asthma1_case[[1,3]], 1, 0)) %>%
  arrange(cdpheid, date_admit) # order by id and date






start <- Sys.time()
for(j in var_list){ # begin first loop of variable names (outcomes)
  
  # Case-Crossover loop --------------------------------------------------------
  outcome_col <- which(colnames(disease1) == j) # use to keep outcome var

  outcome_id <- disease1 %>%
    filter(disease1[[j]] == 1) %>% # jth outcome
    # filter date in July 1, 2012 and Oct 31, 2012
    filter(date_admit >= '2012-07-01' & 
             date_admit <= '2012-10-31') %>%
    arrange(date_admit) %>%
    select(cdpheid, (outcome_col), # keep in bracket for outcome var num
           admit, date_admit, dx1,   ZIP, 
           county_final, WRFGRID_ID, RACE, sex_ind, age_ind)
  
  outcome_col2 <- which(colnames(outcome_id) == j) # use to keep outcome var
  
  # create dataset to populate
  id_date_df <- data_frame(cdpheid = NA, admit = NA, date_admit = NA, dx1 = NA,   
                           ZIP = NA, county_final = NA, WRFGRID_ID = NA, 
                           RACE = NA, sex_ind = NA, age_ind = NA)
  
  # begin second loop to create counterfactual observations for each case subject
  for (k in 1:nrow(outcome_id)){
    
  # find the replicate times of weeks
  dates <- outcome_id[[k,3]] 
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
  
  dates <- outcome_id[[k,3]] 
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
    cov_df <- do.call("bind_rows", replicate(n1+n2, outcome_id,simplify = F))
    cov_df$date_admit <- c(outcome_id[[k,3]], e[1:n2-1])
  } else if (n2==1){
    cov_df <- do.call("bind_rows", replicate(n1+n2, outcome_id,simplify = F))
    cov_df$date_admit <- c(outcome_id[[k,3]], d[1:(n1-1)])
  }else{
    cov_df <- do.call("bind_rows", replicate(n1+n2-1, outcome_id,simplify = F))
    cov_df$date_admit <- c(outcome_id[[k,3]], d[1:(n1-1)], e[1:n2-1])
  }
  }
} # End of the overall loop

# sweet this works
total_time <- Sys.time() - start
total_time




start <- Sys.time()
for(j in var_list){ # begin first loop of variable names (outcomes)
  
  # Case-Crossover loop ----------------------------------------------------------
  outcome_col <- which(colnames(disease1) == j) # use to keep outcome var
  
  outcome_id <- disease1 %>%
    filter(disease1[[j]] == 1) %>% # jth outcome
    filter(date_admit >= '2012-07-01' & 
             date_admit <= '2012-10-31') %>% 
    arrange(date_admit) %>%
    mutate(id = seq(1, nrow(.), by = 1)) %>% # create subject id
    select(cdpheid, (outcome_col), # keep in bracket for outcome var num
           admit, date_admit, dx1,   ZIP, 
           county_final, WRFGRID_ID, RACE, sex_ind, age_ind)
  
  outcome_col2 <- which(colnames(outcome_id) == j) # use to keep outcome var
  
  # create dataset to populate
  id_date_df <- data_frame(cdpheid = NA, admit = NA, date_admit = NA, dx1 = NA,   
                           ZIP = NA, county_final = NA, WRFGRID_ID = NA, 
                           RACE = NA, sex_ind = NA, age_ind = NA)
  
  # begin second loop to create counterfactual observations for each case subject
  for (i in 1:nrow(outcome_id)){
    # code dates for each id up to two months before and after the event
    date <- seq(as.Date(outcome_id[[i, 3]] - 56), 
                as.Date(outcome_id[[i, 3]] + 56), by = '1 week')
    
    # covariates to preserve
    covariate <- filter(outcome_id, id == i) %>% 
      select(cdpheid, (outcome_col2), # keep in bracket for outcome var num
             admit, date_admit, dx1,   ZIP, 
             county_final, WRFGRID_ID, RACE, sex_ind, age_ind)
    # replicate covariates length of counterfactual dates
    cov_df <- do.call("bind_rows", replicate(length(date), covariate, simplify = F))
    
    # bind unique id and date of the year with covariates
    id_date <- data_frame(date) %>% bind_cols(cov_df)
    # iteration which binds rows of unique ids
    id_date_df <- bind_rows(id_date_df, id_date)
  } # end inner lop
  
  
  # Create a permanent case-cross over dataset
  file_name <- paste(j, 'jul_to_oct_casecross.csv', sep = '_')
  
  # write permanent dataset
  write_csv(outcome_casecross, file_name)
}




