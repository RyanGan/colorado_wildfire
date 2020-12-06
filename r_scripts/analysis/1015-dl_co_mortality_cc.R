# ------------------------------------------------------------------------------
# Title: Analysis of Smoke PM2.5 and Mortality in Colorado 2010-2015
# Author: Ryan Gan
# Date Created: 2018-03-12
# ------------------------------------------------------------------------------

# This script creates monthly time-stratified case-crossover dataframes for 
# colorado mortality outcomes from 2010 to 2016.
# Note: I may move this to the Colorado wildfire repo.

# libraries ----
library(tidyverse)
library(survival)

# prepare pm2.5 data ----
# defining a lag function
funlag <- function(var, n=5){
  var <- enquo(var)
  indices <- seq_len(n)
  map( indices, ~quo(lag(!!var, !!.x)) ) %>% 
    set_names(sprintf("%s_lag%d", rlang::quo_text(var), indices))
}

# read pm
pm <- read_csv("./data/smoke/1015-county_popwt_pm.csv") %>% 
  filter(str_sub(fips,start=1,end=2) %in% c("08")) %>% 
  rename(pm_diff = pm_smk) %>% 
  mutate(day = as.factor(weekdays(date)),
         weekend = ifelse(day %in% c("Saturday", "Sunday"), 1, 0),
         month = as.factor(lubridate::month(date)),
         year = as.factor(lubridate::year(date)),
         season = as.factor(case_when(month %in% c(12, 1, 2) ~ "winter",
                                      month %in% c(3:5) ~ "spring",
                                      month %in% c(6:8) ~ "summer",
                                      month %in% c(9:11)~ "fall")),
         smoke0_hms = ifelse(pm_diff > 0 & month %in% c(3:10) & hms > 0.5, 1, 0),
         smoke5_hms = ifelse(pm_diff > 5 & month %in% c(3:10) & hms > 0.1, 1, 0),
         smoke10_hms = ifelse(pm_diff > 10 & month %in% c(3:10) & hms > 0.1, 1, 0),
         smoke15_hms = ifelse(pm_diff > 15 & month %in% c(3:10) & hms > 0.1, 1, 0),
         pm = pm_krig/10) %>% 
  arrange(fips, date) %>% 
  group_by(fips) %>% 
  mutate(., !!!funlag(pm,6), !!!funlag(smoke0_hms,6), !!!funlag(smoke5_hms,6),
         !!!funlag(smoke10_hms,6), !!!funlag(smoke15_hms,6), 
         !!!funlag(temp_f, 6)) %>% 
  select(-state, -month)

# load casecrossover list -----
load("./data/health/co_mortality_cc_list.RData")
# load mortality outcome list
load("./data/health/icd10_outcome.RData")

outcomes <- names(icd10_outcomes)

# reduce case-crossover list to only summer months and join pm
co_cc_list <- casecross_list %>% 
  map(~ mutate(., outcome = as.numeric(as.character(outcome)),
               date = as.Date(as.character(date_of_death)),
               month = as.factor(lubridate::month(date))) %>% 
      # filter out 2016; I don't have pm data yet
      filter(date <= "2015-12-30") %>% 
      filter(month %in% 4:10) %>% 
      left_join(pm, by = c("fips", "date"))) %>% 
  # add outcome name to each dataframe
  map2(.x = ., .y = outcomes, ~mutate(.x, out_name = .y))

# same-day association with pm and mortality during wildfire season
pm_results <- co_cc_list %>% 
  map_dfr(. , function(df){
    mod <- clogit(outcome ~ pm + temp_f + strata(id), data = df)
    est <- broom::tidy(mod) %>% filter(term == "pm") %>% 
      select(estimate, conf.low, conf.high) %>% 
      rename(lower95 = conf.low, upper95 = conf.high) %>% 
      mutate_all(exp)
    }) %>% # end map
  cbind(outcomes, .)

pm_results

# function to extract dl estimates ---------------------------------------------
distribute_that_lag <- function(lag_mod, strata) {
  # output pm basis estimates
  parms <- broom::tidy(lag_mod) %>% 
    filter(stringr::str_detect(term, strata)) %>% 
    select(estimate) %>% 
    as_vector()
  # output estimate names for cov matrix
  names <- stringr::str_subset(names(lag_mod$coefficients), strata)
  # define lagged basis spline if it doesn't exist
  exp_b <- ns(0:6, df = 3, intercept = T)
  # estimate associations
  est <- exp_b %*% parms
  # estimate standard error for each interval
  # time variable
  time <- ((rep(1:length(est))-1))
  # covariance matrix for knots 
  cov_mat <- as.matrix(vcov(lag_mod))[names, names]
  # estimate variance of spline
  var <- exp_b %*% cov_mat %*% t(exp_b)
  # estimate lag ----
  # estimate standard error for each lag day for smoke
  l_se <- sqrt(diag(var))
  # calculate lower and upper bound for smoke
  l_est_l95 <- est + (l_se*qnorm(1-0.975))
  l_est_u95 <- est + (l_se*qnorm(0.975))
  l_type <- "lag"
  # lag dataframe
  l_df <- data.frame(strata, l_type, time, 
                     exp(est), exp(l_est_l95), exp(l_est_u95), 
                     row.names = NULL) 
  # assign column names
  colnames(l_df) <- c("strata", "type", "time", 
                      "odds_ratio", "lower_95", "upper_95")
  # cumulative estimates
  c_est <- sapply(seq_along(est), function(x){
    sum(est[1:x])
  })
  # stderr cumulative effect smk
  c_se <- sapply(seq_along(c_est), function(y){
    sqrt(sum(var[1:y,1:y]))
  })
  # estimate 95% CI
  c_l95 <- c_est+(c_se*qnorm(1-0.975))
  c_u95 <- c_est+(c_se*qnorm(0.975))
  # type
  c_type <- "cumulative"
  # return dataframe
  c_df <- data.frame(strata, c_type, time, exp(c_est), 
                     exp(c_l95), exp(c_u95), row.names = NULL) 
  # assign column names
  colnames(c_df) <- c("strata", "type", "time", 
                      "odds_ratio", "lower_95", "upper_95")
  # bind lagged and cumulative 
  lag_est <- rbind(l_df, c_df) %>% 
    mutate(strata = as.character(strata),
           type = as.character(type))
  # return lagged estimate
  return(lag_est)
} # end lag estimate function

# continous PM2.5 distributed lag results --------------------------------------
start <- Sys.time()
# distributed lag function
mort_dl_pm_results  <- lapply(co_cc_list, function(x){
  # output dataframe from list
  data <- x %>% 
    mutate(date = as.Date(date),
           outcome = as.numeric(as.character(outcome))) %>%
    # remove missing lagged data
    filter(!is.na(pm_lag6))

  # output outcome name
  out_name <- as.character(unique(data$out_name))
  print(out_name)
  # create lagged matrix
  pm_mat <- as.matrix(select(data, pm, contains("pm_lag")))
  temp_mat <- as.matrix(select(data, contains("temp_f")))
  # define lagged basis spline
  exp_b <- ns(0:(ncol(pm_mat)-1), df = 3, intercept = T)
  # pm basis
  pm_basis <- pm_mat %*% exp_b
  # temp basis
  temp_basis <- temp_mat %*% exp_b
  # run lagged model
  lag_mod <- clogit(outcome ~ pm_basis + temp_basis + strata(id), data = data)
  
  # estimate lag estimate
  lag_est <- distribute_that_lag(lag_mod, strata = "pm") %>% 
    mutate(outcome = out_name) %>% select(outcome, strata:upper_95)
  return(lag_est)
  }) %>%  #end lappply
  # bind rows
  map_dfr(.,rbind)
# stop time
stop <- Sys.time()
time <- stop - start
# print time
print(time)

# saving continous pm results
write_csv(mort_dl_pm_results, paste0("./data/health/",
  "1015-co_mortality_dl_pm_results.csv"))

# Interaction model ------------------------------------------------------------

# start time
start <- Sys.time()
# distributed lag function
casecross_dl_int_results  <- lapply(co_cc_list, function(x){
  # output dataframe from list
  data <- x %>% 
    mutate(date = as.Date(date),
           outcome = as.numeric(as.character(outcome))) %>%
    # remove missing lagged data
    filter(!is.na(pm_lag6))
  
  # output outcome name
  out_name <- unique(data$out_name)
  # create lagged matrix
  pm_mat <- as.matrix(select(data, pm, contains("pm_lag")))
  # define lagged basis spline
  exp_b <- ns(0:(ncol(pm_mat)-1), df = 3, intercept = T)  
  # temp matrix and basis
  temp_mat <- as.matrix(select(data, contains("temp_f")))
  temp_basis <- temp_mat %*% exp_b
  
  # create vector of smoke var to estimate over
  smoke_var <- c("smoke0_hms", "smoke5_hms", "smoke10_hms", "smoke15_hms")
  
  smoke_results <- smoke_var %>% 
    map(function(s){ 
      smk_mat <- as.matrix(select(data, contains(s)))
      # lagged pm x basis
      pm_basis <- pm_mat %*% exp_b
      smk_basis <- smk_mat %*% exp_b
      # smoke basis for interaction
      pm_smk_b <- pm_basis * smk_basis 
      pm_nosmk_b <- pm_basis * ifelse(smk_basis == 1, 0, 1)
      # run lagged model
      lag_mod <- clogit(outcome ~ pm_smk_b + pm_nosmk_b + smk_basis + temp_basis +
                          strata(id), data = data)
      # define strata terms
      strata_terms <- c("pm_smk_b", "pm_nosmk_b", "smk_basis")
      # estimate cumulative and lagged effect for each basis
      
      lagged_estimates <- strata_terms %>% 
        map_dfr(~distribute_that_lag(lag_mod = lag_mod, strata = .)) %>% 
        mutate(outcome = out_name, smoke = s) %>% 
        select(outcome, smoke, strata:upper_95)
      
      return(lagged_estimates)      
    }) %>%  # end smoke map
    # bind rows
    map_dfr(.,rbind)
}) %>%  #end lappply
  # bind rows
  map_dfr(.,rbind)

# stop time
stop <- Sys.time()
time <- stop - start
# print time
print(time)

# write file
write_csv(casecross_dl_int_results, paste0("./data/health/",
  "1015-co_mortality_dl_int_results.csv"))

# category distributed lag -----------------------------------------------------
# pm category
pm_cat <- pm %>% 
  filter(str_sub(fips,start=1,end=2) %in% c("08")) %>% 
  select(fips, date, pm_krig, temp_f) %>% 
  mutate(pm_cat = factor(case_when(pm_krig <= 10 ~ "0-10", 
                                   pm_krig > 10 & pm_krig <= 20 ~ "10-20",
                                   pm_krig > 20 & pm_krig <= 30 ~ "20-30",
                                   pm_krig > 30 ~ ">30"), 
                         levels = c("0-10", "10-20", "20-30", ">30")),
         pm_cat0 = ifelse(pm_cat == "0-10", 1, 0),
         pm_cat1 = ifelse(pm_cat == "10-20", 1, 0),
         pm_cat2 = ifelse(pm_cat == "20-30", 1, 0),
         pm_cat3 = ifelse(pm_cat == ">30", 1, 0)) %>% 
  arrange(fips, date) %>% 
  group_by(fips) %>% 
  mutate(., !!!funlag(pm_cat0,6), !!!funlag(pm_cat1,6),
         !!!funlag(pm_cat2,6), !!!funlag(pm_cat3,6), !!!funlag(temp_f,6)) %>% 
  select(-pm_krig)
# bind pm cats to list and create lag 

# estimating for just respiratory and cvd
co_cc_list_cat <- casecross_list[c(1,4)] %>% 
  map(~ mutate(., outcome = as.numeric(as.character(outcome)),
               date = as.Date(as.character(date_of_death)),
               month = as.factor(lubridate::month(date))) %>% 
        # filter out 2016; I don't have pm data yet
        filter(date <= "2015-12-30") %>% 
        filter(month %in% 4:10) %>% 
        left_join(pm_cat, by = c("fips", "date"))) %>% 
  # add outcome name to each dataframe
  map2(.x = ., .y = outcomes[c(1,4)], ~mutate(.x, out_name = .y))

mort_dl_cat_results  <- lapply(co_cc_list_cat, function(x){
  # output dataframe from list
  data <- x %>% 
    mutate(date = as.Date(date),
           outcome = as.numeric(as.character(outcome))) %>%
    # remove missing lagged data
    filter(!is.na(pm_cat3_lag6))
  
  # output outcome name
  out_name <- as.character(unique(data$out_name))
  print(out_name)
  # create lagged matrix
  # dummy matrix
  pm_cat1_mat <- as.matrix(select(data, contains("pm_cat1")))
  pm_cat2_mat <- as.matrix(select(data, contains("pm_cat2")))
  pm_cat3_mat <- as.matrix(select(data, contains("pm_cat3")))
  # temp
  temp_mat <- as.matrix(select(data, contains("temp_f")))
  # define lagged basis spline
  exp_b <- ns(0:(ncol(pm_mat)-1), df = 3, intercept = T)
  # pm basis lagged dummy cat basis
  p_cat1_b <- pm_cat1_mat %*% exp_b
  p_cat2_b <- pm_cat2_mat %*% exp_b
  p_cat3_b <- pm_cat3_mat %*% exp_b
  # temp basis
  temp_b <- temp_mat %*% exp_b
  # run lagged model
  lag_mod <- clogit(outcome ~ p_cat1_b + p_cat2_b + p_cat3_b + temp_b + 
                      strata(id), data = data)
  
  # estimate lag estimate
  lag_est <- c("p_cat1_b", "p_cat2_b", "p_cat3_b") %>% 
    map_dfr(~distribute_that_lag(lag_mod, strata = .)) %>% 
    mutate(pm_strata = factor(
        case_when(strata == "p_cat1_b" ~ "10-20 ug/m^3",
                strata == "p_cat2_b" ~ "20-30 ug/m^3",
                strata == "p_cat3_b" ~ ">30 ug/m^3"), 
      levels = c("10-20 ug/m^3", "20-30 ug/m^3", ">30 ug/m^3")),
      outcome = out_name) 
  return(lag_est)
  }) %>%  # end smoke map
  map_dfr(.,rbind)


# stop time
stop <- Sys.time()
time <- stop - start
# print time
print(time)

# write file
write_csv(mort_dl_cat_results, 
          paste0("./data/health/1015-co_mortality_dl_cat_results.csv"))




