# Creation of lagged PM2.5 data frame-----
# read grid pm
grid_pm <- read_csv("../../data/smoke/1015-grid_pm.csv", col_types = "cDdddd") %>% 
  mutate(pm_diff_grid = pm25_grid - sbg_pm_grid,
         month = as.factor(lubridate::month(date)), # extract month as factor
         year = as.factor(lubridate::year(date)), # extract year as factor
         season = as.factor(case_when(month %in% c(12, 1, 2) ~ "winter",
                                      month %in% c(3:5) ~ "spring",
                                      month %in% c(6:8) ~ "summer",
                                      month %in% c(9:11)~ "fall")),
         # creating binary smoke to hms to be equal to 1 
         # and difference between estimate and seasonal background to be >0
         # and to be within the month of April to Octoboer
         smk0_hms_g = ifelse(pm_diff_grid > 0 & month %in% c(4:10) & 
                               hms_grid == 1, 1, 0),
         smk5_hms_g = ifelse(pm_diff_grid > 5 & month %in% c(4:10) & 
                               hms_grid == 1, 1, 0),
         smk10_hms_g = ifelse(pm_diff_grid > 10 & month %in% c(4:10) & 
                                hms_grid ==  1, 1, 0),
         smk15_hms_g = ifelse(pm_diff_grid > 15 & month %in% c(4:10) & 
                                hms_grid ==  1, 1, 0),
         # transforming pm kriged estimates to a 10 unit increase in pm
         pm25_g_10u = pm25_grid/10) %>% 
  # sorting by fips and date to estimate lag for each county by date
  arrange(GRID_ID, date) %>% 
  # group by fips 
  group_by(GRID_ID) %>%
  # apply funlag to create lagged estimates
  mutate(., !!!funlag(pm25_g_10u,6), 
         !!!funlag(smk0_hms_g,6), !!!funlag(smk5_hms_g,6),
         !!!funlag(smk10_hms_g,6), !!!funlag(smk15_hms_g,6), 
         !!!funlag(temp_f_grid, 6))


# estiamte of same day associatoin ------
# outcome name vector without asthma
outcome_name <- outcomes[-2]
# same-day association with pm and mortality during wildfire season
pm_grid_results <- co_mort_cc_list %>%
  map_dfr(. , function(df){
    mod <- clogit(outcome ~ pm25_g_10u + temp_f_grid + strata(id), data = df)
    est <- broom::tidy(mod) %>% filter(term == "pm25_g_10u") %>% 
      select(estimate, std.error, conf.low, conf.high) %>% 
      rename(se = std.error, lower_95 = conf.low, upper_95 = conf.high) %>% 
      mutate_at(vars(estimate, lower_95, upper_95), funs(round(exp(.),3))) %>% 
      mutate(level = "grid") 
  }) %>% # end map
  cbind(outcome_name, .)


# plot of same day association -----
# bind dataframes
plot_df <- bind_rows(pm_grid_results, pm_co_results)

# plot
plot <- ggplot(plot_df, aes(x=level, y = estimate, colour = level)) +
  geom_point() +
  geom_errorbar(aes(ymin=lower_95, ymax=upper_95), width = 0.3) +
  scale_color_manual(values = c("#9cecfb", "#ff00cc")) +
  geom_hline(yintercept = 1, linetype = "dashed", colour = "red") +
  facet_wrap(~outcome_name, scales = "free_y") +
  ylab("Odds Ratio: 10 ug/m^3") +
  xlab("Outcome") +
  ggtitle(paste0("Association between PM2.5 and Mortality")) +
  ryan_theme +
  theme(panel.grid.major.x = element_blank(),
        panel.grid.major.y = element_line(linetype = "dotted"),
        panel.grid.minor = element_blank(),
        axis.text.x = element_text(angle = 90, hjust=0.95, vjust = 0.75))
# print plot
plot

# intreaction
### Estimating Effect of PM~2.5~ on Smoke Days and Non-Smoke Days


# start time
start_time <- Sys.time()
# distributed lag function
mort_dl_pm_int_results  <- lapply(co_mort_cc_list, function(x){
  # output dataframe from list
  data <- x %>% 
    mutate(date = as.Date(date),
           outcome = as.numeric(as.character(outcome))) %>%
    # remove missing lagged data
    filter(!is.na(pm25_g_10u_lag6))
  # output outcome name
  out_name <- as.character(unique(data$out_name))
  # print(out_name) # track which outcome dataframe it's on
  # create lagged matrix
  pm_mat <- as.matrix(select(data, contains("pm25_g_10u")))
  temp_mat <- as.matrix(select(data, contains("temp_f_grid")))
  # define lagged basis spline
  exp_b <- ns(0:(ncol(pm_mat)-1), df = 4, intercept = T)
  # temp basis
  temp_basis <- temp_mat %*% exp_b
  # create vector of smoke var to estimate over
  smoke_var <- c("smk0_hms_g", "smk5_hms_g", "smk10_hms_g")
  
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
      lag_mod <- clogit(outcome ~ pm_smk_b + pm_nosmk_b + smk_basis + 
                          temp_basis + strata(id), data = data)
      # define strata terms
      strata_terms <- c("pm_smk_b", "pm_nosmk_b", "smk_basis")
      # estimate cumulative and lagged effect for each basis
      
      lagged_estimates <- strata_terms %>% 
        map_dfr(~distribute_that_lag(lag_mod = lag_mod, strata = ., 
                                     exposure_basis = exp_b)) %>% 
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
stop_time <- Sys.time() - start_time





cumulative_nosmk <- mort_dl_pm_int_results %>% 
  filter(type == "cumulative" & smoke == "smk5_hms_g" & 
           strata == "pm_nosmk_b" & outcome != "card_arrest")

# plot results
plot_nosmk <- ggplot(cumulative_nosmk, aes(x=time, y=odds_ratio)) +
  geom_line(colour = "blue", size = 1) +
  geom_ribbon(aes(ymin = lower_95, ymax = upper_95), 
              fill = "blue", alpha = 0.3) + 
  scale_x_continuous(breaks = c(seq(0,7, by=1))) +
  geom_hline(yintercept = 1, linetype = 2, colour = "red") +
  # adding facet wrap to estimate for each outcome
  facet_wrap(~outcome, scales = "free_y") +
  ylab(expression("Odds Ratio")) +
  xlab("Lagged Days") +
  ggtitle("No Smoke") +
  theme(panel.grid.major.x = element_blank(),
        panel.grid.major.y = element_line(linetype = "dotted"),
        panel.grid.minor = element_blank()) +
  theme_minimal()

# print plot
print(plot_nosmk)




cumulative_smk <- mort_dl_pm_int_results %>% 
  filter(type == "cumulative" & smoke == "smk5_hms_g" & 
           strata == "pm_smk_b" &  outcome != "card_arrest")

# plot results
plot_smk <- ggplot(cumulative_smk, aes(x=time, y=odds_ratio)) +
  geom_line(colour = "blue", size = 1) +
  geom_ribbon(aes(ymin = lower_95, ymax = upper_95), 
              fill = "blue", alpha = 0.3) + 
  scale_x_continuous(breaks = c(seq(0,7, by=1))) +
  geom_hline(yintercept = 1, linetype = 2, colour = "red") +
  # adding facet wrap to estimate for each outcome
  facet_wrap(~outcome, scales = "free_y") +
  ylab(expression("Odds Ratio")) +
  xlab("Lagged Days") +
  ggtitle("Smoke") +
  theme(panel.grid.major.x = element_blank(),
        panel.grid.major.y = element_line(linetype = "dotted"),
        panel.grid.minor = element_blank()) +
  theme_minimal()

# print plot
print(plot_smk)
