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