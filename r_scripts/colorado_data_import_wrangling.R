# ------------------------------------------------------------------------------
# Title: Script for data import and management of CDPHE data
# Author: Jingyang Liu
# Date Created: 1/24/17
# R version: 3.3.2
# ------------------------------------------------------------------------------

# load libraries
library(tidyverse)
library(data.table)

# define relative path for csv file
file_path <- paste0("./data/co_hosp_1015.csv")

# read in dataframe
co_hosp_df <- read_csv(file_path)

samp_df <- co_hosp_df[5434, ]

summary(as.factor(co_hosp_df$dx1))

samp_df <- co_hosp_df %>% filter(is.na(dx1))
