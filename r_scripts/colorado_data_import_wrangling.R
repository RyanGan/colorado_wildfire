# ------------------------------------------------------------------------------
# Title: Script for data import and management of CDPHE data
# Author: Jingyang Liu
# Date Created: 1/24/17
# R version: 3.3.2
# ------------------------------------------------------------------------------

# load libraries
library(tidyverse)
library(data.table)
library(dplyr)

# define relative path for csv file
file_path <- paste0("./data/co_hosp_1015.csv")

# read in dataframe (Warning: 7802 parsing failures)
co_hosp_df <- read_csv(file_path)

# checking on what is in dataframe
glimpse(co_hosp_df)

# checking dates
summary(as.factor(co_hosp_df$admit))
as.character(co_hosp_df$admit) # needs to be character

summary(as.factor(co_hosp_df$ADMMM))
as.character(co_hosp_df$ADMMM) # needs to be character

summary(as.factor(co_hosp_df$ADMDD))
as.character(co_hosp_df$ADMDD) # needs to be character
    # checking if date 31 in right months (yes)
    samp_df_admdd1 <- co_hosp_df %>% filter(ADMDD==31)
    samp <- samp_df_admdd1 %>% filter(ADMMM %in% c(2,4,6,9,11))

    # checking if date 29 and 30 in right months (yes)
    samp_df_admdd2 <- co_hosp_df %>% filter(ADMDD==29|ADMDD==30)
    samp2 <- samp_df_admdd2 %>% filter(ADMMM==2)

summary(as.factor(co_hosp_df$ADMYY)) # 2010-2015
as.numeric(co_hosp_df$ADMYY) # needs to be numeric

summary(as.factor(co_hosp_df$ADMHH)) # one missing, ten shows 34
as.character(co_hosp_df$ADMHH) # needs to be character
samp_df_ADMHH <- co_hosp_df %>% filter(ADMHH==34)
samp_df_ADMHH2 <- co_hosp_df %>% filter(is.na(ADMHH))

# checking sex, five are 3, and another one is missing
summary(as.factor(co_hosp_df$SEX))
samp_df_sex <- co_hosp_df %>% filter(is.na(SEX))
samp_df_sex2 <- co_hosp_df %>% filter(SEX==3)

# checking Zip, 96 are missing
summary(as.factor(co_hosp_df$ZIP))
samp_df_zip <- co_hosp_df %>% filter(is.na(ZIP))

# checking race, 295 are missing, four data shows 8 and 21 data shows 9
summary(as.factor(co_hosp_df$RACE))
samp_df_race <- co_hosp_df %>% filter(is.na(RACE))
samp_df_race2 <- co_hosp_df %>% filter(RACE==8|RACE==9)

# 


# checking on the missing value of 'dx1', 5 (can be ingored)
summary(as.factor(co_hosp_df$dx1))
samp_df <- co_hosp_df %>% filter(is.na(dx1))



# checking type of admission, 3916 is missing
summary(as.factor(co_hosp_df$ADMTNO))
samp_df_admtno <- co_hosp_df %>% filter(is.na(ADMTNO))







