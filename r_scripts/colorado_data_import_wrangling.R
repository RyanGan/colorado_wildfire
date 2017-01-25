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
library(readxl)


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

summary(as.factor(co_hosp_df$ADMYY)) # 2010-2015 right
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

# checking on the missing value of 'dx1', 5 (can be ingored)
summary(as.factor(co_hosp_df$dx1))
samp_df_dx1 <- co_hosp_df %>% filter(is.na(dx1))
# checking the missing value of 'dx2', 3608
summary(as.factor(co_hosp_df$dx2))
samp_df_dx2 <- co_hosp_df %>% filter(is.na(dx2))
# checking the missing value of 'dx3', 20904
summary(as.factor(co_hosp_df$dx3))
samp_df_dx3 <- co_hosp_df %>% filter(is.na(dx3))
# checking the missing value of 'dx4', 53378
summary(as.factor(co_hosp_df$dx4))
samp_df_dx4  <- co_hosp_df %>% filter(is.na(dx4))
# checking the missing value of 'dx5', 99629
summary(as.factor(co_hosp_df$dx5))
samp_df_dx5 <- co_hosp_df %>% filter(is.na(dx5))

# checking Age, 3891 is missing
summary(as.factor(co_hosp_df$AGEYRS))
max(co_hosp_df$AGEYRS, na.rm = T)  # 820
samp_df_age <- co_hosp_df %>% filter(AGEYRS>= 150)  #46

# checking type of admission, 3916 is missing
summary(as.factor(co_hosp_df$ADMTNO))
samp_df_admtno <- co_hosp_df %>% filter(is.na(ADMTNO))
samp_df_admtno2 <- co_hosp_df %>% filter(ADMTNO==1|ADMTNO==2) # 1058762(70%)

# checking cardioresp, 3510 is missing
summary(as.factor(co_hosp_df$cardioresp))
samp_df_card <- co_hosp_df %>% filter(is.na(cardioresp)) 

# checking Ulna/radius fracture-related diagnosis code, 1502808 is missing
summary(as.factor(co_hosp_df$limbfx))
samp_df_lim <- co_hosp_df %>% filter(is.na(limbfx)) 

# checking wrfgrid id, 3886 is missing
summary(as.factor(co_hosp_df$WRFGRID_ID))
samp_df_wrf <- co_hosp_df %>% filter(is.na(WRFGRID_ID)) 

# check missing for state of residence
xtabs(~ACCSTATE, co_hosp_df)
xtabs(~ZIP, co_hosp_df)
co_hosp_df$ZIP
summary(co_hosp_df)



# ------------------------------------------------------------------------------
# Title: Cleaning CDPHE data
# Author: Jingyang Liu
# Date Created: 1/25/17
# R version: 3.3.2
# ------------------------------------------------------------------------------

# data cleaning of CDPHE
# at least a primary ICD9 code present
co_hosp_w_outcome_df <- co_hosp_df %>% filter(!is.na(dx1)) %>% 
  # filter to only ER or urgent care
  filter(ADMTNO==1|ADMTNO==2) %>% 
  # indicator for male=0, female=1
  mutate(sex_ind =ifelse(SEX == 2, 1, 
                  ifelse(SEX == 1, 0, NA)),
         age_ind = ifelse(AGEYRS < 15, 0,
                   ifelse(AGEYRS >= 15 & AGEYRS < 65, 1,
                   ifelse(AGEYRS >= 65 & AGEYRS <=110, 2, NA)))
         ) # end of mutate

ggplot(co_hosp_w_outcome_df, aes(co_hosp_w_outcome_df$AGEYRS)) +
         geom_density(aes(group=co_hosp_w_outcome_df$age_ind,
                          color=co_hosp_w_outcome_df$age_ind)) + xlim(0,110)

# Creating vectors of outcome claims -------------------------------------------
# import chars diagnosis code key
icd9_key <- read_excel("CMS32_DESC_LONG_SHORT_DX.xlsx")

# changing variable name, 'code' for 'diagonsis code'.
summary(icd9_key)
names(icd9_key)[1:3] <- c("code","long","short")

# Coding Respiratory Disease ICD-9 Outcomes ------------------------------------
# All Respiratory Diseases 460 to 519 ------------------------------------------
which(icd9_key$code == '460') # row 5067 (cold) start of resp outcomes
# last icd9 code in resp disease is 519.9 (unsp dis of resp sys)
which(icd9_key$code == '5199') # row 5321 end of resp outcomes

# sort by icd9 code add row variable 
icd9_key$X <- NULL
icd9_key <- arrange(icd9_key, code) %>%
  mutate(n = as.numeric(row.names(icd9_key)))

resp_icd9 <- filter(icd9_key, n >= 5067 & n <= 5321) %>%
  select(code)
c
resp_icd9 <- as.vector(as.matrix(resp_icd9))   

# CHARS indicator of resp_icd9
co_hosp_w_outcome_df <- co_hosp_w_outcome_df %>%
  mutate(resp1 = ifelse(dx1 %in% resp_icd9, 1, 0),
         resp2 = ifelse(dx2 %in% resp_icd9, 1, 0),
         resp3 = ifelse(dx3 %in% resp_icd9, 1, 0),
         resp4 = ifelse(dx4 %in% resp_icd9, 1, 0),
         resp5 = ifelse(dx5 %in% resp_icd9, 1, 0),
         # sum up the indicators
         resp_sum = (resp1 + resp2 + resp3 + resp4 +resp5 ),
         resp_dx = ifelse(resp_sum>0, 1, 0))

# primary dx of any resp outcome
xtabs(~ resp1, co_hosp_w_outcome_df)
188340/(188340+870418) # 0.1779

# Asthma, ICD-9 493 ------------------------------------------------------------
# try asthma 493 to 49392; identify rows with following code
which(icd9_key$code == '49300') # start of asthma is row 5206
which(icd9_key$code == '49392') # end of asthma is row 5219

# limit just to asthma code and just the diagnosis column
icd9_check <- filter(icd9_key, n >= 5206 & n <= 5219) 
icd9_check

asthma_icd9 <- filter(icd9_key, n >= 5206 & n <= 5219) %>%
  select(code)
# convert to vector
asthma_icd9 <- as.vector(as.matrix(asthma_icd9))   

# now can I make a new variable, asthma1, that indicates an asthma claim?
co_hosp_w_outcome_df <- co_hosp_w_outcome_df %>%
  mutate(asthma1 = ifelse(dx1 %in% asthma_icd9, 1, 0),
         asthma2 = ifelse(dx2 %in% asthma_icd9, 1, 0),
         asthma3 = ifelse(dx3 %in% asthma_icd9, 1, 0),
         asthma4 = ifelse(dx4 %in% asthma_icd9, 1, 0),
         asthma5 = ifelse(dx5 %in% asthma_icd9, 1, 0),
         # sum up the asthma indicators
         asthma_sum = (asthma1 + asthma2 + asthma3 + asthma4 + asthma5),
         asthma_dx = ifelse(asthma_sum>0, 1, 0))

# check if the binary asthma_dx code aligns with the sum
asthma_dx_check <- table(co_hosp_w_outcome_df$asthma_dx, co_hosp_w_outcome_df$asthma_sum)
asthma_dx_check

summary(co_hosp_w_outcome_df)

# seems to do the same thing, but both codes could be incorrect. i should check
# the number of asthma claims in diagnoses 1 
asthma_claims <- subset(co_hosp_w_outcome_df, dx1 %in% asthma_icd9)
asthma_claims$DIAG1 <- as.factor(asthma_claims$dx1)
# check asthma claims subset
summary(asthma_claims$dx1)

# number of asthma claims; first convert claims to as.factor
co_hosp_w_outcome_df$asthma_dx <- as.factor(co_hosp_w_outcome_df$asthma_dx)

summary(co_hosp_w_outcome_df$asthma_dx)
# percentage of claims that are asthma claims
(69429/(69429+989329)) * 100 # 6.558%

rm(asthma_claims) # remove to save space
# 6.558% of claims in Washington in 2012 were related to asthma

# checking if all data are in right range
xtabs(~asthma1 + dx1, asthma_check )
glimpse(asthma_check)
asthma_check <- co_hosp_w_outcome_df %>% filter(asthma1 == 1)

# Creating a Permanent DataFrame -----------------------------------------------
# write a permanent chars confidential dataset
write_path <- paste0('C:/Users/jyliu/Desktop/local_git_repo/colorado_wildfire/',
                     'co_hosp_w_outcome_df.csv')
write_csv(co_hosp_w_outcome_df, write_path)

