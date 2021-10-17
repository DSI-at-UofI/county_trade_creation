rm(list = ls())
options(scipen = 9999)

# Noe J Nava
# noejn2@illinois.edu // noejnava2@gmail.com
# https://noejn2.github.io

# Purpose: ----------------
# Calibrate a logit model with state level data to calcuate the probability
# of dyadic-conty observations to have trade

# Conclusions: ---------
# Gains from having different model specication (year or intra), and
# having different estimators (Machine learning, normal logistic and King and Zeng)
# are minimal. 
# Using Konar's paper as benchmark, we predict substantially lower observations
# We employ model 9 since it gives us the highest number of ones.

groundhog_day <- "2021-09-05"
pkgs <- c("tidyverse", "readstata13")
groundhog::groundhog.library(pkgs, 
                             groundhog_day,
                             tolerate.R.version = '4.0.3')

dy_cnty <- readRDS(file = 'output/dyadic_county_2017.rds')
dy_stat <- read.dta13(file = 'output/dyadic_state_2017_merge.dta')
dy_prob <- readRDS(file = "output/dy_cnty_probs.rds")
dy_prob_st <- readRDS(file = "output/dy_state_probs.rds") 

# Setting-up the dataset for county flows -----
# The state level dataset is created for the county level
dy_cnty <- dy_cnty %>%
  select(orig,
         dest,
         orig_stName,
         dest_stName)

dy_cnty <- left_join(dy_cnty, 
                     dy_prob_st, 
                     by = c("orig_stName" = "orig",
                            "dest_stName" = "dest"))

# The following merges the necessary variables from the state level
# dataset with our county level dataset  
dy_stat <- dy_stat %>%
  mutate(istrade = if_else(notrade == 0, 1, 0)) %>%
  select(orig_stName,
         dest_stName,
         istrade)

dy_cnty <- left_join(dy_cnty,
                     dy_stat,
                     by = c("orig_stName",
                            "dest_stName"))

dy_cnty <- left_join(dy_cnty, dy_prob, by = c("orig", "dest"))
dy_cnty$posterior <- (dy_cnty$istrade*dy_cnty$probs)/dy_cnty$probs_st


sum(dy_cnty$istrade == 1 & dy_cnty$probs > PofB)



sum(dy_cnty$istrade & dy_cnty$probs < .5)
sum(dy_cnty$istrade & dy_cnty$posterior < .5)











