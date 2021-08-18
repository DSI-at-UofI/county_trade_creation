library(tidyverse)
library(readstata13)

# From: Noé Nava
# Modified: Jian Zou 
# Date: 08/16/2021

rm(list = ls())
# Adding climate region to dyadic datasets
key <- read_csv(file = 'assets/state_clm_region.csv')
key <- key %>%
  select(long_name, clm_region)

years <- c("2017")
#for(y in years) { #states
  
#  directory <- 'data/dyadic_datasets/'
#  file <- paste0(directory, "dyadic_state_", y, ".dta", sep = "")
#  df <- read.dta13(file = file)
  
#  df <- left_join(df, key, by = c('orig' = 'long_name'))
#  names(df)[length(df)] <- "clm_region_i"
#  
#  df <- left_join(df, key, by = c('dest' = 'long_name'))
#  names(df)[length(df)] <- "clm_region_j"
  
#  save.dta13(df, file = file)

#}
y = "2017"
for(y in years) { #county
  
  directory <- 'data/'
  file <- paste0(directory, "dyadic_county_", y, ".dta", sep = "")
  df <- read.dta13(file = file)
  
  df <- left_join(df, key, by = c('orig_stName' = 'long_name'))
  names(df)[length(df)] <- "clm_region_i"
  
  df <- left_join(df, key, by = c('dest_stName' = 'long_name'))
  names(df)[length(df)] <- "clm_region_j"
  
  save.dta13(df, file = file)
  
}
