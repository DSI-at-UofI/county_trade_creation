rm(list = ls())

library(tidyverse)
options(scipen = 9999)


# Noe J Nava
# noejn2@illinois.edu
# https://noejn2.github.io/
# Script creates the dyadic_county_flows_adjusted.rds, which is our 
# main dataset in this repository
# Code takes approximately 15 minutes
# on Intel(R) Xeon(R) CPU E3-12250 v6 3.00Ghz 
# 64 GB ram
# Mumford computer

dy_stat <- readstata13::read.dta13(file = 'output/dyadic_state_2017_merge.dta')


dist_eta <- -0.4495181

sale_eta <- 0.6802411

gdpj_eta <- 0.9762757

cnst     <- -18.11583 

key <- read_csv(file = 'assets/state_clm_region.csv')
key <- key %>%
  select(initial, clm_region) %>%
  mutate(initial = toupper(initial))

key <- key[!is.na(key$clm_region),]

southeast <- key$initial[key$clm_region == "southeast"]
southwest <- key$initial[key$clm_region == "southwest"]
south <- key$initial[key$clm_region == "south"]
west <- key$initial[key$clm_region == "west"]
northeast <- key$initial[key$clm_region == "northeast"]
northwest <- key$initial[key$clm_region == "northwest"]
central <- key$initial[key$clm_region == "central"]
east_north_central <- key$initial[key$clm_region == "east north central"]
west_north_central <- key$initial[key$clm_region == "west north central"]

dy_stat <- dy_stat %>%
  filter(orig_stName %in% c("louisiana", "washington") | dest_stName %in% c("louisiana", "washington"))
# Climate region
for(ini in c("LA", "WA")) {#origin is region dest is state
  if(ini == "LA") {
    FE <- -.1931769 # FE associated with LA on the destination
  }else{
    FE <- .9586682 # FE associated with WA on the destination
  }
  
  dy_stat$sum_FE[dy_stat$orig_stIni %in% central & dy_stat$dest_stIni == ini] <- .246745 + FE
  dy_stat$sum_FE[dy_stat$orig_stIni %in% east_north_central & dy_stat$dest_stIni == ini] <- .2517795 + FE
  dy_stat$sum_FE[dy_stat$orig_stIni %in% northeast & dy_stat$dest_stIni == ini] <- .0216743 + FE
  dy_stat$sum_FE[dy_stat$orig_stIni %in% northwest & dy_stat$dest_stIni == ini] <-  .1990032 + FE
  dy_stat$sum_FE[dy_stat$orig_stIni %in% south & dy_stat$dest_stIni == ini] <- .5277888 + FE
  dy_stat$sum_FE[dy_stat$orig_stIni %in% southeast & dy_stat$dest_stIni == ini] <- .2361734 + FE
  dy_stat$sum_FE[dy_stat$orig_stIni %in% southwest & dy_stat$dest_stIni == ini] <- .6013342 + FE
  dy_stat$sum_FE[dy_stat$orig_stIni %in% west & dy_stat$dest_stIni == ini] <- 0 + FE
  dy_stat$sum_FE[dy_stat$orig_stIni %in% west_north_central & dy_stat$dest_stIni == ini] <- .7725402 + FE

}

for(ini in c("LA", "WA")) {#destination is region origin is state
  if(ini == "LA") {
    FE <- .5277888 # FE associated with LA on the origin
  }else{
    FE <- .1990032 # FE associated with WA on the origin
  }
  
  dy_stat$sum_FE[dy_stat$dest_stIni %in% central & dy_stat$orig_stIni == ini] <-  -.7289201 + FE
  dy_stat$sum_FE[dy_stat$dest_stIni %in% east_north_central & dy_stat$orig_stIni == ini] <- -.4574307 + FE
  dy_stat$sum_FE[dy_stat$dest_stIni %in% northeast & dy_stat$orig_stIni == ini] <-  -.1415597  + FE
  dy_stat$sum_FE[dy_stat$dest_stIni %in% northwest & dy_stat$orig_stIni == ini] <- .9586682   + FE
  dy_stat$sum_FE[dy_stat$dest_stIni %in% south & dy_stat$orig_stIni == ini] <-  -.1931769 + FE
  dy_stat$sum_FE[dy_stat$dest_stIni %in% southeast & dy_stat$orig_stIni == ini] <-  .6142957  + FE
  dy_stat$sum_FE[dy_stat$dest_stIni %in% southwest & dy_stat$orig_stIni == ini] <-  .3942852  + FE
  dy_stat$sum_FE[dy_stat$dest_stIni %in% west & dy_stat$orig_stIni == ini] <- .3494075  + FE
  dy_stat$sum_FE[dy_stat$dest_stIni %in% west_north_central & dy_stat$orig_stIni == ini] <-  -.6121513  + FE
}


# contiguity
dy_stat$sum_FE[dy_stat$orig_stIni == "LA" & dy_stat$dest_stIni == "AR"] <- .7813085 + dy_stat$sum_FE[dy_stat$orig_stIni == "LA" & dy_stat$dest_stIni == "AR"]
dy_stat$sum_FE[dy_stat$orig_stIni == "WA" & dy_stat$dest_stIni == "ID"] <- .7813085 + dy_stat$sum_FE[dy_stat$orig_stIni == "WA" & dy_stat$dest_stIni == "ID"]
dy_stat$sum_FE[dy_stat$orig_stIni == "TX" & dy_stat$dest_stIni == "LA"] <- .7813085 + dy_stat$sum_FE[dy_stat$orig_stIni == "TX" & dy_stat$dest_stIni == "LA"]
dy_stat$sum_FE[dy_stat$orig_stIni == "MS" & dy_stat$dest_stIni == "LA"] <- .7813085 + dy_stat$sum_FE[dy_stat$orig_stIni == "MS" & dy_stat$dest_stIni == "LA"]
dy_stat$sum_FE[dy_stat$orig_stIni == "AR" & dy_stat$dest_stIni == "LA"] <- .7813085 + dy_stat$sum_FE[dy_stat$orig_stIni == "AR" & dy_stat$dest_stIni == "LA"]
dy_stat$sum_FE[dy_stat$orig_stIni == "LA" & dy_stat$dest_stIni == "MS"] <- .7813085 + dy_stat$sum_FE[dy_stat$orig_stIni == "LA" & dy_stat$dest_stIni == "MS"]
dy_stat$sum_FE[dy_stat$orig_stIni == "WA" & dy_stat$dest_stIni == "OR"] <- .7813085 + dy_stat$sum_FE[dy_stat$orig_stIni == "WA" & dy_stat$dest_stIni == "OR"]
dy_stat$sum_FE[dy_stat$orig_stIni == "LA" & dy_stat$dest_stIni == "TX"] <- .7813085 + dy_stat$sum_FE[dy_stat$orig_stIni == "LA" & dy_stat$dest_stIni == "TX"]
dy_stat$sum_FE[dy_stat$orig_stIni == "OR" & dy_stat$dest_stIni == "WA"] <- .7813085 + dy_stat$sum_FE[dy_stat$orig_stIni == "OR" & dy_stat$dest_stIni == "WA"]
dy_stat$sum_FE[dy_stat$orig_stIni == "ID" & dy_stat$dest_stIni == "WA"] <- .7813085 + dy_stat$sum_FE[dy_stat$orig_stIni == "ID" & dy_stat$dest_stIni == "WA"]

# intra
dy_stat$sum_FE[dy_stat$orig_stIni == "LA" & dy_stat$dest_stIni == "LA"] <- 3.526352 + dy_stat$sum_FE[dy_stat$orig_stIni == "LA" & dy_stat$dest_stIni == "LA"]
dy_stat$sum_FE[dy_stat$orig_stIni == "WA" & dy_stat$dest_stIni == "WA"] <- 3.526352 + dy_stat$sum_FE[dy_stat$orig_stIni == "WA" & dy_stat$dest_stIni == "WA"]

dy_stat <- dy_stat %>%
  mutate(distance_c = distance*dist_eta) %>%
  mutate(sales_i_c  = sales_i*sale_eta) %>%
  mutate(gdp_j_c    = gdp_j*gdpj_eta) %>%
  mutate(FE_w       = (cnst + sum_FE)) %>% # check if you want to use weights
  mutate(st_flows = exp(distance_c + sales_i_c + gdp_j_c + FE_w),
         ratio = ppml_hat/st_flows)

dy_state <- dy_stat %>%
  select(orig_stName,
         dest_stName,
         ppml_hat,
         st_flows,
         ratio)
#end