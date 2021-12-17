rm(list = ls())
options(scipen = 9999)

# Noe J Nava
# noejn2@illinois.edu // noejnava2@gmail.com
# https://noejn2.github.io

# Purpose: ----------------
# Calibrate a hurdle model with state level data to calcuate the probability
# of dyadic-conty observations to have trade

library(tidyverse)
library(readstata13)
library(pscl)

### Obtaining the datasets
st_trade_flows <- read.dta13(file = "output/st_trade_flows_mean.dta")
dy_cnty <- readRDS(file = 'output/dyadic_county_2017.rds')

### Doing some cleaning
## so both the new data (dy_cnty) and the old data (st_trade_flows) have same
## variable names
dy_cnty <- dy_cnty %>%
  mutate(distance = log(distance + head_mayer_2002)) %>%
  mutate(sales_i = log(sales_i)) %>%
  mutate(gdp = log(gdp)) %>%
  rename(gdp_j = gdp) %>%
  select(orig,
         dest,
         orig_stName,
         dest_stName,
         orig_stIni,
         dest_stIni,
         distance,
         sales_i,
         gdp_j)

## Getting rid of some zeroes
dy_cnty$sales_i[is.na(dy_cnty$sales_i)] <- 0
dy_cnty$gdp_j[is.na(dy_cnty$gdp_j)] <- 0 

dy_cnty$sales_i[is.infinite(dy_cnty$sales_i)] <- 0
dy_cnty$gdp_j[is.infinite(dy_cnty$gdp_j)] <- 0 

dy_cnty$distance[is.na(dy_cnty$distance)] <- 0
dy_cnty$distance[is.infinite(dy_cnty$distance)] <- 0

## Create categorical variable for trade (1 == trade exists)
st_trade_flows$trade[is.na(st_trade_flows$trade)] <- 0
st_trade_flows <- st_trade_flows %>% 
  mutate(istrade = if_else(trade != 0, 1, 0))

## Create dummy for 2017
st_trade_flows$y17 <- ifelse(st_trade_flows$year == 2017, 1, 0)

## List of FE names for climate regions (origin and destination) at both years
list_FE <- names(st_trade_flows)[grepl("^orig_FE", names(st_trade_flows))]
list_FE <- append(list_FE, names(st_trade_flows)[grepl("^dest_FE", names(st_trade_flows))])

## Put previous FE into dy_cnty dataset
temp <- st_trade_flows %>% 
  select(c(orig_ini, dest_ini, year, list_FE, intra, contiguity, y17)) %>%
  filter(year == 2017) %>%
  select(!year)

dy_cnty <- left_join(dy_cnty, temp, by = c("orig_stIni" = "orig_ini", 
                                           "dest_stIni" = "dest_ini"))

### FE structure for regression formula
FE_formula <- ""
for(i in 1:18) {
  orig_FE <- paste0("orig_FE", i)
  FE_formula <- paste(FE_formula, orig_FE, sep = " + ")
  
  dest_FE <- paste0("dest_FE", i)
  FE_formula <- paste(FE_formula, dest_FE, sep = " + ")
}
reg_formula <- "istrade ~ distance + sales_i + gdp_j + y17"

hmodel <- hurdle(istrade ~ distance + sales_i + gdp_j + y17 + orig_FE1 + dest_FE1 + orig_FE2 + dest_FE2 + orig_FE3 + dest_FE3 + orig_FE4 + dest_FE4 + orig_FE5 + dest_FE5 + orig_FE6 + dest_FE6 + orig_FE7 + dest_FE7 + orig_FE8 + dest_FE8 + orig_FE9 + dest_FE9 + orig_FE10 + dest_FE10 + orig_FE11 + dest_FE11 + orig_FE12 + dest_FE12 + orig_FE13 + dest_FE13 + orig_FE14 + dest_FE14 + orig_FE15 + dest_FE15 + orig_FE16 + dest_FE16, 
       data = st_trade_flows, 
       dist = "poisson", 
       zero.dist = "binomial")

st_trade_flows$probs_st <- predict(hmodel,
                                   newdata = st_trade_flows,
                                   type = "response")

dy_prob_St <- st_trade_flows %>% # Saving state probs from model 9
  filter(year == 2017) %>%
  select(orig, dest, probs_st)
saveRDS(dy_prob_St, file = "output/dy_state_probs_v2.rds")

dy_cnty$probs <-  predict(hmodel, newdata = dy_cnty, type = "response")
dy_cnty_probs <- dy_cnty %>% select(orig, dest, probs)
sum(dy_cnty_probs$probs > .5)
saveRDS(dy_cnty_probs, file = "output/dy_cnty_probs_v2.rds")
#end