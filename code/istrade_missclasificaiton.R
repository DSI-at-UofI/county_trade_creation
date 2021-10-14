rm(list = ls())

# Noe J Nava
# noejn2@illinois.edu // noejnava2@gmail.com
# https://noejn2.github.io/

# Purpose: ----------------
# Here, I study the dyads that have probabilities of having trade
# even though they actually have trade

# Conclusion for this exercise:

# Miss-classification occurs whenever we have trade (either intra or inter)
# that is lower than average. Therefore, we are miss-classifying mostly at 
# the lower bound. 

# There are 533 dyads being missclassified as no trade, whenever there should be
# trade. Out of these, 444 are missclassified whenever trade is lower than their
# avearge. The rest have trade above average.

# Out of the 89 that have more values for their main variables than average,
# 60 have lower distance than average. 

# I believe that the major conclusions is that we are excluding all those
# dyads that have the lowest probability.

# set-up ------
options(scipen = 9999)

groundhog_day <- "2021-09-05"
pkgs <- c("tidyverse", "readstata13")
groundhog::groundhog.library(pkgs, 
                             groundhog_day,
                             tolerate.R.version = '4.0.3')

st_trade_flows <- read.dta13(file = 'output/st_trade_flows.dta')
st_trade_flows <- st_trade_flows %>%
  filter(year == 2017) %>%
  select(orig,
         dest,
         trade,
         sales_i,
         gdp_j,
         distance,
         contiguity)

which_errors <- readRDS(file = 'diagnostics/which_errors.rds')
which_errors$error <- 1

st_trade_flows <- left_join(st_trade_flows,
                             which_errors,
                             by = c("orig", "dest"))
st_trade_flows$error[is.na(st_trade_flows$error)] <- 0

st_list <- st_trade_flows %>% distinct(orig) %>% pull()

# Firs question: Any dyad has values lower than their means? -----

# Notes about the exercise:
# Notice that trade is the single biggest determine that explains an 
# mistaken non-trade value for the dyad (whenever there should be trade)
# The number is not even that high when we interact with mean gdp or mean sales.

# In fact, the number of right predictions whenever there is trade and trade
# is less than their mean is just 13. This is telling you that the dyads
# that we are getting wrong are the very low values.

# Another finding is that there are 419 of the missclassifications that have at 
# least one of their values with lower than their means, but there is only 25
# values that have lower than their means but were correctly classified as ones.

mean_trade <- mean(st_trade_flows$trade)
mean_sales <- mean(st_trade_flows$sales_i)
mean_gdp   <- mean(st_trade_flows$gdp_j)

st_trade_flows <- st_trade_flows %>%
  mutate(less_trade = if_else(trade < mean_trade, 1, 0)) %>%
  mutate(less_sales = if_else(sales_i < mean_sales, 1, 0)) %>%
  mutate(less_gdp = if_else(gdp_j < mean_gdp, 1, 0))

# All three: 34
st_trade_flows %>%
  filter(error == 1) %>%
  filter(less_trade == 1) %>%
  filter(less_sales == 1) %>%
  filter(less_gdp == 1) %>%
  select(orig, dest)

# Only trade and sales: 105
st_trade_flows %>%
  filter(error == 1) %>%
  filter(less_trade == 1) %>%
  filter(less_sales == 1) %>%
  select(orig, dest)

# Only trade and gdp: 117
st_trade_flows %>%
  filter(error == 1) %>%
  filter(less_trade == 1) %>%
  filter(less_gdp == 1) %>%
  select(orig, dest)

# Only gdp and sales: 36
st_trade_flows %>%
  filter(error == 1) %>%
  filter(less_sales == 1) %>%
  filter(less_gdp == 1) %>%
  select(orig, dest)

# None of them: 89
st_trade_flows %>%
  filter(error == 1) %>%
  filter(less_trade == 0) %>%
  filter(less_sales == 0) %>%
  filter(less_gdp == 0) %>%
  select(orig, dest)

# Only trade: 419
st_trade_flows %>%
  filter(error == 1) %>%
  filter(less_trade == 1) %>%
  select(orig, dest)

# Only gdp: 135
st_trade_flows %>%
  filter(error == 1) %>%
  filter(less_gdp == 1) %>%
  select(orig, dest)

# Only sales: 114
st_trade_flows %>%
  filter(error == 1) %>%
  filter(less_sales == 1) %>%
  select(orig, dest)

# Number of dyads that have less than at least one of the means but incorrectly classified: 444
st_trade_flows %>%
  filter(error == 1) %>%
  filter(less_sales == 1 | less_trade == 1 | less_gdp == 1) %>%
  select(orig, dest)

# Number of dyads that have less than at least one of the means but correctly classified as 1: 26
st_trade_flows %>%
  filter(error == 0 & trade > 0) %>%
  filter(less_sales == 1 | less_trade == 1 | less_gdp == 1) %>%
  select(orig, dest)

# How many correct ones are predicted whenever trade is less than mean?: 13
st_trade_flows %>%
  filter(error == 0 & trade > 0) %>%
  filter(less_trade == 1) %>%
  select(orig, dest)

# Second question: Any intra dyad indicates no trade, whenever we have trade? -----

# Notes about the exercise:
# There is 7 dyads that have intra trade, but were missclassified as no trade
# Three of these cases (Florida, Georgia and Montana) have trade whose trade values
# are above the average. But we notice that Montana has an average gdp lower
# than average, and Florida and Georgia have averages that are lower than average
 
# How many dyads?: 7
st_trade_flows %>%
  filter(orig == dest) %>%
  filter(error == 1) %>%
  select(orig, dest)

# How many dyads with less than mean trade?: 4
st_trade_flows %>%
  filter(orig == dest) %>%
  filter(error == 1) %>%
  filter(less_trade == 1) %>%
  select(orig, dest)

# How many dyads with less than mean gdp?: 5 --  Montana here
st_trade_flows %>%
  filter(orig == dest) %>%
  filter(error == 1) %>%
  filter(less_gdp == 1) %>%
  select(orig, dest)

# How many dyads with less than mean gdp?: 6 -- Florida and Georgia
st_trade_flows %>%
  filter(orig == dest) %>%
  filter(error == 1) %>%
  filter(less_sales == 1) %>%
  select(orig, dest)

# Third question: How many times each state is missclasiffied? ----

# Notes about the exercise:

# Whenever a state is exporter, the single biggest predictor for miss-classification
# is that trade from there is lower than the average. 

# Whenever a state is importer, the single biggest predictor for miss-classification
# is that trade to there is lower than average.

# The exporters:
tab1 <- st_trade_flows %>%
  filter(error == 1) %>%
  select(orig) %>% 
  count(orig) %>%
  rename(all = n)

tab2 <- st_trade_flows %>%
  filter(error == 1) %>%
  filter(less_trade == 1) %>%
  select(orig) %>% 
  count(orig) %>%
  rename(less_trade = n)

tab3 <- st_trade_flows %>%
  filter(error == 1) %>%
  filter(less_gdp == 1) %>%
  select(orig) %>% 
  count(orig) %>%
  rename(less_gdp = n)

tab4 <- st_trade_flows %>%
  filter(error == 1) %>%
  filter(less_sales == 1) %>%
  select(orig) %>% 
  count(orig) %>%
  rename(less_sales = n)

tab_expo <- left_join(tab1, tab2, by = "orig")
tab_expo <- left_join(tab_expo, tab3, by = "orig")
tab_expo <- left_join(tab_expo, tab4, by = "orig")
tab_expo[is.na(tab_expo)] <- 0

# The importers:
tab1 <- st_trade_flows %>%
  filter(error == 1) %>%
  select(dest) %>% 
  count(dest) %>%
  rename(all = n)

tab2 <- st_trade_flows %>%
  filter(error == 1) %>%
  filter(less_trade == 1) %>%
  select(dest) %>% 
  count(dest) %>%
  rename(less_trade = n)

tab3 <- st_trade_flows %>%
  filter(error == 1) %>%
  filter(less_gdp == 1) %>%
  select(dest) %>% 
  count(dest) %>%
  rename(less_gdp = n)

tab4 <- st_trade_flows %>%
  filter(error == 1) %>%
  filter(less_sales == 1) %>%
  select(dest) %>% 
  count(dest) %>%
  rename(less_sales = n)

tab_impo <- left_join(tab1, tab2, by = "dest")
tab_impo <- left_join(tab_impo, tab3, by = "dest")
tab_impo <- left_join(tab_impo, tab4, by = "dest")
tab_impo[is.na(tab_impo)] <- 0
  

# Fourth question: Who are the instances that we are missclassifying------- 
# but their values are higher than averages? 

# Notes about the exercise:
# Out of the 89 that have more values for their main variables than average,
# 60 have lower distance than average. 

st_trade_flows %>%
  filter(distance > mean(distance)) %>%
  filter(error == 1) %>%
  filter(less_trade == 0 & less_sales == 0 & less_gdp == 0) %>%
  select(orig, dest, trade, sales_i, gdp_j, distance, contiguity)

#end -----