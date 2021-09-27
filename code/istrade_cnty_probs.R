rm(list = ls())
options(scipen = 9999)

# Noe J Nava
# noejn2@illinois.edu // noejnava2@gmail.com
# https://noejn2.github.io/

# Purpose: ----------------
# Calibrate a logit model with state level data to calcuate the probability
# of dyadic-conty observations to have trade

groundhog_day <- "2021-09-05"
pkgs <- c("tidyverse", "readstata13", "plm")
groundhog::groundhog.library(pkgs, groundhog_day)

### Obtaining the datasets
st_trade_flows <- read.dta13(file = "output/st_trade_flows.dta")
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

## Create categorical variable for trade (1 ==trade exists)
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
  select(c(orig_ini, dest_ini, year, list_FE, intra, contiguity)) %>%
  filter(year == 2017) %>%
  select(!year)

dy_cnty <- left_join(dy_cnty, temp, by = c("orig_stIni" = "orig_ini", 
                                           "dest_stIni" = "dest_ini"))

### The following code calibrates the model
reg_formula <- "istrade ~ distance + intra + contiguity + sales_i + gdp_j"
for(i in 1:18) {
  orig_FE <- paste0("orig_FE", i)
  reg_formula <- paste(reg_formula, orig_FE, sep = " + ")

  dest_FE <- paste0("dest_FE", i)
  reg_formula <- paste(reg_formula, dest_FE, sep = " + ")
}

glm_fit <- glm(formula = reg_formula, 
               data = st_trade_flows, 
               family = "binomial")
summary(glm_fit)
exp(coef(glm_fit))

### The following code calculates probabilities at the county level
dy_cnty$probs <- predict(glm_fit, newdata = dy_cnty, type = "response")
dy_cnty_probs <- dy_cnty %>% select(orig, dest, probs)
sum(dy_cnty_probs$probs > .5)
saveRDS(dy_cnty_probs, file = "output/dy_cnty_probs.rds")
# end