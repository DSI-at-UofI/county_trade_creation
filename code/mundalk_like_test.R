rm(list = ls())

# Purpose: Here, I performed a mundlak-like test to check if some state flows
# may have some unobserved influence that can be remove with unit fixed-effects
# The problem is that we cannot use fixed-effects in because our gdp/sales variables
# would loose information. But we want to check if there is some additional
# variable (conditional on gdp/sales) that affect one direction of the flows
# for each state.

# I also show that including an indicator for the problematic observations
# is the same as dropping the observation. 

# A caveat: We know that we do not want to use importer/exporter FE since those
# may affect our gdp/sales coefficients. However, dropping observations is 
# as bad (or I would say worse) since we are not only loosing the information
# for an appropiate covariate for gdp/sales, but we do not know what other info
# we are loosing for say the distance measure.

# My conclusion: I still think that we are better off by not dropping observation,
# or including an indicator, but if we have to, I would use the indicator since
# using an indicator is less intrusive,

# Reference: https://blog.stata.com/2015/10/29/fixed-effects-or-random-effects-the-mundlak-approach/

library(readstata13)
library(tidyverse)
library(glm2)
library(sandwich)

data <- read.dta13(file = 'output/st_trade_flows.dta')

# Creating formula
formula <- ""
for(i in 1:18) {
  orig_FE <- "orig_FE"
  orig_FE <- paste0(orig_FE, i)
  formula <- paste(formula, orig_FE, sep = "+")
}
for(i in 1:18) {
  dest_FE <- "dest_FE"
  dest_FE <- paste0(dest_FE, i)
  formula <- paste(formula, dest_FE, sep = "+")
}
formula <- paste0("trade ~ indicator + distance + sales_i + gdp_j + contiguity + intra", formula)

# Show it will be (almost) the same if we treat "outliers" with indicators
# From stata:
## distance  == -.4495181
## sales_i   == .6802411 
## gdp_j     ==  .9762757
## intercept == -18.11583 
data$trade[is.na(data$trade)] <- 0
indices <- (data$dest == "louisiana" | data$dest == "washington")
indicator <- ifelse(indices, 1, 0)

out <- glm(formula, family = "quasipoisson", data = data)
summary(out)

# The mundlak-like test for destination
st_list <- data %>% select(orig) %>% distinct() %>% arrange(orig) %>% pull()
for(st in st_list) {
  for(year in c(2012, 2017)) {
    
    indices <- (data$dest == st) & data$year == year
    
    mean_sales <- mean(data$sales_i, na.rm = TRUE)
    mean_gdp_j <- mean(data$gdp_j, na.rm = TRUE)
    
    mean_sales <- mean_sales*indicator
    out <- glm(formula, family = "quasipoisson", data = data)
    indicator <- ifelse(indices, 1, 0)
    pval_sales <- summary(out)$coefficients[2,4] # Recover p-value
    
    indicator <- mean_gdp_j*indicator
    out <- glm(formula, family = "quasipoisson", data = data)
    indicator <- ifelse(indices, 1, 0)
    pval_gdp_j <- summary(out)$coefficients[2,4] # Recover p-value
    
    if(pval_sales < 0.05 & pval_gdp_j < 0.05) {
      cat("\n")
      cat(":::", st, "is significant for the year of", year)
      cat("\n")
    }
  }
}

# The mundlak-like test for origin
for(st in st_list) {
  for(year in c(2012, 2017)) {
    
    indices <- (data$orig == st) & data$year == year
    
    mean_sales <- mean(data$sales_i, na.rm = TRUE)
    mean_gdp_j <- mean(data$gdp_j, na.rm = TRUE)
    
    mean_sales <- mean_sales*indicator
    out <- glm(formula, family = "quasipoisson", data = data)
    indicator <- ifelse(indices, 1, 0)
    pval_sales <- summary(out)$coefficients[2,4] # Recover p-value
    
    indicator <- mean_gdp_j*indicator
    out <- glm(formula, family = "quasipoisson", data = data)
    indicator <- ifelse(indices, 1, 0)
    pval_gdp_j <- summary(out)$coefficients[2,4] # Recover p-value
    
    if(pval_sales < 0.05 & pval_gdp_j < 0.05) {
      cat("\n")
      cat(":::", st, "is significant for the year of", year)
      cat("\n")
    }
  }
}
in
