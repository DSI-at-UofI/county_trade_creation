rm(list = ls())

# Purpose: Here, I performed a simple statistical test to check if some state flows
# may have some unobserved influence that can be remove with unit fixed-effects
# The problem is that we cannot use year importer/exporter FE because our gdp/sales 
# variables would not be identified. But we want to check if there is some additional
# variable (conditional on a fully specified model) that affect one direction of the flows
# for each state.

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

# The mundlak-like test for destination
st_list <- data %>% select(orig) %>% distinct() %>% arrange(orig) %>% pull()

mundalkOut_dest <- data.frame()
mundalkOut_dest <- rbind.data.frame(mundalkOut_dest, rep(NA,6))
mundalkOut_dest <- cbind.data.frame(st_list, mundalkOut_dest)
names(mundalkOut_dest) <- c("state", 
                            "coeff_12", "SE_12", "pval_12",
                            "coeff_17", "SE_17", "pval_17")

for(st in st_list) {
  for(year in c(2012, 2017)) {

    indices <- (data$dest == st) & data$year == year
    indicator <- ifelse(indices, 1, 0)
    
    out  <- glm(formula, family = "quasipoisson", data = data)
    pval <- summary(out)$coefficients[2,4] # Recover p-value
    coef <- summary(out)$coefficients[2,1] # Recover coefficient
    se   <- summary(out)$coefficients[2,2] # Recover SE
    
    if(year == 2012) {
      mundalkOut_dest[mundalkOut_dest$state == st,][2:4] <- c(coef,
                                                              se,
                                                              pval)
    }else{
      mundalkOut_dest[mundalkOut_dest$state == st,][5:7] <- c(coef,
                                                              se,
                                                              pval)
    }
    
    if(pval < 0.05) {
      cat("\n")
      cat(":::", st, "is significant for the year of", year)
      cat("\n")
    }
  }
}

# The mundlak-like test for origin
mundalkOut_orig <- data.frame()
mundalkOut_orig <- rbind.data.frame(mundalkOut_orig, rep(NA,6))
mundalkOut_orig <- cbind.data.frame(st_list, mundalkOut_orig)
names(mundalkOut_orig) <- c("state", 
                            "coeff_12", "SE_12", "pval_12",
                            "coeff_17", "SE_17", "pval_17")
for(st in st_list) {
  for(year in c(2012, 2017)) {
    
    indices <- (data$orig == st) & data$year == year
    indicator <- ifelse(indices, 1, 0)
        
    out   <- glm(formula, family = "quasipoisson", data = data)
    pval  <- summary(out)$coefficients[2,4] # Recover p-value
    coef  <- summary(out)$coefficients[2,1] # Recover coefficient
    se    <- summary(out)$coefficients[2,2] # Recover SE
    
    if(year == 2012) {
      mundalkOut_orig[mundalkOut_orig$state == st,][2:4] <- c(coef,
                                                              se,
                                                              pval)
    }else{
      mundalkOut_orig[mundalkOut_orig$state == st,][5:7] <- c(coef,
                                                              se,
                                                              pval)
    }
    
    if(pval < 0.05) {
      cat("\n")
      cat(":::", st, "is significant for the year of", year)
      cat("\n")
    }
  }
}
saveRDS(mundalkOut_dest, file = "diagnostics/mundlakOut_dest.rds")
saveRDS(mundalkOut_orig, file = "diagnostics/mundlakOut_orig.rds")
#end
