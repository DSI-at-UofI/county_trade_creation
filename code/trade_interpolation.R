rm(list = ls())

library(tidyverse)

# Noe J Nava
# noejn2@illinois.edu
# https://noejn2.github.io/
# Script creates the dyadic_county_flows_adjusted.rds, which is our 
# main dataset in this repository
# Code takes approximately 27 minutes
# on Intel(R) Xeon(R) CPU E3-12250 v6 3.00Ghz 
# 64 GB ram
# Mumford computer
 
start.time <- Sys.time()
start.time

#dy_cnty <- readstata13::read.dta13(file = 'data/dyadic_county_2017.dta')
dy_cnty <- readRDS(file = 'output/dyadic_county_2017.rds')
dy_stat <- readstata13::read.dta13(file = 'output/dyadic_state_2017_merge.dta')

# Setting-up the dataset for county flows -----

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

# The following checks the number if infinity and NAs
# in the following vector of variables
var_list <- c("distance",
              "sales_i",
              "gdp_j")
nobs <- length(dy_cnty$orig)
for(var in var_list) {
  varval <- dy_cnty[[var]]
  sum_inf <- sum(is.infinite(varval))
  sum_nas <- sum(is.na(varval))

  cat("::::: For the variable", var, "the percentage of :::::", "\n")
  cat("::::: Infinity values is", 100*(sum_inf/nobs), "\n")
  cat("\n")
  cat("::::: For the variable", var, "the percentage of :::::", "\n")
  cat("::::: NAs values is", 100*(sum_nas/nobs), "\n")
  cat("\n")
}

# I will assume that all log(X) = -Inf are zeros
# This way, when value is multiplied by estimated parameter,
# they simply turn into zeros:
dy_cnty$sales_i[is.na(dy_cnty$sales_i)] <- 0
dy_cnty$gdp_j[is.na(dy_cnty$gdp_j)] <- 0 

dy_cnty$sales_i[is.infinite(dy_cnty$sales_i)] <- 0
dy_cnty$gdp_j[is.infinite(dy_cnty$gdp_j)] <- 0 

# The following merges the necessary variables from the state level
# dataset with our county level dataset  
dy_stat <- dy_stat %>%
  select(orig_stName,
         dest_stName,
         notrade,
         sum_FE,
         trade,
         imports,
         exports,
         domestic)

dy_cnty <- left_join(dy_cnty,
                     dy_stat,
                     by = c("orig_stName",
                            "dest_stName"))

### add code here to remove zero observations that we know are zero at the state level

# We create the weights to divided the SUM of Fixed effects ----
# To allocate the Fixed Effect values (which are obtained at the state level-state dyadic)
# to the counties, I count the number of county dyadic interactions belonging to eac
# state dyadic interaction and the inverse of the weight is multiplied by the FE summation
rm(list=setdiff(ls(), c("dy_cnty", "start.time")))

# Creating pair_id list based on states to assign the weights
#dy_cnty <- dy_cnty %>%
#  mutate(pair_id = group_indices(., orig_stName, dest_stName))
#pair_id_list <- dy_cnty %>% distinct(pair_id) %>% pull()

#dy_cnty$weight <- NA
#p <- 0
#for(id in pair_id_list) { # Creating the weights
  
#  p <- p + 1
#  cat(":::: Percentage: ", 100*(p/2304), "% ::::", "\n")
#  w <- sum(dy_cnty$pair_id == id)
#  dy_cnty$weight[dy_cnty$pair_id == id] <- w
  
#}

# The following code uses the parameters and the variables to create the county flows ----
# These are the parameters:
# These are directly typed from stata regression result by running state_gravity_reg.do

dist_eta <- -0.47147
dist_SE  <- .1014149

sale_eta <- 0.6398341
sale_SE  <- 0.0849893

gdpj_eta <- 0.6620246
gdpj_SE  <- 0.2542218

cnst     <- -11.27216
cnst_SE  <- 3.302367

# variance(beta_variance*variable) --- formula

dy_cnty <- dy_cnty %>%
  mutate(distance_c = distance*dist_eta) %>%
  mutate(sales_i_c  = sales_i*sale_eta) %>%
  mutate(gdp_j_c    = gdp_j*gdpj_eta) %>%
  mutate(FE_w       = (cnst + sum_FE)) %>% # check if you want to use weights
  mutate(cnty_flows = exp(distance_c + sales_i_c + gdp_j_c + FE_w))

# I conduct the following clean-up to make sure that county flows reflect reality----
rm(list=setdiff(ls(), c("dy_cnty", "start.time")))

# 1st Step -- Adjust for the non-zero observations that we know they should not exist

# The adjustments are done in this order:
# 1) Zero cnty_flows if notrade == 0
dy_cnty$cnty_flows[dy_cnty$notrade == 1] <- 0

# 2) Zero cnty_flows if sales_i == 0
dy_cnty$cnty_flows[dy_cnty$sales_i == 0] <- 0

# 3) Zero cnty_flows if gdp_j  == 0
dy_cnty$cnty_flows[dy_cnty$gdp_j == 0] <- 0

# 2nd Step -- Adjust dyadic county flows to match state dyadic flows
st_list <- dy_cnty %>% distinct(orig_stName) %>% pull()
i <- 0
for(st_ori in st_list[1]) {
  for(st_des in st_list[1]) {
    
    i <- i + 1
    cat("\n")
    cat(":::: Iteration number ", i, "out of 2,304")
    cat("\n")
    
    indices <- dy_cnty$orig_stName == st_ori & dy_cnty$dest_stName == st_des 
    
    a <- dy_cnty[indices,]
    cnty_flw_sim <- sum(dy_cnty$cnty_flows[indices])
    cnty_flw_obs <- mean(dy_cnty$trade[indices])
    
    if(cnty_flw_sim == 0) { #if simulated flows is zero, then we allocate as follows:
      dy_cnty$cnty_flows[indices] <- cnty_flw_obs/length(dy_cnty$cnty_flows[indices])
    }else{
      diff <- (cnty_flw_obs/cnty_flw_sim)
      
      dy_cnty$cnty_flows[indices] <- dy_cnty$cnty_flows[indices]*diff
    }
  }
}

dy_cnty <- dy_cnty %>%
  select(orig,
         dest,
         orig_stName,
         dest_stName,
         orig_stIni,
         dest_stName,
         cnty_flows,
         sales_i,
         gdp_j)

saveRDS(dy_cnty, file = 'output/dyadic_county_flows_adjusted.rds')
dy_cnty <- as.data.frame(dy_cnty)
readstata13::save.dta13(dy_cnty, file = 'output/dyadic_county_flows_adjusted.dta')

mins <- as.numeric(Sys.time() - start.time, units = "mins")
cat("\n")
cat("::: Script took ", mins, "minutes.")


# The code below is not being used since 07/29/201
#if(FALSE) {
# 2nd Step -- Adjust county flows to match exports, imports and domestic trade
#st_list <- dy_cnty %>% distinct(orig_stName) %>% pull()
#for(st in st_list[43]) { #exports
#  indices <- dy_cnty$orig_stName == st 
  
#  expo_sim <- sum(dy_cnty$cnty_flows[indices])
#  expo_obs <- mean(dy_cnty$exports[indices])
  
  #if(expo_sim == 0) { #if simulated flows is zero, then we allocate as follows:
  #  dy_cnty$cnty_flows[indices] <- expo_obs/length(dy_cnty$cnty_flows[indices])
  #}else{
  #  diff <- (expo_obs/expo_sim)
  
  #  dy_cnty$cnty_flows[indices] <- dy_cnty$cnty_flows[indices]*diff
  #}
#}

#for(st in st_list) { #imports
#  indices <- dy_cnty$dest_stName == st & dy_cnty$orig_stName != dy_cnty$dest_stName
  
#  impo_sim <- sum(dy_cnty$cnty_flows[indices])
#  impo_obs <- mean(dy_cnty$imports[indices])
  
#  if(impo_sim == 0) {  #if simulated flows is zero, then we allocate as follows:
#    dy_cnty$cnty_flows[indices] <- impo_obs/length(dy_cnty$cnty_flows[indices])
#    }else{
#      diff <- (impo_obs/impo_sim)
      
#      dy_cnty$cnty_flows[indices] <- dy_cnty$cnty_flows[indices]*diff
#    }
#}

#for(st in st_list[43]) { #domestic
#  indices <- dy_cnty$orig_stName == st & dy_cnty$orig_stName == dy_cnty$dest_stName
  
#  dome_sim <- sum(dy_cnty$cnty_flows[indices])
#  dome_obs <- mean(dy_cnty$domestic[indices])
  
  #if(dome_sim == 0) {  #if simulated flows is zero, then we allocate as follows:
    
  #  dy_cnty$cnty_flows[indices] <- dome_sim/length(dy_cnty$cnty_flows[indices])
    
  #  }else{
#      diff <- (dome_obs/dome_sim)
  
  #    dy_cnty$cnty_flows[indices] <- dy_cnty$cnty_flows[indices]*diff
  #  }
#}
#saveRDS(dy_cnty, file = 'output/dyadic_county_flows.rds')
#}




