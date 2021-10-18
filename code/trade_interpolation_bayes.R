rm(list = ls())

# Noe J Nava
# noejn2@illinois.edu // noejnava2@gmail.com
# https://noejn2.github.io/

# Configuration of script:------
apply_correction  <- TRUE # Select if you want to apply a proposed correction or not
cut_off           <- 0.5  # Define a cut-off for posterior to select if there exist trade between two counties



# Purpose: ----------------
# Script is the simulation that we employ to estimate the county flows
# using the parameters from our gravity regression:
# 1) FE parameters
# 2) elasticities associated with crop sales and gdp
# But also, we employ here our IMPLAN and NASS crop sales county level data
# This comes from the dyadic_county_2017.rds dataset that we created

# This script also corrects for the following:
# 1) WA and LA flows were not estimated, so we use FE parameters
# to estimate county flows for these states

# In addition, we use probablities of having trade at the county level
# from a previous logistic regression.

# The final correction is that our simulated flows at the county level,
# when aggregated back to the state level, must add-up to the observed state 
# levels. We do that here too, except for exports to LA and WA.

# Code takes approximately 11 minutes ----------
# on MacBook Pro
# 2.3 GHz Quad-Core Intel Core i7 
# 32 GB ram
# Personal Mac

start.time <- Sys.time()
groundhog_day <- "2021-09-05"
pkgs <- c("tidyverse", "readstata13")
groundhog::groundhog.library(pkgs, 
                             groundhog_day,
                             tolerate.R.version = '4.0.3')

###########################################################
# Data wringling -----
####
dy_cnty <- readRDS(file = 'output/dyadic_county_2017.rds')
dy_stat <- read.dta13(file = 'output/dyadic_state_2017_merge.dta')

# Setting-up the dataset for county flows
# The state level dataset is created from the county level
# These are the variables used in Stata gravity regressions
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
# in the following vector of variables, and how many
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

dy_cnty$distance[is.na(dy_cnty$distance)] <- 0
dy_cnty$distance[is.infinite(dy_cnty$distance)] <- 0

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

###########################################################
# The following code uses the parameters and the variables to create the county flows ----
####
rm(list=setdiff(ls(), c("dy_cnty", "start.time", "apply_correction", "cut_off")))

# These are the parameters:
# These are directly typed from stata regression result by running state_gravity_reg.do
# The estimates are from regression that excludes Louisiana and Washington as discussed on
# August 20th, 2021 meeting
 
dist_eta <- -0.4495181
sale_eta <- 0.6802411
gdpj_eta <- 0.9762757
cnst     <- -15.46002 #from ppmlhdfe regression --- main regression that creates sum_FE variable
ppml_FE  <- -18.11583 # from ppml regression --- to adjust WA and LA

key <- read_csv(file = 'assets/state_clm_region.csv')
key <- key %>%
  select(initial, clm_region) %>%
  mutate(initial = toupper(initial))
key <- key[!is.na(key$clm_region),]

# These allocates all counties to their climate regions
southeast <- key$initial[key$clm_region == "southeast"]
southwest <- key$initial[key$clm_region == "southwest"]
south <- key$initial[key$clm_region == "south"]
west <- key$initial[key$clm_region == "west"]
northeast <- key$initial[key$clm_region == "northeast"]
northwest <- key$initial[key$clm_region == "northwest"]
central <- key$initial[key$clm_region == "central"]
east_north_central <- key$initial[key$clm_region == "east north central"]
west_north_central <- key$initial[key$clm_region == "west north central"]

# The following adds-up the FE in the same way as Stata's ppmlhdfe such that they
# are later used to recover state flows for comparison.
# Notice that we account for FE for origin and destination climate zones

# Climate region
for(ini in c("LA", "WA")) {#origin is region dest is state
  if(ini == "LA") {
    FE <- -.1931769 + ppml_FE - cnst # FE associated with LA on the destination
  }else{
    FE <- .9586682 + ppml_FE - cnst  # FE associated with WA on the destination
  }
  
  dy_cnty$sum_FE[dy_cnty$orig_stIni %in% central & dy_cnty$dest_stIni == ini] <- .246745 + FE
  dy_cnty$sum_FE[dy_cnty$orig_stIni %in% east_north_central & dy_cnty$dest_stIni == ini] <- .2517795 + FE
  dy_cnty$sum_FE[dy_cnty$orig_stIni %in% northeast & dy_cnty$dest_stIni == ini] <- .0216743 + FE
  dy_cnty$sum_FE[dy_cnty$orig_stIni %in% northwest & dy_cnty$dest_stIni == ini] <-  .1990032 + FE
  dy_cnty$sum_FE[dy_cnty$orig_stIni %in% south & dy_cnty$dest_stIni == ini] <- .5277888 + FE
  dy_cnty$sum_FE[dy_cnty$orig_stIni %in% southeast & dy_cnty$dest_stIni == ini] <- .2361734 + FE
  dy_cnty$sum_FE[dy_cnty$orig_stIni %in% southwest & dy_cnty$dest_stIni == ini] <- .6013342 + FE
  dy_cnty$sum_FE[dy_cnty$orig_stIni %in% west & dy_cnty$dest_stIni == ini] <- 0 + FE
  dy_cnty$sum_FE[dy_cnty$orig_stIni %in% west_north_central & dy_cnty$dest_stIni == ini] <- .7725402 + FE
  
}

for(ini in c("LA", "WA")) {#destination is region origin is state
  if(ini == "LA") {
    FE <- .5277888 + ppml_FE - cnst  # FE associated with LA on the origin
  }else{
    FE <- .1990032 + ppml_FE - cnst  # FE associated with WA on the origin
  }
  
  dy_cnty$sum_FE[dy_cnty$dest_stIni %in% central & dy_cnty$orig_stIni == ini] <-  -.7289201 + FE
  dy_cnty$sum_FE[dy_cnty$dest_stIni %in% east_north_central & dy_cnty$orig_stIni == ini] <- -.4574307 + FE
  dy_cnty$sum_FE[dy_cnty$dest_stIni %in% northeast & dy_cnty$orig_stIni == ini] <-  -.1415597  + FE
  dy_cnty$sum_FE[dy_cnty$dest_stIni %in% northwest & dy_cnty$orig_stIni == ini] <- .9586682   + FE
  dy_cnty$sum_FE[dy_cnty$dest_stIni %in% south & dy_cnty$orig_stIni == ini] <-  -.1931769 + FE
  dy_cnty$sum_FE[dy_cnty$dest_stIni %in% southeast & dy_cnty$orig_stIni == ini] <-  .6142957  + FE
  dy_cnty$sum_FE[dy_cnty$dest_stIni %in% southwest & dy_cnty$orig_stIni == ini] <-  .3942852  + FE
  dy_cnty$sum_FE[dy_cnty$dest_stIni %in% west & dy_cnty$orig_stIni == ini] <- .3494075  + FE
  dy_cnty$sum_FE[dy_cnty$dest_stIni %in% west_north_central & dy_cnty$orig_stIni == ini] <-  -.6121513  + FE
}

# contiguity
dy_cnty$sum_FE[dy_cnty$orig_stIni == "LA" & dy_cnty$dest_stIni == "AR"] <- .7813085 + dy_cnty$sum_FE[dy_cnty$orig_stIni == "LA" & dy_cnty$dest_stIni == "AR"]
dy_cnty$sum_FE[dy_cnty$orig_stIni == "WA" & dy_cnty$dest_stIni == "ID"] <- .7813085 + dy_cnty$sum_FE[dy_cnty$orig_stIni == "WA" & dy_cnty$dest_stIni == "ID"]
dy_cnty$sum_FE[dy_cnty$orig_stIni == "TX" & dy_cnty$dest_stIni == "LA"] <- .7813085 + dy_cnty$sum_FE[dy_cnty$orig_stIni == "TX" & dy_cnty$dest_stIni == "LA"]
dy_cnty$sum_FE[dy_cnty$orig_stIni == "MS" & dy_cnty$dest_stIni == "LA"] <- .7813085 + dy_cnty$sum_FE[dy_cnty$orig_stIni == "MS" & dy_cnty$dest_stIni == "LA"]
dy_cnty$sum_FE[dy_cnty$orig_stIni == "AR" & dy_cnty$dest_stIni == "LA"] <- .7813085 + dy_cnty$sum_FE[dy_cnty$orig_stIni == "AR" & dy_cnty$dest_stIni == "LA"]
dy_cnty$sum_FE[dy_cnty$orig_stIni == "LA" & dy_cnty$dest_stIni == "MS"] <- .7813085 + dy_cnty$sum_FE[dy_cnty$orig_stIni == "LA" & dy_cnty$dest_stIni == "MS"]
dy_cnty$sum_FE[dy_cnty$orig_stIni == "WA" & dy_cnty$dest_stIni == "OR"] <- .7813085 + dy_cnty$sum_FE[dy_cnty$orig_stIni == "WA" & dy_cnty$dest_stIni == "OR"]
dy_cnty$sum_FE[dy_cnty$orig_stIni == "LA" & dy_cnty$dest_stIni == "TX"] <- .7813085 + dy_cnty$sum_FE[dy_cnty$orig_stIni == "LA" & dy_cnty$dest_stIni == "TX"]
dy_cnty$sum_FE[dy_cnty$orig_stIni == "OR" & dy_cnty$dest_stIni == "WA"] <- .7813085 + dy_cnty$sum_FE[dy_cnty$orig_stIni == "OR" & dy_cnty$dest_stIni == "WA"]
dy_cnty$sum_FE[dy_cnty$orig_stIni == "ID" & dy_cnty$dest_stIni == "WA"] <- .7813085 + dy_cnty$sum_FE[dy_cnty$orig_stIni == "ID" & dy_cnty$dest_stIni == "WA"]

# intra
dy_cnty$sum_FE[dy_cnty$orig_stIni == "LA" & dy_cnty$dest_stIni == "LA"] <- 3.526352 + dy_cnty$sum_FE[dy_cnty$orig_stIni == "LA" & dy_cnty$dest_stIni == "LA"]
dy_cnty$sum_FE[dy_cnty$orig_stIni == "WA" & dy_cnty$dest_stIni == "WA"] <- 3.526352 + dy_cnty$sum_FE[dy_cnty$orig_stIni == "WA" & dy_cnty$dest_stIni == "WA"]

dy_cnty <- dy_cnty %>%
  mutate(distance_c = distance*dist_eta) %>%
  mutate(sales_i_c  = sales_i*sale_eta) %>%
  mutate(gdp_j_c    = gdp_j*gdpj_eta) %>%
  mutate(FE_w       = (cnst + sum_FE)) %>%
  mutate(cnty_flows = exp(distance_c + sales_i_c + gdp_j_c + FE_w))

###########################################################
# I conduct the following baysian updating beliefs to create a posterior probability ----
####
rm(list=setdiff(ls(), c("dy_cnty", "start.time", "apply_correction", "cut_off")))

# Just checking for further NA and inf
sum(is.na(dy_cnty$cnty_flows))
sum(is.infinite(dy_cnty$cnty_flows))

dy_prob <- readRDS(file = "output/dy_cnty_probs.rds") # cnty probabilities
dy_cnty <- left_join(dy_cnty, dy_prob, by = c("orig", "dest"))

dy_prob <- readRDS(file = "output/dy_state_probs.rds") # State probabilities
dy_cnty <- left_join(dy_cnty,
                     dy_prob, 
                     by = c("orig_stName" = "orig",
                            "dest_stName" = "dest"))

rm(list=setdiff(ls(), c("dy_cnty", "start.time", "apply_correction", "cut_off")))

# Creating the priors for bayesian belief updating
# Notice that priors are our cnty probabilities from logistic regression
dy_cnty <- dy_cnty %>% rename(priors = probs)

# a) notrade (at the state level) coming from the CFS data
dy_cnty$priors <- ifelse(dy_cnty$notrade == 1, 0, dy_cnty$priors)

# b) whenever sales_i and gdp_j are zero
dy_cnty$priors[dy_cnty$sales_i == 0] <- 0 
dy_cnty$priors[dy_cnty$gdp_j == 0]   <- 0

# Bayes rule (posterior):
# P(.): probability
# E: evidence-two states trade
# H: hypothesis-two counties within the two states trade
# xb: logistic regression estimated probability
# P(H|E,xb) = P(E|H,xb)*P(H|xb) / P(E|xb) 
# P(H|E,xb) <- probablity hypothesis being true
# if the evidence is true
# P(E|H,xb) = 1 <- probability of evidence being true if hypothesis is true
# notice this is always 1 since whenever the two counties within the
# state trade, then two states must trade
# P(H|xb) = prior <- probability of hypothesis being true
# from logistic regression and further refined by a) and b)
# P(E|xb) <- probability of evidence being true
# from logistic regression

dy_cnty$posterior <- (1*dy_cnty$priors)/dy_cnty$probs_st

# Next, we employ our posterior probability at the county level to define a variable
# that indicates if there is trade or not: istrade.

# Notice, if posterior > cut-off, then istrade == 1
# if posterior < cut-off, but notrade == 0, then a further correction is applied.

# The loop below also saves the total number of errors at the state level using
# such cut-off, but also which states

dy_cnty$istrade <- ifelse(dy_cnty$posterior > cut_off, 1, 0)

total_errors <- 0
which_errors <- data.frame()
which_corrections <- data.frame()
i <- 0
st_list <- dy_cnty %>% distinct(orig_stName) %>% pull()
for(st_orig in st_list) {
  for(st_dest in st_list) {
    
    i <- i + 1
    cat("\n")
    cat(":::: Iteration number ", i, "out of 2,304")
    cat("\n")
    
    df_temp <- dy_cnty %>%
      filter(orig_stName == st_orig & dest_stName == st_dest) %>%
      select(orig, dest, posterior, istrade, notrade)
    
    if(all(df_temp$notrade == 0 & df_temp$posterior < cut_off)){
      total_errors <- 1 + total_errors
      which_errors <- rbind(which_errors, c(st_orig, st_dest))
      
      if(apply_correction) {
        # Applying correction for whenever notrade == 0, but posterior is 
        # below threshold
        
        df_temp <- df_temp[with(df_temp, order(-posterior)),][1:10,]
        which_corrections <- rbind(which_corrections, df_temp)
        
        for(c in 1:10){
          
          row <- df_temp[c,]
          dy_cnty$istrade[dy_cnty$orig == row$orig & dy_cnty$dest == row$dest] <- 1
          
        }
      }
    }
  }
}

names(which_errors) <- c("orig", "dest")
saveRDS(which_errors, file = 'diagnostics/which_errors.rds')
saveRDS(which_corrections, file = 'diagnostics/which_corrections.rds')

cat("\n")
cat("Total of errors is", total_errors)
cat("\n")
cat("Total of dyadic interactions", sum(dy_cnty$istrade == 1))
cat("\n")

# 2nd Step -- Balance dyadic county flows to match state dyadic flows
i <- 0
st_list <- st_list[st_list != "louisiana"] 
st_list <- st_list[st_list != "washington"]
dy_cnty$cnty_flows[dy_cnty$istrade == 0] <- 0
# Notice that louisiana and washington are excluded from this adjustment
for(st_ori in c(st_list, "louisiana", "washington")) {
  for(st_des in st_list) {
    
    if(i == 0) {
      note <- "These are the observed-to-simulated ratios for dyadic flows at the state level."
      write(note, file = "output/sim_to_obs_ratio.txt", append = TRUE)
    }
    
    i <- i + 1
    cat("\n")
    cat(":::: Iteration number ", i, "out of 2,208")
    cat("\n")
    
    indices <- dy_cnty$istrade == 1
    
    indices <- dy_cnty$orig_stName == st_ori & dy_cnty$dest_stName == st_des & indices
    
    st_flw_sim <- sum(dy_cnty$cnty_flows[indices], na.rm = TRUE)
    
    if(all(!indices)) {
      st_flw_obs <- 0
    }else{
      st_flw_obs <- mean(dy_cnty$trade[indices], na.rm = TRUE)
    }
    
    if(st_flw_sim == 0) { #if simulated flows is zero, then we allocate as follows:
      dy_cnty$cnty_flows[indices] <- st_flw_obs/length(dy_cnty$cnty_flows[indices])
      
      if(st_flw_obs != 0) {
        line <- paste("Orig: ", st_ori, "Dest: ", st_des, "Simulated value is zero!!!!")
        write(line, file = "output/sim_to_obs_ratio.txt", append = TRUE)
      }else{
        line <- paste("Orig: ", st_ori, "Dest: ", st_des, "Simulated and observed value are zero!!!!")
        write(line, file = "output/sim_to_obs_ratio.txt", append = TRUE)
      }
      
    }else{
      diff <- (st_flw_obs/st_flw_sim)
      
      dy_cnty$cnty_flows[indices] <- dy_cnty$cnty_flows[indices]*diff
      
      line <- paste("Orig: ", st_ori, "Dest: ", st_des, "Obs-to-sim ratio: ", diff)
      write(line, file = "output/sim_to_obs_ratio.txt", append = TRUE)
    }
  }
}

cat("\n")
cat("Total of dyadic interactions", sum(dy_cnty$cnty_flows > 0))
cat("\n")

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

mins <- as.numeric(Sys.time() - start.time, units = "mins")
cat("\n")
cat("::: Script took ", mins, "minutes.")
#end
