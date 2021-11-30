rm(list = ls())

library(readstata13)
library(tidyverse)
library(sandwich)
library(glm2)

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
formula <- paste0("trade ~ sales_i + gdp_j + distance + contiguity + intra", formula)

data$destCont1 <- ifelse(data$dest == "arkansas" & data$year == 2017, 1, 0)
data$destCont2 <- ifelse(data$dest == "idaho" & data$year == 2012, 1, 0)
data$destCont3 <- ifelse(data$dest == "kansas" & data$year == 2017, 1, 0)
data$destCont4 <- ifelse(data$dest == "louisiana" & data$year == 2017, 1, 0)
data$destCont5 <- ifelse(data$dest == "ohio" & data$year == 2012, 1, 0)
data$destCont6 <- ifelse(data$dest == "washington" & data$year == 2017, 1, 0)

data$origCont1 <- ifelse(data$dest == "arkansas" & data$year == 2017, 1, 0)
data$origCont2 <- ifelse(data$dest == "florida" & data$year == 2017, 1, 0)
data$origCont3 <- ifelse(data$dest == "illinois" & data$year == 2017, 1, 0)
data$origCont4 <- ifelse(data$dest == "iowa" & data$year == 2017, 1, 0)
data$origCont5 <- ifelse(data$dest == "kansas" & data$year == 2012, 1, 0)
data$origCont6 <- ifelse(data$dest == "louisiana" & data$year == 2017, 1, 0)
data$origCont7 <- ifelse(data$dest == "minnesota" & data$year == 2017, 1, 0)
data$origCont8 <- ifelse(data$dest == "new york" & data$year == 2017, 1, 0)

combination_list <- c("destCont1",
                      "destCont2",
                      "destCont3",
                      "destCont4",
                      "destCont5",
                      "destCont6",
                      "origCont1",
                      "origCont2",
                      "origCont3",
                      "origCont4",
                      "origCont5",
                      "origCont6",
                      "origCont7",
                      "origCont8")

totalCombis <- function(n, x) {# total combinations no repeations
  factorial(n) / (factorial(n-x)*factorial(x))
}

results <- data.frame()
# One combination
r <- 1 
mcombi <- combn(combination_list, r)
for(c in 1:totalCombis(length(combination_list),r)) {r
  
  combi1 <- mcombi[1,c]
  FE <- paste(combi1,
              sep = "+")
  combis <- paste(formula, FE, sep = "+")
  
  out <- glm(combis, family = "quasipoisson", data = data)
  
  # sales_i
  coeff_sales_i <- summary(out)$coefficients[2,1]
  sterr_sales_i <- summary(out)$coefficients[2,2]
  
  # gdp_j
  coeff_gdp_j <- summary(out)$coefficients[3,1]
  sterr_gdp_j <- summary(out)$coefficients[3,2]
  
  outputs <- c(FE,
               coeff_sales_i,
               sterr_sales_i,
               coeff_gdp_j,
               sterr_gdp_j)
  
  results <- rbind.data.frame(results, outputs)
  
}

# Two combinations
r <- 2 
mcombi <- combn(combination_list, r)
for(c in 1:totalCombis(length(combination_list),r)) { 
  
  combi1 <- mcombi[1,c]
  combi2 <- mcombi[2,c]
  FE <- paste(combi1, 
              combi2, 
              sep = "+")
  combis <- paste(formula, FE, sep = "+")

  out <- glm(combis, family = "quasipoisson", data = data)
  
  # sales_i
  coeff_sales_i <- summary(out)$coefficients[2,1]
  sterr_sales_i <- summary(out)$coefficients[2,2]
  
  # gdp_j
  coeff_gdp_j <- summary(out)$coefficients[3,1]
  sterr_gdp_j <- summary(out)$coefficients[3,2]
  
  outputs <- c(FE,
               coeff_sales_i,
               sterr_sales_i,
               coeff_gdp_j,
               sterr_gdp_j)
  
  results <- rbind.data.frame(results, outputs)

}

# Three combinations
r <- 3
mcombi <- combn(combination_list, r)
for(c in 1:totalCombis(length(combination_list),3)) { 
  
  combi1 <- mcombi[1,c]
  combi2 <- mcombi[2,c]
  combi3 <- mcombi[3,c]
  FE <- paste(combi1, 
              combi2,
              combi3,
              sep = "+")
  combis <- paste(formula, FE, sep = "+")

  out <- glm(combis, family = "quasipoisson", data = data)
  
  # sales_i
  coeff_sales_i <- summary(out)$coefficients[2,1]
  sterr_sales_i <- summary(out)$coefficients[2,2]
  
  # gdp_j
  coeff_gdp_j <- summary(out)$coefficients[3,1]
  sterr_gdp_j <- summary(out)$coefficients[3,2]
  
  outputs <- c(FE,
               coeff_sales_i,
               sterr_sales_i,
               coeff_gdp_j,
               sterr_gdp_j)
  
  results <- rbind.data.frame(results, outputs)
  
}

# Four combinations
r <- 4
mcombi <- combn(combination_list, r)
for(c in 1:totalCombis(length(combination_list),r)) { 
  
  combi1 <- mcombi[1,c]
  combi2 <- mcombi[2,c]
  combi3 <- mcombi[3,c]
  combi4 <- mcombi[4,c]
  FE <- paste(combi1, 
              combi2,
              combi3,
              combi4,
              sep = "+")
  combis <- paste(formula, FE, sep = "+")
  
  out <- glm(combis, family = "quasipoisson", data = data)
  
  # sales_i
  coeff_sales_i <- summary(out)$coefficients[2,1]
  sterr_sales_i <- summary(out)$coefficients[2,2]
  
  # gdp_j
  coeff_gdp_j <- summary(out)$coefficients[3,1]
  sterr_gdp_j <- summary(out)$coefficients[3,2]
  
  outputs <- c(FE,
               coeff_sales_i,
               sterr_sales_i,
               coeff_gdp_j,
               sterr_gdp_j)
  
  results <- rbind.data.frame(results, outputs)
  
}

# Five combinations
r <- 5
mcombi <- combn(combination_list, r)
for(c in 1:totalCombis(length(combination_list),r)) { 
  
  combi1 <- mcombi[1,c]
  combi2 <- mcombi[2,c]
  combi3 <- mcombi[3,c]
  combi4 <- mcombi[4,c]
  combi5 <- mcombi[5,c]
  FE <- paste(combi1, 
              combi2,
              combi3,
              combi4,
              combi5,
              sep = "+")
  combis <- paste(formula, FE, sep = "+")
  
  out <- glm(combis, family = "quasipoisson", data = data)
  
  # sales_i
  coeff_sales_i <- summary(out)$coefficients[2,1]
  sterr_sales_i <- summary(out)$coefficients[2,2]
  
  # gdp_j
  coeff_gdp_j <- summary(out)$coefficients[3,1]
  sterr_gdp_j <- summary(out)$coefficients[3,2]
  
  outputs <- c(FE,
               coeff_sales_i,
               sterr_sales_i,
               coeff_gdp_j,
               sterr_gdp_j)
  
  results <- rbind.data.frame(results, outputs)
  
}

# Six combinations
r <- 6
mcombi <- combn(combination_list, r)
for(c in 1:totalCombis(length(combination_list),r)) { 
  
  combi1 <- mcombi[1,c]
  combi2 <- mcombi[2,c]
  combi3 <- mcombi[3,c]
  combi4 <- mcombi[4,c]
  combi5 <- mcombi[5,c]
  combi6 <- mcombi[6,c]
  FE <- paste(combi1, 
              combi2,
              combi3,
              combi4,
              combi5,
              combi6,
              sep = "+")
  combis <- paste(formula, FE, sep = "+")
  
  out <- glm(combis, family = "quasipoisson", data = data)
  
  # sales_i
  coeff_sales_i <- summary(out)$coefficients[2,1]
  sterr_sales_i <- summary(out)$coefficients[2,2]
  
  # gdp_j
  coeff_gdp_j <- summary(out)$coefficients[3,1]
  sterr_gdp_j <- summary(out)$coefficients[3,2]
  
  outputs <- c(FE,
               coeff_sales_i,
               sterr_sales_i,
               coeff_gdp_j,
               sterr_gdp_j)
  
  results <- rbind.data.frame(results, outputs)
  
}

# Seven combinations
r <- 7
mcombi <- combn(combination_list, r)
for(c in 1:totalCombis(length(combination_list),r)) { 
  
  combi1 <- mcombi[1,c]
  combi2 <- mcombi[2,c]
  combi3 <- mcombi[3,c]
  combi4 <- mcombi[4,c]
  combi5 <- mcombi[5,c]
  combi6 <- mcombi[6,c]
  combi7 <- mcombi[7,c]
  FE <- paste(combi1, 
              combi2,
              combi3,
              combi4,
              combi5,
              combi6,
              combi7,
              sep = "+")
  combis <- paste(formula, FE, sep = "+")
  
  out <- glm(combis, family = "quasipoisson", data = data)
  
  # sales_i
  coeff_sales_i <- summary(out)$coefficients[2,1]
  sterr_sales_i <- summary(out)$coefficients[2,2]
  
  # gdp_j
  coeff_gdp_j <- summary(out)$coefficients[3,1]
  sterr_gdp_j <- summary(out)$coefficients[3,2]
  
  outputs <- c(FE,
               coeff_sales_i,
               sterr_sales_i,
               coeff_gdp_j,
               sterr_gdp_j)
  
  results <- rbind.data.frame(results, outputs)
  
}

# Eight combinations
r <- 8
mcombi <- combn(combination_list, r)
for(c in 1:totalCombis(length(combination_list),r)) { 
  
  combi1 <- mcombi[1,c]
  combi2 <- mcombi[2,c]
  combi3 <- mcombi[3,c]
  combi4 <- mcombi[4,c]
  combi5 <- mcombi[5,c]
  combi6 <- mcombi[6,c]
  combi7 <- mcombi[7,c]
  combi8 <- mcombi[8,c]
  FE <- paste(combi1, 
              combi2,
              combi3,
              combi4,
              combi5,
              combi6,
              combi7,
              combi8,
              sep = "+")
  combis <- paste(formula, FE, sep = "+")
  
  out <- glm(combis, family = "quasipoisson", data = data)
  
  # sales_i
  coeff_sales_i <- summary(out)$coefficients[2,1]
  sterr_sales_i <- summary(out)$coefficients[2,2]
  
  # gdp_j
  coeff_gdp_j <- summary(out)$coefficients[3,1]
  sterr_gdp_j <- summary(out)$coefficients[3,2]
  
  outputs <- c(FE,
               coeff_sales_i,
               sterr_sales_i,
               coeff_gdp_j,
               sterr_gdp_j)
  
  results <- rbind.data.frame(results, outputs)
  
}

# Nine combinations
r <- 9
mcombi <- combn(combination_list, r)
for(c in 1:totalCombis(length(combination_list),r)) { 
  
  combi1 <- mcombi[1,c]
  combi2 <- mcombi[2,c]
  combi3 <- mcombi[3,c]
  combi4 <- mcombi[4,c]
  combi5 <- mcombi[5,c]
  combi6 <- mcombi[6,c]
  combi7 <- mcombi[7,c]
  combi8 <- mcombi[8,c]
  combi9 <- mcombi[9,c]
  FE <- paste(combi1, 
              combi2,
              combi3,
              combi4,
              combi5,
              combi6,
              combi7,
              combi8,
              combi9,
              sep = "+")
  combis <- paste(formula, FE, sep = "+")
  
  out <- glm(combis, family = "quasipoisson", data = data)
  
  # sales_i
  coeff_sales_i <- summary(out)$coefficients[2,1]
  sterr_sales_i <- summary(out)$coefficients[2,2]
  
  # gdp_j
  coeff_gdp_j <- summary(out)$coefficients[3,1]
  sterr_gdp_j <- summary(out)$coefficients[3,2]
  
  outputs <- c(FE,
               coeff_sales_i,
               sterr_sales_i,
               coeff_gdp_j,
               sterr_gdp_j)
  
  results <- rbind.data.frame(results, outputs)
  
}

# Ten combinations
r <- 10
mcombi <- combn(combination_list, r)
for(c in 1:totalCombis(length(combination_list),r)) { 
  
  combi1 <- mcombi[1,c]
  combi2 <- mcombi[2,c]
  combi3 <- mcombi[3,c]
  combi4 <- mcombi[4,c]
  combi5 <- mcombi[5,c]
  combi6 <- mcombi[6,c]
  combi7 <- mcombi[7,c]
  combi8 <- mcombi[8,c]
  combi9 <- mcombi[9,c]
  combi10 <- mcombi[10,c]
  FE <- paste(combi1, 
              combi2,
              combi3,
              combi4,
              combi5,
              combi6,
              combi7,
              combi8,
              combi9,
              combi10,
              sep = "+")
  combis <- paste(formula, FE, sep = "+")
  
  out <- glm(combis, family = "quasipoisson", data = data)
  
  # sales_i
  coeff_sales_i <- summary(out)$coefficients[2,1]
  sterr_sales_i <- summary(out)$coefficients[2,2]
  
  # gdp_j
  coeff_gdp_j <- summary(out)$coefficients[3,1]
  sterr_gdp_j <- summary(out)$coefficients[3,2]
  
  outputs <- c(FE,
               coeff_sales_i,
               sterr_sales_i,
               coeff_gdp_j,
               sterr_gdp_j)
  
  results <- rbind.data.frame(results, outputs)
  
}

# Eleven combinations
r <- 11
mcombi <- combn(combination_list, r)
for(c in 1:totalCombis(length(combination_list),r)) { 
  
  combi1 <- mcombi[1,c]
  combi2 <- mcombi[2,c]
  combi3 <- mcombi[3,c]
  combi4 <- mcombi[4,c]
  combi5 <- mcombi[5,c]
  combi6 <- mcombi[6,c]
  combi7 <- mcombi[7,c]
  combi8 <- mcombi[8,c]
  combi9 <- mcombi[9,c]
  combi10 <- mcombi[10,c]
  combi11 <- mcombi[11,c]
  FE <- paste(combi1, 
              combi2,
              combi3,
              combi4,
              combi5,
              combi6,
              combi7,
              combi8,
              combi9,
              combi10,
              combi11,
              sep = "+")
  combis <- paste(formula, FE, sep = "+")
  
  out <- glm(combis, family = "quasipoisson", data = data)
  
  # sales_i
  coeff_sales_i <- summary(out)$coefficients[2,1]
  sterr_sales_i <- summary(out)$coefficients[2,2]
  
  # gdp_j
  coeff_gdp_j <- summary(out)$coefficients[3,1]
  sterr_gdp_j <- summary(out)$coefficients[3,2]
  
  outputs <- c(FE,
               coeff_sales_i,
               sterr_sales_i,
               coeff_gdp_j,
               sterr_gdp_j)
  
  results <- rbind.data.frame(results, outputs)
  
}

# Twelve combinations
r <- 12
mcombi <- combn(combination_list, r)
for(c in 1:totalCombis(length(combination_list),r)) { 
  
  combi1 <- mcombi[1,c]
  combi2 <- mcombi[2,c]
  combi3 <- mcombi[3,c]
  combi4 <- mcombi[4,c]
  combi5 <- mcombi[5,c]
  combi6 <- mcombi[6,c]
  combi7 <- mcombi[7,c]
  combi8 <- mcombi[8,c]
  combi9 <- mcombi[9,c]
  combi10 <- mcombi[10,c]
  combi11 <- mcombi[11,c]
  combi12 <- mcombi[12,c]
  FE <- paste(combi1, 
              combi2,
              combi3,
              combi4,
              combi5,
              combi6,
              combi7,
              combi8,
              combi9,
              combi10,
              combi11,
              combi12,
              sep = "+")
  combis <- paste(formula, FE, sep = "+")
  
  out <- glm(combis, family = "quasipoisson", data = data)
  
  # sales_i
  coeff_sales_i <- summary(out)$coefficients[2,1]
  sterr_sales_i <- summary(out)$coefficients[2,2]
  
  # gdp_j
  coeff_gdp_j <- summary(out)$coefficients[3,1]
  sterr_gdp_j <- summary(out)$coefficients[3,2]
  
  outputs <- c(FE,
               coeff_sales_i,
               sterr_sales_i,
               coeff_gdp_j,
               sterr_gdp_j)
  
  results <- rbind.data.frame(results, outputs)
  
}

# Thirteen combinations
r <- 13
mcombi <- combn(combination_list, r)
for(c in 1:totalCombis(length(combination_list),r)) { 
  
  combi1 <- mcombi[1,c]
  combi2 <- mcombi[2,c]
  combi3 <- mcombi[3,c]
  combi4 <- mcombi[4,c]
  combi5 <- mcombi[5,c]
  combi6 <- mcombi[6,c]
  combi7 <- mcombi[7,c]
  combi8 <- mcombi[8,c]
  combi9 <- mcombi[9,c]
  combi10 <- mcombi[10,c]
  combi11 <- mcombi[11,c]
  combi12 <- mcombi[12,c]
  combi13 <- mcombi[13,c]
  FE <- paste(combi1, 
              combi2,
              combi3,
              combi4,
              combi5,
              combi6,
              combi7,
              combi8,
              combi9,
              combi10,
              combi11,
              combi12,
              combi13,
              sep = "+")
  combis <- paste(formula, FE, sep = "+")
  
  out <- glm(combis, family = "quasipoisson", data = data)
  
  # sales_i
  coeff_sales_i <- summary(out)$coefficients[2,1]
  sterr_sales_i <- summary(out)$coefficients[2,2]
  
  # gdp_j
  coeff_gdp_j <- summary(out)$coefficients[3,1]
  sterr_gdp_j <- summary(out)$coefficients[3,2]
  
  outputs <- c(FE,
               coeff_sales_i,
               sterr_sales_i,
               coeff_gdp_j,
               sterr_gdp_j)
  
  results <- rbind.data.frame(results, outputs)
  
}

# Thirteen combinations
r <- 14
mcombi <- combn(combination_list, r)
for(c in 1:totalCombis(length(combination_list),r)) { 
  
  combi1 <- mcombi[1,c]
  combi2 <- mcombi[2,c]
  combi3 <- mcombi[3,c]
  combi4 <- mcombi[4,c]
  combi5 <- mcombi[5,c]
  combi6 <- mcombi[6,c]
  combi7 <- mcombi[7,c]
  combi8 <- mcombi[8,c]
  combi9 <- mcombi[9,c]
  combi10 <- mcombi[10,c]
  combi11 <- mcombi[11,c]
  combi12 <- mcombi[12,c]
  combi13 <- mcombi[13,c]
  combi14 <- mcombi[14,c]
  
  FE <- paste(combi1, 
              combi2,
              combi3,
              combi4,
              combi5,
              combi6,
              combi7,
              combi8,
              combi9,
              combi10,
              combi11,
              combi12,
              combi13,
              combi14,
              sep = "+")
  
  out <- glm(combis, family = "quasipoisson", data = data)
  
  # sales_i
  coeff_sales_i <- summary(out)$coefficients[2,1]
  sterr_sales_i <- summary(out)$coefficients[2,2]
  
  # gdp_j
  coeff_gdp_j <- summary(out)$coefficients[3,1]
  sterr_gdp_j <- summary(out)$coefficients[3,2]
  
  outputs <- c(FE,
               coeff_sales_i,
               sterr_sales_i,
               coeff_gdp_j,
               sterr_gdp_j)
  
  results <- rbind.data.frame(results, outputs)
  
}

names(results) <- c("FE", "sales", "SE_sales", "gdp", "SE_gdp")
results <- results %>%
  mutate(sales    = as.numeric(sales),
         SE_sales = as.numeric(SE_sales),
         gdp      = as.numeric(gdp),
         SE_gdp   = as.numeric(SE_gdp))

plot(density(results$sales))
plot(density(results$gdp))

results$dest_AR_17 <- ifelse(str_detect(results$FE, "destCont1"), 1, 0)
results$dest_ID_12 <- ifelse(str_detect(results$FE, "destCont2"), 1, 0)
results$dest_KS_17 <- ifelse(str_detect(results$FE, "destCont3"), 1, 0)
results$dest_LA_17 <- ifelse(str_detect(results$FE, "destCont4"), 1, 0)
results$dest_OH_12 <- ifelse(str_detect(results$FE, "destCont5"), 1, 0)
results$dest_WA_17 <- ifelse(str_detect(results$FE, "destCont6"), 1, 0)

results$orig_AR_17 <- ifelse(str_detect(results$FE, "origCont1"), 1, 0)
results$orig_FL_17 <- ifelse(str_detect(results$FE, "origCont2"), 1, 0)
results$orig_IL_17 <- ifelse(str_detect(results$FE, "origCont3"), 1, 0)
results$orig_IA_12 <- ifelse(str_detect(results$FE, "origCont4"), 1, 0)
results$orig_KS_17 <- ifelse(str_detect(results$FE, "origCont5"), 1, 0)
results$orig_LA_17 <- ifelse(str_detect(results$FE, "origCont6"), 1, 0)
results$orig_MN_17 <- ifelse(str_detect(results$FE, "origCont7"), 1, 0)
results$orig_NY_17 <- ifelse(str_detect(results$FE, "origCont8"), 1, 0)

results <- results %>% select(!FE)
saveRDS(results, file = "output/spec_curveAnalysis_results.rds")
#end