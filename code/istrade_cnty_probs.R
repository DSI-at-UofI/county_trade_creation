rm(list = ls())
options(scipen = 9999)

# Noe J Nava
# noejn2@illinois.edu // noejnava2@gmail.com
# https://noejn2.github.io

# Purpose: ----------------
# Calibrate a logit model with state level data to calcuate the probability
# of dyadic-conty observations to have trade

# Conclusions: ---------
# Gains from having different model specication (year or intra), and
# having different estimators (Machine learning, normal logistic and King and Zeng)
# are minimal. 
# Using Konar's paper as benchmark, we predict substantially lower observations
# We employ model 9 since it gives us the highest number of ones.

library(tidyverse)
library(readstata13)
library(plm)
library(caret)
library(glmnet)
library(Zelig)
library(pscl)

set.seed(123)
cutoff <- 0.5 # <------ select probabilitiy cut-off

### Obtaining the datasets
st_trade_flows <- read.dta13(file = "output/st_trade_flows.dta")
st_trade_flows <- st_trade_flows %>% select(!notrade)
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

### function to test predictive power at three points:
## For all data in the model
## For data that observe trade (at 1)
## For fata that observe no trade (at 0)
pred_power <- function(observed, predicted, cutoff, model) {
  
  observed[is.na(observed)] <- 0
  predicted[is.na(predicted)] <- 0

  predicted <- ifelse(predicted > cutoff, 1, 0)
  
  obs_n <- length(observed)
  right_n    <- sum(observed == predicted) # Number of corrected predictions
  
  obs_n_1 <- length(observed[observed == 1])
  right_n_1  <-sum(observed == predicted & observed == 1)
  
  obs_n_0  <- length(observed[observed == 0])
  right_n_0  <-sum(observed == predicted & observed == 0)
  
  
  cat("\n")
  cat(" ::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::", "\n")
  cat(" For all data -- the number of observations is:", obs_n, "\n")
  cat(" Percentage of correct predictions:", 100*right_n/obs_n, "\n")
  cat("\n")
  cat(" For observed trade data -- the number of observations is:", obs_n_1, "\n")
  cat(" Percentage of correct predictions (at 1):", 100*right_n_1/obs_n_1, "\n")
  cat("\n")
  cat(" For observed no trade data -- the number of observations is:", obs_n_0, "\n")
  cat(" Percentage of correct predictions (at 0):", 100*right_n_0/obs_n_0, "\n")  
  cat(" ::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::", "\n")
  
  output <- c(model, 
              round(100*right_n/obs_n, 2), 
              round(100*right_n_1/obs_n_1, 2), 
              round(100*right_n_0/obs_n_0, 2))
  
  output

}

### This is the priors that we employ in King and Zeng that requires us
### to have some weights about what we know the share of ones is in our 
### dataset
num_ones <- sum(st_trade_flows$istrade == 1)
num_all <- length(st_trade_flows$orig)
prior <- num_ones/num_all

# Includes intra for only 2017--- model 1
reg_formula <- "istrade ~ distance + intra + contiguity + sales_i + gdp_j + y17"
indices <- st_trade_flows$year == 2017
glm_fit <- glm(formula = paste0(reg_formula, FE_formula),
               data = st_trade_flows[indices,], 
               family = "binomial")
st_trade_flows$model1 <- predict(glm_fit,
                                 newdata = st_trade_flows, 
                                 type = "response")
model1 <- pred_power(st_trade_flows$istrade[indices],
                     st_trade_flows$model1[indices],
                     cutoff = cutoff,
                     "Logi intra -- 2017")

# Includes intra for only 2012--- model 2
reg_formula <- "istrade ~ distance + intra + contiguity + sales_i + gdp_j + y17"
indices <- st_trade_flows$year == 2012
glm_fit <- glm(formula = paste0(reg_formula, FE_formula),
               data = st_trade_flows[indices,], 
               family = "binomial")
st_trade_flows$model2 <- predict(glm_fit,
                                 newdata = st_trade_flows, 
                                 type = "response")
model2 <- pred_power(st_trade_flows$istrade[indices],
                     st_trade_flows$model2[indices],
                     cutoff = cutoff,
                     "Logi intra -- 2012")

# Without intra for only 2017--- model 3
reg_formula <- "istrade ~ distance + intra + contiguity + sales_i + gdp_j + y17"
indices <- st_trade_flows$year == 2017 & st_trade_flows$orig_ini != st_trade_flows$dest_ini
glm_fit <- glm(formula = paste0(reg_formula, FE_formula),
               data = st_trade_flows[indices,], 
               family = "binomial")
st_trade_flows$model3 <- predict(glm_fit,
                                 newdata = st_trade_flows, 
                                 type = "response")
model3 <- pred_power(st_trade_flows$istrade[indices],
                     st_trade_flows$model3[indices],
                     cutoff = cutoff,
                     "Logi no intra -- 2017 A")

# Without intra for only 2012--- model 4
reg_formula <- "istrade ~ distance + intra + contiguity + sales_i + gdp_j + y17"
indices <- st_trade_flows$year == 2012 & st_trade_flows$orig_ini != st_trade_flows$dest_ini
glm_fit <- glm(formula = paste0(reg_formula, FE_formula),
               data = st_trade_flows[indices,], 
               family = "binomial")
st_trade_flows$model4 <- predict(glm_fit,
                                 newdata = st_trade_flows, 
                                 type = "response")
model4 <- pred_power(st_trade_flows$istrade[indices],
                     st_trade_flows$model4[indices],
                     cutoff = cutoff,
                     "Logi no intra -- 2012 A")

# Without intra for only 2017--- model 5
reg_formula <- "istrade ~ distance + sales_i + gdp_j + y17"
indices <- st_trade_flows$year == 2017 & st_trade_flows$orig_ini != st_trade_flows$dest_ini
glm_fit <- glm(formula = paste0(reg_formula, FE_formula),
               data = st_trade_flows[indices,], 
               family = "binomial")
st_trade_flows$model5 <- predict(glm_fit,
                                 newdata = st_trade_flows, 
                                 type = "response")
model5 <- pred_power(st_trade_flows$istrade[indices],
                     st_trade_flows$model5[indices],
                     cutoff = cutoff,
                     "Logi no intra -- 2017 B")

# Without intra for only 2012--- model 6
reg_formula <- "istrade ~ distance + sales_i + gdp_j + y17"
indices <- st_trade_flows$year == 2012 & st_trade_flows$orig_ini != st_trade_flows$dest_ini
glm_fit <- glm(formula = paste0(reg_formula, FE_formula),
               data = st_trade_flows[indices,], 
               family = "binomial")
st_trade_flows$model6 <- predict(glm_fit,
                                 newdata = st_trade_flows, 
                                 type = "response")
model6 <- pred_power(st_trade_flows$istrade[indices],
                     st_trade_flows$model6[indices],
                     cutoff = cutoff,
                     "Logi no intra -- 2012 B")

# Machine learning --- Penalized logistic regression -- analogous to model 5---- model 7
data <- st_trade_flows %>%
  filter(year == 2017) %>%
  filter(orig != dest) %>%
  select(istrade,
         distance,
         sales_i,
         gdp_j,
         names(st_trade_flows)[grepl("^dest_FE", names(st_trade_flows))],
         names(st_trade_flows)[grepl("^orig_FE", names(st_trade_flows))])

train_samples <- data$istrade %>% 
  createDataPartition(p = 0.5, list = FALSE)

train_data <- data[train_samples,]
test_data <- data[-train_samples,]

x <- model.matrix(istrade~., train_data)[,-1]
y <- train_data[,1]

cv_lasso <- cv.glmnet(x, y, alpha = 1, family = "binomial")

model <- glmnet(x, y, 
                alpha = 1, 
                family = "binomial",
                lambda = cv_lasso$lambda.min)
coef(model)

x_test <- model.matrix(istrade~., test_data)[,-1]

probs <- model %>% predict(newx = x_test)

model7 <- pred_power(test_data$istrade, probs, .5, "ML no intra -- 2017")

# Machine learning --- Penalized logistic regression -- analogous to model 6---- model 8
data <- st_trade_flows %>%
  filter(year == 2012) %>%
  filter(orig != dest) %>%
  select(istrade,
         distance,
         sales_i,
         gdp_j,
         names(st_trade_flows)[grepl("^dest_FE", names(st_trade_flows))],
         names(st_trade_flows)[grepl("^orig_FE", names(st_trade_flows))])
data <- na.omit(data)

train_samples <- data$istrade %>% 
  createDataPartition(p = 0.5, list = FALSE)

train_data <- data[train_samples,]
test_data <- data[-train_samples,]

x <- model.matrix(istrade~., train_data)[,-1]
y <- train_data[,1]

cv_lasso <- cv.glmnet(x, y, alpha = 1, family = "binomial")

model <- glmnet(x, y, 
                alpha = 1, 
                family = "binomial",
                lambda = cv_lasso$lambda.min)
coef(model)

x_test <- model.matrix(istrade~., test_data)[,-1]

probs <- model %>% predict(newx = x_test)

model8 <- pred_power(test_data$istrade, probs, .5, "ML no intra -- 2012")

# Includes intra for both years--- model 9
reg_formula <- "istrade ~ distance + intra + contiguity + sales_i + gdp_j + y17"
glm_fit09 <- glm(formula = paste0(reg_formula, FE_formula),
                 data = st_trade_flows,
                 family = "binomial")
st_trade_flows$model9 <- predict(glm_fit09,
                                 newdata = st_trade_flows, 
                                 type = "response")
model9 <- pred_power(st_trade_flows$istrade,
                     st_trade_flows$model9,
                     cutoff = cutoff,
                     "Logi intra -- both")

dy_prob_St <- st_trade_flows %>% # Saving state probs from model 9
  filter(year == 2017) %>%
  select(orig, dest, model9) %>%
  rename(probs_st = model9)
saveRDS(dy_prob_St, file = "output/dy_state_probs.rds")

# Without intra for both years--- model 10
reg_formula <- "istrade ~ distance + sales_i + gdp_j + y17"
indices <- st_trade_flows$orig_ini != st_trade_flows$dest_ini
glm_fit <- glm(formula = paste0(reg_formula, FE_formula),
               data = st_trade_flows[indices,], 
               family = "binomial")
st_trade_flows$model10 <- predict(glm_fit,
                                 newdata = st_trade_flows, 
                                 type = "response")
model10 <- pred_power(st_trade_flows$istrade[indices],
                      st_trade_flows$model10[indices],
                      cutoff = cutoff,
                      "Logi no intra -- both")

# Machine learning --- Penalized logistic regression -- analogous to model 9---- model 11
data <- st_trade_flows %>%
  filter(orig != dest) %>%
  select(istrade,
         distance,
         sales_i,
         gdp_j,
         names(st_trade_flows)[grepl("^dest_FE", names(st_trade_flows))],
         names(st_trade_flows)[grepl("^orig_FE", names(st_trade_flows))])

data <- na.omit(data)
train_samples <- data$istrade %>% 
  createDataPartition(p = 0.5, list = FALSE)

train_data <- data[train_samples,]
test_data <- data[-train_samples,]

x <- model.matrix(istrade~., train_data)[,-1]
y <- train_data[,1]

cv_lasso <- cv.glmnet(x, y, alpha = 1, family = "binomial")

model <- glmnet(x, y, 
                alpha = 1, 
                family = "binomial",
                lambda = cv_lasso$lambda.min)
coef(model)

x_test <- model.matrix(istrade~., test_data)[,-1]

probs <- model %>% predict(newx = x_test)

model11 <- pred_power(test_data$istrade, probs, .5, "ML intra -- both")

# Machine learning --- Penalized logistic regression -- analogous to model 10---- model 12
data <- st_trade_flows %>%
  select(istrade,
         distance,
         sales_i,
         gdp_j,
         names(st_trade_flows)[grepl("^dest_FE", names(st_trade_flows))],
         names(st_trade_flows)[grepl("^orig_FE", names(st_trade_flows))])

data <- na.omit(data)
train_samples <- data$istrade %>% 
  createDataPartition(p = 0.5, list = FALSE)

train_data <- data[train_samples,]
test_data <- data[-train_samples,]

x <- model.matrix(istrade~., train_data)[,-1]
y <- train_data[,1]

cv_lasso <- cv.glmnet(x, y, alpha = 1, family = "binomial")

model <- glmnet(x, y, 
                alpha = 1, 
                family = "binomial",
                lambda = cv_lasso$lambda.min)
coef(model)

x_test <- model.matrix(istrade~., test_data)[,-1]

probs <- model %>% predict(newx = x_test)

model12 <- pred_power(test_data$istrade, probs, .5, "ML intra -- both")

# Without intra for both years with small sample--- model 13
reg_formula <- "istrade ~ distance + sales_i + gdp_j + y17"
st_trade_flow_train <- st_trade_flows[st_trade_flows$orig != st_trade_flows$dest,]

st_trade_flow_train_A <- st_trade_flow_train[st_trade_flow_train$istrade == 1,]
st_trade_flow_train_B <- st_trade_flow_train[sample(which(st_trade_flows$istrade == 0), num_ones),]

st_trade_flow_train <- rbind.data.frame(st_trade_flow_train_A, st_trade_flow_train_B)
st_trade_flow_train <- na.omit(st_trade_flow_train)

weigs <- rep(prior, length(st_trade_flow_train$orig))
weigs <- ifelse(st_trade_flow_train$istrade == 1, weigs, 1 - prior)

glm_fit13 <- glm(formula = paste0(reg_formula, FE_formula),
                 data = st_trade_flow_train,
                 weights = weigs,
                 family = "binomial")
st_trade_flows$model13 <- predict(glm_fit13,
                                  newdata = st_trade_flows,
                                  type = "response")

indices <- st_trade_flows$orig_ini != st_trade_flows$dest_ini
model13 <- pred_power(st_trade_flows$istrade[indices],
                      st_trade_flows$model13[indices],
                      cutoff = cutoff,
                      "King_Zeng no intra_small -- both")

# Without intra for both years with small sample--- model 14
num_ones <- sum(st_trade_flows$istrade == 1)
num_all <- sum(st_trade_flows$orig != st_trade_flows$dest)

prior <- num_ones/num_all

reg_formula <- "istrade ~ distance + sales_i + gdp_j + y17"
st_trade_flow_train <- st_trade_flows

st_trade_flow_train_A <- st_trade_flow_train[st_trade_flow_train$istrade == 1,]
st_trade_flow_train_B <- st_trade_flow_train[sample(which(st_trade_flows$istrade == 0), num_ones),]

st_trade_flow_train <- rbind.data.frame(st_trade_flow_train_A, st_trade_flow_train_B)
st_trade_flow_train <- na.omit(st_trade_flow_train)

weigs <- rep(prior, length(st_trade_flow_train$orig))
weigs <- ifelse(st_trade_flow_train$istrade == 1, weigs, 1 - prior)

glm_fit <- glm(formula = paste0(reg_formula, FE_formula),
               data = st_trade_flow_train,
               weights = weigs,
               family = "binomial")

st_trade_flows$model14 <- predict(glm_fit,
                                  newdata = st_trade_flows, 
                                  type = "response")

indices <- st_trade_flows$orig_ini != st_trade_flows$dest_ini
model14 <- pred_power(st_trade_flows$istrade[indices],
                      st_trade_flows$model14[indices],
                      cutoff = cutoff,
                      "King_Zeng small -- both")

# Without intra for both years--- model 15
reg_formula <- "istrade ~ distance + sales_i + gdp_j + y17"
indices <- st_trade_flows$orig_ini != st_trade_flows$dest_ini

weigs <- rep(prior, length(st_trade_flows$orig[indices]))
weigs <- ifelse(st_trade_flows$istrade[indices] == 1, weigs, 1 - prior)

glm_fit <- glm(formula = paste0(reg_formula, FE_formula),
               data = st_trade_flows[indices,],
               weights = weigs,
               family = "binomial")
st_trade_flows$model15 <- predict(glm_fit,
                                  newdata = st_trade_flows, 
                                  type = "response")

model15 <- pred_power(st_trade_flows$istrade[indices],
                      st_trade_flows$model15[indices],
                      cutoff = cutoff,
                      "King_Zeng no intra -- both")

# Without intra for both years --- model 16
weigs <- rep(prior, length(st_trade_flow_train$orig))
weigs <- ifelse(st_trade_flow_train$istrade == 1, weigs, 1 - prior)

glm_fit <- glm(formula = paste0(reg_formula, FE_formula),
               data = st_trade_flow_train,
               weights = weigs,
               family = "binomial")

st_trade_flows$model16 <- predict(glm_fit,
                                  newdata = st_trade_flows, 
                                  type = "response")

model16 <- pred_power(st_trade_flows$istrade,
                      st_trade_flows$model16,
                      cutoff = cutoff,
                      "King_Zeng small -- both")

cbind(model1, model2, model3, model4, 
      model5, model6, model7, model8, 
      model9, model10, model11, model12,
      model13, model14, model15, model16)

### The following code calculates probabilities at the county level
dy_cnty$probs <- predict(glm_fit09, newdata = dy_cnty, type = "response")
dy_cnty_probs <- dy_cnty %>% select(orig, dest, probs)
sum(dy_cnty_probs$probs > .5)
saveRDS(dy_cnty_probs, file = "output/dy_cnty_probs.rds")
# end