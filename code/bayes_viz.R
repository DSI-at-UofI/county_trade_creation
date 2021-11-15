rm(list = ls())

# Noe J Nava
# noejn2@illinois.edu // noejnava2@gmail.com
# https://noejn2.github.io/

# Purpose: ----------------
# Script is the simulation that we employ to estimate the county flows

# Set-up ----
# Configuration:
prop        <- 0.10    # proportion of non-classified to be included in viz
list_graphs <- TRUE    # Print list of graphs

library(tidyverse)
dy_cnty <- readRDS(file = 'output/dy_cnty_viz.rds')

# manipulating data for beauty purposes
dy_cnty <- dy_cnty %>%
  mutate(sales = log10(exp(sales_i))) %>%
  mutate(gdp   = log10(exp(gdp_j)*1000000)) %>%
  mutate(dist  = exp(distance))

sales_mu <- mean(dy_cnty$sales)
gdp_mu   <- mean(dy_cnty$gdp)
dist_mu  <- mean(dy_cnty$dist)
trade_mu <- mean(dy_cnty$cnty_flows)

expand.grid.unique <- function(x, y, include.equals=FALSE) {
  x <- unique(x)
  
  y <- unique(y)
  
  g <- function(i)
  {
    z <- setdiff(y, x[seq_len(i-include.equals)])
    
    if(length(z)) cbind(x[i], z, deparse.level=0)
  }
  
  do.call(rbind, lapply(seq_along(x), g))
  
}
if(list_graphs) {
  vars <- c("sales", "gdp", "distance", "trade")
  cat("\n")
  cat(" The following is a list of all graphs that you will see here:")
  cat("\n")
  print(expand.grid.unique(vars,vars))
}

# 1. X = sales vs y = GDP ----
ggplot() +
  geom_point(data = dy_cnty %>% 
               filter(istrade == 1),
             aes(x = sales,
                 y = gdp),
             shape = 1,
             color = "#31a354") +
  geom_point(data = dy_cnty %>% 
               filter(istrade == 0) %>% 
               slice_sample(prop = .01),
             aes(x = sales,
                 y = gdp),
             shape = 1,
             color = "#3182bd",
             alpha = 0.3) +
  geom_hline(yintercept = gdp_mu, 
             color = "#999999",
             linetype = "dashed",
             size = 0.75) +
  geom_vline(xintercept = sales_mu, 
             color = "#999999",
             linetype = "dashed",
             size = 0.75) + 
  labs(x = "Log base 10 of crop sales", 
       y = "Log base 10 of food processing GDP")

# 2. X = sales vs y = distance ----
ggplot() +
  geom_point(data = dy_cnty %>% 
               filter(istrade == 1),
             aes(x = sales,
                 y = dist),
             shape = 1,
             color = "#31a354") +
  geom_point(data = dy_cnty %>% 
               filter(istrade == 0) %>% 
               slice_sample(prop = .01),
             aes(x = sales,
                 y = dist),
             shape = 1,
             color = "#3182bd",
             alpha = 0.3) +
  geom_point(data = dy_cnty %>% 
               filter(istrade == 1),
             aes(x = sales,
                 y = dist),
             shape = 1,
             color = "#31a354") +
  geom_hline(yintercept = dist_mu, 
             color = "#999999",
             linetype = "dashed",
             size = 0.75) +
  geom_vline(xintercept = sales_mu, 
             color = "#999999",
             linetype = "dashed",
             size = 0.75) + 
  labs(x = "Log base 10 of crop sales", 
       y = "Distance between counties (km)")

# 3. X = sales vs y = trade ----
ggplot() +
  geom_point(data = dy_cnty %>% 
               filter(istrade == 1),
             aes(x = sales,
                 y = cnty_flows),
             shape = 1,
             color = "#31a354") +
  geom_point(data = dy_cnty %>% 
               filter(istrade == 0) %>% 
               slice_sample(prop = .01),
             aes(x = sales,
                 y = cnty_flows),
             shape = 1,
             color = "#3182bd",
             alpha = 0.3) +
  geom_hline(yintercept = trade_mu, 
             color = "#999999",
             linetype = "dashed",
             size = 0.75) +
  geom_vline(xintercept = sales_mu, 
             color = "#999999",
             linetype = "dashed",
             size = 0.75) + 
  labs(x = "Log base 10 of crop sales", 
       y = "SCTG 02 bilateral trade")

# 4. X = distance vs Y = gdp ----
ggplot() +
  geom_point(data = dy_cnty %>% 
               filter(istrade == 1),
             aes(x = dist,
                 y = gdp),
             shape = 1,
             color = "#31a354") +
  geom_point(data = dy_cnty %>% 
               filter(istrade == 0) %>% 
               slice_sample(prop = .01),
             aes(x = dist,
                 y = gdp),
             shape = 1,
             color = "#3182bd",
             alpha = 0.3) +
  geom_hline(yintercept = gdp_mu, 
             color = "#999999",
             linetype = "dashed",
             size = 0.75) +
  geom_vline(xintercept = dist_mu, 
             color = "#999999",
             linetype = "dashed",
             size = 0.75) + 
  labs(x = "Distance between counties (km)", 
       y = "Log base 10 of food processing GDP")

# 5. X = gdp vs Y = trade ----
ggplot() +
  geom_point(data = dy_cnty %>% 
               filter(istrade == 1),
             aes(y = cnty_flows,
                 x = gdp),
             shape = 1,
             color = "#31a354") +
  geom_point(data = dy_cnty %>% 
               filter(istrade == 0) %>% 
               slice_sample(prop = .01),
             aes(y = cnty_flows,
                 x = gdp),
             shape = 1,
             color = "#3182bd",
             alpha = 0.3) +
  geom_hline(yintercept = trade_mu, 
             color = "#999999",
             linetype = "dashed",
             size = 0.75) +
  geom_vline(xintercept = gdp_mu, 
             color = "#999999",
             linetype = "dashed",
             size = 0.75) + 
  labs(y = "SCTG 02 bilateral trade", 
       x = "Log base 10 of food processing GDP")

# 6. X = distance vs Y = trade ----
ggplot() +
  geom_point(data = dy_cnty %>% 
               filter(istrade == 1),
             aes(y = cnty_flows,
                 x = dist),
             shape = 1,
             color = "#31a354") +
  geom_point(data = dy_cnty %>% 
               filter(istrade == 0) %>% 
               slice_sample(prop = .01),
             aes(y = cnty_flows,
                 x = dist),
             shape = 1,
             color = "#3182bd",
             alpha = 0.3) +
  geom_hline(yintercept = trade_mu, 
             color = "#999999",
             linetype = "dashed",
             size = .75) +
  geom_vline(xintercept = dist_mu, 
             color = "#999999",
             linetype = "dashed",
             size = .75) + 
  labs(y = "SCTG 02 bilateral trade", 
       x = "Distance between counties (km)")
# end-----