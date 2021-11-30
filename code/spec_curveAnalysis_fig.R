rm(list = ls())

library(readstata13)
library(tidyverse)

outs <- readRDS(file = "output/spec_curveAnalysis_results.rds")

out_sales <- outs %>%
  select(!c(gdp, SE_gdp)) %>%
  arrange(sales) %>%
  mutate(id = row_number()) %>%
  mutate(upper = sales + 1.96*SE_sales,
         lower = sales - 1.96*SE_sales)

out_gdp <- outs %>%
  select(!c(sales, SE_sales)) %>%
  arrange(gdp) %>%
  mutate(id = row_number()) %>%
  mutate(upper = gdp + 1.96*SE_gdp,
         lower = gdp - 1.96*SE_gdp)

# Sales
ggplot(out_sales) +
  geom_ribbon(aes(ymax = upper,
                  ymin = lower,
                  x = id),
              fill = "grey70") +
  geom_line(aes(x = id,y = sales)) +
  ylim(0.4,1) +
  theme(panel.background = element_rect(fill = NA, 
                                        color = NA))

# GDP
ggplot(out_gdp) +
  geom_ribbon(aes(ymax = upper,
                  ymin = lower,
                  x = id),
              fill = "grey70") +
  geom_line(aes(x = id,y = gdp)) +
  ylim(0.4,1) +
  theme(panel.background = element_rect(fill = NA, 
                                        color = NA))
