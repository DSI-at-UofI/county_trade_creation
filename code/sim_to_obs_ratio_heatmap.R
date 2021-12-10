rm(list = ls())

data <- readRDS(file = 'diagnostics/sim_to_obs_ratio.rds')

groundhog_day <- "2021-09-05"
pkgs <- c("tidyverse", "ggplot2", "hrbrthemes")
groundhog::groundhog.library(pkgs, groundhog_day)

ggplot(data,
       aes(x = orig,
           y = dest,
           fill = ratio)) +
  geom_tile() +
  scale_fill_gradient(low = "white", high = "darkgreen") +
  theme_ipsum() +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, size = 8),
        axis.text.y = element_text(size = 7)) +
  labs(y = "Origin (state)", x = "Destination (state)")
