rm(list = ls())

# Script creates heat map figures of cfs 2012 and 2017 for final paper

groundhog_day <- "2021-09-05"
pkgs <- c("tidyverse", "readstata13", "ggplot2", "hrbrthemes")
groundhog::groundhog.library(pkgs, groundhog_day)

cfs <- read.dta13(file = 'output/st_trade_flows.dta')

cfs <- cfs %>% select(orig_ini, dest_ini, trade, year)

cfs %>% filter(year == 2017) %>%
  ggplot(aes(x = dest_ini,
             y = orig_ini,
             fill = trade)) +
  geom_tile() +
  scale_fill_gradient(low = "white", high = "darkgreen") +
  theme_ipsum() +
  theme(axis.text.x = element_text(angle = 45, vjust = 0.5, size = 8),
        axis.text.y = element_text(size = 7)) +
  labs(y = "Origin (state)", x = "Destination (state)") +
  ggtitle("2017")

cfs$trade[is.na(cfs$trade)] <- 0
cfs %>% filter(year == 2012) %>%
  ggplot(aes(x = dest_ini,
             y = orig_ini,
             fill = trade)) +
  geom_tile() +
  scale_fill_gradient(low = "white", high = "darkgreen") +
  theme_ipsum() +
  theme(axis.text.x = element_text(angle = 45, vjust = 0.5, size = 8),
        axis.text.y = element_text(size = 7)) +
  labs(y = "Origin (state)", x = "Destination (state)") +
  ggtitle("2012")
