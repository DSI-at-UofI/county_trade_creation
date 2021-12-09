rm(list = ls())

library(tidyverse)

key <- read_csv(file = "data/data_needs/location_master_key.csv")
key <- key %>% 
  select(st_initial, st_name) %>%
  mutate(st_name = tolower(st_name)) %>%
  distinct()

### Destination
mundalkOut_dest <- readRDS(file = "diagnostics/mundlakOut_dest.rds")
mundalkOut_dest <- mundalkOut_dest %>%
  mutate(upper_12 = coeff_12 + 1.96*SE_12,
         lower_12 = coeff_12 - 1.96*SE_12,
         upper_17 = coeff_12 + 1.96*SE_17,
         lower_17 = coeff_12 - 1.96*SE_17) %>%
  mutate(id = row_number())
mundalkOut_dest <- left_join(mundalkOut_dest, 
                             key, 
                             by = c("state" = "st_name"))

# Dest // 2012
ggplot(mundalkOut_dest) +
  geom_errorbar(aes(ymax = upper_12,
                    ymin = lower_12,
                    x = st_initial)) +
  geom_point(aes(x = st_initial,
                 y = coeff_12)) + 
  geom_hline(aes(yintercept = 0), 
             linetype = "dashed") +
  scale_x_discrete(expand = c(.01, 0)) +
  theme(panel.background = element_rect(fill = NA, 
                                        color = NA),
        axis.text.x = element_text(angle = 90))

# Dest // 2017
ggplot(mundalkOut_dest) +
  geom_errorbar(aes(ymax = upper_17,
                    ymin = lower_17,
                    x = st_initial)) +
  geom_point(aes(x = st_initial,
                 y = coeff_17)) + 
  geom_hline(aes(yintercept = 0), 
             linetype = "dashed") +
  scale_x_discrete(expand = c(.01, 0)) +
  theme(panel.background = element_rect(fill = NA, 
                                        color = NA),
        axis.text.x = element_text(angle = 90))

### Origin
mundalkOut_orig <- readRDS(file = "diagnostics/mundlakOut_orig.rds")
mundalkOut_orig <- mundalkOut_orig %>%
  mutate(upper_12 = coeff_12 + 1.96*SE_12,
         lower_12 = coeff_12 - 1.96*SE_12,
         upper_17 = coeff_12 + 1.96*SE_17,
         lower_17 = coeff_12 - 1.96*SE_17) %>%
  mutate(id = row_number())
mundalkOut_orig <- left_join(mundalkOut_orig, 
                             key, 
                             by = c("state" = "st_name"))

# Orig // 2012
ggplot(mundalkOut_orig) +
  geom_errorbar(aes(ymax = upper_12,
                    ymin = lower_12,
                    x = st_initial)) +
  geom_point(aes(x = st_initial,
                 y = coeff_12)) + 
  geom_hline(aes(yintercept = 0), 
             linetype = "dashed") +
  scale_x_discrete(expand = c(.01, 0)) +
  theme(panel.background = element_rect(fill = NA, 
                                        color = NA),
        axis.text.x = element_text(angle = 90))

# Orig // 2017
ggplot(mundalkOut_orig) +
  geom_errorbar(aes(ymax = upper_17,
                    ymin = lower_17,
                    x = st_initial)) +
  geom_point(aes(x = st_initial,
                 y = coeff_17)) + 
  geom_hline(aes(yintercept = 0), 
             linetype = "dashed") +
  scale_x_discrete(expand = c(.01, 0)) +
  theme(panel.background = element_rect(fill = NA, 
                                        color = NA),
        axis.text.x = element_text(angle = 90))
#end  