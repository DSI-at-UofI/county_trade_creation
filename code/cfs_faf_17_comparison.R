rm(list = ls())

groundhog_day <- "2021-09-05"
pkgs <- c("tidyverse", "readstata13", "collapse")
groundhog::groundhog.library(pkgs, groundhog_day)

cfs_2017 <- read.dta13(file = 'data/cfs_cleaned_data/cfs_2017_sctg02_collapsed.dta')
faf_2017 <- read.dta13(file = 'data/data_needs/faf4.5.1_2017_state.dta')

# cleaning faf data
faf_2017 <- faf_2017 %>% 
  filter(sctg2 == "2") %>%
  filter(!orig_state %in% c("HI", "DC", "AK")) %>%
  filter(!dest_state %in% c("HI", "DC", "AK")) %>%
  select(orig_state,
         dest_state,
         value)
faf_2017 <- collap(faf_2017, ~orig_state+dest_state, FUN = sum)

faf_2017 <- faf_2017 %>%
  rename(orig = orig_state,
         dest = dest_state,
         trade_faf = value)

# cleaning cfs data
cfs_2017 <- cfs_2017 %>%
  select(orig_ini,
         dest_ini,
         value) %>%
  rename(orig = orig_ini,
         dest = dest_ini,
         trade_cfs = value)

cfs_2017 <- collap(cfs_2017, ~orig+dest, FUN = sum)

# creating repo dataset
state_list <- faf_2017 %>% distinct(orig) %>% pull()

trade_tb <- expand.grid(state_list, state_list)
names(trade_tb) <- c("orig", "dest")

# Merging the datasets
trade_tb <- left_join(trade_tb, faf_2017,
                      by = c("orig", "dest"))
trade_tb$trade_faf[is.na(trade_tb$trade_faf)] <- 0

trade_tb <- left_join(trade_tb, cfs_2017,
                      by = c("orig", "dest"))
trade_tb$trade_cfs[is.na(trade_tb$trade_cfs)] <- 0

trade_tb$trade_cfs <- trade_tb$trade_cfs/1000000

# Flows:
mean(trade_tb$trade_faf)
sd(trade_tb$trade_faf)

mean(trade_tb$trade_cfs)
sd(trade_tb$trade_cfs)

cor(trade_tb$trade_faf, trade_tb$trade_cfs)

# Nodes:

#faf
nodes_faf_o <- trade_tb %>%
  filter(trade_tb$trade_faf != 0) %>%
  distinct(orig) %>%
  pull()

nodes_faf_d <- trade_tb %>%
  filter(trade_tb$trade_faf != 0) %>%
  distinct(dest) %>%
  pull()

nodes_faf <- tibble(c(nodes_faf_o, nodes_faf_d))
nodes_faf <- nodes_faf %>% distinct()
length(nodes_faf$`c(nodes_faf_o, nodes_faf_d)`)

#cfs
nodes_cfs_o <- trade_tb %>%
  filter(trade_tb$trade_cfs != 0) %>%
  distinct(orig) %>%
  pull()

nodes_cfs_d <- trade_tb %>%
  filter(trade_tb$trade_cfs != 0) %>%
  distinct(dest) %>%
  pull()

nodes_cfs <- tibble(c(nodes_cfs_o, nodes_cfs_d))
nodes_cfs <- nodes_cfs %>% distinct()
length(nodes_cfs$`c(nodes_cfs_o, nodes_cfs_d)`)

# Links:
sum(trade_tb$trade_faf != 0)
sum(trade_tb$trade_cfs != 0)

# Exports:
cfs_exports <- aggregate(trade_tb$trade_cfs,
                         by = list(states = trade_tb$orig),
                         FUN = sum)
mean(cfs_exports$x)
sd(cfs_exports$x)

faf_exports <- aggregate(trade_tb$trade_faf,
                         by = list(states = trade_tb$orig),
                         FUN = sum)
mean(faf_exports$x)
sd(faf_exports$x)

cor(cfs_exports$x, faf_exports$x)

#Imports:
cfs_imports <- aggregate(trade_tb$trade_cfs,
                         by = list(states = trade_tb$dest),
                         FUN = sum)
mean(cfs_imports$x)
sd(cfs_imports$x)

faf_imports <- aggregate(trade_tb$trade_faf,
                         by = list(states = trade_tb$dest),
                         FUN = sum)
mean(faf_imports$x)
sd(faf_imports$x)

cor(cfs_imports$x, faf_imports$x)
















