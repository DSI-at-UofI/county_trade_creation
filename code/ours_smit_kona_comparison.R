rm(list = ls())

# Noe J Nava
# noejn2@illinois.edu // noejnava2@gmail.com
# https://noejn2.github.io/

# Purpose: ----------------
# Here, we compare  our datasets
# and the dataset in Lin, Ruess, Marston and Konar (2019) dataset,
# and Smith et al (2017)

# This is groundhog config to keep useful versions of packages
# Do not change groundhog_day!!
# check: http://datacolada.org/95
groundhog_day <- "2021-09-05"
pkgs <- c("tidyverse", "rgdal", "collapse", "readstata13")
groundhog::groundhog.library(pkgs, groundhog_day)

# Cleaning data to be comparable and put into single dataset -----
dy_cnty <- readRDS(file = 'output/dyadic_county_flows_adjusted.rds')
dy_kona <- read_csv(file = 'data/cntyflows_12_lin_2019.csv')
dy_smit <- read_csv(file = 'data/cntyflows_12_smith_2017.csv')

# Fixing our dataset
dy_cnty <- dy_cnty %>% select(!c(sales_i, gdp_j, orig_stIni))
class(dy_cnty$orig)

# Fixing Konar's dataset
dy_kona <- dy_kona %>% select(ori, des, sctg_2)
dy_kona <- dy_kona %>%
  mutate(orig = as.character(as.numeric(ori))) %>%
  mutate(dest = as.character(as.numeric(des))) %>%
  select(!c(ori, des)) %>%
  rename(kona = sctg_2)

# Fixing Smith's dataset
dy_smit <- dy_smit %>% select(`Demand FIPS`,
                              `Corn FIPS`,
                              `Corn (bu)`) %>%
  rename(orig = `Demand FIPS`,
         dest = `Corn FIPS`,
         smit = `Corn (bu)`) %>%
  mutate(orig = as.character(orig),
         dest = as.character(dest))

dy_smit <- collap(dy_smit, ~orig+dest, FUN = sum)

# Putting together all the datasets
dy_cnty <- left_join(dy_cnty, dy_kona, by = c("orig", "dest"))
dy_cnty$kona[is.na(dy_cnty$kona)] <- 0

cnty_list <- dy_cnty %>% distinct(orig) %>% pull()
kona_list <- dy_kona %>% distinct(orig) %>% pull()

# Notice that Konar's dataset has more cnties. The following counties:
sum(dy_cnty$kona) == sum(dy_kona$kona)
kona_list[!kona_list %in% cnty_list]

dy_cnty <- left_join(dy_cnty, dy_smit, by = c("orig", "dest"))
dy_cnty$smit[is.na(dy_cnty$smit)] <- 0

smit_list <- dy_smit %>% distinct(orig) %>% pull()

# Notice that all counties on Smith's appear on our dataset
sum(dy_cnty$smit) == sum(dy_smit$smit)

# Harmonizing konars' data into bushels so they are comparable
dy_cnty$kona <- dy_cnty$kona/25.4

# now into millions of bushels
dy_cnty$kona <- dy_cnty$kona/1000000
dy_cnty$smit <- dy_cnty$smit/1000000

rm(list = setdiff(ls(), "dy_cnty"))

# Filling up the graph theory table; Nodes, links, no imports, no exporters ----
# Nodes:
flows_list <- c("cnty_flows", "kona", "smit")
for(flow in flows_list) {
  nodes_cnty_orig <- dy_cnty %>%
    filter(dy_cnty[[flow]] != 0) %>%
    distinct(orig) %>%
    pull()
  
  nodes_cnty_dest <- dy_cnty %>%
    filter(dy_cnty[[flow]] != 0) %>%
    distinct(dest) %>%
    pull()
  
  nodes_cnty <- tibble(c(nodes_cnty_orig, nodes_cnty_dest))
  nodes_cnty <- nodes_cnty %>% distinct()
  nodes_cnty <- length(nodes_cnty$`c(nodes_cnty_orig, nodes_cnty_dest)`)
  cat("\n")
  cat(":::::", flow, "has", nodes_cnty, "nodes. ::::::")
}

# Links:
flows_list <- c("cnty_flows", "kona", "smit")
for(flow in flows_list) {
  links_cnty <- sum(dy_cnty[[flow]] != 0)
  cat("\n")
  cat(":::::", flow, "has", links_cnty, "links. ::::::")
}

# No importers:
flows_list <- c("cnty_flows", "kona", "smit")
for(flow in flows_list) {
  cnty_exp <- aggregate(dy_cnty[[flow]], 
                        by = list(fips = dy_cnty$orig), 
                        FUN = sum)
  names(cnty_exp)[2] <- "exports"
  cnty_exp$exports[is.na(cnty_exp$exports)] <- 0
  no_exp <- sum(cnty_exp$exports == 0)
  
  cnty_imp <- aggregate(dy_cnty[[flow]], 
                        by = list(fips = dy_cnty$dest), 
                        FUN = sum)
  names(cnty_imp)[2] <- "imports"
  cnty_imp$imports[is.na(cnty_imp$imports)] <- 0
  no_imp <- sum(cnty_imp$imports == 0)
  
  cat("\n")
  cat(":::::", flow, "has", no_exp, "no exporters, and", no_imp, " no importers ::::::")
  
}

# Filling up the correlation table ----
flows_list <- c("cnty_flows", "kona", "smit")
for(flow1 in flows_list) {
  for(flow2 in flows_list) {
    rho <- cor(dy_cnty[[flow1]], dy_cnty[[flow2]])
    cat("\n")
    cat("::: The correlation betwee", flow1, "and", flow2, "is", rho, ":::")
  }
}

# Top county flows across all datasets ----
flows_list <- c("cnty_flows", "kona", "smit")
for(flow in flows_list) {
  flows <- dy_cnty %>% 
    arrange(desc(dy_cnty[[flow]])) %>% 
    select(orig, dest, flow)
  
  cat("\n")
  cat("::: Top flows for", flow, "are shown below: ::::::")
  cat("\n")
  print(head(flows, 5))
}
# end