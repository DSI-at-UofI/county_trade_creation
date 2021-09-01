rm(list = ls())
# Noe J Nava
# noejn2@illinois.edu
# https://noejn2.github.io/
# Script creates the dyadic_county_flows.rds, which is our 
# main dataset in this repository
# 
# script calculates county exports and imports, but also calculates
# correlaiton between gdp_j and imports, and sales_i and exports
# The last part of the code is interested with mapping the values

#print.results <- askYesNo("Print Results?")

library(tidyverse)
library(rgdal)

dy_cnty <- readRDS(file = 'output/dyadic_county_flows_adjusted.rds')
dy_kona <- read_csv(file = 'data/cntyflows_12_lin_2019.csv')
dy_kona <- dy_kona %>%
  select(ori, des, sctg_2) %>%
  rename(orig = ori,
         dest = des,
         kona = sctg_2)

#exports <- dy_cnty %>% distinct(orig, sales_i)
#imports <- dy_cnty %>% distinct(dest, gdp_j)

# Calculating nodes and links ----
# Konar

# Calculating the links 
links_kona <- sum(dy_kona$kona != 0)

# Calculating the nodes
nodes_kona_orig <- dy_kona %>% 
  filter(dy_kona$kona != 0) %>%
  distinct(orig) %>% 
  pull()

nodes_kona_dest <- dy_kona %>% 
  filter(dy_kona$kona != 0) %>%
  distinct(dest) %>% 
  pull()

nodes_kona <- tibble(c(nodes_kona_orig, nodes_kona_dest))
nodes_kona <- nodes_kona %>% distinct()
nodes_kona <- length(nodes_kona$`c(nodes_kona_orig, nodes_kona_dest)`)

# Ridley

# Calculating the links
dy_cnty$cnty_flows[is.na(dy_cnty$cnty_flows)] <- 0
links_cnty <- sum(dy_cnty$cnty_flows != 0)

# Calculating the nodes
nodes_cnty_orig <- dy_cnty %>%
  filter(dy_cnty$cnty_flows != 0) %>%
  distinct(orig) %>%
  pull()

nodes_cnty_dest <- dy_cnty %>%
  filter(dy_cnty$cnty_flows != 0) %>%
  distinct(dest) %>%
  pull()

nodes_cnty <- tibble(c(nodes_cnty_orig, nodes_cnty_dest))
nodes_cnty <- nodes_cnty %>% distinct()
nodes_cnty <- length(nodes_cnty$`c(nodes_cnty_orig, nodes_cnty_dest)`)

# Dyadic correlation
dyadic <- left_join(dy_cnty,
                    dy_kona,
                    by = c("orig", "dest"))

dyadic$kona[is.na(dyadic$kona)] <- 0
dy_corr <- cor(dyadic$cnty_flows, dyadic$kona)

dyadic <- dyadic[dyadic$cnty_flows != 0,]
dyadic <- dyadic[dyadic$kona != 0,]

dy_corr_rmzero <- cor(dyadic$cnty_flows, dyadic$kona)

# Calculating exports and imports -----
cnty_exp <- aggregate(dy_cnty$cnty_flows, 
                      by = list(fips = dy_cnty$orig), 
                      FUN = sum)
names(cnty_exp)[2] <- "cnty_exports"

cnty_imp <- aggregate(dy_cnty$cnty_flows, 
                      by = list(fips = dy_cnty$dest), 
                      FUN = sum)
names(cnty_imp)[2] <- "cnty_imports"

cnty_exp_kona <- aggregate(dy_kona$kona,
                           by = list(fips = dy_kona$orig),
                           FUN = sum)
names(cnty_exp_kona)[2] <- "kona_exports"

cnty_imp_kona <- aggregate(dy_kona$kona,
                           by = list(fips = dy_kona$dest),
                           FUN = sum)
names(cnty_imp_kona)[2] <- "kona_imports"

# mo stands for monadic  
mo_cnty_orig <- dy_cnty %>%
  select(orig,
         sales_i) %>%
  distinct(orig, sales_i)

mo_cnty_dest <- dy_cnty %>%
  select(dest,
         gdp_j) %>%
  distinct(dest, gdp_j)

# Merging the tibbles
mo_cnty <- left_join(mo_cnty_orig, 
                     mo_cnty_dest,
                     by = c("orig" = "dest"))
names(mo_cnty)[1] <- "fips"

mo_cnty <- left_join(mo_cnty,
                     cnty_exp,
                     by = "fips")

mo_cnty <- left_join(mo_cnty,
                     cnty_imp,
                     by = "fips")

mo_cnty <- left_join(mo_cnty,
                     cnty_imp_kona,
                     by = "fips")

mo_cnty <- left_join(mo_cnty,
                     cnty_exp_kona,
                     by = "fips")

mo_cnty$kona_imports[is.na(mo_cnty$kona_imports)] <- 0 
mo_cnty$kona_exports[is.na(mo_cnty$kona_exports)] <- 0 

# Estimating the correlation -----
cor_imp <- cor(mo_cnty$cnty_imports, mo_cnty$kona_imports)
cor_exp <- cor(mo_cnty$cnty_exports, mo_cnty$kona_exports)

# Checking number of zeros
no_imp <- sum(mo_cnty$cnty_imports == 0)
no_exp <- sum(mo_cnty$cnty_exports == 0)

no_imp_kona <- sum(mo_cnty$kona_imports == 0)
no_exp_kona <- sum(mo_cnty$kona_exports == 0)

if(TRUE) { # Printing results
  cat("\n")
  cat(":::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::", "\n")
  cat("::: The following are comparisons between Konar's county trade flows ::::::::::::::::::::::", "\n")
  cat("::: and our simulated county trade flows ::::::::::::::::::::::::::::::::::::::::::::::::::", "\n")
  cat("::: Note: Konar's flows are weights. ::::::::::::::::::::::::::::::::::::::::::::::::::::::", "\n")
  cat(":::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::", "\n")
  cat("::: Konar number of nodes is:", nodes_kona, "::::::::::::::::::::::::::::::::::::::::::::::", "\n")
  cat("::: Our number of nodes is:", nodes_cnty, "::::::::::::::::::::::::::::::::::::::::::::::", "\n")
  cat(":::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::", "\n")
  cat("::: Konar number of links is:", links_kona, "::::::::::::::::::::::::::::::::::::::::::::::", "\n")
  cat("::: Our number of links is:", links_cnty, "::::::::::::::::::::::::::::::::::::::::::::::", "\n")
  cat(":::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::", "\n")
  cat("::: Dyadic correlation between Konar's and ours is: ", dy_corr, "::::::::::::::::::::::::::", "\n")
  cat("::: Dyadic correlation between Konar's and ours removing zeroes is: ", dy_corr_rmzero, ":::", "\n")
  cat(":::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::", "\n")
  cat("::: Number of no importers in Konar's: ", no_imp_kona, ":::::::::::::::::::::::::::::::::::", "\n")
  cat("::: Number of no importers in ours: ", no_imp, "::::::::::::::::::::::::::::::::::::::::", "\n")
  cat(":::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::", "\n")
  cat("::: Number of no exporters in Konar's: ", no_exp_kona, ":::::::::::::::::::::::::::::::::::", "\n")
  cat("::: Number of no exporters in ours: ", no_exp, "::::::::::::::::::::::::::::::::::::::::", "\n")
  cat(":::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::", "\n")
  cat("::: Monadic correlation between Konar's and ours (imports) is: ", cor_imp, ":::::::::::::::", "\n")
  cat("::: Monadic correlation between Konar's and ours (exports) is: ", cor_exp, ":::", "\n")
  cat(":::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::", "\n")
  cat("::::::::::::::::::: Descriptive statistics of our simulated data ::::::::::::::::::::::::::", "\n")
  cat("::: Average of exports/imports:", mean(mo_cnty$cnty_imports), "::::::::::::::::::::::::::::", "\n")
  cat("::: SD of exports:", sd(mo_cnty$cnty_exports), ":::::::::::::::::::::::::::::::::::::::::::", "\n")
  cat("::: SD of imports:", sd(mo_cnty$cnty_imports), ":::::::::::::::::::::::::::::::::::::::::::", "\n")
  cat("::: Min of exports:", min(mo_cnty$cnty_exports), "::::::::::::::::::::::::::::::::::::::::::", "\n")
  cat("::: Min of imports:", min(mo_cnty$cnty_imports), "::::::::::::::::::::::::::::::::::::::::::", "\n")
  cat("::: Max of exports:", max(mo_cnty$cnty_exports), "::::::::::::::::::::::::::::::::::::::::::", "\n")
  cat("::: Max of imports:", max(mo_cnty$cnty_imports), "::::::::::::::::::::::::::::::::::::::::::", "\n")
  cat(":::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::", "\n")
}

# Creating maps ----
# First we make the tibble files using the shapefiles
# I will save it to save this step in future maps
map_loc <- 'assets/USmap_cnty_df.rds'
if(!file.exists(map_loc)) { # County
  
  USmap_cnty <- readOGR(dsn = 'assets/shp_files/3109_county',
                        layer = 'USmap_county')

  USmap_cnty@data$id <- as.character(0:(length(USmap_cnty@data$ANSI_ST_CO) - 1))
  USmap_cnty@data <- USmap_cnty@data %>%
    select(id, ANSI_ST_CO) %>%
    rename(fips = ANSI_ST_CO) %>%
    mutate(fips = as.character(as.numeric(fips)))
  USmap_cnty_df <- broom::tidy(USmap_cnty)
  USmap_cnty_df <- left_join(USmap_cnty_df,
                             USmap_cnty@data,
                             by = "id")
  saveRDS(USmap_cnty_df, file = map_loc)
}else{
  USmap_cnty_df <- readRDS(map_loc)
}

map_loc <- 'assets/USmap_st_df.rds'
if(!file.exists(map_loc)) { # State
  
  USmap_stat <- readOGR(dsn = 'assets/shp_files/49_state',
                        layer = 'USmap_state')
  
  
  USmap_stat@data$id <- as.character(0:(length(USmap_stat@data$GEOID) - 1))
  USmap_stat@data <- USmap_stat@data %>%
    select(id, GEOID) %>%
    rename(fips = GEOID) %>%
    mutate(fips = as.character(as.numeric(fips)))
  USmap_st_df <- broom::tidy(USmap_stat)
  USmap_st_df <- left_join(USmap_st_df,
                             USmap_stat@data,
                             by = "id")
  saveRDS(USmap_st_df, file = map_loc)
}else{
  USmap_st_df <- readRDS(map_loc)
}

# Second, we create the maps
USmap_cnty_df <- left_join(USmap_cnty_df,
                           mo_cnty,
                           by = "fips")

# I decided to do it in different maps
# Exports
no_breaks <- 9
mu <- mean(USmap_cnty_df$cnty_exports, na.rm = TRUE)
sd <- sd(USmap_cnty_df$cnty_exports, na.rm = TRUE)
max <- max(USmap_cnty_df$cnty_exports, na.rm = TRUE)
seq <- numeric()
for(i in 1:no_breaks){
  if(i == 1) {
    low <- 0
    upp <- 1
    seq <- append(seq, c(low, upp))
  }else{
    upp <- round(upp + sd, digits = 0)
    if(upp >= max | i == no_breaks) {
      upp <- round(max, digits = 0)
      seq <- append(seq, upp)
    }else{
      seq <- append(seq, upp)
    }
  }
}

labels <- character()
for(b in 1:length(seq)) {
  
  break_low <- seq[b]
  break_upp <- seq[b + 1]
  
  if(!is.na(seq[b + 1])){
    interval <- paste(break_low, break_upp, sep = " to ")
    labels <- append(labels, interval)
  }
}

labels

ggplot() +
  geom_polygon(data = USmap_cnty_df,
               aes(x = long,
                   y = lat,
                   group = group,
                   fill = cut(cnty_exports,
                              breaks = seq,
                              labels = labels))) +  
  geom_polygon(data = USmap_st_df,
               aes(x = long,
                   y = lat,
                   group = group),
               fill = NA,
               color = "black",
               size = .1)  +
  labs(
    title = "Simulated exports"
  ) +
  scale_fill_manual(values = c(
    "0 to 1"    = "#f7fbff",
    "1 to 93"  = "#deebf7",
    "93 to 185"  = "#c6dbef",
    "185 to 277"  = "#9ecae1",
    "277 to 369"  = "#6baed6",
    "369 to 461"  = "#4292c6",
    "461 to 553"  = "#2171b5",
    "553 to 645"  = "#08519c",
    "645 to 973"  = "#08306b")) +
  theme(panel.background = element_rect(fill = NA, 
                                        color = NA)) +
  coord_equal() +
  theme(legend.direction = "horizontal",
        legend.position = "bottom") +
  guides(fill = guide_legend(title = "Exports:",
                             title.position = "top"))

# Imports
no_breaks <- 9
mu <- mean(USmap_cnty_df$cnty_imports, na.rm = TRUE)
sd <- sd(USmap_cnty_df$cnty_imports, na.rm = TRUE)
max <- max(USmap_cnty_df$cnty_imports, na.rm = TRUE)
seq <- numeric()
for(i in 1:no_breaks){
  if(i == 1) {
    low <- 0
    upp <- 1
    seq <- append(seq, c(low, upp))
  }else{
    upp <- round(upp + sd, digits = 0)
    if(upp >= max | i == no_breaks) {
      upp <- round(max, digits = 0)
      seq <- append(seq, upp)
    }else{
      seq <- append(seq, upp)
    }
  }
}

labels <- character()
for(b in 1:length(seq)) {
  
  break_low <- seq[b]
  break_upp <- seq[b + 1]
  
  if(!is.na(seq[b + 1])){
    interval <- paste(break_low, break_upp, sep = " to ")
    labels <- append(labels, interval)
  }
}

labels

ggplot() +
  geom_polygon(data = USmap_cnty_df,
               aes(x = long,
                   y = lat,
                   group = group,
                   fill = cut(cnty_imports,
                              breaks = seq,
                              labels = labels))) +  
  geom_polygon(data = USmap_st_df,
               aes(x = long,
                   y = lat,
                   group = group),
               fill = NA,
               color = "black",
               size = .1)  +
  labs(
    title = "Simulated imports"
  ) +
  scale_fill_manual(values = c(
    "0 to 1"    = "#f7fbff",
    "1 to 305"  = "#deebf7",
    "305 to 609"  = "#c6dbef",
    "609 to 913"  = "#9ecae1",
    "913 to 1217"  = "#6baed6",
    "1217 to 1521"  = "#4292c6",
    "1521 to 1825"  = "#2171b5",
    "1825 to 2129"  = "#08519c",
    "2129 to 7630"  = "#08306b")) +
  theme(panel.background = element_rect(fill = NA, 
                                        color = NA)) +
  coord_equal() +
  theme(legend.direction = "horizontal",
        legend.position = "bottom") +
  guides(fill = guide_legend(title = "Imports:",
                             title.position = "top"))
♦# end