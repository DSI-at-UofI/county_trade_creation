rm(list = ls())

# Noe J Nava
# noejn2@illinois.edu // noejnava2@gmail.com
# https://noejn2.github.io/

# Purpose: ----------------
# Create figure of maps highlighting expors, imports and domestic
# To highlight in-state domestic consumption, imports and exports
# are excluded at the state level, not county levels

# This is groundhog config to keep useful versions of packages
# Do not change groundhog_day!!
# check: http://datacolada.org/95
groundhog_day <- "2021-09-05"
pkgs <- c("tidyverse", "rgdal", "collapse", "readstata13")
groundhog::groundhog.library(pkgs, groundhog_day)

# Setting up the tibbles for the maps ----
dy_cnty <- readRDS(file = 'output/dyadic_county_flows_adjusted.rds')
dy_cnty <- dy_cnty %>% select(!c(orig_stIni, sales_i, gdp_j))

dy_kona <- read_csv(file = 'data/cntyflows_12_lin_2019.csv')
dy_kona <- dy_kona %>%
  select(ori, des, sctg_2) %>%
  rename(orig = ori,
         dest = des,
         cnty_flows_kona = sctg_2) %>%
  mutate(orig = as.character(as.numeric(orig))) %>%
  mutate(dest = as.character(as.numeric(dest)))
key <- read_csv(file = "data/data_needs/location_master_key.csv")
key <- key %>% 
  select(st_name, fips) %>% 
  mutate(st_name = tolower(st_name)) %>%
  mutate(fips = as.character(fips))

dy_kona <- left_join(dy_kona, key, by = c("orig" = "fips"))
names(dy_kona)[4] <- "orig_stName"

dy_kona <- left_join(dy_kona, key, by = c("dest" = "fips"))
names(dy_kona)[5] <- "dest_stName"

# To be able to used log10:
dy_cnty$cnty_flows <- dy_cnty$cnty_flows*1000000
dy_kona$cnty_flows_kona <- dy_kona$cnty_flows_kona/25.4 #bushels

# imports:
# ours
imports_cnty <- dy_cnty %>% filter(orig_stName != dest_stName)
imports_cnty <- aggregate(imports_cnty$cnty_flows,
                          by = list(fips = imports_cnty$dest),
                          FUN = sum)
names(imports_cnty)[2] <- "imports"
#konars
imports_cnty_kona <- dy_kona %>% filter(orig_stName != dest_stName)
imports_cnty_kona <- aggregate(imports_cnty_kona$cnty_flows_kona,
                               by = list(fips = imports_cnty_kona$dest),
                               FUN = sum)
names(imports_cnty_kona)[2] <- "imports_kona"

# exports:
#ours
exports_cnty <- dy_cnty %>% filter(orig_stName != dest_stName)
exports_cnty <- aggregate(exports_cnty$cnty_flows,
                          by = list(fips = exports_cnty$orig),
                          FUN = sum)
names(exports_cnty)[2] <- "exports"
#konars
exports_cnty_kona <- dy_kona %>% filter(orig_stName != dest_stName)
exports_cnty_kona <- aggregate(exports_cnty_kona$cnty_flows_kona,
                               by = list(fips = exports_cnty_kona$orig),
                               FUN = sum)
names(exports_cnty_kona)[2] <- "exports_kona"

# domestic:
#ours
domestic_cnty <- dy_cnty %>% filter(orig_stName == dest_stName)
domestic_cnty <- aggregate(domestic_cnty$cnty_flows,
                           by = list(fips = domestic_cnty$orig),
                           FUN = sum)
names(domestic_cnty)[2] <- "domestic"
#konars
domestic_cnty_kona <- dy_kona %>% filter(orig_stName == dest_stName)
domestic_cnty_kona <- aggregate(domestic_cnty_kona$cnty_flows_kona,
                           by = list(fips = domestic_cnty_kona$orig),
                           FUN = sum)
names(domestic_cnty_kona)[2] <- "domestic_kona"

# Merging tibbles for porgramming simplification
cnty_tb <- left_join(domestic_cnty, 
                     exports_cnty,
                     by = "fips")
cnty_tb <- left_join(cnty_tb,
                     imports_cnty,
                     by = "fips")

cnty_tb_kona <- left_join(domestic_cnty_kona,
                          exports_cnty_kona,
                          by = "fips")
cnty_tb_kona <- left_join(cnty_tb_kona,
                          imports_cnty_kona,
                          by = "fips")

rm(list = setdiff(ls(), c("cnty_tb", "cnty_tb_kona")))
# Creating maps ----
# First we make the tibble files using the shapefiles
# I will save it to save this step in future maps:
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
USmap_cnty_df <- left_join(USmap_cnty_df, cnty_tb, by = "fips")
USmap_cnty_df <- left_join(USmap_cnty_df, cnty_tb_kona, by = "fips")

# Exports (outflows excluding in-state)
## ours
USmap_cnty_df$exports <- log10(USmap_cnty_df$exports)
USmap_cnty_df$exports[is.infinite(USmap_cnty_df$exports)] <- 0
summary(USmap_cnty_df$exports)

breaks = c(0, 5, 6, 7, 8, 9, 10)
labels = c("Below $100,000",
           " $100,000 and $1 M",
           " $1 M and $10 M",
           " $10 M and $100 M",
           " $100 M and  $1 B",
           "Above $1 B")

ggplot() +
  geom_polygon(data = USmap_cnty_df,
               aes(x = long,
                   y = lat,
                   group = group,
                   fill = cut(exports,
                              breaks = breaks,
                              labels = labels)),
               color = "grey",
               size = .05) +
  geom_polygon(data = USmap_st_df,
               aes(x = long,
                   y = lat,
                   group = group),
               fill = NA,
               color = "black",
               size = .6) +
  labs(
    title = "Out-state SCTG 02 exports (county level)"
  ) +
  scale_fill_manual(values   = c(
    "Below $100,000"         = "#edf8e9",
    " $100,000 and $1 M"     = "#c7e9c0",
    " $1 M and $10 M"        = "#a1d99b",
    " $10 M and $100 M"      = "#74c476",
    " $100 M and  $1 B"      = "#31a354",
    "Above $1 B"             = "#006d2c")) +
  theme(panel.background = element_rect(fill = NA, 
                                        color = NA)) +
  coord_equal() +
  theme(legend.direction = "horizontal",
        legend.position = "bottom") +
  guides(fill = guide_legend(title = "",
                             title.position = "top"))

## konars
USmap_cnty_df$exports_kona <- log10(USmap_cnty_df$exports_kona)
USmap_cnty_df$exports_kona[is.infinite(USmap_cnty_df$exports_kona)] <- 0
summary(USmap_cnty_df$exports_kona)

breaks = c(-2, 5, 6, 7, 8, 9, 10)
labels = c("Below 100,000 Bushels",
           " 100,000 and 1 M Bushels",
           " 1 M and 10 M Bushels",
           " 10 M and 100 M Bushels",
           " 100 M and  1 B Bushels",
           "Above $1 B Bushels")

ggplot() +
  geom_polygon(data = USmap_cnty_df,
               aes(x = long,
                   y = lat,
                   group = group,
                   fill = cut(exports_kona,
                              breaks = breaks,
                              labels = labels)),
               color = "grey",
               size = .05) +
  geom_polygon(data = USmap_st_df,
               aes(x = long,
                   y = lat,
                   group = group),
               fill = NA,
               color = "black",
               size = .6) +
  labs(
    title = "Out-state SCTG 02 exports (county level)"
  ) +
  scale_fill_manual(values   = c(
    "Below 100,000 Bushels"    = "#edf8e9",
    " 100,000 and 1 M Bushels" = "#c7e9c0",
    " 1 M and 10 M Bushels"    = "#a1d99b",
    " 10 M and 100 M Bushels"  = "#74c476",
    " 100 M and  1 B Bushels"  = "#31a354",
    "Above $1 B Bushels"       = "#006d2c")) +
  theme(panel.background = element_rect(fill = NA, 
                                        color = NA)) +
  coord_equal() +
  theme(legend.direction = "horizontal",
        legend.position = "bottom") +
  guides(fill = guide_legend(title = "",
                             title.position = "top"))

# Imports (outflows excluding in-state)
## ours
USmap_cnty_df$imports <- log10(USmap_cnty_df$imports)
USmap_cnty_df$imports[is.infinite(USmap_cnty_df$imports)] <- 0
summary(USmap_cnty_df$imports)

breaks = c(0, 5, 6, 7, 8, 9, 10)
labels = c("Below $100,000",
           " $100,000 and $1 M",
           " $1 M and $10 M",
           " $10 M and $100 M",
           " $100 M and  $1 B",
           "Above $1 B")

ggplot() +
  geom_polygon(data = USmap_cnty_df,
               aes(x = long,
                   y = lat,
                   group = group,
                   fill = cut(imports,
                              breaks = breaks,
                              labels = labels)),
               color = "grey",
               size = .05) +
  geom_polygon(data = USmap_st_df,
               aes(x = long,
                   y = lat,
                   group = group),
               fill = NA,
               color = "black",
               size = .6) +
  labs(
    title = "Out-state SCTG 02 imports (county level)"
  ) +
  scale_fill_manual(values   = c(
    "Below $100,000"         = "#edf8e9",
    " $100,000 and $1 M"     = "#c7e9c0",
    " $1 M and $10 M"        = "#a1d99b",
    " $10 M and $100 M"      = "#74c476",
    " $100 M and  $1 B"      = "#31a354",
    "Above $1 B"             = "#006d2c")) +
  theme(panel.background = element_rect(fill = NA, 
                                        color = NA)) +
  coord_equal() +
  theme(legend.direction = "horizontal",
        legend.position = "bottom") +
  guides(fill = guide_legend(title = "",
                             title.position = "top"))

## konars
USmap_cnty_df$imports_kona <- log10(USmap_cnty_df$imports_kona)
USmap_cnty_df$imports_kona[is.infinite(USmap_cnty_df$imports_kona)] <- 0
summary(USmap_cnty_df$imports_kona)

breaks = c(-2, 5, 6, 7, 8, 9, 10)
labels = c("Below 100,000 Bushels",
           " 100,000 and 1 M Bushels",
           " 1 M and 10 M Bushels",
           " 10 M and 100 M Bushels",
           " 100 M and  1 B Bushels",
           "Above $1 B Bushels")

ggplot() +
  geom_polygon(data = USmap_cnty_df,
               aes(x = long,
                   y = lat,
                   group = group,
                   fill = cut(imports_kona,
                              breaks = breaks,
                              labels = labels)),
               color = "grey",
               size = .05) +
  geom_polygon(data = USmap_st_df,
               aes(x = long,
                   y = lat,
                   group = group),
               fill = NA,
               color = "black",
               size = .6) +
  labs(
    title = "Out-state SCTG 02 imports (county level)"
  ) +
  scale_fill_manual(values   = c(
    "Below 100,000 Bushels"    = "#edf8e9",
    " 100,000 and 1 M Bushels" = "#c7e9c0",
    " 1 M and 10 M Bushels"    = "#a1d99b",
    " 10 M and 100 M Bushels"  = "#74c476",
    " 100 M and  1 B Bushels"  = "#31a354",
    "Above $1 B Bushels"       = "#006d2c")) +
  theme(panel.background = element_rect(fill = NA, 
                                        color = NA)) +
  coord_equal() +
  theme(legend.direction = "horizontal",
        legend.position = "bottom") +
  guides(fill = guide_legend(title = "",
                             title.position = "top"))

# Domestic (focus on in-state flows only)
## ours
USmap_cnty_df$domestic <- log10(USmap_cnty_df$domestic)
USmap_cnty_df$domestic[is.infinite(USmap_cnty_df$domestic)] <- 0
summary(USmap_cnty_df$domestic)

breaks = c(0, 5, 6, 7, 8, 9, 10)
labels = c("Below $100,000",
           " $100,000 and $1 M",
           " $1 M and $10 M",
           " $10 M and $100 M",
           " $100 M and  $1 B",
           "Above $1 B")

ggplot() +
  geom_polygon(data = USmap_cnty_df,
               aes(x = long,
                   y = lat,
                   group = group,
                   fill = cut(domestic,
                              breaks = breaks,
                              labels = labels)),
               color = "grey",
               size = .05) +
  geom_polygon(data = USmap_st_df,
               aes(x = long,
                   y = lat,
                   group = group),
               fill = NA,
               color = "black",
               size = .6) +
  labs(
    title = "In-state SCTG 02 domestic (county level)"
  ) +
  scale_fill_manual(values   = c(
    "Below $100,000"         = "#edf8e9",
    " $100,000 and $1 M"     = "#c7e9c0",
    " $1 M and $10 M"        = "#a1d99b",
    " $10 M and $100 M"      = "#74c476",
    " $100 M and  $1 B"      = "#31a354",
    "Above $1 B"             = "#006d2c")) +
  theme(panel.background = element_rect(fill = NA, 
                                        color = NA)) +
  coord_equal() +
  theme(legend.direction = "horizontal",
        legend.position = "bottom") +
  guides(fill = guide_legend(title = "",
                             title.position = "top"))
## konars
USmap_cnty_df$domestic_kona <- log10(USmap_cnty_df$domestic_kona)
USmap_cnty_df$domestic_kona[is.infinite(USmap_cnty_df$domestic_kona)] <- 0
summary(USmap_cnty_df$domestic_kona)

breaks = c(-2, 5, 6, 7, 8, 9, 10)
labels = c("Below 100,000 Bushels",
           " 100,000 and 1 M Bushels",
           " 1 M and 10 M Bushels",
           " 10 M and 100 M Bushels",
           " 100 M and  1 B Bushels",
           "Above $1 B Bushels")

ggplot() +
  geom_polygon(data = USmap_cnty_df,
               aes(x = long,
                   y = lat,
                   group = group,
                   fill = cut(domestic_kona,
                              breaks = breaks,
                              labels = labels)),
               color = "grey",
               size = .05) +
  geom_polygon(data = USmap_st_df,
               aes(x = long,
                   y = lat,
                   group = group),
               fill = NA,
               color = "black",
               size = .6) +
  labs(
    title = "In-state SCTG 02 domestic (county level)"
  ) +
  scale_fill_manual(values   = c(
    "Below 100,000 Bushels"    = "#edf8e9",
    " 100,000 and 1 M Bushels" = "#c7e9c0",
    " 1 M and 10 M Bushels"    = "#a1d99b",
    " 10 M and 100 M Bushels"  = "#74c476",
    " 100 M and  1 B Bushels"  = "#31a354",
    "Above $1 B Bushels"       = "#006d2c")) +
  theme(panel.background = element_rect(fill = NA, 
                                        color = NA)) +
  coord_equal() +
  theme(legend.direction = "horizontal",
        legend.position = "bottom") +
  guides(fill = guide_legend(title = "",
                             title.position = "top"))
# end