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

# To be able to used log10:
dy_cnty$cnty_flows <- dy_cnty$cnty_flows*1000000

# imports:
imports_cnty <- dy_cnty %>% filter(orig_stName != dest_stName)

imports_stat <- aggregate(imports_cnty$cnty_flows,
                          by = list(state = imports_cnty$dest_stName),
                          FUN = sum)
names(imports_stat)[2] <- "imports"

imports_cnty <- aggregate(imports_cnty$cnty_flows,
                          by = list(fips = imports_cnty$dest),
                          FUN = sum)
names(imports_cnty)[2] <- "imports"

# exports:
exports_cnty <- dy_cnty %>% filter(orig_stName != dest_stName)

exports_stat <- aggregate(exports_cnty$cnty_flows,
                          by = list(state = exports_cnty$orig_stName),
                          FUN = sum)
names(exports_stat)[2] <- "exports"

exports_cnty <- aggregate(exports_cnty$cnty_flows,
                          by = list(fips = exports_cnty$orig),
                          FUN = sum)
names(exports_cnty)[2] <- "exports"

# domestic:
domestic_cnty <- dy_cnty %>% filter(orig_stName == dest_stName)

domestic_stat <- aggregate(domestic_cnty$cnty_flows,
                           by = list(state = domestic_cnty$orig_stName),
                           FUN = sum)
names(domestic_stat)[2] <- "domestic"

domestic_cnty <- aggregate(domestic_cnty$cnty_flows,
                           by = list(fips = domestic_cnty$orig),
                           FUN = sum)
names(domestic_cnty)[2] <- "domestic"

# Merging tibbles for porgramming simplification
cnty_tb <- left_join(domestic_cnty, 
                     exports_cnty,
                     by = "fips")
cnty_tb <- left_join(cnty_tb,
                     imports_cnty,
                     by = "fips")

stat_tb <- left_join(domestic_stat,
                     exports_stat,
                     by = "state")
stat_tb <- left_join(stat_tb,
                     imports_stat,
                     by = "state")

# I need to add state's fips to be able to merge the stat info with their maps
key_loc <- read_csv(file = 'data/data_needs/location_master_key.csv')
key_loc <- key_loc %>% 
  select(st_name, st_fips) %>%
  mutate(st_name = tolower(st_name)) %>%
  distinct() %>% 
  filter(st_name != "district of columbia")

stat_tb <- left_join(stat_tb,
                     key_loc,
                     by = c("state" = "st_name"))
stat_tb$st_fips <- as.character(stat_tb$st_fips)

rm(list = setdiff(ls(), c("cnty_tb", "stat_tb")))
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

USmap_st_df <- left_join(USmap_st_df, stat_tb, by = c("fips" = "st_fips"))
USmap_cnty_df <- left_join(USmap_cnty_df, cnty_tb, by = "fips")

# Exports (outflows excluding in-state)
## state
USmap_st_df$exports <- log10(USmap_st_df$exports)
USmap_st_df$exports[is.infinite(USmap_st_df$exports)] <- 0
summary(USmap_st_df$exports)

breaks = c(0, 5, 6, 7, 8, 9, 10)
labels = c("Below $100,000",
           " $100,000 and $1 M",
           " $1 M and $10 M",
           " $10 M and $100 M",
           " $100 M and  $1 B",
           "Above $1 B")

ggplot() +
  geom_polygon(data = USmap_st_df,
               aes(x = long,
                   y = lat,
                   group = group,
                   fill = cut(exports,
                              breaks = breaks,
                              labels = labels)),
               color = "grey",
               size = .02) +
  geom_polygon(data = USmap_st_df,
               aes(x = long,
                   y = lat,
                   group = group),
               fill = NA,
               color = "black",
               size = .6) +
  geom_polygon(data = USmap_cnty_df,
               aes(x = long,
                   y = lat,
                   group = group),
               fill = NA,
               color = "grey",
               size = .02) +
  labs(
    title = "Out-state SCTG 02 exports (state level)"
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

## county
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

# Imports (outflows excluding in-state)
## state
USmap_st_df$imports <- log10(USmap_st_df$imports)
USmap_st_df$imports[is.infinite(USmap_st_df$imports)] <- 0
summary(USmap_st_df$imports)

breaks = c(0, 5, 6, 7, 8, 9, 10)
labels = c("Below $100,000",
           " $100,000 and $1 M",
           " $1 M and $10 M",
           " $10 M and $100 M",
           " $100 M and  $1 B",
           "Above $1 B")

ggplot() +
  geom_polygon(data = USmap_st_df,
               aes(x = long,
                   y = lat,
                   group = group,
                   fill = cut(imports,
                              breaks = breaks,
                              labels = labels)),
               color = "grey",
               size = .02) +
  geom_polygon(data = USmap_st_df,
               aes(x = long,
                   y = lat,
                   group = group),
               fill = NA,
               color = "black",
               size = .6) +
  geom_polygon(data = USmap_cnty_df,
               aes(x = long,
                   y = lat,
                   group = group),
               fill = NA,
               color = "grey",
               size = .02) +
  labs(
    title = "Out-state SCTG 02 imports (state level)"
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

## county
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

# Domestic (focus on in-state flows only)
## state
USmap_st_df$domestic <- log10(USmap_st_df$domestic)
USmap_st_df$domestic[is.infinite(USmap_st_df$domestic)] <- 0
summary(USmap_st_df$domestic)

breaks = c(0, 5, 6, 7, 8, 9, 10)
labels = c("Below $100,000",
           " $100,000 and $1 M",
           " $1 M and $10 M",
           " $10 M and $100 M",
           " $100 M and  $1 B",
           "Above $1 B")

ggplot() +
  geom_polygon(data = USmap_st_df,
               aes(x = long,
                   y = lat,
                   group = group,
                   fill = cut(domestic,
                              breaks = breaks,
                              labels = labels)),
               color = "grey",
               size = .02) +
  geom_polygon(data = USmap_st_df,
               aes(x = long,
                   y = lat,
                   group = group),
               fill = NA,
               color = "black",
               size = .6) +
  geom_polygon(data = USmap_cnty_df,
               aes(x = long,
                   y = lat,
                   group = group),
               fill = NA,
               color = "grey",
               size = .02) +
  labs(
    title = "In-state SCTG 02 domestic (state level)"
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

## county
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
# end