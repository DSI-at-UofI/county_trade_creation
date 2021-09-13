rm(list = ls())

# Noe J Nava
# noejn2@illinois.edu // noejnava2@gmail.com
# https://noejn2.github.io/

# Purpose: ----------------
# Here, we create a map for sales (supply) and gdp (demand) for sctg02 crops

# This is groundhog config to keep useful versions of packages
# Do not change groundhog_day!!
# check: http://datacolada.org/95
groundhog_day <- "2021-09-05"
pkgs <- c("tidyverse", "rgdal")
groundhog::groundhog.library(pkgs, groundhog_day)


dy_cnty <- readRDS(file = 'output/dyadic_county_flows_adjusted.rds')
sales <- dy_cnty %>% select(orig, sales_i) %>% distinct()
gdp <- dy_cnty %>% select(dest, gdp_j) %>% distinct()

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

# Sales ---- 
sales$sales_i <- exp(sales$sales_i)
sales$sales_i <- log10(sales$sales_i)

summary(sales$sales_i)
sum(is.na(sales$sales_i))
sum(is.infinite(sales$sales_i))

USmap_cnty_df <- left_join(USmap_cnty_df,
                           sales,
                           by = c("fips" = "orig"))


breaks = c(0, 4, 5, 6, 7, 8, 9)
labels = c("Below $10,000",
           " $10,000 and $100,000",
           " $100,000 and $1 M",
           " $1 M and $10 M",
           " $10 M and  $100 M",
           "Above $100 M")

ggplot() +
  geom_polygon(data = USmap_cnty_df,
               aes(x = long,
                   y = lat,
                   group = group,
                   fill = cut(sales_i,
                              breaks = breaks,
                              labels = labels)),
               color = "grey",
               size = .1) +
  geom_polygon(data = USmap_st_df,
               aes(x = long,
                   y = lat,
                   group = group),
               fill = NA,
               color = "black",
               size = 0.60) +
  labs(
    title = "Crop sales - supply for SCTG 02"
  ) +
  scale_fill_manual(values   = c(
    "Below $10,000"          = "#edf8e9",
    " $10,000 and $100,000"  = "#c7e9c0",
    " $100,000 and $1 M"        = "#a1d99b",
    " $1 M and $10 M"      = "#74c476",
    " $10 M and  $100 M"      = "#31a354",
    "Above $100 M"             = "#006d2c")) +
  theme(panel.background = element_rect(fill = NA, 
                                        color = NA)) +
  coord_equal() +
  theme(legend.direction = "horizontal",
        legend.position = "bottom") +
  guides(fill = guide_legend(title = "",
                             title.position = "top"))

# GDP ----
gdp$gdp_j <- exp(gdp$gdp_j)*1000000
gdp$gdp_j <- log10(gdp$gdp_j)

summary(gdp$gdp_j)
sum(is.na(gdp$gdp_j))
sum(is.infinite(gdp$gdp_j))

USmap_cnty_df <- left_join(USmap_cnty_df,
                           gdp,
                           by = c("fips" = "dest"))


breaks = c(0, 5, 6, 7, 8, 9, 11)
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
                   fill = cut(gdp_j,
                              breaks = breaks,
                              labels = labels)),
               color = "grey",
               size = .1) +
  geom_polygon(data = USmap_st_df,
               aes(x = long,
                   y = lat,
                   group = group),
               fill = NA,
               color = "black",
               size = 0.60) +
  labs(
    title = "GDP - demand for SCTG 02 crops"
  ) +
  scale_fill_manual(values   = c(
    "Below $10,000"          = "#edf8e9",
    " $10,000 and $100,000"  = "#c7e9c0",
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