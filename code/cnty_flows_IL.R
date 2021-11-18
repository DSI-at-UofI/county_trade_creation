rm(list = ls())

# Noe J Nava
# noejn2@illinois.edu // noejnava2@gmail.com
# https://noejn2.github.io/

# Purpose: ----------------
# Here, we create the christian county (17021) flows, 
# and from Champaign (17019) to the rest of IL 

library(tidyverse)
library(rgdal)
library(collapse)
library(readstata13)

dy_cnty <- readRDS(file = 'output/dyadic_county_flows_adjusted.rds')

#dy_cnty$cnty_flows <- dy_cnty$cnty_flows*1000000
#a <- dy_cnty[dy_cnty$cnty_flows > 0,]
#sum(a$cnty_flows < 10000)
#mean(a$cnty_flows) < quantile(a$cnty_flows, .90)

dy_cnty <- dy_cnty %>% filter(orig == "17021" | orig == "17019") %>%
  select(orig, dest, cnty_flows)
dy_kona <- read_csv(file = 'data/cntyflows_12_lin_2019.csv')
dy_smit <- read_csv(file = 'data/cntyflows_12_smith_2017.csv')

# Fixing Konar's dataset
dy_kona <- dy_kona %>% select(ori, des, sctg_2)
dy_kona <- dy_kona %>%
  mutate(orig = as.character(as.numeric(ori))) %>%
  mutate(dest = as.character(as.numeric(des))) %>%
  select(!c(ori, des)) %>%
  rename(kona = sctg_2) %>%
  filter(orig == "17021" | orig == "17019")# %>%
  #filter(substr(dest, 1, 2) == "17")

# Fixing Smith's dataset
dy_smit <- dy_smit %>% select(`Demand FIPS`,
                              `Corn FIPS`,
                              `Corn (bu)`) %>%
  rename(orig = `Demand FIPS`,
         dest = `Corn FIPS`,
         smit = `Corn (bu)`) %>%
  mutate(orig = as.character(orig),
         dest = as.character(dest)) %>%
  filter(orig == "17021" | dest == "17019")# %>%
  #filter(substr(dest, 1, 2) == "17")
dy_smit <- collap(dy_smit, ~orig+dest, FUN = sum)

# putting the datasets together
dy_cnty <- left_join(dy_cnty, dy_kona, by = c("orig", "dest"))
dy_cnty$kona[is.na(dy_cnty$kona)] <- 0

dy_cnty <- left_join(dy_cnty, dy_smit, by = c("orig", "dest"))
dy_cnty$smit[is.na(dy_cnty$smit)] <- 0

dy_cnty$kona <- dy_cnty$kona/25.4

### Descriptive statistics:
#dy_cnty$cnty_flows <- dy_cnty$cnty_flows*1000000
#quantile(dy_cnty$cnty_flows[dy_cnty$cnty_flows > 0], .83) > mean(dy_cnty$cnty_flows[dy_cnty$cnty_flows > 0])
#sum(dy_cnty$cnty_flows < 10000)

# Creating the dataset
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

key_loc <- read_csv(file = 'data/data_needs/location_master_key.csv')
key_loc <- key_loc %>% 
  select(st_name, fips) %>%
  mutate(st_name = tolower(st_name)) %>%
  mutate(fips = as.character(fips)) %>%
  distinct() %>% 
  filter(st_name != "district of columbia")

USmap_cnty_df <- left_join(USmap_cnty_df,
                           key_loc,
                           by = "fips")

#USmap_cnty_df <- USmap_cnty_df %>% filter(st_name == "illinois")

# Adding to dy_cnty the latitude and longitude of the origin and destination
# counties.... here, color = cnty_flows
USmap_cnty <- readOGR(dsn = 'assets/shp_files/3109_county',
                      layer = 'USmap_county')

coords <- coordinates(USmap_cnty)
coords <- as.data.frame(coords)

USmap_cnty <- USmap_cnty@data

USmap_cnty <- cbind.data.frame(USmap_cnty, coords)

USmap_cnty <- USmap_cnty %>%
  select(ANSI_ST_CO, V1, V2) %>%
  mutate(ANSI_ST_CO = as.character(as.numeric(ANSI_ST_CO)))
names(USmap_cnty) <- c("fips", "long", "lat")

dy_cnty <- left_join(dy_cnty, USmap_cnty, by = c("orig" = "fips"))
dy_cnty <- dy_cnty %>%
  rename(long_orig = long,
         lat_orig = lat)

dy_cnty <- left_join(dy_cnty, USmap_cnty, by = c("dest" = "fips"))
dy_cnty <- dy_cnty %>%
  rename(long_dest = long,
         lat_dest = lat)

### Descriptive statistics
polygon <- ggplot() + # Ours /// U.S.
  geom_polygon(data = USmap_cnty_df %>%
                 filter(substr(fips, 1, 2) == "17"), # Comment for focus only on US,
               aes(x = long,
                   y = lat,
                   group = group),
               fill = NA,
               colour = "black")

# Christian County: -----
polygon +
  geom_segment(data = dy_cnty %>% 
                 filter(orig == "17021") %>%
                 filter(substr(dest, 1, 2) == "17") %>%
                 filter(cnty_flows != 0),
               aes(x = long_orig,
                   y = lat_orig,
                   xend = long_dest,
                   yend = lat_dest,
                   color = cnty_flows)) +
  scale_color_gradient(low = "blue", high = "red") +
  theme(panel.background = element_rect(fill = NA, 
                                        color = NA)) +
  coord_equal() +
  theme(legend.direction = "horizontal",
        legend.position = "bottom") +
  labs(
    title = "SCTG 02 Christian County flow (RND)",
    color = "Flow ($)"
  )

polygon +
  geom_segment(data = dy_cnty %>% 
                 filter(orig == "17021") %>%
                 filter(substr(dest, 1, 2) == "17") %>%
                 filter(kona != 0),
               aes(x = long_orig,
                   y = lat_orig,
                   xend = long_dest,
                   yend = lat_dest,
                   color = cnty_flows)) +
  scale_color_gradient(low = "blue", high = "red") +
  theme(panel.background = element_rect(fill = NA, 
                                        color = NA)) +
  coord_equal() +
  theme(legend.direction = "horizontal",
        legend.position = "bottom") +
  labs(
    title = "SCTG 02 Christian County flow (LRMK)",
    color = "Flow (Volume)"
  )

polygon +
  geom_segment(data = dy_cnty %>% 
                 filter(orig == "17021") %>%
                 filter(substr(dest, 1, 2) == "17") %>%
                 filter(smit != 0),
               aes(x = long_orig,
                   y = lat_orig,
                   xend = long_dest,
                   yend = lat_dest,
                   color = cnty_flows)) +
  scale_color_gradient(low = "blue", high = "red") +
  theme(panel.background = element_rect(fill = NA, 
                                        color = NA)) +
  coord_equal() +
  theme(legend.direction = "horizontal",
        legend.position = "bottom") +
  labs(
    title = "SCTG 02 Christian County flow (SGKPSS)",
    color = "Flow (Volume)"
  )
  
# Champaign County
polygon +
  geom_segment(data = dy_cnty %>% 
                 filter(orig == "17019") %>%
                 filter(substr(dest, 1, 2) == "17") %>%
                 filter(cnty_flows != 0),
               aes(x = long_orig,
                   y = lat_orig,
                   xend = long_dest,
                   yend = lat_dest,
                   color = cnty_flows)) +
  scale_color_gradient(low = "blue", high = "red") +
  theme(panel.background = element_rect(fill = NA, 
                                        color = NA)) +
  coord_equal() +
  theme(legend.direction = "horizontal",
        legend.position = "bottom") +
  labs(
    title = "SCTG 02 Champaign County flow (RND)",
    color = "Flow ($)"
  )

polygon +
  geom_segment(data = dy_cnty %>% 
                 filter(orig == "17019") %>%
                 filter(substr(dest, 1, 2) == "17") %>%
                 filter(kona != 0),
               aes(x = long_orig,
                   y = lat_orig,
                   xend = long_dest,
                   yend = lat_dest,
                   color = cnty_flows)) +
  scale_color_gradient(low = "blue", high = "red") +
  theme(panel.background = element_rect(fill = NA, 
                                        color = NA)) +
  coord_equal() +
  theme(legend.direction = "horizontal",
        legend.position = "bottom") +
  labs(
    title = "SCTG 02 Champaign County flow (LRMK)",
    color = "Flow (Volume)"
  )

polygon +
  geom_segment(data = dy_cnty %>% 
                 filter(orig == "17019") %>%
                 filter(smit != 0),
               aes(x = long_orig,
                   y = lat_orig,
                   xend = long_dest,
                   yend = lat_dest,
                   color = cnty_flows)) +
  scale_color_gradient(low = "blue", high = "red") +
  theme(panel.background = element_rect(fill = NA, 
                                        color = NA)) +
  coord_equal() +
  theme(legend.direction = "horizontal",
        legend.position = "bottom") +
  labs(
    title = "SCTG 02 Champaign County flow (SGKPSS)",
    color = "Flow (Volume)"
  )


