rm(list = ls())

# Noe J Nava
# noejn2@illinois.edu
# https://noejn2.github.io/
# Script creates the dyadic_county_flows.rds, which is our 
# main dataset in this repository


# Script creates dyadic_county_2017.dta. This is the dataset that we
# employ to create county food flows.
# Code takes approximately 28 minutes
# on Intel(R) Xeon(R) CPU E3-12250 v6 3.00Ghz 
# 64 GB ram
# Mumford computer

start.time <- Sys.time()

library(tidyverse)
library(readstata13)

# Focusing on state ids for the keys ----
key <- read_csv(file = 'data/data_needs/location_master_key.csv')
key <- key %>% select(fips, st_name, st_initial)
key <- key[key$st_initial != "DC",]
key$st_name <- tolower(key$st_name)

# Creating the final dataset
data_tb <- expand.grid(key$fips, key$fips)
names(data_tb) <- c('orig', 'dest')
data_tb <- as_tibble(data_tb)

data_tb$orig <- as.character(data_tb$orig)
data_tb$dest <- as.character(data_tb$dest)

# Adding orig other ids
key$fips <- as.character(key$fips)
data_tb <- left_join(data_tb, key, by = c('orig' = 'fips'))
data_tb <- data_tb %>% rename(orig_stName = st_name,
                              orig_stIni  = st_initial)

# Adding dest other ids
data_tb <- left_join(data_tb, key, by = c('dest' = 'fips'))
data_tb <- data_tb %>% rename(dest_stName = st_name,
                              dest_stIni  = st_initial)

# Adding crop sales ----
rm(list = setdiff(ls(), c("data_tb", 
                          "start.time")))
sales <- readstata13::read.dta13(file = 'data/data_needs/NASS SCTG02 sales by county.dta')

sales <- sales %>% filter(year == 2017)
sales$sales[is.na(sales$sales)] <- 0

sales$state_fips <- as.character(sales$state_fips)
sales$countyansi <- as.character(sales$countyansi)
sales$countyansi <- stringr::str_pad(sales$countyansi, width = 3, pad = "0")
sales$fips <- paste0(sales$state_fips, sales$countyansi)

sales <- collapse::collap(sales, sales~fips, FUN = sum)

# As supply (_i)
data_tb <- left_join(data_tb, sales, by = c("orig" = "fips"))
data_tb$sales[is.na(data_tb$sales)] <- 0
data_tb <- data_tb %>%
  rename(sales_i = sales)

# Acreage (supply)----
rm(list = setdiff(ls(), c("data_tb", 
                          "start.time")))
acreage <- read_csv(file = 'data/data_needs/acreage_2017.csv')

acreage$Value <- as.numeric(gsub(",", "", acreage$Value))
acreage$Value[is.na(acreage$Value)] <- 0

acreage$`State ANSI` <- as.character(acreage$`State ANSI`)
acreage$`County ANSI` <- as.character(acreage$`County ANSI`)
acreage$fips <- paste0(acreage$`State ANSI`, acreage$`County ANSI`)
acreage$fips <- as.numeric(acreage$fips)
acreage <- acreage %>% rename(acres = Value)

acreage <- collapse::collap(acreage, acres~fips, FUN = sum)

acreage$fips <- as.character(acreage$fips)

# As supply (_i)
data_tb <- left_join(data_tb, acreage, by = c("orig" = "fips"))
data_tb$acres[is.na(data_tb$acres)] <- 0
data_tb <- data_tb %>%
  rename(acres_i = acres)

## Including IMPLAN data ----
rm(list = setdiff(ls(), c("data_tb", 
                          "start.time")))
implan <- read_csv(file = 'data/data_needs/industry_export_cleaned_2017.csv')

ind_id_list <- c(
  #"001", # Oilseed farming
  "002", # Grain farming
  #"003", # Vegetable and melon farming
  "011", # Beef cattle ranching and farming
  "063", # Dog and cat food manufacturing
  "064", # Other animal food manufacturing
  "065", # Flour milling
  "066", # Rice milling
  "068", # Wet corn milling
  #"069", # Soybean and other oilseed processing
  #"076", # Confectionery manufacturing from chocolate
  #"077", # Frozen fruits, juices and vegetables manufacturing
  "078", # Frozen specialities manufacturing
  #"080", # Canned specialties
  "093", # Bread and bakery product, except frozen, manufacturing
  #"097", # Roasted nuts and peanut butter manufacturing
  #"099", # Coffee and tea manufacturing
  #"100", # Flavoring syrup and concetrate manufacturing
  #"103", # All other food manufacturing
  "106", # Breweries
  #"107", # Wineries
  "163" # Other basic organic chemical manufacturing
)
implan <- implan %>%
  filter(ind_id %in% ind_id_list) %>%
  select(fips,
         output)

implan <- collapse::collap(implan, output~fips, FUN = sum, na.rm = TRUE)

# As supply (_j)
data_tb <- left_join(data_tb, implan, by = c("dest" = "fips"))
data_tb <- data_tb %>% rename(gdp = output)
data_tb$gdp[is.na(data_tb$gdp)] <- 0
data_tb$gdp <- data_tb$gdp/1000000

# Including distance and contiguity measures ----
rm(list = setdiff(ls(), c("data_tb", 
                          "start.time")))
#distance:
county_tools <- readstata13::read.dta13(file = 'data/data_needs/county_tools.dta')
county_tools <- as_tibble(county_tools)

county_tools <- county_tools %>%
  select(origin, 
         destination, 
         dist_km) 
county_tools <- county_tools %>%
  rename(orig = origin,
         dest = destination,
         distance = dist_km)

county_tools$orig <- as.character(county_tools$orig)
county_tools$dest <- as.character(county_tools$dest)

data_tb <- left_join(data_tb, county_tools,
                     by = c('orig' = 'orig',
                            'dest' = 'dest'))

#contiguity:
rm(list = setdiff(ls(), c("data_tb", 
                          "start.time")))
shpFile <- rgdal::readOGR(dsn = 'assets/shp_files/3109_county', layer = 'USmap_county')
shpFile@data$ANSI_ST_CO <- as.numeric(shpFile@data$ANSI_ST_CO)
shpFile@data$ANSI_ST_CO <- as.character(shpFile@data$ANSI_ST_CO)

list_nb <- rgeos::gTouches(shpFile, byid = TRUE, returnDense = FALSE)
list_nb <- lapply(list_nb, function(x) shpFile$ANSI_ST_CO[x])
names(list_nb) <- shpFile$ANSI_ST_CO

data_tb$contiguity <- 0
list_fip <- data_tb %>% distinct(orig) %>% pull()
i = 0
for(fip in list_fip) {
  i = i + 1
  completed <- i/length(list_fip)
  cat("Completed total: ", completed*100, " percentage.", "\n")
  list <- list_nb[fip]
  list <- as_tibble(list) %>% pull()
  data_tb$contiguity[data_tb$orig == fip & (data_tb$dest %in% list)] <- 1
}

# creating internal distance measures for all i == i and j == j----
## Wei (1996): d_ii = 0.25*min_j {d_ij: for all j}
internal_d <- data_tb %>% select(orig, dest, distance)
internal_d$distance[internal_d$distance == 0] <- 99999
list_fips <- internal_d %>% distinct(orig) %>% pull()
i = 0
for (f in list_fips) {
  i = i + 1
  completed <- i/length(list_fip)
  cat("Completed total: ", completed*100, " percentage.", "\n")
  tmp_tb <- internal_d %>% filter(orig == f)
  int_d <- min(tmp_tb$distance)*0.25
  internal_d$distance[internal_d$orig == f & internal_d$dest == f] <- int_d
}
internal_d <- internal_d %>% rename(wei_1996 = distance)
internal_d$wei_1996[internal_d$orig != internal_d$dest] <- 0

## head and mayer (2002): d_ii = 0.67*sqrt{area_i / pi} ----
county_areas <- read_csv(file = 'assets/county-areas.csv')
county_areas$fips <- as.character(county_areas$fips)
internal_d <- left_join(internal_d, county_areas, 
                        by = c("orig" = "fips"))

internal_d$`area (sq. mi)`[internal_d$orig != internal_d$dest] <- 0

internal_d$`area (sq. mi)` <- 0.67*sqrt(internal_d$`area (sq. mi)`/pi)

internal_d <- internal_d %>% 
  rename(head_mayer_2002 = `area (sq. mi)`)

data_tb <- left_join(data_tb, internal_d,
                     by = c("orig", "dest"))

# Saving -----
data_df <- as.data.frame(data_tb)
#readstata13::save.dta13(data_df, file = 'data/dyadic_county_2017.dta')
saveRDS(data_df, file = 'output/dyadic_county_2017.rds')

mins <- as.numeric(Sys.time() - start.time, units = "mins")
cat("\n")
cat("::: Script took ", mins, "minutes.")
#end