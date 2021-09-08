rm(list = ls())

# Noe J Nava
# noejn2@illinois.edu // noejnava2@gmail.com
# https://noejn2.github.io/

# Purpose: ----------------
# Script creates dyadic_county_2017.dta. This is the dataset that we
# employ to create county food flows. Notice that this is the data
# used for the county flows simulations, not the data used for the
# gravity regression.

# Code takes approximately 11 minutes ----------
# on MacBook Pro
# 2.3 GHz Quad-Core Intel Core i7 
# 32 GB ram
# Personal Mac

start.time <- Sys.time()

# This is groundhog config to keep useful versions of packages
# Do not change groundhog_day!!
# check: http://datacolada.org/95
groundhog_day <- "2021-09-05"
pkgs <- c("tidyverse", "readstata13")
groundhog::groundhog.library(pkgs, groundhog_day)

# Focusing on state ids for the keys ----
# Remember: Request data/data_needs directory from Noe (script's author)
key <- read_csv(file = 'data/data_needs/location_master_key.csv')
key <- key %>% select(fips, st_name, st_initial)
key <- key[key$st_initial != "DC",]
key$st_name <- tolower(key$st_name)

# Creating the ids for the final dataset
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
# Remember: Request data/data_needs directory from Noe (script's author)
rm(list = setdiff(ls(), c("data_tb", "start.time")))
sales <- read.dta13(file = 'data/data_needs/NASS SCTG02 sales by county.dta')

sales <- sales %>% filter(year == 2017)
sales$sales[is.na(sales$sales)] <- 0

# Creating county fips out of county ansi and state fips
sales$state_fips <- as.character(sales$state_fips)
sales$countyansi <- as.character(sales$countyansi)
sales$countyansi <- stringr::str_pad(sales$countyansi, width = 3, pad = "0")
sales$fips <- paste0(sales$state_fips, sales$countyansi)

# All different crops data are collapsed (summed) into the counties
sales <- collapse::collap(sales, sales~fips, FUN = sum)

# The notation "_i" refers to supply side variable
data_tb <- left_join(data_tb, sales, by = c("orig" = "fips"))
data_tb$sales[is.na(data_tb$sales)] <- 0
data_tb <- data_tb %>% rename(sales_i = sales)

# Adding GDP 
# Remember: Request data/data_needs directory from Noe (script's author)
rm(list = setdiff(ls(), c("data_tb", "start.time")))
implan <- read_csv(file = 'data/data_needs/industry_export_cleaned_2017.csv')

ind_id_list <- c(
  "002", # Grain farming
  "011", # Beef cattle ranching and farming
  "063", # Dog and cat food manufacturing
  "064", # Other animal food manufacturing
  "065", # Flour milling
  "066", # Rice milling
  "068", # Wet corn milling
  "078", # Frozen specialities manufacturing
  "093", # Bread and bakery product, except frozen, manufacturing
  "106", # Breweries
  "163" # Other basic organic chemical manufacturing
)

implan <- implan %>%
  filter(ind_id %in% ind_id_list) %>%
  select(fips, output)

implan <- collapse::collap(implan, output~fips, FUN = sum, na.rm = TRUE)
implan <- implan %>%
  mutate(fips = as.character(as.numeric(fips)))

# The notation "_j" refers to demand side variable
data_tb <- left_join(data_tb, implan, by = c("dest" = "fips"))
data_tb <- data_tb %>% rename(gdp = output)
data_tb$gdp[is.na(data_tb$gdp)] <- 0
data_tb$gdp <- data_tb$gdp/1000000

# Including distance and contiguity measures ----
# Remember: Request data/data_needs directory from Noe (script's author)
rm(list = setdiff(ls(), c("data_tb", "start.time")))

#distance:
# Notice that our distance measured is not calculated but comes from
# the county_tools.dta dataset
county_tools <- read.dta13(file = 'data/data_needs/county_tools.dta')
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
# Notice here that we determine whether two counties share the same border
# with the shapefile, suing the rgeos::gTouches() function
rm(list = setdiff(ls(), c("data_tb", "start.time")))
shpFile <- rgdal::readOGR(dsn = 'assets/shp_files/3109_county', 
                          layer = 'USmap_county')
shpFile@data$ANSI_ST_CO <- as.numeric(shpFile@data$ANSI_ST_CO)
shpFile@data$ANSI_ST_CO <- as.character(shpFile@data$ANSI_ST_CO)

# Notice that we create a list of each county's fips that includes 
# all countys' fips that are contiguous to that one
# then that is used in the loop below to create our vector that indicates
# whether counties are contiguous to each other
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
## head and mayer (2002): d_ii = 0.67*sqrt{area_i / pi} ----
internal_d <- data_tb %>% select(orig, dest)
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
saveRDS(data_df, file = 'output/dyadic_county_2017.rds')

mins <- as.numeric(Sys.time() - start.time, units = "mins")
cat("\n")
cat("::: Script took ", mins, "minutes.")
#end