###########################################################################
###########################################################################
###                                                                     ###
###                            SECTION 4.6:                             ###
###                    ACCESSIBILITY TO SUPERMARKETS                    ###
###                                                                     ###
###########################################################################
###########################################################################

library(tidyverse)
library(mapview)
library(sf)

##---------------------------------------------------------------
##                         Format data                         --
##---------------------------------------------------------------

# Read data ---------------------------------------------------------------

# MSOA polygons
msoa_london <- st_read('data/london_data_service/MSOA/MSOA_(Dec_2011)_Boundaries_Generalised_Clipped_(BGC)_EW_V3.shp')

# Get OSM supermarket data ------------------------------------------------

library(osmdata)

# Bounding box
bb <- getbb("London", featuretype = "city")

# Download supermarket data
#supermarket_all <- bb %>% 
#  opq(timeout = 60*30) %>% 
#  add_osm_feature(key = 'shop', value = 'supermarket') %>% 
#  osmdata_sf()

# Create a directory to save the data if it doesn't already exist
#if (!dir.exists('data/shops/osm')) {
#  dir.create('data/shops/osm', recursive = TRUE)
#}

## Save raw OSM data as RDS
#saveRDS(supermarket_all, 'data/shops/osm/supermarket_raw.rds')

# Read raw OSM data
supermarket_all <- readRDS('data/shops/osm/supermarket_raw.rds')

# Filter points including names only
supermarket_points <- 
  supermarket_all$osm_points %>% 
  filter(!is.na(name)) %>% 
  mutate(long = st_coordinates(.)[,1],
         lat = st_coordinates(.)[,2])

# Supermarket polygon as centroid
supermarket_poly <- 
  supermarket_all$osm_polygons %>% 
  st_centroid(.) %>% 
  mutate(long = st_coordinates(.)[,1],
         lat = st_coordinates(.)[,2])
supermarket_poly$area_sqm <- 
  supermarket_all$osm_polygons %>% 
  st_transform(27700) %>% 
  st_area(.) %>% 
  as.numeric

# Filter 'large' shop > 280 sqm^2.
# Ref: The Sunday Trading Act 1994 (the STA 1994)
# https://commonslibrary.parliament.uk/research-briefings/sn05522/
supermarket_poly <- supermarket_poly %>% 
  filter(area_sqm > 280)

# Map supermarket centroids
supermarket_poly %>% 
  filter(lat > 51.28 & lat < 51.68 & long > -0.5103 & long < 0.3346) %>% 
  mapview(col.region = "blue")

# Bind rows
supermarket_bind <- 
  bind_rows(supermarket_points, supermarket_poly)
# Subset amenity is NA
supermarket_bind <- filter(supermarket_bind, is.na(amenity))
# Select relevant variables
supermarket_bind <- select(supermarket_bind, osm_id, name, brand, area_sqm, long, lat)
summary(supermarket_bind)

# Show the most frequent brands
brand_freq <- sort(table(supermarket_bind$brand), decreasing = TRUE)
brand_freq[1:20]

# Filter larger brands
brands <- c('Lidl', 'ALDI', 'Co-op', 'Sainsbury', 'Tesco','Asda','Morrisons')
supermarket_bind <- supermarket_bind %>% 
  filter(grepl(paste(brands, collapse = '|'), name, ignore.case = TRUE) |
           grepl(paste(brands, collapse = '|'), brand, ignore.case = TRUE) )


# transform supermarket_bind to the CRS of msoa_london
supermarket_bind <- st_transform(supermarket_bind, st_crs(msoa_london))

# Then do the intersection
supermarkets_london <- st_filter(supermarket_bind, msoa_london)

# View
View(supermarkets_london)


# Spatially join MSOA
supermarkets_london <- supermarkets_london %>% 
  st_transform(st_crs(msoa_london)) %>% 
  st_join(., select(msoa_london, MSOA11CD))

#supermarkets_london <- supermarkets_london %>% 
#  mutate(MSOA11CD = coalesce(MSOA11CD.x, MSOA11CD.y)) %>% 
#  select(-MSOA11CD.x, -MSOA11CD.y)


# Check for missing MSOA
#supermarkets_london %>% 
#  filter(!grepl("^[A-z]", MSOA11CD)) %>% 
#  mapview()

# Keep Supermarkets within London only
supermarkets_london <- supermarkets_london %>% 
  filter(grepl("^(E)", MSOA11CD))

# Save data
supermarkets_london <- st_set_geometry(supermarkets_london, NULL)
write_csv(supermarkets_london, 'data/shops/supermarkets_osm.csv')


##----------------------------------------------------------------
##                    Estimate accessibility                    --
##----------------------------------------------------------------


# Read data ---------------------------------------------------------------

library(data.table)
setDTthreads(0)

# Supermarkets
supermarkets <- read_csv('data/shops/supermarkets_osm.csv')

# Read TT by PT
ttm_pt <- fread('output/ttm/ttm_pt_20211122.csv')

# MSOA polygons
msoa_london <- st_read('data/london_data_service/MSOA/MSOA_(Dec_2011)_Boundaries_Generalised_Clipped_(BGC)_EW_V3.shp')

# Load accessibility function
source('R/00_fn_accessibility.R')
# Fn format big number
format2 <- function(x, digits = 2)  formatC(x, format="f", big.mark=" ", digits=digits)

# Nearest supermarket -----------------------------------------------------

# Count supermarkets
supermarket_count <- count(supermarkets, MSOA11CD)

## PTF
# Join data
ttm_pt[setDT(supermarket_count), on=c(to_id = "MSOA11CD"), n_supermkt := n]
# Find nearest supermarket
nearestSuper_pt <- 
  ttm_pt[!is.na(n_supermkt), 
         .(nearest_supermarket = min(travel_time_p50, na.rm = TRUE)),
         by=.(from_id)]
# Transform Inf values to NA
nearestSuper_pt[,nearest_supermarket := fifelse(is.infinite(nearest_supermarket), NA_integer_, nearest_supermarket)]

# Cumulative accessibility to supermarkets --------------------------------

## PT
# Access multiple time-cuts
# According to TT percentile 50
time_cuts <- seq(15, 120, 15)
access_pt <- 
  lapply(time_cuts, function(x){
    access <- accessibility(ttm_pt, tt = "travel_time_p50", w = "n_supermkt", beta = x)
    access[,access_pc := (accessibility / sum(supermarket_count$n)) * 100]
    access[,time_cut := x]
  })
# Show summary
lapply(access_pt, summary)
# Bind estimates in a single DF
access_pt <- rbindlist(access_pt)


##----------------------------------------------------------------
##                        Save estimates                        --
##----------------------------------------------------------------

# Save data ---------------------------------------------------------------

# Create dir
dir.create('output/accessibility/supermarket')
# Read centroids
centroids_london <- read_csv('data/centroids/london_msoa_centroid2011.csv')
centroids_london <- select(centroids_london, -easting, -northing)

## PT
# Restructure data
access_ptF <- access_pt %>% 
  mutate(access_pc = round(access_pc, 4)) %>% 
  rename(MSOA11CD = from_id, 
         supermarket = accessibility,
         supermarket_pct = access_pc) %>% 
  pivot_wider(id_cols = MSOA11CD, names_from = time_cut, values_from = supermarket:supermarket_pct)

# Join nearest supermarket
access_ptF <- 
  left_join(centroids_london, access_ptF, by = 'MSOA11CD') %>% 
  left_join(nearestSuper_pt, by = c('MSOA11CD' = 'from_id'))
summary(access_ptF)
# Save PT accessibility estimates
write_csv(access_ptF, 'output/accessibility/supermarket/access_supermarkets_pt.csv')


# Clean env.
rm(list = ls())




