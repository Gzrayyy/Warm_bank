############################################################################
############################################################################
###                                                                      ###
###                              SECTION 2:                              ###
###                   DEFINE OD POINTS. LSOA CENTROIDS                   ###
###                                                                      ###
############################################################################
############################################################################

# The population centroid data read from scot_cent and engl_cent matches the LSOA polygon data in lsoa_gb. 
# This is done with a *spatial join* operation, 
# which combines the population centroid data and the LSOA polygon data and checks 
# that the geocodes in the resulting dataset are the same.

# Packages ----------------------------------------------------------------

library(sf)
library(tidyverse)
library(mapview)


# Read data ---------------------------------------------------------------


# MSOA polygons
msoa_london <- st_read('data/london_data_service/MSOA/MSOA_(Dec_2011)_Boundaries_Generalised_Clipped_(BGC)_EW_V3.shp')

msoa_pop <- st_read('data/london_data_service/MSOA_PCW/MSOA_PopCentroids_london_2021.shp')


# Make sure points correspond to geometries for the London --------------

# Spatial join to get geo_code from geometries and check if there is mismatch (o means match)
# London
london_cent <- st_join(msoa_pop, msoa_london)
# Are geo_code in cent the same as in polygons?
sum(london_cent$DataZone != london_cent$geo_code)


# Merge and save pop. w. centroids ----------------------------------------

# Rename name/code to make compatible
#msoa_pop <- msoa_pop %>% 
#  rename(geo_code = MSOA11,
#         geo_label = MSOA11)

london_cent_final <- london_cent %>% 
  select(MSOA11CD, MSOA11NM, geometry)

# Save London centroids  ---------------------------------------- 

# As GPKG
st_write(london_cent_final, 'data/centroids/london_msoa_centroid2011.gpkg')
# As csv
london_cent_finaldf <- london_cent_final %>% 
  cbind(st_coordinates(.)) %>% 
  rename(northing = Y,
         easting = X) %>% 
  st_set_geometry(NULL)
write_csv(london_cent_finaldf, 'data/centroids/london_msoa_centroid2011.csv')


# Clean env.
rm(list = ls())
gc(reset = TRUE)