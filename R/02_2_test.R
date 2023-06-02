# Using .shp file to setting boundries

# Packages ----------------------------------------------------------------

library(dplyr)
library(gtfstools)
library(gtfsio)
library(mapview)
library(sf)

# reading data
gtfs <- read_gtfs("data/atoc/ttis739.gtfs.zip")

# Filter ----------------------------------------------------------------
# Stops
# setting range of London
london_lat_range <- c(51.28, 51.68)
london_lon_range <- c(-0.5103, 0.3346)

london_stops <- gtfs$stops %>%
  filter(stop_lat >= london_lat_range[1], 
         stop_lat <= london_lat_range[2], 
         stop_lon >= london_lon_range[1], 
         stop_lon <= london_lon_range[2])

# create a new one
london_gtfs <- gtfs
london_gtfs$stops <- london_stops

# Stop_times using filtered stops
london_stop_times <- gtfs$stop_times %>%
  filter(stop_id %in% london_stops$stop_id)
london_gtfs$stop_times <- london_stop_times

# Trips using filtered stop_times
london_trips <- gtfs$trips %>%
  filter(trip_id %in% london_stop_times$trip_id)
london_gtfs$trips <- london_trips

# Routes using filtered trips
london_routes <- gtfs$routes %>%
  filter(route_id %in% london_trips$route_id)
london_gtfs$routes <- london_routes

# Calendar and Calendar_dates using filtered trips
london_calendar <- gtfs$calendar %>%
  filter(service_id %in% london_trips$service_id)
london_gtfs$calendar <- london_calendar

london_calendar_dates <- gtfs$calendar_dates %>%
  filter(service_id %in% london_trips$service_id)
london_gtfs$calendar_dates <- london_calendar_dates

# Transfers using filtered stops
london_transfers <- gtfs$transfers %>%
  filter(from_stop_id %in% london_stops$stop_id & to_stop_id %in% london_stops$stop_id)
london_gtfs$transfers <- london_transfers

# save
write_gtfs(london_gtfs, "data/atoc/london.gtfs.zip")

# Check ----------------------------------------------------------------

london_gtfs <- read_gtfs("data/atoc/london.gtfs.zip")

london_stops <- london_gtfs$stops %>%
  st_as_sf(coords = c('stop_lon', 'stop_lat'), crs = 4326)

mapview(london_stops)



