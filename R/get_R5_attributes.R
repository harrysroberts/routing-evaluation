## Load libraries and set Java memory allowance

library(tidyverse)
library(sf)

#Install Java if not already installed
if (!(rJavaEnv::java_check_version_rjava()>0)){
  rJavaEnv::java_quick_install(version = 21)
}

#Detach r5r if it is already loaded, in order to pass new memory allowance
if("package:r5r" %in% search()){detach("package:r5r")}

#Allocate 16gb memory to Java
options(java.parameters = "-Xmx16G")

#Load r5r once memory is allocated
library(r5r)

## Load trips

trips <- read_csv("input/processed/decisions_processed.csv")

## Define relevant modes

modes <- c("walk","bike","car","bus","rail")

## Set walk and bike speeds

# Walk and bike speeds on level ground and age/gender factors taken from 
# MatSim GitHub repository, Based on MatSim documentation by Horni et al.
#  (2016) available from https://www.ubiquitypress.com/books/e/10.5334/baw
# Weidmann reference speed of 1.34 m/s (4.824 km/h) used by MatSim
# Parkin and Rotheram reference speed of 6.01 m/s (21.636 km/h) used by MatSim
# age and gender factors can be applied to resultant times

level_walk_speed = 1.34
level_bike_speed = 6.01

## Process origin and destination datasets

origins <- trips %>%
  st_as_sf(coords = c("start_easting", "start_northing"), crs = 27700) %>%
  st_transform(crs = 4326) %>%
  mutate(
    id = as.character(trip_id),
    lon = st_coordinates(.)[,1],
    lat = st_coordinates(.)[,2],
    departure_time = as.POSIXct("2026-01-19") + days(wday(start_at)-1) + hours(hour) + minutes(minute)
  ) %>%
  st_drop_geometry() %>%
  select(id,lat,lon,departure_time)

destinations <- trips %>%
  st_as_sf(coords = c("stop_easting", "stop_northing"), crs = 27700) %>%
  st_transform(crs = 4326) %>%
  mutate(
    id = as.character(trip_id),
    lon = st_coordinates(.)[,1],
    lat = st_coordinates(.)[,2],
    departure_time = as.POSIXct("2026-01-19") + days(wday(start_at)-1) + hours(hour) + minutes(minute)
  ) %>%
  st_drop_geometry() %>%
  select(id,lat,lon,departure_time)

departure_times <- trips %>%
  mutate(
    departure_time = as.POSIXct("2026-01-19") + days(wday(start_at)-1) + hours(hour) + minutes(minute)
  ) %>%
  select(departure_time) %>%
  unique()

## Build the r5r network
r5r_network <- build_network(normalizePath("input/raw/r5r"))

## Run the algorithm

#Walk - run across paired origins and destinations
walk_attributes <- detailed_itineraries(
  r5r_network = r5r_network,
  origins = origins,
  destinations = destinations,
  mode = c("WALK"),
  max_trip_duration = 1440,
  walk_speed = level_walk_speed * 3600/1000,
  progress = TRUE,
  drop_geometry = TRUE,
  ) %>%
  mutate(
    trip_id = from_id,
    walk_distance = total_distance,
    walk_time = total_duration
  ) %>%
  select(trip_id,walk_distance,walk_time)

#Bike - run across paired origins and destinations
bike_attributes <- detailed_itineraries(
  r5r_network = r5r_network,
  origins = origins,
  destinations = destinations,
  mode = c("BICYCLE"),
  max_trip_duration = 1440,
  max_lts = 4,
  walk_speed = level_walk_speed * 3600/1000,
  bike_speed = level_bike_speed * 3600/1000,
  progress = TRUE,
  drop_geometry = TRUE,
  ) %>%
  mutate(
    trip_id = from_id,
    bike_distance = total_distance,
    bike_time = total_duration
  ) %>%
  select(trip_id,bike_distance,bike_time)

#Car - run across paired origins and destinations
car_attributes <- detailed_itineraries(
  r5r_network = r5r_network,
  origins = origins,
  destinations = destinations,
  mode = c("CAR"),
  max_trip_duration = 1440,
  walk_speed = level_walk_speed * 3600/1000,
  progress = TRUE,
  drop_geometry = TRUE,
  ) %>%
  mutate(
    trip_id = from_id,
    car_distance = total_distance,
    car_time = total_duration
  ) %>%
  select(trip_id,car_distance,car_time)

#Bus and rail - initialise first then loop over each unique departure time individually

n <- nrow(departure_times)

bus_list <- list()
rail_list <- list()

time_key <- function(x) format(x, tz = "GMT", usetz = TRUE, format = "%Y-%m-%d %H:%M:%S%z")
origin_split <- split(origins,      f = time_key(origins$departure_time))
destination_split   <- split(destinations, f = time_key(destinations$departure_time))


for(i in c(1:n)){
  
  d <- departure_times[i,] %>% pull()
  k <- time_key(d)
  
  message("Departure time ", d, " (",i," of ",n,")")
  
  bus_list[[i]] <- detailed_itineraries(
    r5r_network = r5r_network,
    origins = origin_split[[k]],
    destinations = destination_split[[k]],
    mode = c("WALK","BUS"),
    departure_datetime = d,
    max_trip_duration = 1440,
    walk_speed = level_walk_speed * 3600/1000,
    drop_geometry = TRUE
    )
  
  rail_list[[i]] <- detailed_itineraries(
    r5r_network = r5r_network,
    origins = origin_split[[k]],
    destinations = destination_split[[k]],
    mode = c("WALK","RAIL"),
    departure_datetime = d,
    max_trip_duration = 1440,
    walk_speed = level_walk_speed * 3600/1000,
    drop_geometry = TRUE
    )
}

bus_attributes <- bus_list %>%
  bind_rows() %>%
  rename(trip_id = from_id) %>%
  group_by(trip_id) %>%
  filter(n()>1) %>% #ensures at least one bus stage
  summarise(
    bus_distanceinvehicle = sum(distance*(mode=="BUS")),
    bus_timeinvehicle = sum(segment_duration*(mode=="BUS")),
    bus_distancetransfer = sum(distance*(mode=="WALK")),
    bus_timetransfer = sum(segment_duration*(mode=="WALK")),
    bus_timewaiting = sum(wait),
    bus_legs = sum(1*(mode=="BUS"))
  ) %>%
  mutate(
    bus_distance = bus_distanceinvehicle + bus_distancetransfer,
    bus_time = bus_timeinvehicle + bus_timetransfer + bus_timewaiting
  )

rail_attributes <- rail_list %>%
  bind_rows() %>%
  rename(trip_id = from_id) %>%
  group_by(trip_id) %>%
  filter(n()>1) %>% #ensures at least one rail stage
  summarise(
    rail_distanceinvehicle = sum(distance*(mode=="RAIL")),
    rail_timeinvehicle = sum(segment_duration*(mode=="RAIL")),
    rail_distancetransfer = sum(distance*(mode=="WALK")),
    rail_timetransfer = sum(segment_duration*(mode=="WALK")),
    rail_timewaiting = sum(wait),
    rail_legs = sum(1*(mode=="RAIL"))
  ) %>%
  mutate(
    rail_distance = rail_distanceinvehicle + rail_distancetransfer,
    rail_time = rail_timeinvehicle + rail_timetransfer + rail_timewaiting
  )

#Join attributes to the full dataset
R5_attributes <- trips %>%
  left_join(
    walk_attributes,
    by = join_by(trip_id)
  ) %>%
  left_join(
    bike_attributes,
    by = join_by(trip_id)
  ) %>%
  left_join(
    car_attributes,
    by = join_by(trip_id)
  ) %>%
  left_join(
    bus_attributes,
    by = join_by(trip_id)
  ) %>%
  left_join(
    rail_attributes,
    by = join_by(trip_id)
  )

## Save

write_csv(R5_attributes,"input/processed/R5_attributes.csv")


