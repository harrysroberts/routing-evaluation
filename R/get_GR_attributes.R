library(httr)
library(jsonlite)
library(tidyverse)

# Get API key
api_key <- read_file("input/raw/api_key.txt")

## Load and process Decisions dataset

trips <- read_csv("input/processed/decisions_processed.csv")

## Set walk and bike speeds

# Walk and bike speeds on level ground and age/gender factors taken from 
# MatSim GitHub repository, Based on MatSim documentation by Horni et al.
#  (2016) available from https://www.ubiquitypress.com/books/e/10.5334/baw
# Weidmann reference speed of 1.34 m/s (4.824 km/h) used by MatSim
# Parkin and Rotheram reference speed of 6.01 m/s (21.636 km/h) used by MatSim

level_walk_speed = 1.34
level_bike_speed = 6.01

# Function for one pair
get_travel_time_routes <- function(o_lat, o_lon, d_lat, d_lon, mode, time, api_key) {
  url <- "https://routes.googleapis.com/directions/v2:computeRoutes"
  
  if(mode=="walk"){
    
    body <- list(
      origin = list(location = list(latLng = list(latitude = o_lat, longitude = o_lon))),
      destination = list(location = list(latLng = list(latitude = d_lat, longitude = d_lon))),
      travelMode = "WALK"
    )
    res <- POST(
      url,
      add_headers(
        "Content-Type" = "application/json",
        "X-Goog-Api-Key" = api_key,
        "X-Goog-FieldMask" = "routes.duration,routes.distanceMeters"
      ),
      body = toJSON(body, auto_unbox = TRUE)
    )
    
    content <- content(res, as = "parsed", type = "application/json")
    
    tibble(
      walk_distance = content$routes[[1]]$distanceMeters %||% NA,
      walk_time  = suppressWarnings(as.numeric(gsub("s", "", content$routes[[1]]$duration %||% NA)))/60
    )
    
  }else if(mode=="bike"){
    
    body <- list(
      origin = list(location = list(latLng = list(latitude = o_lat, longitude = o_lon))),
      destination = list(location = list(latLng = list(latitude = d_lat, longitude = d_lon))),
      travelMode = "BICYCLE"
    )
    
    res <- POST(
      url,
      add_headers(
        "Content-Type" = "application/json",
        "X-Goog-Api-Key" = api_key,
        "X-Goog-FieldMask" = "routes.duration,routes.distanceMeters"
      ),
      body = toJSON(body, auto_unbox = TRUE)
    )
    
    content <- content(res, as = "parsed", type = "application/json")
    
    tibble(
      bike_distance = content$routes[[1]]$distanceMeters %||% NA,
      bike_time  = suppressWarnings(as.numeric(gsub("s", "", content$routes[[1]]$duration %||% NA)))/60
    )
    
  }else if(mode=="car"){
    
    body <- list(
      origin = list(location = list(latLng = list(latitude = o_lat, longitude = o_lon))),
      destination = list(location = list(latLng = list(latitude = d_lat, longitude = d_lon))),
      travelMode = "DRIVE",
      #Set the routing preference to traffic aware
      routingPreference = "TRAFFIC_AWARE_OPTIMAL",
      #Set departure time to the same weekday and time but at least a week after the current date
      departure_time = sub(
        "([+-][0-9]{2})([0-9]{2})$", "\\1:\\2",
        format(
          update(
            today() + weeks(1) + days((wday(time) - wday(today()) + 7) %% 7),
            hour = hour(time),
            minute = minute(time),
            second = second(time)
          ),
          "%Y-%m-%dT%H:%M:%S%z"
        )
      )
    )
    
    res <- POST(
      url,
      add_headers(
        "Content-Type" = "application/json",
        "X-Goog-Api-Key" = api_key,
        "X-Goog-FieldMask" = "routes.duration,routes.distanceMeters"
      ),
      body = toJSON(body, auto_unbox = TRUE)
    )
    
    content <- content(res, as = "parsed", type = "application/json")
    
    tibble(
      car_distance = content$routes[[1]]$distanceMeters %||% NA,
      car_time  = suppressWarnings(as.numeric(gsub("s", "", content$routes[[1]]$duration %||% NA)))/60
    )
    
  } else if(mode=="bus") {
    
    body <- list(
      origin = list(location = list(latLng = list(latitude = o_lat, longitude = o_lon))),
      destination = list(location = list(latLng = list(latitude = d_lat, longitude = d_lon))),
      travelMode = "TRANSIT",
      transitPreferences = list(
        allowedTravelModes = list("BUS") 
      ),
      #Set departure time to the same weekday and time but at least a week after the current date
      departure_time = sub(
        "([+-][0-9]{2})([0-9]{2})$", "\\1:\\2",
        format(
          update(
            today() + weeks(1) + days((wday(time) - wday(today()) + 7) %% 7),
            hour = hour(time),
            minute = minute(time),
            second = second(time)
          ),
          "%Y-%m-%dT%H:%M:%S%z"
        )
      )
    )
    
    res <- POST(
      url,
      add_headers(
        "Content-Type" = "application/json",
        "X-Goog-Api-Key" = api_key,
        "X-Goog-FieldMask" = "routes.duration,routes.distanceMeters,routes.legs.steps.travelMode,routes.legs.steps.transitDetails.transitLine.vehicle.type,routes.legs.steps.distanceMeters,routes.legs.steps.staticDuration"
      ),
      body = toJSON(body, auto_unbox = TRUE)
    )
    
    content <- content(res, as = "parsed", type = "application/json")
    
  } else { #mode is rail
    
    body <- list(
      origin = list(location = list(latLng = list(latitude = o_lat, longitude = o_lon))),
      destination = list(location = list(latLng = list(latitude = d_lat, longitude = d_lon))),
      travelMode = "TRANSIT",
      transitPreferences = list(
        allowedTravelModes = list("RAIL") 
      ),
      #Set departure time to the same weekday and time but at least a week after the current date
      departure_time = sub(
        "([+-][0-9]{2})([0-9]{2})$", "\\1:\\2",
        format(
          update(
            today() + weeks(1) + days((wday(time) - wday(today()) + 7) %% 7),
            hour = hour(time),
            minute = minute(time),
            second = second(time)
          ),
          "%Y-%m-%dT%H:%M:%S%z"
        )
      )
    )
    
    res <- POST(
      url,
      add_headers(
        "Content-Type" = "application/json",
        "X-Goog-Api-Key" = api_key,
        "X-Goog-FieldMask" = "routes.duration,routes.distanceMeters,routes.legs.steps.travelMode,routes.legs.steps.transitDetails.transitLine.vehicle.type,routes.legs.steps.distanceMeters,routes.legs.steps.staticDuration"
      ),
      body = toJSON(body, auto_unbox = TRUE)
    )
    
    content <- content(res, as = "parsed", type = "application/json")
    
  }
}

##  Run for the trips dataset

walk_results <- trips %>%
  st_as_sf(coords = c("start_easting", "start_northing"), crs = 27700) %>%
  st_transform(crs = 4326) %>%
  mutate(
    start_lon = st_coordinates(.)[,1],
    start_lat = st_coordinates(.)[,2]
  ) %>%
  st_drop_geometry() %>%
  st_as_sf(coords = c("stop_easting", "stop_northing"), crs = 27700) %>%
  st_transform(crs = 4326) %>%
  mutate(
    stop_lon = st_coordinates(.)[,1],
    stop_lat = st_coordinates(.)[,2]
  ) %>%
  st_drop_geometry() %>%
  rowwise() %>%
  mutate(
    result = list(
      get_travel_time_routes(
        o_lat = start_lat, 
        o_lon = start_lon, 
        d_lat = stop_lat, 
        d_lon = stop_lon, 
        mode = "walk", 
        time = start_at, 
        api_key = api_key))
  ) %>%
  unnest(result)

bike_results <- trips %>%
  st_as_sf(coords = c("start_easting", "start_northing"), crs = 27700) %>%
  st_transform(crs = 4326) %>%
  mutate(
    start_lon = st_coordinates(.)[,1],
    start_lat = st_coordinates(.)[,2]
  ) %>%
  st_drop_geometry() %>%
  st_as_sf(coords = c("stop_easting", "stop_northing"), crs = 27700) %>%
  st_transform(crs = 4326) %>%
  mutate(
    stop_lon = st_coordinates(.)[,1],
    stop_lat = st_coordinates(.)[,2]
  ) %>%
  st_drop_geometry() %>%
  rowwise() %>%
  mutate(
    result = list(
      get_travel_time_routes(
        o_lat = start_lat, 
        o_lon = start_lon, 
        d_lat = stop_lat, 
        d_lon = stop_lon, 
        mode = "bike", 
        time = start_at, 
        api_key = api_key))
  ) %>%
  unnest(result)

car_results <- trips %>%
  st_as_sf(coords = c("start_easting", "start_northing"), crs = 27700) %>%
  st_transform(crs = 4326) %>%
  mutate(
    start_lon = st_coordinates(.)[,1],
    start_lat = st_coordinates(.)[,2]
  ) %>%
  st_drop_geometry() %>%
  st_as_sf(coords = c("stop_easting", "stop_northing"), crs = 27700) %>%
  st_transform(crs = 4326) %>%
  mutate(
    stop_lon = st_coordinates(.)[,1],
    stop_lat = st_coordinates(.)[,2]
  ) %>%
  st_drop_geometry() %>%
  rowwise() %>%
  mutate(
    result = list(
      get_travel_time_routes(
        o_lat = start_lat, 
        o_lon = start_lon, 
        d_lat = stop_lat, 
        d_lon = stop_lon, 
        mode = "car", 
        time = start_at, 
        api_key = api_key))
  ) %>%
  unnest(result)

bus_results_raw <- trips %>%
  st_as_sf(coords = c("start_easting", "start_northing"), crs = 27700) %>%
  st_transform(crs = 4326) %>%
  mutate(
    start_lon = st_coordinates(.)[,1],
    start_lat = st_coordinates(.)[,2]
  ) %>%
  st_drop_geometry() %>%
  st_as_sf(coords = c("stop_easting", "stop_northing"), crs = 27700) %>%
  st_transform(crs = 4326) %>%
  mutate(
    stop_lon = st_coordinates(.)[,1],
    stop_lat = st_coordinates(.)[,2]
  ) %>%
  st_drop_geometry() %>%
  rowwise() %>%
  mutate(
    result = list(
      get_travel_time_routes(
        o_lat = start_lat, 
        o_lon = start_lon, 
        d_lat = stop_lat, 
        d_lon = stop_lon, 
        mode = "bus", 
        time = start_at, 
        api_key = api_key))
  )

bus_results_processed <- tibble(
  trip_id = NA,
  bus_distanceinvehicle = NA,
  bus_timeinvehicle = NA,
  bus_distancetransfer = NA,
  bus_timetransfer = NA,
  bus_timewaiting = NA,
  bus_legs = NA,
  bus_distance = NA,
  bus_time = NA,
)

for(i in c(1:nrow(bus_results_raw))){
  
  if(is.null(bus_results_raw$result[[i]]$routes[[1]])){
    res <- tibble(
      trip_id = bus_results_raw$trip_id[[i]],
      bus_distanceinvehicle = NA,
      bus_timeinvehicle = NA,
      bus_distancetransfer = NA,
      bus_timetransfer = NA,
      bus_timewaiting = NA,
      bus_legs = NA,
      bus_distance = NA,
      bus_time = NA
    )
    
  } else {
    
    steps <- bus_results_raw$result[[i]]$routes[[1]]$legs[[1]]$steps %>%
      map_df(function(x) {
        tibble(
          distance  = if (is.null(x$distanceMeters)) NA_real_ else x$distanceMeters,
          duration  = if (is.null(x$staticDuration)) NA_character_ else x$staticDuration,
          mode      = if (is.null(x$travelMode)) NA_character_ else x$travelMode,
          type      = if (is.null(x$transitDetails$transitLine$vehicle$type)) NA_character_ else x$transitDetails$transitLine$vehicle$type
        )
      }) %>%
      mutate(duration = as.numeric(str_sub(duration, 1, -2))) %>%
      group_by(mode,type) %>%
      summarise(total_distance = sum(distance),total_duration = sum(duration),N = n(),.groups = "drop")
    
    #If there are no bus stages, or if any of the transit stages do not use bus, then the trip is deemed unroutable
    #Transit vehicle types from https://docs.cloud.google.com/nodejs/docs/reference/routing/latest/routing/protos.google.maps.routing.v2.transitvehicle.transitvehicletype
    if(!any(steps$type %in% c("BUS","INTERCITY_BUS","TROLLEYBUS"))|
       any(steps$mode=="TRANSIT" & !(steps$type %in% c("BUS","INTERCITY_BUS","TROLLEYBUS")))){
      res <- tibble(
        trip_id = bus_results_raw$trip_id[[i]],
        bus_distanceinvehicle = NA,
        bus_timeinvehicle = NA,
        bus_distancetransfer = NA,
        bus_timetransfer = NA,
        bus_timewaiting = NA,
        bus_legs = NA,
        bus_distance = NA,
        bus_time = NA
      )
    } else if(!any(steps$mode == "WALK")) {
      res <- steps %>%
        mutate(
          trip_id = bus_results_raw$trip_id[[i]],
          bus_distanceinvehicle = total_distance,
          bus_timeinvehicle = total_duration/60,
          bus_distancetransfer = 0,
          bus_timetransfer = 0,
          bus_timewaiting = round(suppressWarnings(as.numeric(gsub("s", "", bus_results_raw$result[[i]]$routes[[1]]$duration %||% NA)))/60 - bus_timeinvehicle, 6),
          bus_legs = N,
          bus_distance = bus_distanceinvehicle,
          bus_time = bus_timeinvehicle + bus_timewaiting
        ) %>%
        select(trip_id:bus_time)
    } else {
      res <- steps %>%
        select(!type) %>%
        pivot_wider(
          names_from = mode,
          values_from = c(total_distance,total_duration,N)
        ) %>%
        mutate(
          trip_id = bus_results_raw$trip_id[[i]],
          bus_distanceinvehicle = total_distance_TRANSIT,
          bus_timeinvehicle = total_duration_TRANSIT/60,
          bus_distancetransfer = total_distance_WALK,
          bus_timetransfer = total_duration_WALK/60,
          bus_timewaiting = round(suppressWarnings(as.numeric(gsub("s", "", bus_results_raw$result[[i]]$routes[[1]]$duration %||% NA)))/60 - bus_timeinvehicle - bus_timetransfer, 6),
          bus_legs = N_TRANSIT,
          bus_distance = bus_distanceinvehicle + bus_distancetransfer,
          bus_time = bus_timeinvehicle + bus_timetransfer + bus_timewaiting
        ) %>%
        select(trip_id:bus_time)
      
    }
  }
  
  bus_results_processed <- bind_rows(bus_results_processed,res)
  
}

bus_results <- bus_results_raw %>%
  select(!result) %>%
  left_join(bus_results_processed,by=join_by(trip_id))

rail_results_raw <- trips %>%
  st_as_sf(coords = c("start_easting", "start_northing"), crs = 27700) %>%
  st_transform(crs = 4326) %>%
  mutate(
    start_lon = st_coordinates(.)[,1],
    start_lat = st_coordinates(.)[,2]
  ) %>%
  st_drop_geometry() %>%
  st_as_sf(coords = c("stop_easting", "stop_northing"), crs = 27700) %>%
  st_transform(crs = 4326) %>%
  mutate(
    stop_lon = st_coordinates(.)[,1],
    stop_lat = st_coordinates(.)[,2]
  ) %>%
  st_drop_geometry() %>%
  rowwise() %>%
  mutate(
    result = list(
      get_travel_time_routes(
        o_lat = start_lat, 
        o_lon = start_lon, 
        d_lat = stop_lat, 
        d_lon = stop_lon, 
        mode = "rail", 
        time = start_at, 
        api_key = api_key))
  )

rail_results_processed <- tibble(
  trip_id = NA,
  rail_distanceinvehicle = NA,
  rail_timeinvehicle = NA,
  rail_distancetransfer = NA,
  rail_timetransfer = NA,
  rail_timewaiting = NA,
  rail_legs = NA,
  rail_distance = NA,
  rail_time = NA
)

for(i in c(1:nrow(rail_results_raw))){
  
  if(is.null(rail_results_raw$result[[i]]$routes[[1]])){
    res <- tibble(
      trip_id = rail_results_raw$trip_id[[i]],
      rail_distanceinvehicle = NA,
      rail_timeinvehicle = NA,
      rail_distancetransfer = NA,
      rail_timetransfer = NA,
      rail_timewaiting = NA,
      rail_legs = NA,
      rail_distance = NA,
      rail_time = NA
    )
    
  } else {
    
    steps <- rail_results_raw$result[[i]]$routes[[1]]$legs[[1]]$steps %>%
      map_df(function(x) {
        tibble(
          distance  = if (is.null(x$distanceMeters)) NA_real_ else x$distanceMeters,
          duration  = if (is.null(x$staticDuration)) NA_character_ else x$staticDuration,
          mode      = if (is.null(x$travelMode)) NA_character_ else x$travelMode,
          type      = if (is.null(x$transitDetails$transitLine$vehicle$type)) NA_character_ else x$transitDetails$transitLine$vehicle$type
        )
      }) %>%
      mutate(duration = as.numeric(str_sub(duration, 1, -2))) %>%
      group_by(mode,type) %>%
      summarise(total_distance = sum(distance),total_duration = sum(duration),N = n(),.groups="drop")
    
    #If there are no rail stages, or if any of the transit stages do not use rail, then the trip is deemed unroutable
    #Transit vehicle types from https://docs.cloud.google.com/nodejs/docs/reference/routing/latest/routing/protos.google.maps.routing.v2.transitvehicle.transitvehicletype
    if(!any(steps$type %in% c("COMMUTER_TRAIN","HEAVY_RAIL","HIGH_SPEED_TRAIN","LONG_DISTANCE_TRAIN","METRO_RAIL","RAIL"))|
       any(steps$mode=="TRANSIT" & !(steps$type %in% c("COMMUTER_TRAIN","HEAVY_RAIL","HIGH_SPEED_TRAIN","LONG_DISTANCE_TRAIN","METRO_RAIL","RAIL")))){
      res <- tibble(
        trip_id = rail_results_raw$trip_id[[i]],
        rail_distanceinvehicle = NA,
        rail_timeinvehicle = NA,
        rail_distancetransfer = NA,
        rail_timetransfer = NA,
        rail_timewaiting = NA,
        rail_legs = NA,
        rail_distance = NA,
        rail_time = NA
      )
    } else if(!any(steps$mode == "WALK")) {
      res <- steps %>%
        mutate(
          trip_id = rail_results_raw$trip_id[[i]],
          rail_distanceinvehicle = total_distance,
          rail_timeinvehicle = total_duration/60,
          rail_distancetransfer = 0,
          rail_timetransfer = 0,
          rail_timewaiting = round(suppressWarnings(as.numeric(gsub("s", "", rail_results_raw$result[[i]]$routes[[1]]$duration %||% NA)))/60 - rail_timeinvehicle, 6),
          rail_legs = N,
          rail_distance = rail_distanceinvehicle,
          rail_time = rail_timeinvehicle + rail_timewaiting
        ) %>%
        select(trip_id:rail_time)
    } else {
      res <- steps %>%
        select(!type) %>%
        pivot_wider(
          names_from = mode,
          values_from = c(total_distance,total_duration,N)
        ) %>%
        mutate(
          trip_id = rail_results_raw$trip_id[[i]],
          rail_distanceinvehicle = total_distance_TRANSIT,
          rail_timeinvehicle = total_duration_TRANSIT/60,
          rail_distancetransfer = total_distance_WALK,
          rail_timetransfer = total_duration_WALK/60,
          rail_timewaiting = round(suppressWarnings(as.numeric(gsub("s", "", rail_results_raw$result[[i]]$routes[[1]]$duration %||% NA)))/60 - rail_timeinvehicle - rail_timetransfer, 6),
          rail_legs = N_TRANSIT,
          rail_distance = rail_distanceinvehicle + rail_distancetransfer,
          rail_time = rail_timeinvehicle + rail_timetransfer + rail_timewaiting
        ) %>%
        select(trip_id:rail_time)
      
    }
  }
  
  rail_results_processed <- bind_rows(rail_results_processed,res)
  
}

rail_results <- rail_results_raw %>%
  select(!result) %>%
  left_join(rail_results_processed,by=join_by(trip_id))


#Bind datasets together
GR_attributes <- walk_results %>%
  left_join(select(bike_results,trip_id,bike_distance,bike_time),by=join_by(trip_id)) %>%
  left_join(select(car_results,trip_id,car_distance,car_time),by=join_by(trip_id)) %>%
  left_join(select(bus_results,trip_id,bus_distanceinvehicle:bus_time),by=join_by(trip_id)) %>%
  left_join(select(rail_results,trip_id,rail_distanceinvehicle:rail_time),by=join_by(trip_id))

write_csv(GR_attributes,"input/processed/GR_attributes.csv")
