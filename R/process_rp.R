library(tidyverse)

#Load data
trips_attributes <- read_csv("input/processed/trips_attributes.csv")
parking_times <- read_csv("input/processed/parking_times.csv")

#Leeds Hackney Carriage fares as of Jan 2026 are given by https://www.leeds.gov.uk/licensing/taxi-and-private-hire-licensing/are-you-taxi-aware
#Taxi fares increment for either distance and time
#To reverse engineer - assume taxi is either travelling at 30mph or stationary
#Time spent at 30mph is total distance divided by 30mph - time spent stationary is total time less this quantity
compute_taxi_fare <- function(base_fare,threshold_fare,lower_rate,higher_rate,lower_d,higher_d,lower_t,higher_t,distance,time){
  
  distance <- distance/0.9144 #convert metres to yards
  time = time*60 #convert minutes to seconds
  
  freeflow_speed <- 30*1760/3600 #convert mph to yards per second
  
  time_stationary <- time - pmin(time,distance/freeflow_speed)
  
  #no rounding as averages are taken
  lower_d_incs <- distance/lower_d
  lower_t_incs <- time_stationary/lower_t
  
  threshold_incs <- (threshold_fare - base_fare)/lower_rate + 1
  
  threshold_distance <- threshold_incs * lower_d_incs/(lower_d_incs+lower_t_incs) * lower_d
  threshold_time_stationary <- threshold_distance/freeflow_speed + threshold_incs * lower_t_incs/(lower_d_incs+lower_t_incs) * lower_t
  
  higher_d_incs <- (distance-threshold_distance)/higher_d
  higher_t_incs <- (time_stationary-threshold_time_stationary)/higher_t
  
  fare <- case_when(
    distance<lower_d & time<lower_t ~ 
      base_fare,
    lower_d_incs + lower_t_incs < threshold_incs ~ 
      base_fare + (lower_d_incs + lower_t_incs - 1)* lower_rate,
    TRUE ~ 
      threshold_fare + (higher_d_incs + higher_t_incs) * higher_rate 
  )
  
  return(fare)
  
}

#asks whether the date is in the Christmas/New Year period
is_cny <- function(date){
  return(
    case_when(
      month(date)==12 & day(date) %in% c(25,26)                             ~ TRUE,
      month(date)==12 & day(date) %in% c(24,31) & between(hour(date),18,23) ~ TRUE,
      month(date)==12 & day(date) == 27         & between(hour(date),0,5)   ~ TRUE,
      month(date)==1  & day(date) == 1                                      ~ TRUE,
      month(date)==12 & day(date) == 2          & between(hour(date),0,5)   ~ TRUE,
      TRUE ~ FALSE
    )
  )
}

trips_rp <- trips_attributes %>%
  
  mutate(
    
    #There were 727 trips where R5 generated a bus route but GR didn't, including 28 where bus was the chosen mode
    #There were 906 trips where GR generated a bus route but R5 didn't, including 16 where bus was the chosen mode
    av_bus_R5 = 1*(!is.na(bus_distance_R5)),
    av_bus_GR = 1*(!is.na(bus_distance_GR)),
    
    #There were 935 trips where R5 generated a rail route but GR didn't, including 24 where rail was the chosen mode
    #There were 184 trips where GR generated a rail route but R5 didn't, of which none had rail as the chosen mode
    av_rail_R5 = 1*(!is.na(rail_distance_R5)),
    av_rail_GR = 1*(!is.na(rail_distance_GR)),
    
    choice = case_when(
      mode=="walk" ~ 1,
      mode=="bike" ~ 2,
      mode=="car" & !taxi  ~ 3,
      mode=="car" & taxi  ~ 4,
      mode=="bus"  ~ 5,
      mode=="rail" ~ 6,
      TRUE ~ NA
      ),
    
    taxi_time_R5 = car_time_R5,
    taxi_time_DO = car_time_DO,
    taxi_time_GR = car_time_GR,
    
    bike_time_R5 = bike_time_R5 + parking_times$bike_parking,
    bike_time_DO = bike_time_DO + parking_times$bike_parking,
    bike_time_GR = bike_time_GR + parking_times$bike_parking,
    car_time_R5 = car_time_R5 + parking_times$car_parking,
    car_time_DO = car_time_DO + parking_times$car_parking,
    car_time_GR = car_time_GR + parking_times$car_parking,
    
    car_cost_GR = 0.45 * car_distance_GR/1609.34, #assumed car cost of 45p per mile
    taxi_cost_GR = 1/1.38*case_when(
      is_cny(start_at)                                         ~ compute_taxi_fare(6,10.8,0.3,0.3,103,140,36,36,car_distance_GR,car_time_GR),
      !(wday %in% c("Sat","Sun"))        & between(hour,6,17)  ~ compute_taxi_fare(3.6,6,0.2,0.2,140,153,36,36,car_distance_GR,car_time_GR),
      (wday %in% c("Sat","Sun"))         & between(hour,6,17)  ~ compute_taxi_fare(4,6.8,0.2,0.2,121,153,36,36,car_distance_GR,car_time_GR),
      !(wday %in% c("Fri","Sat","Sun"))  & between(hour,18,23) ~ compute_taxi_fare(4,6.8,0.2,0.2,121,153,36,36,car_distance_GR,car_time_GR),
      !(wday %in% c("Mon","Sat","Sun"))  & between(hour,0,5)   ~ compute_taxi_fare(4,6.8,0.2,0.2,121,153,36,36,car_distance_GR,car_time_GR),
      (wday %in% c("Fri","Sat","Sun"))   & between(hour,18,23) ~ compute_taxi_fare(4,7.2,0.2,0.2,103,140,36,36,car_distance_GR,car_time_GR),
      (wday %in% c("Mon","Sat","Sun"))   & between(hour,0,5)   ~ compute_taxi_fare(4,7.2,0.2,0.2,103,140,36,36,car_distance_GR,car_time_GR),
      TRUE ~ NA
      ),
    car_cost_R5 = 0.45 * car_distance_R5/1609.34, #assumed car cost of 45p per mile
    taxi_cost_R5 = 1/1.38*case_when(
      is_cny(start_at)                                         ~ compute_taxi_fare(6,10.8,0.3,0.3,103,140,36,36,car_distance_R5,car_time_R5),
      !(wday %in% c("Sat","Sun"))        & between(hour,6,17)  ~ compute_taxi_fare(3.6,6,0.2,0.2,140,153,36,36,car_distance_R5,car_time_R5),
      (wday %in% c("Sat","Sun"))         & between(hour,6,17)  ~ compute_taxi_fare(4,6.8,0.2,0.2,121,153,36,36,car_distance_R5,car_time_R5),
      !(wday %in% c("Fri","Sat","Sun"))  & between(hour,18,23) ~ compute_taxi_fare(4,6.8,0.2,0.2,121,153,36,36,car_distance_R5,car_time_R5),
      !(wday %in% c("Mon","Sat","Sun"))  & between(hour,0,5)   ~ compute_taxi_fare(4,6.8,0.2,0.2,121,153,36,36,car_distance_R5,car_time_R5),
      (wday %in% c("Fri","Sat","Sun"))   & between(hour,18,23) ~ compute_taxi_fare(4,7.2,0.2,0.2,103,140,36,36,car_distance_R5,car_time_R5),
      (wday %in% c("Mon","Sat","Sun"))   & between(hour,0,5)   ~ compute_taxi_fare(4,7.2,0.2,0.2,103,140,36,36,car_distance_R5,car_time_R5),
      TRUE ~ NA
      ),
    car_cost_DO = 0.45 * car_distance_DO/1609.34, #assumed car cost of 45p per mile
    taxi_cost_DO = 1/1.38*case_when(
      is_cny(start_at)                                         ~ compute_taxi_fare(6,10.8,0.3,0.3,103,140,36,36,car_distance_DO,car_time_DO),
      !(wday %in% c("Sat","Sun"))        & between(hour,6,17)  ~ compute_taxi_fare(3.6,6,0.2,0.2,140,153,36,36,car_distance_DO,car_time_DO),
      (wday %in% c("Sat","Sun"))         & between(hour,6,17)  ~ compute_taxi_fare(4,6.8,0.2,0.2,121,153,36,36,car_distance_DO,car_time_DO),
      !(wday %in% c("Fri","Sat","Sun"))  & between(hour,18,23) ~ compute_taxi_fare(4,6.8,0.2,0.2,121,153,36,36,car_distance_DO,car_time_DO),
      !(wday %in% c("Mon","Sat","Sun"))  & between(hour,0,5)   ~ compute_taxi_fare(4,6.8,0.2,0.2,121,153,36,36,car_distance_DO,car_time_DO),
      (wday %in% c("Fri","Sat","Sun"))   & between(hour,18,23) ~ compute_taxi_fare(4,7.2,0.2,0.2,103,140,36,36,car_distance_DO,car_time_DO),
      (wday %in% c("Mon","Sat","Sun"))   & between(hour,0,5)   ~ compute_taxi_fare(4,7.2,0.2,0.2,103,140,36,36,car_distance_DO,car_time_DO),
      TRUE ~ NA
      ),
    bus_cost_GR = case_when( ## 2016 bus fares taken from https://www.urbantransportgroup.org/system/files/general-docs/Fares%2520research.docx
      bus_own_ticket==1 ~ 0, ## 1 mile: £1.40, 3 miles: £2.40, max: £3.00 (for First - most common service in Leeds)
      bus_distance_GR<=1609.34 ~ 1.4,
      bus_distance_GR<=4823.03 ~ 2.4,
      bus_distance_GR>4823.03 ~ 3,
      TRUE ~ NA
      ),
    bus_cost_R5 = case_when( ## 2016 bus fares taken from https://www.urbantransportgroup.org/system/files/general-docs/Fares%2520research.docx
      bus_own_ticket==1 ~ 0, ## 1 mile: £1.40, 3 miles: £2.40, max: £3.00 (for First - most common service in Leeds)
      bus_distance_R5<=1609.34 ~ 1.4,
      bus_distance_R5<=4823.03 ~ 2.4,
      bus_distance_R5>4823.03 ~ 3,
      TRUE ~ NA
      ),
    #assume rail calculated the same way as bus (broadly true in Leeds)
    rail_cost_GR = case_when( 
      train_own_ticket==1 ~ 0, 
      rail_distance_GR<=1609.34 ~ 1.4,
      rail_distance_GR<=4823.03 ~ 2.4,
      rail_distance_GR>4823.03 ~ 3,
      TRUE ~ NA
      ),
    rail_cost_R5 = case_when(
      train_own_ticket==1 ~ 0,
      rail_distance_R5<=1609.34 ~ 1.4,
      rail_distance_R5<=4823.03 ~ 2.4,
      rail_distance_R5>4823.03 ~ 3,
      TRUE ~ NA
      )
    )
  
write_csv(trips_rp,"input/processed/trips_rp.csv")
