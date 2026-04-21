library(tidyverse)
library(sf)

decisions_personal <- read_csv("input/raw/decisions_personal.csv")
decisions_trips <- read_csv("input/raw/decisions_trips.csv")

time_periods <- c(
  "MoFr04000700", "MoFr07000900", "MoFr09001200", "MoFr12001400",
  "MoFr14001600", "MoFr16001900", "MoFr19002200", "MoFr22000400",
  "SaSu04000700", "SaSu07001000", "SaSu10001400", "SaSu14001900",
  "SaSu19002200", "SaSu22000400"
)

boundary <- read_sf("input/raw/boundary/district_borough_unitary_region.shp") %>%
  filter(str_detect(NAME,"Leeds"))

decisions_processed <- decisions_trips %>%
  
  #assign an id to each trip
  mutate(trip_id = str_c("trip",row_number())) %>%
  
  #only accept trips with defined start and end points
  filter(!is.na(start_point) & !is.na(stop_point)) %>%
  
  #convert WKB coordinates to BNG
  mutate(
    start_raw = map(start_point, ~ as.raw(strtoi(str_extract_all(.x, "..")[[1]], base = 16))),
    stop_raw  = map(stop_point,  ~ as.raw(strtoi(str_extract_all(.x, "..")[[1]], base = 16))),
    start_geom = st_as_sfc(start_raw, EWKB = TRUE) %>% st_transform(27700),
    stop_geom = st_as_sfc(stop_raw, EWKB = TRUE) %>% st_transform(27700),
    start_easting = st_coordinates(start_geom)[,1],
    start_northing = st_coordinates(start_geom)[,2],
    stop_easting = st_coordinates(stop_geom)[,1],
    stop_northing = st_coordinates(stop_geom)[,2]
  ) %>%
  
  #remove trips with either end outside the Leeds boundary
  filter(
    st_within(start_geom, boundary, sparse = FALSE)[, 1] &
      st_within(stop_geom,  boundary, sparse = FALSE)[, 1]
  ) %>%
  
  
  #reject trips with a distance shorter than the beeline distance, or no distance or duration at all
  filter(
    distance >= distance_beeline,
    distance>0,
    duration>0
    ) %>%
  
  #reject trips with a distance over 10x longer than the beeline distance
  filter(distance/distance_beeline<10) %>%
  
  #reject trips that are tagged as a change of travel mode
  filter(purpose!="Change Travel Mode") %>%
  
  mutate(
    
    #add a flag that identifies when a trip is by taxi (so no addition of parking time)
    taxi = str_detect(mode,"(?i)taxi"),
    
    #tag trips that use walk, bike, car, bus and rail modes only
    mode = case_when(
      str_detect(mode,"(?i)walk") & !str_detect(mode,",") ~ "walk", #walk and nothing else
      str_detect(mode,"(?i)bicycle") & !str_detect(mode,"(?i)vehicle|car|motorcycle|auto|taxi|atv|bus|rail|metro|tram|other mode") ~ "bike", 
      str_detect(mode,"(?i)vehicle|car|motorcycle|auto|taxi|atv") & !str_detect(mode,"(?i)bus|rail|metro|tram|other mode") ~ "car",
      str_detect(mode,"(?i)bus") & !str_detect(mode,"(?i)other|intercity|school|shuttle|bicycle|vehicle|car|motorcycle|auto|taxi|atv|rail|metro|tram|other mode") ~ "bus",
      str_detect(mode,"(?i)rail") & !str_detect(mode,"(?i)bicycle|vehicle|car|motorcycle|auto|taxi|atv|bus|metro|tram|other mode") ~ "rail",
      TRUE ~ NA
    ),
    
    #assign trips to Ordnance Survey time period
    wday = wday(start_at, label = TRUE, week_start = 1),
    hour = hour(start_at),
    minute = minute(start_at),
    time = hour + minute/60,
    period = case_when(
      wday %in% c("Mon", "Tue", "Wed", "Thu", "Fri") & time > 4 & time <= 7 ~ "MoFr04000700",
      wday %in% c("Mon", "Tue", "Wed", "Thu", "Fri") & time > 7 & time <= 9 ~ "MoFr07000900",
      wday %in% c("Mon", "Tue", "Wed", "Thu", "Fri") & time > 9 & time <= 12 ~ "MoFr09001200",
      wday %in% c("Mon", "Tue", "Wed", "Thu", "Fri") & time > 12 & time <= 14 ~ "MoFr12001400",
      wday %in% c("Mon", "Tue", "Wed", "Thu", "Fri") & time > 14 & time <= 16 ~ "MoFr14001600",
      wday %in% c("Mon", "Tue", "Wed", "Thu", "Fri") & time > 16 & time <= 19 ~ "MoFr16001900",
      wday %in% c("Mon", "Tue", "Wed", "Thu", "Fri") & time > 19 & time <= 22 ~ "MoFr19002200",
      wday %in% c("Mon", "Tue", "Wed", "Thu", "Fri") & (time > 22 | time <= 4) ~ "MoFr22000400",
      
      wday %in% c("Sat", "Sun") & time > 4 & time <= 7 ~ "SaSu04000700",
      wday %in% c("Sat", "Sun") & time > 7 & time <= 10 ~ "SaSu07001000",
      wday %in% c("Sat", "Sun") & time > 10 & time <= 14 ~ "SaSu10001400",
      wday %in% c("Sat", "Sun") & time > 14 & time <= 19 ~ "SaSu14001900",
      wday %in% c("Sat", "Sun") & time > 19 & time <= 22 ~ "SaSu19002200",
      wday %in% c("Sat", "Sun") & (time > 22 | time <= 4) ~ "SaSu22000400",
      
      TRUE ~ "Unknown"
    ),
    modeperiod = if_else(mode %in% c("walk","bus","rail"),mode,str_c(mode,"_",period))
  ) %>%
  
  #remove data with no tagged mode
  filter(!is.na(mode)) %>%
  
  left_join(
    select(decisions_personal,ID,n_car,n_bicycle,n_motorcycle,bus_own_ticket,train_own_ticket),
    by = join_by(ID)
  ) %>%
  
  mutate(
    av_bike = 1*(n_bicycle>0),
    av_car = 1*(n_car>0|n_motorcycle>0),
  ) %>%
  
  mutate(
    distance_decisions = distance,
    time_decisions = duration/60
  ) %>%
  select(trip_id,ID,mode,taxi,purpose,start_at,period,wday,hour,minute,av_bike,av_car,bus_own_ticket,train_own_ticket,start_easting:stop_northing,distance_decisions,time_decisions)

write_csv(decisions_processed,"input/processed/decisions_processed.csv")
