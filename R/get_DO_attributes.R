library(tidyverse)
library(sf)

devtools::install_github("harrysroberts/losdos")
library(losdos)

read_sf("input/boundary/district_borough_unitary_region.shp") %>%
  filter(str_detect(NAME,"Leeds")) %>%
  st_buffer(2000) %>%
  write_sf("input/raw/boundary.gpkg")

trips <- read_csv("input/processed/decisions_processed.csv") %>%
  rename(
    from_easting = start_easting,
    from_northing = start_northing,
    to_easting = stop_easting,
    to_northing = stop_northing
    ) %>%
  select(
    trip_id,
    period,
    from_easting,
    from_northing,
    to_easting,
    to_northing
  )


DO_attributes <- osmrn_trip_attributes(trips)

write_csv(DO_attributes,"input/processed/DO_attributes.csv")
