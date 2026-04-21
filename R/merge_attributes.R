library(tidyverse)

trips <- read_csv("input/processed/decisions_processed.csv")

R5_attributes <- read_csv("input/processed/R5_attributes.csv") %>%
  select(trip_id,walk_distance:rail_time) %>%
  rename_with(~ str_c(.x, "_R5"), walk_distance:rail_time)

DO_attributes <- read_csv("input/processed/DO_attributes.csv") %>%
  select(trip_id,walk_distance:car_time) %>%
  rename_with(~ str_c(.x, "_DO"), walk_distance:car_time)

GR_attributes <- read_csv("input/processed/GR_attributes.csv") %>%
  select(trip_id,walk_distance:rail_time) %>%
  rename_with(~ str_c(.x, "_GR"), walk_distance:rail_time)

trips_attributes <- trips %>%
  left_join(R5_attributes, by = join_by(trip_id)) %>%
  left_join(DO_attributes, by = join_by(trip_id)) %>%
  left_join(GR_attributes, by = join_by(trip_id))

write_csv(trips_attributes,"input/processed/trips_attributes.csv")