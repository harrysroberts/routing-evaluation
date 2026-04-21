library(tidyverse)
library(elevatr)
library(terra)

data.frame(x = c(-2.18, -1.19), y = c(53.51, 53.97)) %>%
  get_elev_raster(z = 12, prj = 4326) %>%
  writeRaster("input/raw/r5r/elevation.tif", overwrite = TRUE)

