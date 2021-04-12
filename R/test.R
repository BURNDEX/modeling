library(sf)
library(raster)
library(AOI)
library(climateR)
library(dplyr)
source("R/data-utils.R")

aoi <- aoi_get(state = "California", county = "Santa Barbara")

progressr::with_progress({
    agg <- aggregate_gridmet(aoi, "2010-01-01", "2010-02-01", as_sf = FALSE)
})