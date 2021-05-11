library(targets)
library(dplyr)
source("R/data-utils.R")

tar_option_set(packages = c("climateR", "dplyr", "sf"))

get_regions <- function() {
    readRDS("data/California_Regions.rds")
}

list(
    tar_target(
        california_regions,
        get_regions()
    ),
    tar_target(
        region_names,
        california_regions$region
    ),
    tar_target(
        years,
        2015L:2020L
    ),
    tar_target(
        ten_year_data, {
            aoi <- california_regions %>%
                   dplyr::filter(
                       region == region_names
                   )

            climate_data <- aggregate_gridmet(
                aoi,
                paste0(years, "-01-01"),
                paste0(years, "-12-31")
            )

            fst::write_fst(
                climate_data,
                paste0(
                    "/mnt/x/california/",
                    region_names,
                    "_", years, ".fst"
                )
            )

            rm(climate_data)
        },
        pattern = cross(map(region_names), map(years))
    )
)