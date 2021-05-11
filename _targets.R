library(targets)
library(tarchetypes)
library(dplyr)
# library(parallel)
# library(doParallel)
# cl <- parallel::makeCluster(
#     2,
#     outfile = ""
# )
# doParallel::registerDoParallel(cl)

source("R/data-utils.R")
source("R/model-utils.R")
source("R/targets-utils.R")
options(
    tidyverse.quiet = TRUE
)

tar_option_set(
    packages = c("tidyverse", "tidymodels",
                 "spatialsample", "stacks",
                 "timetk", "kknn", "dplyr"),
    memory = "transient",
    format = "qs",
    garbage_collection = TRUE
)

regions <- readRDS("data/California_Regions.rds")$region
years   <- 2015L:2020L
values  <- expand.grid(regions, years) %>%
           setNames(c("region", "year")) %>%
           tibble::as_tibble()

values <- paste0(values$region, "_", values$year)

values <- tibble::tibble(
    region_year = values
)

list(
    tar_target(
        template,
        tibble::tibble(
            lat   = double(0),
            lon   = double(0),
            date  = lubridate::ymd("2021-09-05"),
            prcp  = double(0),
            rhmax = double(0),
            rhmin = double(0),
            shum  = double(0),
            srad  = double(0),
            tmin  = double(0),
            tmax  = double(0),
            burn_index = double(0)
        )
    ),
    preprocessing_factory(),
    tar_target(
        force_structure,
        tune::is_workflow(workflow_mars)
    ),
    tar_map(
        values = values,
        training_factory()
    )
)