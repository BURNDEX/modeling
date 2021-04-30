library(foreach)
library(dplyr)
library(bigrquery)
source("R/data-utils.R")

bigrquery::bq_auth(path = "burndex-19597aebb216.json")

# ====================================================================
# Uploading Tidied gridMET Data to Google BiqQuery
# ====================================================================
# The process is as follows:
# (1) For all lower 48 states:
#    (1.1) Create a BQ Dataset with the name "gridmet_<state_abbr>"
#    (1.2) For each year between 1979 and 2021:
#        (1.2.a) Call aggregate_gridmet()
#        (1.2.b) Create a BQ table with the name "<state_abbr>_<year>"
#        (1.2.c) Upload to BQ table
#    (1.3) End for loop
# (2) End for loop
#
# To run, call the `bq_gridmet()` function. See below.
# ====================================================================
# Function for (1)
#' @title Upload (tidied) gridMET data to Google BigQuery
#' @param years Character vector containing the years to upload
#' @return A matrix of all years uploaded
bq_gridmet <- function(years) {
    states <- state.abb[-which(state.abb %in% c("HI", "AK", "PR"))]
    counties_pair <- unlist(lapply(
        X = states,
        FUN = function(state_abb) {
            paste0(
                AOI::aoi_get(county = "all", state = state_abb)$name,
                "_",
                state_abb
            )
        }
    ))


    years       <- as.character(years)
    start_dates <- paste0(years, "-01-01")
    end_dates   <- paste0(years, "-12-31")

    # (1.1) Create a BQ Dataset with the name "gridmet_<state_abbr>"
    bq_datasets <-
        foreach::foreach(state_abbr = states, .combine = "c") %do% {
            bq_new_dataset <- bigrquery::bq_dataset(
                project = "burndex",
                dataset = paste0("gridmet_", tolower(state_abbr))
            )

            if (!bigrquery::bq_dataset_exists(bq_new_dataset)) {
                bigrquery::bq_dataset_create(bq_new_dataset)
            }

            TRUE
        }

    if (!all(bq_datasets)) {
        rlang::abort("Some datasets failed to create!")
    } else {
        cat(crayon::green("\n✓"), crayon::magenta("All datasets created!"))

        if (interactive()) {
            input <- readline(
                paste0(
                    crayon::yellow("★ "),
                    crayon::magenta("Upload gridMET data from "),
                    crayon::bold(years[1]),
                    crayon::magenta(" to "),
                    crayon::bold(years[length(years)]),
                    crayon::magenta("? "),
                    crayon::silver("["),
                    crayon::green("y"),
                    crayon::silver("/"),
                    crayon::red("n"),
                    crayon::silver("]: ")
                )
            )

            if (tolower(input) != "y" & tolower(input) != "yes") {
                rlang::abort("User stopped upload.")
            }
        }
    }

    required_packages <- c(
        "sf",
        "climateR",
        "dplyr"
    )

    required_exports <- c(
        "bq_upload",
        "aggregate_gridmet",
        "common_params",
        "tidy_stack",
        "tidy_raster",
        "kelvin_to_fahrenheit",
        "chandler_bi"
    )

    # (1.2)
    bq_matrix <-
        foreach::foreach(
            county = counties_pair,
            .combine = "cbind",
            .export = required_exports,
            .packages = required_packages
        ) %:%
            foreach::foreach(
                start_date = start_dates,
                end_date = end_dates,
                .combine = "c",
                .export = required_exports,
                .packages = required_packages
            ) %dopar% {
                split_      <- stringr::str_split(county, "_")[[1]]
                county_name <- split_[1]
                state_abb   <- split_[2]

                cat(
                    crayon::blue("⬤"),
                    crayon::green(paste0("[", state_abb, "]")),
                    crayon::red(paste(
                        "Starting",
                        paste0(county_name, ","),
                        state_abb,
                        "from",
                        start_date,
                        "to",
                        end_date,
                        "\n"
                    ))
                )

                # Get state
                current_aoi <- AOI::aoi_get(
                    county = county_name,
                    state = state_abb
                )

                # Upload to BigQuery
                bq_upload(current_aoi, start_date, end_date)

                # Return table name
                paste0(
                    tolower(current_aoi$state_abb),
                    "_",
                    substr(start_date, 0, 4)
                )
            }

    bq_matrix
}
# ====================================================================
# Function for (1.2)
bq_upload <- function(aoi, start_date, end_date) {
    year          <- substr(start_date, 0, 4)
    county_name   <- tolower(aoi$name) %>%
                     stringr::str_replace(" ", "_")
    state_abbr    <- tolower(aoi$state_abbr)
    bq_table_name <- paste0(county_name, "_", state_abbr, "_", year)
    bq_new_table  <- bigrquery::bq_table(
        project = "burndex",
        dataset = paste0("gridmet_", state_abbr),
        table   = bq_table_name
    )

    if (bigrquery::bq_table_exists(bq_new_table)) {
        return(bq_new_table)
    }

    cat(
        crayon::blue("⬤"),
        crayon::magenta(paste0("[", bq_table_name, "]")),
        crayon::red("Getting gridMET data\n")
    )

    # (1)
    gridmet_data <- aggregate_gridmet(
        aoi        = aoi,
        start_date = start_date,
        end_date   = end_date
    )

    cat(
        crayon::blue("⬤"),
        crayon::magenta(paste0("[", bq_table_name, "]")),
        crayon::red("Creating BQ Table\n")
    )
    # (2)
    bigrquery::bq_table_create(
        x = bq_new_table,
        fields = gridmet_data,
        friendly_name = paste0(
            aoi$name, ", ",
            aoi$state_name,
            " gridMET Climate Data"
        ),
        description = paste0(
            aoi$name, ", ", aoi$state_name,
            " tidied gridMET climate data from ",
            start_date, " to ", end_date
        )
    )

    cat(
        crayon::blue("⬤"),
        crayon::magenta(paste0("[", bq_table_name, "]")),
        crayon::red("Uploading to BQ\n")
    )

    # (3)
    bigrquery::bq_table_upload(x = bq_new_table, values = gridmet_data)

    # Clean up
    rm(gridmet_data)

    cat(
        crayon::green("✓"),
        crayon::magenta(paste0("[", bq_table_name, "]")),
        crayon::red("Uploaded to BQ!\n")
    )

    # Return
    bq_new_table
}
# ====================================================================

# ====================================================================
# Storing 40 years of data
# ====================================================================

cores <- parallel::detectCores()[1] - 2
cl    <- parallel::makeCluster(cores, outfile = "")
doParallel::registerDoParallel(cl)

parallel::clusterEvalQ(
    cl = cl,
    expr = {
        library(foreach)
        library(tidyverse)
        library(sf)
        library(bigrquery)
        library(climateR)
        library(AOI)
        library(crayon)
        source("R/data-utils.R")
    }
)

# Get 40 years
years <- as.character(1980L:2020L)

# Run operation
bq_gridmet(years)

doParallel::stopImplicitCluster()
parallel::stopCluster(cl)