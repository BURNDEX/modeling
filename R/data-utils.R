get_conus <- function() {
    dplyr::filter(
        USAboundaries::us_states(),
        !name %in% c("Hawaii", "Puerto Rico", "Alaska")
    ) %>%
        sf::st_as_sf() %>%
        sf::st_transform(5070)
}

get_north_america <- function() {
    rnaturalearth::ne_countries() %>%
        sf::st_as_sf() %>%
        dplyr::filter(continent == "North America") %>%
        sf::st_transform(
            paste(
                "+proj=aea",
                "+lat_1=20", "+lat_2=60",
                "+lat_0=40", "+lon_0=-96",
                "+x_0=0", "+y_0=0",
                "+ellps=GRS80",
                "+datum=NAD83",
                "+units=m",
                "+no_defs"
            )
        )
}

get_global <- function() {
    rnaturalearth::ne_countries() %>%
        sf::st_as_sf() %>%
        sf::st_transform("+proj=robin")
}

tidy_raster <- function(raster) {
    rtable <- raster %>%
              raster::rasterToPoints() %>%
              tibble::as_tibble() %>%
              dplyr::relocate(x, y) %>%
              setNames(
                  .,
                  c("lon",
                    "lat",
                    stringr::str_sub(colnames(.)[-(1:2)], start = 2L))
              ) %>%
              tidyr::pivot_longer(
                  cols = c(tidyselect::everything(), -(1:2)),
                  names_to = "date"
              ) %>%
              dplyr::mutate(date = lubridate::ymd(date)) %>%
              dplyr::relocate(lon, lat, value)

    rtable
}

tidy_stack <- function(raster_list, as_sf = FALSE) {
    param_names <- names(raster_list)
    tidy_stacks <- lapply(X = raster_list, FUN = tidy_raster)

    p <- progressr::progressor(along = param_names)
    tidy_data <-
        lapply(X = param_names,
           FUN = function(rname) {
               p(paste0("Transforming ", rname, "..."))
               setNames(
                   tidy_stacks[[rname]],
                   c("lon", "lat", rname, "date")
               )
            }
        ) %>%
        purrr::reduce(dplyr::left_join, by = c("date", "lon", "lat")) %>%
        dplyr::relocate(lon, lat, date)

    if (as_sf) {
        tidy_data <-
            tidy_data %>%
            sf::st_as_sf(coords = c("lon", "lat")) %>%
            sf::st_set_crs(4326)
    }

    tidy_data
}

aggregate_gridmet <- function(aoi, start_date, end_date = NULL, as_sf = FALSE) {
    p <- progressr::progressor(steps = 3L)

    p("Getting climate data...")

    climate_data <- climateR::getGridMET(
        AOI       = aoi,
        param     = climateR::param_meta$gridmet$common.name[-c(11, 12, 14)],
        startDate = start_date,
        endDate   = end_date
    )

    fmoist100 <- climateR::getGridMET(
        AOI = aoi,
        param = "fmoist_100",
        startDate = start_date,
        endDate   = end_date
    )

    fmoist1000 <- climateR::getGridMET(
        AOI = aoi,
        param = "fmoist_1000",
        startDate = start_date,
        endDate   = end_date
    )

    p("Tidying climate data...")

    tidy_clim <- tidy_stack(c(climate_data, fmoist100, fmoist1000),
                            as_sf = as_sf)

    p("Tidied!")

    tidy_clim
}