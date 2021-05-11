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

get_sb <- function() {
    AOI::aoi_get(county = "Santa Barbara", state = "CA")
}

get_global <- function() {
    rnaturalearth::ne_countries() %>%
        sf::st_as_sf() %>%
        sf::st_transform("+proj=robin")
}

chandler_bi <- function(rh, t) {
    rh_eq  <- (110 - 1.373 * rh)
    t_eq   <- (10.20 - t)
    rh_exp <- 10 ^ (-0.0142 * rh)
    main   <- ((rh_eq - 0.54 * t_eq) * (124 * rh_exp))

    main / 60
}

kelvin_to_fahrenheit <- function(t) ((t - 273.15) * (9 / 5) + 32)

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

common_params <- function() {
    grid   <- climateR::param_meta$gridmet$common.name
    maca   <- climateR::param_meta$maca$common.name
    common <- which(grid %in% maca)

    grid[common]
}

aggregate_gridmet <- function(aoi, start_date, end_date = NULL, as_sf = FALSE) {
    p <- progressr::progressor(steps = 3L)

    p("Getting GridMET data...")

    climate_data <- climateR::getGridMET(
        AOI       = sf::st_transform(aoi, 4326),
        param     = common_params(),
        startDate = start_date,
        endDate   = end_date
    )

    p("Tidying GridMET data...")

    tidy_clim <-
        tidy_stack(
            c(climate_data),
            as_sf = as_sf
        ) %>%
        dplyr::rename(
            prcp       = tidyselect::contains("prcp"),
            rhmax      = tidyselect::contains("rhmax"),
            rhmin      = tidyselect::contains("rhmin"),
            shum       = tidyselect::contains("shum"),
            srad       = tidyselect::contains("srad"),
            tmin       = tidyselect::contains("tmin"),
            tmax       = tidyselect::contains("tmax"),
        ) %>%
        dplyr::mutate(
            rhavg = (rhmax + rhmin) / 2,
            tavg  = (tmax + tmin) / 2,
            cbi_rhmax_tmax = chandler_bi(rhmax, tmax),
            cbi_rhmin_tmax = chandler_bi(rhmin, tmax),
            cbi_rhavg_tmax = chandler_bi(rhavg, tmax),
            cbi_rhmax_tmin = chandler_bi(rhmax, tmin),
            cbi_rhmin_tmin = chandler_bi(rhmin, tmin),
            cbi_rhavg_tmin = chandler_bi(rhavg, tmin),
            cbi_rhmax_tavg = chandler_bi(rhmax, tavg),
            cbi_rhmin_tavg = chandler_bi(rhmin, tavg),
            cbi_rhavg_tavg = chandler_bi(rhavg, tavg),
            burn_index = (
                cbi_rhmax_tmax + cbi_rhmin_tmax + cbi_rhavg_tmax +
                cbi_rhmax_tmin + cbi_rhmin_tmin + cbi_rhavg_tmin +
                cbi_rhmax_tavg + cbi_rhmin_tavg + cbi_rhavg_tavg
            ) / 9
        ) %>%
        dplyr::select(lat, lon, date, prcp, rhmax, rhmin, shum,
                      srad, tmin, tmax, burn_index)

    p("Tidied!")

    tidy_clim
}

aggregate_maca <- function(aoi, start_date, end_date = NULL, as_sf = FALSE) {
    p <- progressr::progressor(steps = 3L)

    p("Getting MACA data...")

    relative_humidity <- climateR::getMACA(
        AOI       = aoi,
        param     = c("rhmax", "rhmin"),
        startDate = start_date,
        endDate   = end_date,
        model     = "BNU-ESM"
    )

    other_climate <- climateR::getMACA(
        AOI       = aoi,
        param     = common_params()[common_params() %in% c("rhmax", "rhmin")],
        startDate = start_date,
        endDate   = end_date
    )

    climate_data <- c(other_climate, relative_humidity)

    p("Tidying MACA data...")

    tidy_clim <-
        tidy_stack(
            c(climate_data),
            as_sf = as_sf
        ) %>%
        dplyr::rename(
            prcp  = tidyselect::contains("prcp"),
            rhmax = tidyselect::contains("rhmax"),
            rhmin = tidyselect::contains("rhmin"),
            shum  = tidyselect::contains("shum"),
            srad  = tidyselect::contains("srad"),
            tmin  = tidyselect::contains("tmin"),
            tmax  = tidyselect::contains("tmax")
        ) %>%
        dplyr::mutate(
            rhavg = (rhmax + rhmin) / 2,
            tavg  = (tmax + tmin) / 2,
            cbi_rhmax_tmax = chandler_bi(rhmax, tmax),
            cbi_rhmin_tmax = chandler_bi(rhmin, tmax),
            cbi_rhavg_tmax = chandler_bi(rhavg, tmax),
            cbi_rhmax_tmin = chandler_bi(rhmax, tmin),
            cbi_rhmin_tmin = chandler_bi(rhmin, tmin),
            cbi_rhavg_tmin = chandler_bi(rhavg, tmin),
            cbi_rhmax_tavg = chandler_bi(rhmax, tavg),
            cbi_rhmin_tavg = chandler_bi(rhmin, tavg),
            cbi_rhavg_tavg = chandler_bi(rhavg, tavg),
            burn_index = (
                cbi_rhmax_tmax + cbi_rhmin_tmax + cbi_rhavg_tmax +
                cbi_rhmax_tmin + cbi_rhmin_tmin + cbi_rhavg_tmin +
                cbi_rhmax_tavg + cbi_rhmin_tavg + cbi_rhavg_tavg
            ) / 9
        ) %>%
        dplyr::select(lat, lon, date, prcp, rhmax, rhmin, shum,
                      srad, tmin, tmax, burn_index)

    p("Tidied!")

    tidy_clim
}

tidy_to_raster <- function(data, x, y, z) {
    xyz <- data %>%
           dplyr::select({{ x }}, {{ y }}, {{ z }}) %>%
           dplyr::rename(
               x = {{ x }},
               y = {{ y }},
               z = {{ z }}
           )

    raster::rasterFromXYZ(
        xyz = xyz,
        crs = sf::st_crs(4326)$proj4string
    )
}

datename_to_col <- function(string) {
    stringr::str_replace_all(string, "[.]", "-") %>%
        stringr::str_remove("X") %>%
        lubridate::ymd()
}