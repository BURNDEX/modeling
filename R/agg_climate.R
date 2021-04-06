aggregate_temp <- function(aoi, start_date, end_date = NULL) {
    p <- progressr::progressor(steps = 10L)

    p("Gathering temperature data...", amount = 0)

    params <- c("tmax", "tmin")
    temperature_data <- list(
        climateR::getGridMET(
            aoi,
            params,
            startDate = start_date,
            endDate = end_date
        ),
        climateR::getDaymet(
            aoi,
            params,
            startDate = start_date,
            endDate = end_date
        ),
        climateR::getTopoWX(
            aoi,
            params,
            startDate = start_date,
            endDate = end_date
        ),
        climateR::getPRISM(
            aoi,
            params,
            startDate = start_date,
            endDate = end_date
        ),
        climateR::getMACA(
            aoi,
            params,
            startDate = start_date,
            endDate = end_date
        ),
        climateR::getLOCA(
            aoi,
            params,
            startDate = start_date,
            endDate = end_date
        ),
        climateR::getBCCA(
            aoi,
            params,
            startDate = start_date,
            endDate = end_date
        ),
        climateR::getTerraClim(
            aoi,
            params,
            startDate = start_date,
            endDate = end_date
        )
    )

    p("Processing temperature data...")

    tmax <- raster::stack(temperature_data[[1]]$tmax)
    tmin <- raster::stack(temperature_data[[1]]$tmin)

    for (current_iter in 2:length(temperature_data)) {

        p(paste0(current_iter, "/", length(temperature_data),
                 " raster stacks processed..."))

        if (current_iter %in% c(2:4, 8)) {
            temp$tmax <- raster::calc(temp$tmax, fun = function(x) x + 273.15)
            temp$tmin <- raster::calc(temp$tmin, fun = function(x) x + 273.15)
        }

        tmax <- raster::stack(
            temp$tmax,
            tmax
        )

        tmin <- raster::stack(
            temp$tmin,
            tmin
        )
    }

    p("Finished!")

    list(
        tmax = tmax,
        tmin = tmin
    )
}