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