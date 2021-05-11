upper_north <- c(
    "Del Norte",
    "Siskiyou",
    "Modoc",
    "Humboldt",
    "Trinity",
    "Shasta",
    "Lassen",
    "Mendocino",
    "Tehama",
    "Plumas"
)

central_valley <- c(
    "Fresno",
    "San Benito",
    "Madera",
    "Mariposa",
    "Tuolumne",
    "Merced",
    "Stanislaus",
    "Mono",
    "Inyo",
    "Tulare",
    "Kings"
)

south_coastal <- c(
    "Santa Cruz",
    "Santa Clara",
    "Monterey",
    "San Luis Obispo",
    "Santa Barbara",
    "Ventura"
)

south_inner <- c(
    "San Bernardino",
    "Kern",
    "Los Angeles"
)

lower_south <- c(
    "Orange",
    "Riverside",
    "San Diego",
    "Imperial"
)

cal_regions <- aoi %>%
    dplyr::mutate(
        region = dplyr::case_when(
            name %in% upper_north    ~ "Upper.North",
            name %in% central_valley ~ "Central.Valley",
            name %in% south_coastal  ~ "South.Coastal",
            name %in% south_inner    ~ "South.Inner",
            name %in% lower_south    ~ "Lower.South",
            TRUE                     ~ "Upper.Central"
        )
    )

create_union <- function(region) {
    dplyr::filter(cal_regions, region == !!region) %>%
        sf::st_transform(5070) %>%
        sf::st_union() %>%
        sf::st_as_sf() %>%
        sf::st_transform(4326) %>%
        dplyr::mutate(region = !!region)
}

upper_north.sf    <- create_union("Upper.North")
central_valley.sf <- create_union("Central.Valley")
south_coastal.sf  <- create_union("South.Coastal")
south_inner.sf    <- create_union("South.Inner")
upper_central.sf  <- create_union("Upper.Central")
lower_south.sf    <- create_union("Lower.South")

regions <- dplyr::bind_rows(
    upper_north.sf,
    central_valley.sf,
    south_coastal.sf,
    south_inner.sf,
    upper_central.sf,
    lower_south.sf
) %>%
    sf::st_transform(5070) %>%
    sf::st_convex_hull() %>%
    sf::st_make_valid() %>%
    sf::st_intersection(
        AOI::aoi_get(state = "California") %>%
            sf::st_transform(5070)
    ) %>%
    sf::st_transform(4326) %>%
    dplyr::select(region)

saveRDS(regions, "data/California_Regions.rds")