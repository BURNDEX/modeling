tbl_as_Spatial <- function(data, coords) {
    data %>%
        sf::st_as_sf(coords = coords) %>%
        sf::as_Spatial()
}

gwr_tbl <- function(formula, data, coords, ...) {
    GWmodel::gwr.basic(
        formula = formula,
        data = tbl_as_Spatial(data, coords = coords),
        ...
    )
}

gwr_tbl.predict <- function(object, newdata) {
    y_name <- object$fit$GW.arguments$formula %>%
              rlang::as_label() %>%
              stringr::str_split("~") %>%
              `[[`(1) %>%
              `[`(1) %>%
              stringr::str_trim()

    train_data <- object$fit$SDF %>%
                  sf::st_as_sf() %>%
                  dplyr::rename_with(
                      .fn = function(x) {
                          y_name
                      },
                      .cols = y
                  ) %>%
                  sf::as_Spatial()

    GWmodel::gwr.predict(
        formula     = object$fit$GW.arguments$formula,
        predictdata = tbl_as_Spatial(
            newdata,
            coords = rlang::eval_tidy(object$spec$method$fit$args$coords)
        ),
        data = tbl_as_Spatial(
            newdata,
            coords = rlang::eval_tidy(object$spec$method$fit$args$coords)
        ),
        bw       = object$fit$GW.arguments$bw,
        longlat  = object$fit$GW.arguments$longlat,
        kernel   = object$fit$GW.arguments$kernel,
        adaptive = object$fit$GW.arguments$adaptive,
        p        = object$fit$GW.arguments$p,
        theta    = object$fit$GW.arguments$theta
    ) %>%
        `$`(SDF) %>%
        `$`(prediction)
}

# Parsnip definition

parsnip::set_model_engine("linear_reg", "regression", eng = "gwr")
parsnip::set_dependency("linear_reg", eng = "gwr", pkg = "GWmodel")

parsnip::set_fit(
    model = "linear_reg",
    eng = "gwr",
    mode = "regression",
    value = list(
        interface = "formula",
        protect = c("formula", "data"),
        func = c(fun = "gwr_tbl"),
        defaults = list(
            kernel = "gaussian",
            p = 2,
            longlat = TRUE
        )
    )
)

parsnip::set_encoding(
    model = "linear_reg",
    eng = "gwr",
    mode = "regression",
    options = list(
        predictor_indicators = "traditional",
        compute_intercept = TRUE,
        remove_intercept = TRUE,
        allow_sparse_x = FALSE
    )
)

parsnip::set_pred(
    model = "linear_reg",
    eng = "gwr",
    mode = "regression",
    type = "numeric",
    value = list(
        pre = NULL,
        post = NULL,
        func = c(fun = "gwr_tbl.predict"),
        args = list(
            object     = rlang::expr(object),
            newdata    = rlang::expr(new_data)
        )
    )
)

gwr_tst <- linear_reg() %>%
    set_engine("gwr") %>%
    set_args(
        coords = c("longitude", "latitude"),
        bw = 1000
    ) %>%
    fit(price ~ beds + baths + sqft, data = Sacramento[1:500, ])

gwr_pred <- predict(
    gwr_tst,
    new_data = Sacramento[500:900, ]
)