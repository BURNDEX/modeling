make_recipe <- function(template, outcome) {
    recipes::recipe(template) %>%
        recipes::update_role(
            tidyselect::everything(),
            new_role = "predictor"
        ) %>%
        recipes::update_role(
            lon,
            lat,
            date,
            geometry_id,
            new_role = "descriptor"
        ) %>%
        recipes::update_role(
            {{ outcome }},
            new_role = "outcome"
        ) %>%
        timetk::step_timeseries_signature(
            tidyselect::any_of(c("date", "time"))
        ) %>%
        timetk::step_slidify_augment(
            tmax,
            period = c(3, 6, 12, 24),
            .f = mean,
            prefix = "tmax_"
        ) %>%
        timetk::step_slidify_augment(
            tmin,
            period = c(3, 6, 12, 24),
            .f = mean,
            prefix = "tmin_"
        ) %>%
        recipes::step_lag(
            tmax,
            lag = 5,
            prefix = "tmax_lag_"
        ) %>%
        recipes::step_lag(
            tmin,
            lag = 5,
            prefix = "tmin_lag_"
        )
}

make_model <- function(model_type = NULL) {
    if (is.null(model_type)) {
        rlang::abort("No model specified.")
    }

    model_type <- tolower(model_type)

    if (model_type == "prophet") {
        model <-
            modeltime::prophet_boost() %>%
            parsnip::set_args(
                growth                   = "linear",
                changepoint_num          = tune::tune(),
                changepoint_range        = tune::tune(),
                seasonality_yearly       = "auto",
                seasonality_weekly       = "auto",
                seasonality_daily        = "auto",
                season                   = "additive",
                prior_scale_changepoints = tune::tune(),
                prior_scale_seasonality  = tune::tune(),
                prior_scale_holidays     = tune::tune(),
                trees                    = tune::tune(),
                min_n                    = tune::tune(),
                tree_depth               = tune::tune(),
                learn_rate               = tune::tune(),
                loss_reduction           = tune::tune(),
                stop_iter                = 20
            ) %>%
            parsnip::set_engine("prophet_xgboost") %>%
            parsnip::set_mode("regression")
    } else if (model_type == "arima") {
        model <-
            modeltime::arima_boost() %>%
            parsnip::set_args(
                seasonal_period          = "auto",
                non_seasonal_ar          = tune::tune(),
                non_seasonal_differences = tune::tune(),
                non_seasonal_ma          = tune::tune(),
                seasonal_ar              = tune::tune(),
                seasonal_differences     = tune::tune(),
                seasonal_ma              = tune::tune(),
                trees                    = tune::tune(),
                min_n                    = tune::tune(),
                tree_depth               = tune::tune(),
                learn_rate               = tune::tune(),
                loss_reduction           = tune::tune(),
                stop_iter                = 20
            ) %>%
            parsnip::set_engine("arima_xgboost") %>%
            parsnip::set_mode("regression")
    } else if (model_type == "exp") {
        model <-
            modeltime::exp_smoothing() %>%
            parsnip::set_args(
                seasonal_period = "auto",
                error           = "auto",
                trend           = "auto",
                season          = "auto",
                damping         = "auto",
                smooth_level    = tune::tune(),
                smooth_trend    = tune::tune(),
                smooth_seasonal = tune::tune()
            ) %>%
            parsnip::set_engine("ets") %>%
            parsnip::set_mode("regression")
    } else if (model_type == "seasonal") {
        model <-
            modeltime::seasonal_reg() %>%
            parsnip::set_args(
                seasonal_period_1 = "auto",
                seasonal_period_2 = "auto",
                seasonal_period_3 = "auto"
            ) %>%
            parsnip::set_engine("tbats") %>%
            parsnip::set_mode("regression")
    } else {
        model <-
            modeltime::nnetar_reg() %>%
            parsnip::set_args(
                seasonal_period = "auto",
                non_seasonal_ar = tune::tune(),
                seasonal_ar     = tune::tune(),
                hidden_units    = tune::tune(),
                num_networks    = tune::tune(),
                penalty         = tune::tune(),
                epochs          = tune::tune()
            ) %>%
            parsnip::set_engine("nnetar") %>%
            parsnip::set_mode("regression")
    }

    model
}

make_workflow <- function(forecast_recipe, forecast_model) {
    workflows::workflow() %>%
        workflows::add_model(forecast_model) %>%
        workflows::add_recipe(forecast_recipe)
}

make_grid <- function(levels, model_type = NULL) {
    if (is.null(model_type)) {
        rlang::abort("No model specified.")
    }

    model_type <- tolower(model_type)
    if (model_type == "prophet") {
        dials::grid_regular(
            modeltime::changepoint_num(),
            modeltime::changepoint_range(),
            modeltime::prior_scale_changepoints(),
            modeltime::prior_scale_seasonality(),
            modeltime::prior_scale_holidays(),
            dials::trees(),
            dials::min_n(),
            dials::tree_depth(),
            dials::learn_rate(),
            dials::loss_reduction(),
            levels = levels
        )
    } else if (model_type == "arima") {
        dials::grid_regular(
            modeltime::non_seasonal_ar(),
            modeltime::non_seasonal_differences(),
            modeltime::non_seasonal_ma(),
            modeltime::seasonal_ar(),
            modeltime::seasonal_differences(),
            modeltime::seasonal_ma(),
            dials::trees(),
            dials::min_n(),
            dials::tree_depth(),
            dials::learn_rate(),
            dials::loss_reduction(),
            levels = levels
        )
    } else if (model_type == "exp") {
        dials::grid_regular(
            modeltime::smooth_level(),
            modeltime::smooth_trend(),
            modeltime::smooth_seasonal(),
            levels = levels
        )
    } else {
        dials::grid_regular(
            modeltime::non_seasonal_ar(),
            modeltime::seasonal_ar(),
            dials::hidden_units(),
            modeltime::num_networks(),
            dials::penalty(),
            dials::epochs(),
            levels = levels
        )
    }
}

make_folds <- function(data) {
    rsample::sliding_index(
        data = data,
        index = date
    )
    #> rsample::nested_cv(
    #>     data,
    #>     outside = rsample::group_vfold_cv(
    #>         v = 10,
    #>         group = geometry_id
    #>     ),
    #>     inside = rsample::rolling_origin(
    #>         initial = 48,
    #>         lag = 5
    #>     )
    #> )
}

make_resamples <- function(forecast_workflow, resamples, grid) {
    tune::tune_grid(
        forecast_workflow,
        resamples = resamples,
        grid = grid
    )
}

make_fit <- function(forecast_workflow, params, data) {
    tune::finalize_workflow(
        forecast_workflow,
        params
    ) %>%
    fit(data = data)
}