make_recipe <- function(template, outcome) {
    recipes::recipe(template) %>%
        recipes::update_role(
            tidyselect::everything(),
            new_role = "predictor"
        ) %>%
        recipes::update_role(
            lon,
            lat,
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
        dials::grid_random(
            modeltime::changepoint_num(),
            modeltime::changepoint_range(),
            modeltime::prior_scale_changepoints(),
            modeltime::prior_scale_seasonality(),
            modeltime::prior_scale_holidays(),
            dials::trees(range = c(100L, 2000L)),
            dials::min_n(),
            dials::tree_depth(),
            dials::learn_rate(),
            dials::loss_reduction(),
            size = levels
        )
    } else if (model_type == "arima") {
        dials::grid_random(
            modeltime::non_seasonal_ar(),
            modeltime::non_seasonal_differences(),
            modeltime::non_seasonal_ma(),
            modeltime::seasonal_ar(),
            modeltime::seasonal_differences(),
            modeltime::seasonal_ma(),
            dials::trees(range = c(100L, 2000L)),
            dials::min_n(),
            dials::tree_depth(),
            dials::learn_rate(),
            dials::loss_reduction(),
            size = levels
        )
    } else if (model_type == "exp") {
        dials::grid_random(
            modeltime::smooth_level(),
            modeltime::smooth_trend(),
            modeltime::smooth_seasonal(),
            size = levels
        )
    } else {
        dials::grid_random(
            modeltime::non_seasonal_ar(),
            modeltime::seasonal_ar(),
            dials::hidden_units(),
            modeltime::num_networks(),
            dials::penalty(),
            dials::epochs(),
            size = levels
        )
    }
}

make_folds <- function(data) {
    timetk::time_series_cv(data = data,
                           date_var = date,
                           lag = 5)
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

make_mt_table <- function(forecast_workflow) {
    modeltime::modeltime_table(forecast_workflow)
}

make_calibrate <- function(mt_table, new_data) {
    modeltime::modeltime_calibrate(
        mt_table,
        new_data = new_data
    )
}

make_future <- function(new_data, length_out) {
    timetk::future_frame(
        .data = dplyr::group_by(new_data, lat, lon),
        .date_var = date,
        .length_out = length_out
    )
}

make_forecast <- function(mt_table, new_data = NULL, h = NULL) {
    modeltime::modeltime_forecast(
        mt_table,
        new_data = new_data,
        h = h
    )
}

make_accuracy <- function(mt_table, new_data) {
    modeltime::modeltime_accuracy(
        mt_table,
        new_data = new_data
    )
}