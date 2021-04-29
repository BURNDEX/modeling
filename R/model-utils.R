# ==========================================================
# Custom Metric: NRMSE with mean norm
# ==========================================================
nrmse_vec <- function(truth, estimate, na_rm = TRUE, ...) {
    nrmse_impl <- function(truth, estimate) {
        mse_val   <- mean((truth - estimate) ^ 2)
        rmse_val  <- sqrt(mse_val)
        nrmse_val <- abs(100 * rmse_val / mean(truth))

        nrmse_val
    }

    yardstick::metric_vec_template(
        metric_impl = nrmse_impl,
        truth = truth,
        estimate = estimate,
        na_rm = na_rm,
        cls = "numeric",
        ...
    )
}

nrmse <- function(data, truth, estimate, na_rm = TRUE, ...) {

    yardstick::metric_summarizer(
        metric_nm = "nrmse",
        metric_fn = nrmse_vec,
        data = data,
        truth = !! rlang::enquo(truth),
        estimate = !! rlang::enquo(estimate),
        na_rm = na_rm,
        ...
    )

}

nrmse <- yardstick::new_numeric_metric(nrmse, direction = "minimize")
# ==========================================================

make_recipe <- function(template, outcome) {
    if (!deparse(substitute(outcome)) %in% names(template)) {
        rlang::abort("Given template does not have the outcome column.")
    }

    if (!all(common_params() %in% names(template))) {
        rlang::abort("Given template does not contain the required predictors.")
    }

    recipes::recipe(template) %>%
        recipes::update_role(
            tidyselect::everything(),
            new_role = "predictor"
        ) %>%
        recipes::update_role(
            {{ outcome }},
            new_role = "outcome"
        ) %>%
        recipes::update_role(
            lat,
            lon,
            new_role = "geometry"
        ) %>%
        timetk::step_timeseries_signature(date) %>%
        recipes::step_mutate(prcp = prcp + 1) %>%
        recipes::step_log(prcp)
}

# TODO Geographically weighted regression
make_model <- function(
    model_type = c("rand_forest", "nearest_neighbor",
                   "neural_network", "gwr")
) {
    model_type <- match.arg(model_type)

    if (is.null(model_type)) {
        rlang::abort("No model specified.")
    }

    model_type <- tolower(model_type)

    model <- switch(
        model_type,
        "rand_forest" = parsnip::rand_forest(
            mtry  = tune::tune(),
            trees = tune::tune(),
            min_n = tune::tune()
        ),
        "nearest_neighbor" = parsnip::nearest_neighbor(
            neighbors   = tune::tune(),
            weight_func = tune::tune(),
            dist_power  = tune::tune()
        ),
        "neural_network" = parsnip::mlp(
            hidden_units = tune::tune(),
            penalty      = tune::tune(),
            epochs       = tune::tune()
        ),
        "gwr" = NULL
    )

    model_eng <- switch(
        model_type,
        "rand_forest"      = "randomForest",
        "nearest_neighbor" = "kknn",
        "neural_network"   = "nnet",
        "gwr"              = NULL
    )

    parsnip::set_mode(model, "regression") %>%
        parsnip::set_engine(model_eng)
}

make_workflow <- function(rec, model) {
    workflows::workflow() %>%
        workflows::add_model(model) %>%
        workflows::add_recipe(rec)
}

make_grid <- function(
    levels,
    model_type = c("rand_forest", "nearest_neighbor",
                   "neural_network", "gwr")
) {
    if (is.null(model_type)) {
        rlang::abort("No model specified.")
    }

    model_type <- tolower(model_type)

    grid <- switch(
        model_type,
        "rand_forest" = dials::grid_regular(
            dials::mtry(range = c(1L, 15L)),
            dials::trees(),
            dials::min_n(),
            levels = levels
        ),
        "nearest_neighbor" = dials::grid_regular(
            dials::neighbors(),
            dials::weight_func(),
            dials::dist_power(),
            levels = levels
        ),
        "neural_network" = dials::grid_regular(
            dials::hidden_units(),
            dials::penalty(),
            dials::epochs(),
            levels = levels
        ),
        "gwr" = NULL
    )

    grid
}

make_folds <- function(data, nested = FALSE) {
    if (nested) {
        rsample::nested_cv(
            data = data,
            outside = rsample::sliding_index(
                index = date
            ),
            inside = spatialsample::spatial_clustering_cv(
                coords = c(lat, lon),
                v = 10
            )
        )
    } else {
        spatialsample::spatial_clustering_cv(
            data = data,
            coords = c(lat, lon),
            v = 10
        )
    }
}

make_resamples <- function(wkflow, resamples, grid) {
    tune::tune_grid(
        wkflow,
        resamples = resamples,
        grid = grid
    )
}

make_workflow_map <- function(workflowset, resamples) {
    workflowsets::workflow_map(
        workflowset,
        resamples = resamples,
        metrics = yardstick::metric_set(
            rmse,
            #> nrmse, #* Custom metric, see above
            mae,
            mape,
            rsq
        ),
        verbose = TRUE
    )
}

get_best_model <- function(workflowset) {
    workflowsets::rank_results(
        workflowset,
        rank_metric = "rmse",
        select_best = TRUE
    )$wflow_id
}

get_best_params <- function(workflowset, workflow_id) {
    workflowset %>%
        workflowsets::pull_workflow_set_result(workflow_id) %>%
        tune::select_best("rmse")
}


make_sets_fit <- function(workflowset, workflow_id, params, data) {
    workflowset %>%
        workflowsets::pull_workflow(workflow_id) %>%
        make_fit(params = params, data = data)
}

make_fit <- function(wkflow, params, data) {
    tune::finalize_workflow(
        x = wkflow,
        parameters = params
    ) %>%
    fit(data = data)
}

make_validation <- function(fit_model, testing_set, outcome) {
    estimate <- predict(fit_model, new_data = testing_set)
    truth    <- dplyr::select(testing_set, {{ outcome }})
    combined <- dplyr::bind_cols(truth, estimate) %>%
                setNames(c("truth", "estimate")) %>%
                tibble::as_tibble()

    tibble::tibble(
        rmse  = rmse(combined, truth = truth, estimate = estimate),
        nrmse = nrmse(combined, truth = truth, estimate = estimate),
        mae   = mae(combined, truth = truth, estimate = estimate),
        mape  = mape(combined, truth = truth, estimate = estimate),
        rsq   = rsq(combined, truth = truth, estimate = estimate)
    )
}