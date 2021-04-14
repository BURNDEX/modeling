library(targets)

library(parallel)
library(doParallel)
cl <- parallel::makeCluster(
    parallel::detectCores()[1] - 1,
    outfile = ""
)
doParallel::registerDoParallel(cl)

source("R/data-utils.R")
source("R/model-utils.R")
options(tidyverse.quiet = TRUE)
tar_option_set(
    packages = c("tidyverse", "tidymodels",
                 "modeltime", "timetk")
)


list(
    # Load Data
    tar_target(
        tmp_data,
        fst::read_fst("data/test_data.fst")
    ),
    # Create Geometry ID
    tar_target(
        data_with_id,
        tmp_data %>%
            dplyr::group_by(lat, lon) %>%
            dplyr::mutate(
                geometry_id = dplyr::cur_group_id(),
                date = lubridate::as_datetime(date)
            ) %>%
            dplyr::ungroup() %>%
            dplyr::relocate(geometry_id) %>%
            dplyr::arrange(date)
    ),
    # Split Data
    tar_target(
        data_split,
        rsample::initial_time_split(data_with_id, lag = 5)
    ),
    # Get Training Set
    tar_target(
        training_set,
        rsample::training(data_split) %>%
            dplyr::arrange(date),
        format = "fst_tbl"
    ),
    # Get Testing Set
    tar_target(
        testing_set,
        rsample::testing(data_split) %>%
            dplyr::arrange(date),
        format = "fst_tbl"
    ),
    # Make Recipe
    tar_target(
        forecast_recipe,
        make_recipe(data_with_id, tmax)
    ),
    # Get Model Type
    tar_target(
        forecast_type,
        "prophet"
    ),
    # Make Model
    tar_target(
        forecast_model,
        make_model(model_type = forecast_type)
    ),
    # Make Workflow
    tar_target(
        forecast_workflow,
        make_workflow(forecast_recipe,
                      forecast_model)
    ),
    # Make Grid
    tar_target(
        forecast_grid,
        make_grid(1, model_type = forecast_type),
        format = "fst_tbl"
    ),
    # Make Folds
    tar_target(
        forecast_folds,
        make_folds(training_set)
    ),
    # Make Resamples
    tar_target(
        forecast_resamples,
        make_resamples(forecast_workflow,
                       forecast_folds,
                       forecast_grid)
    ),
    # Get Best Params
    tar_target(
        forecast_best,
        tune::select_best(forecast_resamples)
    ),
    # Make Fit
    tar_target(
        forecast_fit,
        make_fit(forecast_workflow,
                 forecast_best,
                 training_set)
    ),
    # Make Modeltime Table
    tar_target(
        forecast_table,
        make_mt_table(forecast_fit)
    ),
    # Calibrate MT Table
    tar_target(
        forecast_calibrate,
        make_calibrate(forecast_table, testing_set)
    ),
    # Make Future Frame
    #> tar_target(
    #>     forecast_future,
    #>     make_future(testing_set, "1 year")
    #> ),
    # Make Forecast
    #> tar_target(
    #>     forecast,
    #>     make_forecast(forecast_calibrate, new_data = forecast_future)
    #> ),
    # Check Accuracy
    tar_target(
        forecast_accuracy,
        make_accuracy(forecast_calibrate, testing_test)
    )
)