library(targets)
library(future)
plan(multisession)
source("R/data-utils.R")
source("R/model-utils.R")
options(tidyverse.quiet = TRUE)
tar_option_set(packages = c("tidyverse", "tidymodels",
                            "modeltime", "timetk"))


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
                geometry_id = dplyr::cur_group_id()
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
            dplyr::arrange(date)
    ),
    # Get Testing Set
    tar_target(
        testing_set,
        rsample::testing(data_split) %>%
            dplyr::arrange(date)
    ),
    # Make Recipe
    tar_target(
        forecast_recipe,
        make_recipe(data_with_id, tmax)
    ),
    # Get Model Type
    tar_target(
        forecast_type,
        "exp"
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
        make_grid(2, model_type = forecast_type)
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
    )
)