preprocessing_factory <- function(...) {
    aoi        <- AOI::aoi_get(...)
    start_date <- "1980-01-01"
    end_date   <- "2021-04-01"

    data_ingest <- list(
        # Get GridMET Data
        tar_target(data_gridmet, aggregate_gridmet(aoi, start_date, end_date))
        # Get MACA Data (Used later)
        #> tar_target(data_maca, aggregate_maca(aoi, start_date, end_date))
    )

    splitting <- list(
        # Split GridMET Data
        tar_target(init_split, rsample::initial_split(data_gridmet)),
        # Get Training Data
        tar_target(init_training, rsample::training(init_split)),
        # Get Testing Data
        tar_target(init_testing, rsample::testing(init_split)),
        # Make Spatial CV Folds
        tar_target(init_cv, make_folds(init_training))
    )

    modeling_setup <- list(
        # Setup Model Recipe
        tar_target(setup_rec, make_recipe(init_training, burn_index)),
        # Setup Grid Levels
        tar_target(setup_lvls, 10),
        # Setup Random Forest Model/Grid
        tar_target(setup_rf, make_model("rand_forest")),
        tar_target(setup_grid_rf, make_grid(setup_lvls, "rand_forest")),
        # Setup K-Nearest Neighbor Model/Grid
        tar_target(setup_knn, make_model("nearest_neighbor")),
        tar_target(setup_grid_knn, make_grid(setup_lvls, "nearest_neighbor")),
        # Setup Geographically Weighted Regression Model/Grid
        tar_target(setup_gwr, make_model("gwr")),
        tar_target(setup_grid_gwr, make_grid(setup_lvls, "gwr"))
    )

    workflow_setup <- list(
        # Make Workflow Set
        tar_target(
            wkflow_base,
            workflowsets::workflow_set(
                preproc = list(
                    default = setup_rec
                ),
                models  = list(
                    rf  = setup_rf,
                    knn = setup_knn,
                    gwr = setup_gwr
                ),
                cross = TRUE
            )
        ),
        # Add Grids
        tar_target(
            wflow_set,
            wkflow_base %>%
                workflowsets::option_add(
                    grid = setup_grid_rf,
                    id = "default_rf"
                ) %>%
                workflowsets::option_add(
                    grid = setup_grid_knn,
                    id = "default_knn"
                ) %>%
                workflowsets::option_add(
                    grid = setup_grid_gwr,
                    id = "default_gwr"
                )
        )
    )

    c(data_ingest, splitting, modeling_setup, workflow_setup)
}

training_factory <- function() {
    resampling <- list(
        # Resample against all 3 models
        tar_target(res_map, make_workflow_map(wflow_set, init_cv)),
        # Get best model
        tar_target(res_best_model, get_best_model(res_map)),
        # Get best parameters
        tar_target(res_best_params, get_best_params(res_map, res_best_model)),
        # Fit final model
        tar_target(
            res_fit,
            make_sets_fit(
                workflowset = res_map,
                workflow_id = res_best_model,
                params      = res_best_params,
                data        = init_training
            )
        )
    )

    validation <- list(
        tar_target(
            model_validation,
            make_validation(
                fit_model   = res_fit,
                testing_set = init_testing,
                outcome     = burn_index
            )
        )
    )

    c(resampling, validation)
}