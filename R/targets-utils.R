preprocessing_factory <- function() {

    modeling_setup <- list(
        # Setup Model Recipe
        tar_target(setup_rec, make_recipe(template, burn_index)),
        # Setup Grid Levels
        tar_target(setup_lvls, 3),
        # Setup Random Forest Model/Grid
        tar_target(setup_rf, make_model("rand_forest")),
        tar_target(setup_grid_rf, make_grid(setup_lvls, "rand_forest")),
        # Setup K-Nearest Neighbor Model/Grid
        tar_target(setup_mars, make_model("mars")),
        tar_target(setup_grid_mars, make_grid(setup_lvls, "mars")) #,
        # Setup Geographically Weighted Regression Model/Grid
        # tar_target(setup_gwr, make_model("gwr")),
        # tar_target(setup_grid_gwr, make_grid(setup_lvls, "gwr"))
    )

    #> workflow_setup <- list(
    #>     # Make Workflow Set
    #>     tar_target(
    #>         wkflow_base,
    #>         workflowsets::workflow_set(
    #>             preproc = list(
    #>                 default = setup_rec
    #>             ),
    #>             models  = list(
    #>                 rf  = setup_rf,
    #>                 knn = setup_knn# ,
    #>                 # gwr = setup_gwr
    #>             ),
    #>             cross = TRUE
    #>         )
    #>     ),
    #>     # Add Grids
    #>     tar_target(
    #>         wflow_set,
    #>         wkflow_base %>%
    #>             workflowsets::option_add(
    #>                 grid = setup_grid_rf,
    #>                 id = "default_rf"
    #>             ) %>%
    #>             workflowsets::option_add(
    #>                 grid = setup_grid_knn,
    #>                 id = "default_knn"
    #>             ) # %>%
    #>             # workflowsets::option_add(
    #>             #     grid = setup_grid_gwr,
    #>             #     id = "default_gwr"
    #>             # )
    #>     )
    #> )

    workflow_setup <- list(
        tar_target(
            workflow_rf,
            workflows::workflow() %>%
                workflows::add_model(setup_rf) %>%
                workflows::add_recipe(setup_rec)
        ),
        tar_target(
            workflow_mars,
            workflows::workflow() %>%
                workflows::add_model(setup_mars) %>%
                workflows::add_recipe(setup_rec)
        )
    )

    c(modeling_setup, workflow_setup)
}

training_factory <- function() {
    #> resampling <- list(
    #>     # Resample against all 3 models
    #>     tar_target(res_map, make_workflow_map(wflow_set, init_cv)),
    #>     # Get best model
    #>     tar_target(res_best_model, get_best_model(res_map)),
    #>     # Get best parameters
    #>     tar_target(res_best_params, get_best_params(res_map, res_best_model)),
    #>     # Fit final model
    #>     tar_target(
    #>         res_fit,
    #>         make_sets_fit(
    #>             workflowset = res_map,
    #>             workflow_id = res_best_model,
    #>             params      = res_best_params,
    #>             data        = init_training
    #>         )
    #>     )
    #> )

    data_ingest <- list(
        # Get GridMET Data
        tar_target(
            data_gridmet,
            if (force_structure) {
                fst::read_fst(
                    paste0(
                        "/mnt/x/california/",
                        region_year,
                        ".fst"
                    )
                ) %>%
                    tibble::as_tibble() %>%
                    dplyr::slice_sample(n = 500000)
            }
        )
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

    resampling <- list(
        tar_target(res_mars, make_resamples(workflow_mars, init_cv, setup_grid_mars)),
        tar_target(best_mars, tune::select_best(res_mars, "rmse")),
        tar_target(
            fit_mars,
            make_fit(workflow_mars, best_mars, init_training)
        )#,
        #> tar_target(res_rf, make_resamples(workflow_rf, init_cv, setup_grid_rf)),
        #> tar_target(res_knn, make_resamples(workflow_knn, init_cv, setup_grid_knn)),
        #> tar_target(best_rf, tune::select_best(res_rf, "rmse")),
        #> tar_target(best_knn, tune::select_best(res_knn, "rmse")),
        #> tar_target(
        #>     fit_rf,
        #>     make_fit(workflow_rf, best_rf, init_training)
        #> ),
        #> tar_target(
        #>     fit_knn,
        #>     make_fit(workflow_knn, best_knn, init_training)
        #> )
    )

    validation <- list(
        tar_target(
            model_validation,
            make_validation(
                fit_model   = fit_mars,
                testing_set = init_testing,
                outcome     = burn_index
            )
        ) #,
        # tar_target(
        #     model_validation,
        #     make_validation(
        #         fit_model   = fit_knn,
        #         testing_set = init_testing,
        #         outcome     = burn_index
        #     )
        # )
    )

    c(data_ingest, splitting, resampling, validation)
}