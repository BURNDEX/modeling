library(tidymodels)
library(stacks)

models <- list.files("_targets/objects/", pattern = "res_mars_")

# Create data stack
mars_data_stack <- stacks::stacks()
for (model in models) {
    mars_data_stack <-
        mars_data_stack %>%
        stacks::add_candidates(
            qs::qread(model),
            name = stringr::str_split_fixed(basename(model), "_", n = 3)[3]
        )
}

# Blend predictions
mars_model <- mars_data_stack %>%
              stacks::blend_predictions()

ggplot2::ggsave(
    filename = "img/mars_validations/ensemble.png",
    plot = ggplot2::autoplot(mars_model),
    width = 10,
    height = 10,
    dpi = 600
)

#> cl <- parallel::makeCluster(4, outfile = "")
#> doParallel::registerDoParallel(cl)
#> 
#> mars_model <- stacks::fit_members(mars_model)
#> 
#> parallel::stopCluster(cl)
#> 
#> test_data <- list.files("_targets/objects/", pattern = "init_testing_", full.names = TRUE) %>%
#>              lapply(FUN = qs::qread) %>%
#>              dplyr::bind_rows()
#> 
#> test_data <-
#>     test_data %>%
#>     dplyr::bind_cols(predict(mars_model, .))
#> 
#> ggplot2::ggsave(
#>     filename = "img/mars_validations/ensemble_validation.png",
#>     plot = ggplot2::ggplot(test_data, aes(x = latency, y = .pred)) +
#>            geom_point() +
#>            coord_obs_pred(),
#>     width = 10,
#>     height = 10,
#>     dpi = 600
#> )