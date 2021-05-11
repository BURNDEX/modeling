resamples   <- list.files("_targets/objects/", pattern = "res_mars_", full.names = TRUE)
validations <- list.files("_targets/objects/", pattern = "model_validation_", full.names = TRUE)

resample_plots <- lapply(
    X = resamples,
    FUN = function(resample) {
        rsm <- qs::qread(resample)

        ggplot2::ggsave(
            filename = paste0("img/mars_validations/", basename(resample), ".png"),
            plot = ggplot2::autoplot(rsm),
            width = 10,
            height = 10,
            dpi = 600
        )
    }
)

metrics <- lapply(
    X = validations,
    FUN = function(rds) {
        region_metrics <- qs::qread(rds)
        tidy_metrics <- dplyr::bind_rows(
            region_metrics$rmse,
            region_metrics$nrmse,
            region_metrics$mae,
            region_metrics$mape,
            region_metrics$rsq
        ) %>%
            dplyr::mutate(model = basename(rds))
    }
) %>%
    dplyr::bind_rows()

