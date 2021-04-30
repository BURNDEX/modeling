bq_poll <- function() {
    state_names <- state.name[!state.abb %in% c("HI", "PR", "AK")]
    state_abbrs <- state.abb[!state.abb %in% c("HI", "PR", "AK")]
    num_years   <- length(1980L:2020L)

    bq_state_datasets <- lapply(
        X = paste0("gridmet_", tolower(state_abbrs)),
        FUN = bigrquery::bq_dataset,
        project = "burndex"
    )

    dataset_lengths <- dplyr::bind_rows(lapply(
        X = 1:48,
        FUN = function(index) {
            tibble::tibble(
                state  = state_names[index],
                tables = length(
                    bigrquery::bq_dataset_tables(
                        bq_state_datasets[[index]]
                    )
                )
            )
        }
    ))

    counties_per_state <- dplyr::bind_rows(lapply(
        X = state_names,
        FUN = function(state) {
            tibble::tibble(
                state    = !!state,
                counties = nrow(
                    AOI::aoi_get(state = !!state, county = "all")
                )
            )
        }
    )) %>%
        dplyr::mutate(expected_tables = counties * num_years)

    final_tbl <- dplyr::left_join(
        dataset_lengths,
        counties_per_state,
        by = "state"
    ) %>%
        dplyr::mutate(
            percent = tables / expected_tables,
            abb = state_abbrs
        ) %>%
        dplyr::relocate(state, abb)

    cli_width  <- cli::console_width()
    partitions <- cli_width / 6
    dots <- unlist(lapply(
        X = state_abbrs,
        FUN = function(state_abb) {
            pct <- final_tbl$percent[which(state_abbrs == state_abb)]
            paste0(
                rep(".", round(partitions * pct)),
                collapse = ""
            )
        }
    ))

    state_progress <- function(i, j) {
        paste0(
            crayon::bold(state_abbrs[i:j]),
            ": ",
            crayon::green("["),
            crayon::green(stringr::str_pad(
                dots[i:j],
                partitions,
                "right",
                " "
            )),
            crayon::green("] "),
            formatC(
                final_tbl$percent[i:j] * 100,
                digits = 1,
                width = 4,
                format = "f"
            ), "%"
        )
    }

    pgr1 <- state_progress(1, 16)
    pgr2 <- state_progress(17, 32)
    pgr3 <- state_progress(33, 48)

    cat(
        crayon::silver(crayon::bold(
            paste0(
                rep("=", crayon::col_nchar(state_progress(1,1)) * 3 + 8),
                collapse = ""
            )
        ))
    )
    cat("\n")
    cat(
        paste0(
            paste0(
                pgr1,
                crayon::silver(" || "),
                pgr2,
                crayon::silver(" || "),
                pgr3
            ),
            sep = "\n",
            collapse = ""
        )
    )
    cat(
        crayon::silver(crayon::bold(
            paste0(
                rep("=", crayon::col_nchar(state_progress(1, 1)) * 3 + 8),
                collapse = ""
            )
        ))
    )
}