library(targets)
# library(parallel)
# library(doParallel)

# cl <- parallel::makeCluster(parallel::detectCores()[1] - 1,
#                             outfile = "")
# 
# doParallel::registerDoParallel(cl)

source("R/data-utils.R")
source("R/model-utils.R")
source("R/targets-utils.R")
options(tidyverse.quiet = TRUE)

# parallel::clusterExport(cl = cl, varlist = list("nrmse_vec", "nrmse"))

tar_option_set(
    packages = c("tidyverse", "tidymodels",
                 "spatialsample", "workflowsets",
                 "timetk", "kknn")
)


c(
    list(
        tar_target(
            data_params,
            list(
                aoi = AOI::aoi_get(county = "Santa Barbara", state = "CA"),
                start_date = "2020-04-01",
                end_date   = "2021-04-01"
            )
        )
    ),
    preprocessing_factory(),
    training_factory()
)