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
source("R/targets-utils.R")
options(tidyverse.quiet = TRUE)
tar_option_set(
    packages = c("tidyverse", "tidymodels",
                 "spatialsample", "workflowsets")
)


c(
    preprocessing_factory(county = "Santa Barbara", state = "CA"),
    training_factory()
)