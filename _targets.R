library(targets)
source("R/functions.R")
options(tidyverse.quiet = TRUE)

tar_option_set(packages = c(
    "tidyverse",
    "cmdstanr",
    "rethinking",
    "phytools",
    "phangorn",
    "patchwork"
))

list(
    # Clean data
    tar_target(d_raw,
     "data_raw/readyfem.csv", format = "file"),
    tar_target(d,
     clean_data(file_data = d_raw)),

    # Establish stan files
    tar_target(stan_file_base,
     "stan/fert_model_base.stan", format = "file"),
     tar_target(stan_file_submode,
     "stan/fert_model_submode.stan", format = "file"),
     tar_target(stan_file_MI,
     "stan/fert_model_MI.stan", format = "file"),
     tar_target(stan_file_linear,
     "stan/fert_model_linear.stan", format = "file"),
     tar_target(stan_file_ordinal,
     "stan/fert_model_ordinal.stan", format = "file"),

    # Compile stan models
    ## Base model with no predictors
    tar_target(model_base,
     cmdstan_model(stan_file_base, stanc_options = list("O1"))),
    ## Submode as pred of pop-level fert
    tar_target(model_submode,
     cmdstan_model(stan_file_submode, stanc_options = list("O1"))),
    ## Market Integration as pred of pop-level fert
    tar_target(model_MI,
     cmdstan_model(stan_file_MI, stanc_options = list("O1"))),
    ## Linear pred of ind-level fert
    tar_target(model_linear,
     cmdstan_model(stan_file_linear, stanc_options = list("O1"))),
    ## Ordinal pred of ind-level fert
    tar_target(model_ordinal,
     cmdstan_model(stan_file_ordinal, stanc_options = list("O1"))),

    # Fit stan models
    tar_target(fit_base,
     stan_fit(model = model_base, data = d, var = "base"))

     #tar_target(fit_MI,
     #stan_fit(model = model_MI, data = d, var = "MI"))

    # Create graphics
)
