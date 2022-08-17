library(targets)
source("R/functions.R")

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
     
    # Fit stan models
    tar_target(fit_base,
     stan_fit(stan_file_base, d, "base"))

     #tar_target(fit_MI,
     #stan_fit(model = model_MI, data = d, var = "MI"))

    # Create graphics
)
