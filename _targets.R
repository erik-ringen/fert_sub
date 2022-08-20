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

    # Compare different poisson likelihoods
    tar_target(stan_file_base_poisson,
     "stan/base_poisson.stan", format = "file"),
    tar_target(stan_file_base_zi_poisson,
     "stan/base_zi_poisson.stan", format = "file"),
    tar_target(stan_file_base_zoi_poisson,
     "stan/base_zoi_poisson.stan", format = "file"),

     # Other models
     tar_target(stan_file_linear,
     "stan/fert_model_linear.stan", format = "file"),
     tar_target(stan_file_ordinal,
     "stan/fert_model_ordinal.stan", format = "file"),
     
    # Fit stan models
    tar_target(fit_base_zi_poisson,
     stan_fit(stan_file_base_poisson, d, "base")),

    tar_target(fit_MI,
     stan_fit(stan_file_MI, d, "MI"))

    # Create graphics
)
