library(targets)
source("R/functions.R")

tar_option_set(packages = c(
    "tidyverse",
    "cmdstanr",
    "rethinking",
    "phytools",
    "phangorn",
    "patchwork",
    "loo",
    "bayesplot"
))

list(
    # Clean data
    tar_target(d_raw,
     "data_raw/readyfem.csv", format = "file"),
    tar_target(d,
     clean_data(file_data = d_raw)),

    # Establish stan files
    tar_target(stan_file_base_poisson,
      "stan/base_poisson.stan", format = "file"),
    tar_target(stan_file_base_oi_poisson,
      "stan/base_oi_poisson.stan", format = "file"),
    tar_target(stan_file_MI_oi_poisson,
      "stan/MI_oi_poisson.stan", format = "file"),
    tar_target(stan_file_submode_oi_poisson,
      "stan/submode_oi_poisson.stan", format = "file"),
     
    # Fit stan models
    tar_target(fit_base_poisson,
     stan_fit(stan_file_base_poisson, d, "base")),
    tar_target(fit_base_oi_poisson,
     stan_fit(stan_file_base_oi_poisson, d, "base")),
    tar_target(fit_MI_oi_poisson,
     stan_fit(stan_file_MI_oi_poisson, d, "MI")),
    tar_target(fit_submode_oi_poisson,
     stan_fit(stan_file_submode_oi_poisson, d, "submode")),
    
    # Calculate loo to compare likelihoods for base models
    tar_target(loo_poisson,
               loo(fit_base_poisson, cores = 8)
               ),
    tar_target(loo_oi_poisson,
               loo(fit_base_oi_poisson, cores = 8)
    )

    # Create graphics
)
