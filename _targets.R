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
    "bayesplot",
    "HDInterval",
    "future",
    "furrr",
    "RColor"
))

list(
    # Clean data
    tar_target(d_raw,
     "data_raw/readyfem.csv", format = "file"),
    tar_target(d,
     clean_data(file_data = d_raw)),
    
    # Establish color schemes
    tar_target(subsistence_cols,
               {x <- c("#E69F00", "#0072B2", "#009E73", "#D55E00", "#CC79A7", "indianred", "black")
               names(x) <- c("Ag", "fish", "HG", "hort", "labour", "past", "Average")}),
    
    tar_target(col_scale, scale_color_manual(name = "subtype", values = subsistence_cols)),
    tar_target(fill_scale, scale_fill_manual(name = "subtype", values = subsistence_cols)),

    # Establish stan files
    tar_target(stan_file_base_poisson,
      "stan/base_poisson.stan", format = "file"),
    tar_target(stan_file_base_oi_poisson,
      "stan/base_oi_poisson.stan", format = "file"),
    tar_target(stan_file_MI_oi_poisson,
      "stan/MI_oi_poisson.stan", format = "file"),
    tar_target(stan_file_submode_oi_poisson,
      "stan/submode_oi_poisson.stan", format = "file"),
    tar_target(stan_file_linear_oi_poisson,
       "stan/linear_oi_poisson.stan", format = "file"),
    tar_target(stan_file_ordinal_oi_poisson,
        "stan/ordinal_oi_poisson.stan", format = "file"),
    
    # Compare poisson and one-inflated poisson likelihoods
    tar_target(fit_base_poisson,
               stan_fit(stan_file_base_poisson, d, "base")),
    tar_target(fit_base_oi_poisson,
               stan_fit(stan_file_base_oi_poisson, d, "base")),
    
    # Calculate loo to compare likelihoods for base models
    tar_target(loo_poisson,
               loo(fit_base_poisson, cores = 8)
    ),
    tar_target(loo_oi_poisson,
               loo(fit_base_oi_poisson, cores = 8)
    ),
    tar_target(loo_compare_base,
               list(
                 elpd_poisson = loo_poisson$estimates,
                 elpd_oi_poison = loo_oi_poisson$estimates,
                 elpd_diff = loo_compare(loo_poisson, loo_oi_poisson)
                 )
               ),
    # Posterior predictive checks for base model
    tar_target(ppc_base_poisson,
               PPD_check(fit_base_poisson),
               ),
    tar_target(ppc_base_oi_poisson,
               PPD_check(fit_base_oi_poisson),
    ),
     
    # Models of pop-level predictors of fertility
    tar_target(fit_MI_oi_poisson,
     stan_fit(stan_file_MI_oi_poisson, d, "MI")),
    tar_target(fit_submode_oi_poisson,
     stan_fit(stan_file_submode_oi_poisson, d, "submode")),

    # Models for individual-level predictors of fertility
    tar_target(fit_propHG_oi_poisson,
      stan_fit(stan_file_linear_oi_poisson, d, "propHG")),
    tar_target(fit_propcult_oi_poisson,
      stan_fit(stan_file_linear_oi_poisson, d, "propcult")),
    tar_target(fit_proplab_oi_poisson,
      stan_fit(stan_file_linear_oi_poisson, d, "proplab")),
    tar_target(fit_dietpropfarm_oi_poisson,
      stan_fit(stan_file_linear_oi_poisson, d, "dietpropfarm")),
    tar_target(fit_dietproplab_oi_poisson,
      stan_fit(stan_file_linear_oi_poisson, d, "dietproplab")),
    tar_target(fit_occ_oi_poisson,
      stan_fit(stan_file_linear_oi_poisson, d, "occ")),
    tar_target(fit_land_oi_poisson,
      stan_fit(stan_file_linear_oi_poisson, d, "land")),
    tar_target(fit_livestock_oi_poisson,
      stan_fit(stan_file_linear_oi_poisson, d, "livestock")),
    tar_target(fit_income_oi_poisson,
      stan_fit(stan_file_linear_oi_poisson, d, "income")),
    tar_target(fit_urban_oi_poisson,
      stan_fit(stan_file_ordinal_oi_poisson, d, "urban")),
    tar_target(fit_edu_oi_poisson,
      stan_fit(stan_file_ordinal_oi_poisson, d, "edu"))
    
    # Make plot results
    
)
