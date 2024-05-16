library(targets)
source("R/functions.R")

tar_option_set(packages = c(
    "tidyverse",
    "rethinking",
    "rstan",
    "patchwork",
    "loo",
    "bayesplot",
    "HDInterval",
    "future",
    "furrr",
    "RColorBrewer"
))

list(
    # Clean data
    tar_target(d_raw,
     "data_raw/readyfemtest.csv", format = "file"),
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
    tar_target(stan_file_linear_oi_poisson,
       "stan/linear_oi_poisson.stan", format = "file"),
    tar_target(stan_file_linear_oi_poisson_wealth,
               "stan/linear_oi_poisson_wealth.stan", format = "file"),
    tar_target(stan_file_ordinal_oi_poisson,
        "stan/ordinal_oi_poisson.stan", format = "file"),
    
    # Compare poisson and one-inflated poisson likelihoods
    # tar_target(fit_base_poisson,
    #            stan_fit(stan_file_base_poisson, d, "base")),
    tar_target(fit_base_oi_poisson,
               stan_fit(stan_file_base_oi_poisson, d, "base")),
    
    # Calculate loo to compare likelihoods for base models
    # tar_target(loo_poisson,
    #            loo(fit_base_poisson, cores = 8)
    # ),
    tar_target(loo_oi_poisson,
               loo(fit_base_oi_poisson, cores = 8)
    ),
    # tar_target(loo_compare_base,
    #            list(
    #              elpd_poisson = loo_poisson$estimates,
    #              elpd_oi_poison = loo_oi_poisson$estimates,
    #              elpd_diff = loo_compare(loo_poisson, loo_oi_poisson)
    #              )
    #            ),
    
    # Posterior predictive checks for base model
    # tar_target(ppc_base_poisson,
    #            PPD_check(fit_base_poisson),
    #            ),
    tar_target(ppc_base_oi_poisson,
               PPD_check(fit_base_oi_poisson, d),
    ),
     
    #### Models of pop-level predictors of fertility ####
    ## Market integration
    tar_target(fit_MI_oi_poisson,
     stan_fit(stan_file_MI_oi_poisson, d, "MI")),
    ## Mode of subsistence
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
    tar_target(fit_dietpropmarket_oi_poisson, 
      stan_fit(stan_file_linear_oi_poisson, d, "dietpropmarket")),
    tar_target(fit_occ_oi_poisson,
      stan_fit(stan_file_linear_oi_poisson, d, "occ")),
    tar_target(fit_land_oi_poisson,
      stan_fit(stan_file_linear_oi_poisson, d, "land")),
    # Land model, adjusted for wealth
    tar_target(fit_land_oi_poisson_wealth,
               stan_fit(stan_file_linear_oi_poisson_wealth, d, "land", wealth_adj = T)),
    tar_target(fit_livestock_oi_poisson,
      stan_fit(stan_file_linear_oi_poisson, d, "livestock")),
    # Livestock model, adjusted for wealth
    tar_target(fit_livestock_oi_poisson_wealth,
               stan_fit(stan_file_linear_oi_poisson_wealth, d, "livestock", wealth_adj = T)),
    tar_target(fit_income_oi_poisson,
      stan_fit(stan_file_linear_oi_poisson, d, "income")),
    tar_target(fit_urban_oi_poisson,
      stan_fit(stan_file_ordinal_oi_poisson, d, "urban")),
    tar_target(fit_edu_oi_poisson,
      stan_fit(stan_file_ordinal_oi_poisson, d, "edu")),
    tar_target(fit_contra_oi_poisson,
               stan_fit(stan_file_linear_oi_poisson, d, "contra")),
    
    ### Make plot results ##
    # Pop-level contrasts
    tar_target(base_results_a,
               fig_4_a(fit_base_oi_poisson, d)),
    tar_target(base_results_b,
               fig_4_b(fit_base_oi_poisson, d)),
    tar_target(subsistence_results,
               subsistence_contrasts(fit_submode_oi_poisson, d)),
    tar_target(MI_results,
               MI_contrasts(fit_MI_oi_poisson, d)),
    
    # Individual-level contrasts
    tar_target(propHG_results,
               zscore_contrasts(fit_propHG_oi_poisson, "propHG", d)),
    tar_target(propcult_results,
               zscore_contrasts(fit_propcult_oi_poisson, "propcult", d)),
    tar_target(proplab_results,
               zscore_contrasts(fit_proplab_oi_poisson, "proplab", d)),
    
    tar_target(dietpropfarm_results,
               zscore_contrasts(fit_dietpropfarm_oi_poisson, "dietpropfarm", d)),
    tar_target(dietproplab_results,
               zscore_contrasts(fit_dietproplab_oi_poisson, "dietproplab", d)),
    tar_target(dietpropmarket_results,
               zscore_contrasts(fit_dietpropmarket_oi_poisson, "dietpropmarket", d)),
    
    tar_target(occ_results,
               binary_contrasts(fit_occ_oi_poisson, "occ", d)),
    tar_target(land_results,
               zscore_contrasts(fit_land_oi_poisson, "land", d)),
    tar_target(land_results_wealth,
               zscore_contrasts(fit_land_oi_poisson_wealth, "land", d, wealth_adj = T)),
    tar_target(livestock_results,
               zscore_contrasts(fit_livestock_oi_poisson, "livestock", d)),
    tar_target(livestock_results_wealth,
               zscore_contrasts(fit_livestock_oi_poisson_wealth, "livestock", d, wealth_adj = T)),
    tar_target(income_results,
               zscore_contrasts(fit_income_oi_poisson, "income", d)),
    tar_target(urban_results,
               urban_contrasts(fit_urban_oi_poisson, "urban", d)),
    tar_target(edu_results,
               edu_contrasts(fit_edu_oi_poisson, "edu", d)),
    tar_target(contra_results,
               binary_contrasts(fit_contra_oi_poisson, "contra", d))
)
