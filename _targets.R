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
    tar_target(d,
     clean_data(file_data = "data_raw/readyfem.csv")),

    # Compile stan models
    ## Base model with no predictors
    tar_target(model_base,
     cmdstan_model("stan/fert_model_base.stan")),
    ## Submode as pred of pop-level fert
    tar_target(model_submode,
     cmdstan_model("stan/fert_model_submode.stan")),
    ## Market Integration as pred of pop-level fert
    tar_target(model_MI,
     cmdstan_model("stan/fert_model_MI.stan")),
    ## Linear pred of ind-level fert
    tar_target(model_linear,
     cmdstan_model("stan/fert_model_linear.stan")),
    ## Ordinal pred of ind-level fert
    tar_target(model_ordinal,
     cmdstan_model("stan/fert_model_ordinal.stan")),

    # Fit stan models
    tar_target(fit_base,
     stan_fit(model = model_base, data = d, var = "base"))
    #tar_target(fit_submode, stan_fit(model = model_submode, var = "submode")),
    #tar_target(fit_MI, stan_fit(model = model_MI, var = "MI"))

    # Create graphics
)
