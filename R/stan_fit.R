require(cmdstanr)

stan_fit <- function(model, data, var){
    fit <- model$sample(
        data = stan_data(data, var),
        chains = 4,
        refresh = 10,
        parallel_chains = 4,
        iter_warmup = 50,
        iter_sampling = 50,
        init = 0.1,
        adapt_delta = 0.96
    )

    fit$save_output_files(dir = "stan",
     basename = var,
      timestamp = FALSE,
       random = FALSE)

    return(fit)
}