stan_fit <- function(model_file, data, var) {

  model <- cmdstan_model(model_file, stanc_options = list("01"))
  data_list <- stan_data(data, var)

    fit <- model$sample(
        data = stan_data(data, var),
        chains = 8,
        parallel_chains = 8,
        refresh = 10,
        iter_warmup = 100,
        iter_sampling = 100,
        init = 0.1,
        adapt_delta = 0.98
    )

    return(list(
      post = extract_samples(fit),
      diagnostics = fit$diagnostic_summary()
      )
      )
}