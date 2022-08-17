stan_fit <- function(model_file, data, var){
  
  model <- cmdstan_model(model_file)
  data_list <- stan_data(data, var)
  
    fit <- model$sample(
        data = stan_data(data, var),
        chains = 8,
        parallel_chains = 8,
        refresh = 10,
        iter_warmup = 200,
        iter_sampling = 800,
        init = 0,
        adapt_delta = 0.9
    )

    return(list(
      post = extract_samples(fit),
      diagnostics = fit$diagnostic_summary()
      )
      )
}