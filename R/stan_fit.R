stan_fit <- function(model_file, data, var, wealth_adj = F) {

  model <- rstan::stan_model(model_file)
  data_list <- stan_data(dat = data, variable = var, wealth_adj = wealth_adj)$data_list

    fit <- sampling(
        model,
        data = data_list,
        chains = 8,
        cores = 8,
        refresh = 10,
        iter = 1500,
        warmup = 250,
        # specify parameters not to save
        pars = c("alpha", "b", "k", "oi", "pop_BY_z", "pid_z", "pop_z"),
        include = F,
        control = list(adapt_delta = 0.98),
        init = "0"
    )
    
    return(fit)
}
