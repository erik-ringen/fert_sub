# First, source all files in dir to get bigger named functions
functions <- list.files(
 path = "R",
 pattern = "*.R",
 recursive = TRUE)

sapply(paste0("R/", functions[functions != "functions.R"]), source, .GlobalEnv)
rm(functions)

# Smaller functions
extract_samples <- function(cmdstanrfit, pars) {
  cmdstanrfit$output_files() |>
    rstan::read_stan_csv() |>
    rethinking::extract.samples(pars = pars)
}

monotonic <- function(scale, level) {
  if (is.na(level)) return(0)
  else if (level > 0) return( sum(scale[1:level]) )
  else return(0)
}

poisson_oi_rng <- function(p_oi, lambda, n_sims) {
  n <- length(lambda)
  oi <- rbinom(n = n_sims, 1, p_oi)
  fert <- rpois(n = n_sims, lambda)

  y_hat <- rep(NA, n_sims)
  y_hat[oi == 1] <- 1
  y_hat[oi == 0] <- fert[oi == 0]
  return(y_hat)
}
