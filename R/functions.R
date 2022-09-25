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

poisson_oi_rng <- function(p_oi, lambda) {
  n <- length(lambda)
  oi <- rbinom(n, 1, p_oi)

  if (oi == 1) y_hat = 1
  else y_hat = rpois(n, lambda);
  return(y_hat)
}
