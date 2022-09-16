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

zaoi_mean <- function(p_nz, p_oi, lambda) {
  return(
    p_nz * (1 - p_oi) * (lambda/(1 - exp(-lambda))) +
      p_nz * p_oi
  )
}

poisson_zaoi_rng <- function(p_nz, p_oi, lambda) {
    nz = rbinom(1, 1, p_nz)
    oi = rbinom(1, 1, p_oi)
    
    if (nz == 0) y_hat = 0

    if (nz == 1 & oi == 1)  y_hat = 1

    if (nz == 1 & oi == 0) {
      T = lambda
      U = runif(1, 0, 1)
      t = -log(1 - U*(1 - exp(-T)))
      T1 = T - t
      
      if (T1 < 0) T1 = 0
      y_hat = rpois(1, T1) + 1
    }
    
    tibble(y_hat)
}
