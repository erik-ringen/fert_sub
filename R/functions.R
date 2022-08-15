# First, source all files in dir to get bigger named functions
functions <- list.files(
 path = "R",
 pattern = "*.R",
 recursive = TRUE)

sapply(paste0("R/", functions[functions != "functions.R"]), source, .GlobalEnv)
rm(functions)

# Smaller functions
extract_samples <- function(cmdstanrfit) {
  cmdstanrfit$output_files() |>
    rstan::read_stan_csv() |>
    rethinking::extract.samples()
}