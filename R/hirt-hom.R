library(cmdstanr)
library(dplyr)
library(here)
library(MASS)
library(purrr)
library(tidyr)
library(qs2)
source(here::here("R", "simulation.R"))

# compile cmdstanr model
model <- here("stan", "hirt-hom.stan") |>
  cmdstan_model()

stan_data <- simulate_data()

# Create initialization function
init_fn <- function() {
  list(
    gamma = rnorm(stan_data$K, -1, 0.5), # Smaller initial values
    sigma_theta = 1, # Conservative sigma
    sigma_beta = 1,
    sigma_alpha = 1,
    alpha_raw = rnorm(stan_data$N_case_id, 0, 0.1),
    theta_raw = rnorm(stan_data$N_judge, 0, 0.1), # Small theta_raw values
    beta_raw = rnorm(stan_data$N_case_id, 0, 0.5)
  )
}

fit <- model$sample(
  data = stan_data,
  seed = 123,
  chains = 4,
  parallel_chains = 4,
  refresh = 100,
  iter_warmup = 20,
  iter_sampling = 20,
  init = init_fn
)
