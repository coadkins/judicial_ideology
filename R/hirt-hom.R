library(cmdstanr)
library(dplyr)
library(here)
library(MASS)
library(purrr)
library(tidyr)
library(qs)
source(here::here("R", "sim_functions.R"))
# rudimentary model id
# todo: is it possible to use the git commit hash instead?
model_id <- stringi::stri_c(format(Sys.Date(), "%m%d%Y"), sample(100:999, 1))

# simulate data
set.seed(123)

## define constants
n_cohort <- 20
year <- factor(1:n_cohort)
party <- rbinom(n_cohort, size = 1, prob = .5)
n_judge <- n_cohort * 50
n_cases <- n_judge * 50
n_case_types <- 50

construct_gamma <- function(party, year) {
  # construct matrix of gamma parameters consistent with my theory
  # this is onerous beceause year is "one-hot encoded"
  x <- model.matrix(~ party + year + party * year)
  dem_years <- which(party == 0 & seq_along(party) != 1) - 1
  year_cols <- grep("^year\\d+$", colnames(x))
  party_year_cols <- grep("^party:year\\d+$", colnames(x))
  gamma <- matrix(NA, ncol = ncol(x), nrow = 1)
  gamma[, 1] <- 0 # Intercept
  gamma[, 2] <- (-1) # gamma for party
  gamma[, year_cols] <- 0 # gamma for each year (theta1)
  # gamma for each party*year(theta)
  gamma[, party_year_cols[-dem_years]] <- seq(
    from = 0,
    by = -.4, # reps.
    length.out = length(party_year_cols[-dem_years])
  )
  gamma[, party_year_cols[dem_years]] <- seq(
    from = 0,
    by = .4, # dems.
    length.out = length(party_year_cols[dem_years])
  )
  return(gamma)
}

draw_theta_ij_raw <- function(n, party, year, gamma, sigma_theta) {
  # variables predicting mu
  x <- model.matrix(~ 1 + party + year + party * year) # 1xk row vector
  mu <- x %*% t(gamma)
  # draw theta_ij
  out <- rnorm(n, mu, sigma_theta)
  return(out)
}

theta_ij_standardize <- function(theta_vector) {
  # de-mean each theta
  theta_standardized <- (theta_vector - mean(theta_vector)) / sd(theta_vector)
  return(theta_standardized)
}

# draw some gammas and lambdas that match the random covariates
gamma_sim <- construct_gamma(party, year)

sigma_theta <- rlnorm(1, 0, .5)
# vectorize draw_theta_ij() over party and year
theta_raw_list <- Map(
  draw_theta_ij_raw,
  party = party,
  year = year,
  MoreArgs = list(
    n = n_judge / n_cohort,
    gamma = gamma_sim,
    sigma_theta = sigma_theta
  )
)

# "standardize" the raw simulated theta

theta_vector <- do.call(c, theta_raw_list)

# combine the values into a data.frame
theta_df <- theta_vector |>
  as.data.frame() |>
  mutate(
    judge_id = 1:length(theta_vector),
    year = factor(
      rep(1:n_cohort, each = n_judge / n_cohort)
    )
  ) |>
  left_join(data.frame(year = factor(1:n_cohort), party = party), by = "year")

colnames(theta_df) <- c("theta", "judge_id", "year", "party")

# output plot of simulated data
library(ggplot2)
library(patchwork)

theta_plot <- theta_df |>
  ggplot(aes(x = year, y = theta, fill = as.factor(party))) +
  geom_boxplot() +
  scale_fill_manual(values = c("#1696d2", "#db2b27")) +
  theme_minimal() +
  theme(legend.position = "none") +
  ylab("Theta D=1") +
  xlab(NULL) +
  with(theta_df, ylim(min(theta), max(theta))) +
  ggtitle("Simulated Distribution of Theta")

ggsave(
  here("graphics", model_id, "1D_corplot_simple.png"),
  plot = theta_plot,
  create.dir = TRUE
)

## simulate judges and cases
draw_case <- function(thetas, mu_beta, mu_alpha) {
  alpha <- rnorm(1, mu_alpha, 1)
  beta <- rnorm(1, mu_beta, 3)
  linear_func <- alpha + t(beta) * thetas
  link_func <- 1 / (1 + exp(-(linear_func)))
  y_out <- rbinom(prob = link_func, n = 1, size = 1)
  return(y_out)
}

draw_panel <- function(case_id, theta_df, mu_case_df, case_type) {
  panel <- theta_df[sample(1:n_judge, size = 3), ] # 3 judges per panel
  thetas <- as.matrix(panel[, 1]) # 1 thetas per judge
  mu_case_df <- with(mu_case_df, mu_case_df[type == case_type, ]) # 1 case type per panel
  case <- apply(
    thetas,
    MARGIN = 1,
    FUN = draw_case,
    mu_beta = with(mu_case_df, mu_case_df[type == case_type, "mu_beta"]),
    mu_alpha = with(mu_case_df, mu_case_df[type == case_type, "mu_alpha"])
  )
  panel$outcome <- case
  panel$case_id <- case_id
  panel$case_type <- mu_case_df[, "type"]
  return(panel)
}

case_params <- data.frame(
  type = 1:n_case_types,
  mu_beta = rnorm(n_case_types, 0, 2),
  mu_alpha = rnorm(n_case_types, 0, 1)
)

cases_df <- Map(
  draw_panel,
  case_id = 1:n_cases,
  case_type = sample(1:n_case_types, n_cases, replace = TRUE),
  MoreArgs = list(
    theta_df = theta_df,
    mu_case_df = case_params
  )
) |>
  list_rbind()

# save simulated data to use later
results_path <- here("results", model_id)
if (!dir.exists(results_path)) {
  dir.create(results_path, recursive = TRUE)
}
qsave(cases_df, here(results_path, "sim_data_1D.qs"))

# transform data for stan
judge_covariates <- cases_df[!duplicated(cases_df$year), ] |>
  arrange(case_id) |>
  dplyr::select(party, year)

x <- with(judge_covariates, model.matrix(~ 1 + party + year + party * year))

stan_data <- list(
  N = nrow(cases_df),
  N_case_id = length(unique(cases_df$case_id)),
  B = length(unique(cases_df$case_type)),
  N_judge = length(unique(cases_df$judge_id)),
  G = length(unique(cases_df[, "year"])),
  K = ncol(x),
  outcome = with(cases_df, outcome[order(case_id)]),
  ii = with(cases_df, judge_id[order(case_id)]),
  jj = with(cases_df, case_id[order(case_id)]),
  x = x
)

# append additional data that facilities identification in Stan
# by fixing theta values for two judges

lib_judge_idx <- fix_judge_idx(cases_df, "judge_id", "theta", target = -2)
con_judge_idx <- fix_judge_idx(cases_df, "judge_id", "theta", target = 2)

# Because Stan only samples the non-fixed judges, we have to extract the other
# two and relevel judge_ids to use as indices in the sampling loop
cases_df_free <- cases_df |>
  filter(!(judge_id %in% c(lib_judge_idx, con_judge_idx))) |>
  # relevel judge_ids to reflect extracted individuals
  mutate(
    judge_id = judge_id -
      (judge_id > lib_judge_idx) -
      (judge_id > con_judge_idx)
  )

# Append additional data that defines indices that facilitate
# vectorized sampling in Stan

stan_data <- c(
  stan_data,
  gen_group_idx(cases_df_free, "judge_id", "year"),
  gen_group_idx(
    cases_df_free,
    "case_id",
    "case_type",
    names = c("cases_by_type", "type_start", "type_end")
  ),
  lib_judge_idx = lib_judge_idx,
  con_judge_idx = con_judge_idx
)

# compile cmdstanr model
model <- here("stan", "hirt-hom.stan") |>
  cmdstan_model()

# Create initialization function
init_fn <- function() {
  list(
    gamma = rnorm(stan_data$K, 0, 0.5), # Smaller initial values
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
  output_dir = here(results_path),
  init = init_fn
)

fit$draws()
try(fit$sampler_diagnostics(), silent = TRUE)
try(fit$init(), silent = TRUE)
try(fit$profiles(), silent = TRUE)
qsave(fit, here(results_path, "stan_fit_hom.qs"))
