library(cmdstanr)
library(dplyr)
library(here)
library(MASS)
library(purrr)
library(tidyr)
library(qs)

# simulate data

set.seed(02139)

## define constants
n_cohort <- 12
party <- rbinom(12, size = 1, prob = .5)
time <- 1:n_cohort
n_judge <- 1200
n_cases <- 100000

## draw theta 2
draw_theta_2 <- function(n, party, time) {
  if (party == 0) {
    y <- rnorm(n, 1 + (1 / n_cohort), .5)
  } else if (party == 1) {
    y <- rnorm(n, -1 + (1 / n_cohort) * time, .5 + (.5 / n_cohort) * time)
  }
  return(y)
}

## construct data frame
names(theta_1) <- as.character(1:12)
theta_1 <- stack(theta_1)

names(theta_2) <- as.character(1:12)
theta_2 <- stack(theta_2)

theta_df <- data.frame(
  year = theta_1$ind,
  theta_1 = theta_1$values,
  theta_2 = theta_2$values
)

## output plot of simulated data
par(mfrow = c(1, 2)) # two panels per plot
plot(
  theta_df$year,
  theta_df$theta_1,
  type = "p",
  col = "blue",
  main = "Theta_1 No Drift Over Time",
  cex.main = 1
)
plot(
  theta_df$year,
  theta_df$theta_2,
  type = "p",
  col = "blue",
  main = "Theta_2 Drifts Right Over Time",
  cex.main = 1
)

### add judge ids
theta_df["judge_id"] <- seq_len(dim(theta_df)[1])
### add back in party
theta_df <- theta_df |>
  left_join(data.frame(year = factor(time), party = party), by = "year")

## simulate judges and cases
draw_case <- function(theta_1, theta_2) {
  case_type <- rbinom(n = 1, size = 1, prob = .5)
  alpha <- rnorm(1, 0, 1)
  beta <- rnorm(1, 0, 1)
  if (case_type == 1) {
    linear_func <- alpha + beta * theta_1
  } else if (case_type == 0) {
    linear_func <- alpha + beta * theta_2
  }
  link_func <- 1 / (1 + exp(-(linear_func)))

  y_out <- Map(rbinom, prob = link_func, MoreArgs = list(n = 1, size = 1))
  y_out <- unlist(y_out)
  return(list(y_out, case_type))
}

draw_panel <- function(case_id) {
  panel <- theta_df[sample(1:n_judge, size = 3), ]
  case <- draw_case(panel$theta_1, panel$theta_2)
  panel$outcome <- case[[1]]
  panel$type <- case[[2]]
  panel$case_id <- case_id
  return(panel)
}

cases_df <- Map(draw_panel, case_id = 1:n_cases) |>
  list_rbind()

# transform data for stan
judge_covariates <- cases_df[!duplicated(cases_df$judge_id), ]
x <- cbind(1, judge_covariates[, "party"], judge_covariates[, "year"])
z <- cbind(1, judge_covariates[, "year"])


stan_data <- list(
  N = dim(cases_df)[1],
  N_case_id = length(unique(cases_df$case_id)),
  N_judge = length(unique(cases_df$judge_id)),
  outcome = cases_df$outcome,
  judge = unique(cases_df$judge_id),
  ii = cases_df$judge_id[order(cases_df$case_id)],
  jj = cases_df$case_id[order(cases_df$case_id)],
  x = x,
  z = z
)

# fit cmdstanr model
model <- here("stan", "hirt.stan") |>
  cmdstan_model()

fit <- model$sample(
  data = stan_data,
  seed = 123,
  chains = 4,
  parallel_chains = 4,
  refresh = 100,
  output_dir = here("results")
)

qs::qsave(x = fit, file = "stan_fit.qs")

fit$draws()
try(fit$sampler_diagnostics(), silent = TRUE)
try(fit$init(), silent = TRUE)
try(fit$profiles(), silent = TRUE)
qs::qsave(x = fit, file = "stan_fit.qs")
qsave(fit, here("results", "stan_fit.qs"))
