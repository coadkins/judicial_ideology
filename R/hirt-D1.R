library(cmdstanr)
library(dplyr)
library(here)
library(MASS)
library(purrr)
library(tidyr)
library(qs)

# simulate data
set.seed(123)

## define constants
n_cohort <- 20 
year <- factor(1:n_cohort)
party <- rbinom(n_cohort, size = 1, prob = .5)
n_judge <- n_cohort * 10
n_cases <- n_judge * 30 

construct_gamma <- function(party, year) {
  # construct matrix of gamma parameters consistent with my theory
  # this is onerous beceause year is "one-hot encoded"
  x <- model.matrix(~ party + year + party * year)
  dem_years <- which(party == 0 & seq_along(party) != 1) - 1
  year_cols <- grep("^year\\d+$", colnames(x))
  party_year_cols <- grep("^party:year\\d+$", colnames(x))
  gamma <- matrix(NA, ncol = ncol(x), nrow = 2)
  gamma[, 1] <- 0 # Intercept
  gamma[, 2] <- (-1) # gamma for party
  gamma[1, year_cols] <- 0 # gamma for each year (theta1)
  #gamma for each year (theta2)
  gamma[2, year_cols] <- seq(from = 0, by = -.2, length.out = length(year_cols))
  # gamma for each party*year(theta1)
  gamma[1, party_year_cols[-dem_years]] <- seq(
    from = 0,
    by = -.4, # reps.
    length.out = length(party_year_cols[-dem_years])
  )
  gamma[1, party_year_cols[dem_years]] <- seq(
    from = 0,
    by = .4, # dems.
    length.out = length(party_year_cols[dem_years])
  )
  # gamma for each party*year(theta2)
  gamma[2, party_year_cols[-dem_years]] <- seq(
    from = 0,
    by = -.2, # reps.
    length.out = length(party_year_cols[-dem_years])
  )
  gamma[2, party_year_cols[dem_years]] <- seq(
    from = 0,
    by = -.1, # dems
    length.out = length(party_year_cols[dem_years])
  )
  return(gamma)
}

construct_lambda <- function(party, year) {
  z <- model.matrix(~ 1 + year)
  dem_years <- which(party == 0 & seq_along(party) != 1) # exclude index 1 for "one-hot encoding"
  lambda <- matrix(NA, ncol = ncol(z), nrow = 2)
  lambda[, 1] <- 1 # intercept
  lambda[1, -1] <- 0 # theta 1
  lambda[2, dem_years] <- 0 # theta 2
  lambda[2, -c(dem_years)] <- seq(
    from = 1,
    by = .2,
    length.out = length(lambda[2, -c(dem_years)])
  )
  return(lambda)
}

draw_theta_ij_raw <- function(n, party, year, gamma, lambda) {
  # variables predicting mu
  x <- model.matrix(~ 1 + party + year + party * year) # 1xk row vector
  z <- model.matrix(~ 1 + year) #1xk row vector
  mu <- x %*% t(gamma)
  sigma_theta_chol <- z %*% t(lambda) |> as.vector() |> diag() # this is forcing orthagonality in my
  sigma_theta <- sigma_theta_chol %*% t(sigma_theta_chol)      # simulation
  # draw pairs of theta_ij
  out <- MASS::mvrnorm(n, mu, sigma_theta)
  return(out)
}

theta_ij_whiten <- function(theta_matrix) {
  # de-mean each theta
  theta_demeaned <- apply(theta_matrix, MARGIN = 2, FUN = \(x) x - mean(x))

  # whiten using choleksy decomp. of inverse cov. matrix
  chol_invcov <- cov(theta_demeaned) |>
    solve() |>
    chol()
  theta_whitened <- theta_demeaned %*% t(chol_invcov)
  return(theta_whitened)
}

# draw some gammas and lambdas that match the random covariates
gamma_sim <- construct_gamma(party, year)
lambda_sim <- construct_lambda(party, year)

# vectorize draw_theta_ij() over party and year
theta_raw_list <- Map(
  draw_theta_ij_raw,
  party = party,
  year = year,
  MoreArgs = list(
    n = n_judge / n_cohort,
    gamma = gamma_sim,
    lambda = lambda_sim
  )
)

# "whiten" the raw simulated theta
theta_whitened_matrix <- theta_ij_whiten(do.call(rbind, theta_raw_list))

# combine the values into a data.frame
theta_df <- theta_whitened_matrix |>
  as.data.frame() |>
  mutate(judge_id = 1:nrow(theta_whitened_matrix), 
         year = factor(
                      rep(1:n_cohort, each = n_judge/n_cohort))) |>
  left_join(data.frame(year = factor(1:n_cohort), party = party), by = "year")

colnames(theta_df) <- c("theta_1", "theta_2", "judge_id", "year", "party")

# output plot of simulated data
 library(ggplot2)
 library(patchwork)
 
 theta_1_plot <- theta_df |>
   ggplot(aes(x = year, y = theta_1, fill = as.factor(party))) +
   geom_boxplot() +
   scale_fill_manual(values = c("#1696d2", "#db2b27")) +
   theme_minimal() +
   theme(legend.position = "none") +
   ylab("Theta D=1") +
   xlab(NULL) +
   with(theta_df, ylim(min(theta_2), max(theta_1))) +
   ggtitle("Simulated Distribution of Theta")
 
 theta_2_plot <- theta_df |>
   ggplot(aes(x = year, y = theta_2, fill = as.factor(party))) +
   geom_boxplot() +
   scale_fill_manual(values = c("#1696d2", "#db2b27")) +
   theme_minimal() +
   theme(legend.position = "none") +
   ylab("Theta D=2") +
   xlab("Group Number") +
   with(theta_df, ylim(min(theta_2), max(theta_1)))
 
  out <- theta_1_plot / theta_2_plot
 
  ggsave(here("graphics", "2D_corplot_n.png"), plot = out)

## simulate judges and cases
draw_case <- function(thetas) {
  alpha <- rnorm(1, 0, 1)
  beta <- MASS::mvrnorm(1, c(0, 0), diag(2)) # beta in D dimensions
  linear_func <- alpha + t(beta) %*% thetas
  link_func <- 1 / (1 + exp(-(linear_func)))
  y_out <- rbinom(prob = link_func, n = 1, size = 1)
  return(y_out)
}

draw_panel <- function(case_id, theta_df) {
  panel <- theta_df[sample(1:n_judge, size = 3), ] # 3 judges per panel
  thetas <- as.matrix(panel[, c(2, 3)]) # 2 thetas per judge
  case <- apply(thetas, MARGIN = 1, FUN = draw_case)
  panel$outcome <- case
  panel$case_id <- case_id
  return(panel)
}

cases_df <- Map(
  draw_panel,
  case_id = 1:n_cases,
  MoreArgs = list(theta_df = theta_df)
) |>
  list_rbind()

# save simulated data to use later
qsave(cases_df, here("results", "sim_data_2D.qs"))

# transform data for stan
judge_covariates <- cases_df[!duplicated(cases_df$year), ] |>
  arrange(case_id) |>
  dplyr::select(party, year)


x <- with(judge_covariates, model.matrix(~ 1 + party + year + party * year))
z <- with(judge_covariates, model.matrix(~ 1 + year))

stan_data <- list(
  N = nrow(cases_df),
  N_case_id = length(unique(cases_df$case_id)),
  N_judge = length(unique(cases_df$judge_id)),
  D = sum(grepl("^theta", names(cases_df))),
  G = length(unique(cases_df[, "year"])),
  K = ncol(x),
  J = ncol(z),
  outcome = with(cases_df, outcome[order(case_id)]),
  ii = with(cases_df, judge_id[order(case_id)]),
  jj = with(cases_df, case_id[order(case_id)]),
  group_id = with(cases_df, cases_df[!duplicated(judge_id), ]) |>
              with(data = _, year[order(case_id)]),
  x = x,
  z = z
)

# fit cmdstanr model
model <- here("stan", "hirt_2D.stan") |>
  cmdstan_model()

fit <- model$sample(
  data = stan_data,
  seed = 123,
  chains = 4,
  parallel_chains = 4,
  refresh = 100,
  output_dir = here("results")
)

fit$draws()
try(fit$sampler_diagnostics(), silent = TRUE)
try(fit$init(), silent = TRUE)
try(fit$profiles(), silent = TRUE)
qsave(fit, here("results", "stan_fit.qs"))
