library(bayesplot)
library(cmdstanr)
library(dplyr)
library(ggplot2)
library(here)
library(mirai)
library(patchwork)
library(posterior)
library(purrr)
library(tidybayes)
library(tidyr)
library(qs)

# source functions.R and utils.R
walk(here::here("R", c("functions.R", "utils.R")), source)

# load posterior draws from .qs
fit <- as_cmdstan_fit(
  files = list.files(
    here("results", "results_simplified"),
    pattern = "hirt*",
    full.names = TRUE
  )
)
fit_array <- fit$draws()

# load the simulated data
sim_df <- qs::qread(here("results", "sim_data_1D.qs"))

# group_ids <- 

# identify signs
identified_array <- identify_sign(
  post_array = fit_array,
  param = theta[i],
  sign = -1
)
# check convergence plots
trace_plots <- bayesplot::mcmc_trace(
  identified_array,
  pars = paste0("theta[", 1:20, "]")
)

# identify draws
identified_draws <- identify_draws(identified_array)

# plot simulation results

## load outcomes and covariates
sim_data <- qread(here("results", "results_simplified", "sim_data_1D.qs"))

## trace plots
# subset to only mu_theta draws
mu_theta_vars <- grep("mu_theta", variables(draws), value = TRUE)
mu_theta_draws <- subset_draws(draws, variable = mu_theta_vars)

# trace plots for mu_theta
trace_plots <- mcmc_trace(
  mu_theta_draws,
  pars = vars("mu_theta[1]":"mu_theta[20]")
)
ggsave(here("graphics", "trace_plots_1D_simplified.png"), trace_plots)

# rotation sign permutation
## transform mu_theta draws
mu_theta_draws <- spread_draws(fit, mu_theta[i])

mu_theta_rsp <- mu_theta_draws |>
  group_by(.draw) |>
  mutate(draw_sign = ifelse(mean(mu_theta) > 0, 1, -1)) |>
  ungroup() |>
  mutate(mu_theta_id = mu_theta * draw_sign)

## transform back into mcmc array
cl <- make_cluster(4)
mu_theta_rsp_array <-
  split(mu_theta_rsp, mu_theta_rsp[, ".chain"]) |>
  mirai_map(\(x) {
    tidybayes::unspread_draws(x, mu_theta_id[i], drop_indices = TRUE) |>
      dplyr::select(paste0("mu_theta_id[", 1:20, "]")) |>
      as.matrix()
  })

### trace plots for reordered chains
trace_rsp <- mcmc_trace(mu_theta_rsp_array[])
ggsave(here("graphics", "trace_plots_1D_rsp.png"))

# rhat plots for mu_theta
theta_vars <- variables(draws)[grep("^theta\\[", variables(draws))]
theta_draws <- subset_draws(draws, variable = theta_vars)
rhat(theta_draws)

# boxplots for estimates
## match up covariates
judge_covariates <- sim_data[!duplicated(sim_data[, "year"]), ] |>
  arrange(case_id) |>
  select(party, year) |>
  mutate(i = seq_along(party))

mu_theta_rsp <- mu_theta_rsp |>
  left_join(judge_covariates, by = i)

## ggplot
theta_plot <- mu_theta_rsp |>
  mutate(year = as.factor(year)) |>
  ggplot(aes(x = year, y = theta, fill = as.factor(party))) +
  geom_boxplot() +
  scale_fill_manual(values = c("#1696d2", "#db2b27")) +
  theme_minimal() +
  theme(legend.position = "none") +
  ylab("Theta D=1") +
  xlab(NULL) +
  with(theta_draws, ylim(min(mu_theta_rsp), max(mu_theta_rsp))) +
  ggtitle("Distribution of Theta Estimates by Cohort")

ggsave(here("graphics", "mu_theta_hat_1D_esp.png"), theta_plot)

# compare to estimate from using gamma

## trace plots
# subset to only gamma draws
gamma_vars <- grep("gamma", variables(draws), value = TRUE)
gamma_draws <- subset_draws(draws, variable = gamma_vars)

# trace plots for mu_theta
trace_plots_gamma <- mcmc_trace(
  gamma_draws,
  pars = vars("gamma[1]":"gamma[20]")
)
ggsave(
  here("graphics", "gamma_trace_plots_1D_simplified.png"),
  trace_plots_gamma
)

# rotation sign permutation
## transform mu_theta draws
gamma_draws <- spread_draws(fit, gamma[i])

gamma_rsp <- gamma_draws |>
  group_by(.draw) |>
  mutate(draw_sign = ifelse(mean(gamma) > 0, 1, -1)) |>
  ungroup() |>
  mutate(gamma_id = gamma * draw_sign)

## transform back into mcmc array
cl <- make_cluster(4)
daemons(4)
gamma_rsp_array <-
  split(gamma_rsp, gamma_rsp[, ".chain"]) |>
  mirai_map(\(x) {
    tidybayes::unspread_draws(x, gamma_id[i], drop_indices = TRUE)[] |>
      dplyr::select(paste0("gamma_id[", 1:20, "]")) |>
      as.matrix()
  })

### trace plots for reordered chains
trace_rsp <- mcmc_trace(gamma_rsp_array[])
ggsave(here("graphics", "gamma_trace_plots_1D_rsp.png"))

## figure out the group means using gamma
## group by chain and draw

# boxplots for estimates
## match up covariates
judge_covariates <- sim_data[!duplicated(sim_data[, "year"]), ] |>
  arrange(case_id) |>
  select(party, year) |>
  mutate(i = seq_along(party))

mu_theta_rsp <- mu_theta_rsp |>
  left_join(judge_covariates, by = i)

## ggplot
theta_plot <- mu_theta_rsp |>
  mutate(year = as.factor(year)) |>
  ggplot(aes(x = year, y = theta, fill = as.factor(party))) +
  geom_boxplot() +
  scale_fill_manual(values = c("#1696d2", "#db2b27")) +
  theme_minimal() +
  theme(legend.position = "none") +
  ylab("Theta D=1") +
  xlab(NULL) +
  with(theta_draws, ylim(min(mu_theta_rsp), max(mu_theta_rsp))) +
  ggtitle("Distribution of Theta Estimates by Cohort")

ggsave(here("graphics", "gamma_mu_theta_hat_1D_esp.png"), theta_plot)
# posterior predictive check
prediction_draws <- spread_draws(fit, y_hat[..])[, -c(1:3)] |>
  as.matrix()

## chart by year
post_pred_plot <- ppc_bars_grouped(
  y = sim_data[, "outcome"],
  yrep = prediction_draws,
  group = sim_data[, "year"]
)

post_pred_plot <- post_pred_plot +
  ggtitle("PPC Check by Cohort")

ggsave(here("graphics", "ppc_1D_rsp.png"), post_pred_plot)
