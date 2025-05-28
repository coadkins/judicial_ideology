library(bayesplot)
library(cmdstanr)
library(dplyr)
library(factor.switching)
library(ggplot2)
library(here)
library(mirai)
library(patchwork)
library(posterior)
library(purrr)
library(tidybayes)
library(tidyr)
library(qs)

# load posterior draws from .qs
fit <- as_cmdstan_fit(
  files = list.files(
    here("results", "results_simplified"),
    pattern = "hirt*",
    full.names = TRUE
  )
)
draws <- fit$draws()

## load outcomes and covariates
sim_data <- qread(here("results", "results_simplified", "sim_data_1D.qs"))

## trace plots
# subset to only mu_theta draws 
mu_theta_vars <- grep("mu_theta", variables(draws), value = TRUE)
mu_theta_draws <- subset_draws(draws, variable = mu_theta_vars)

# trace plots for mu_theta
trace_plots <- mcmc_trace(mu_theta_draws, pars=vars("mu_theta[1]":"mu_theta[20]"))
ggsave(here("graphics","trace_plots_1D_simplified.png"), trace_plots)

# rotation sign permutation
## transform mu_theta draws
mu_theta_draws <- spread_draws(fit, mu_theta[i])

mu_theta_rsp <- mu_theta_draws |>
# group by cohort and then chain
  group_by(i, .chain) |> 
# calculate sign for each group in each chain
  mutate(chain_sign = ifelse(mean(mu_theta) > 0, 1, -1)) |> 
  ungroup(.chain) |>
# calculate sign for each group across all chains
  mutate(group_sign = ifelse(mean(mu_theta) > 0, 1, -1)) |>
  ungroup() |>
# re-sign chains that have different signs than their group 
  mutate(mu_theta = case_when(chain_sign == group_sign ~ mu_theta,
         .default = mu_theta*(-1))) |>
  select(-c("chain_sign", "group_sign"))

## transform back into mcmc array
mu_theta_draws_rsp <- 
  split(mu_theta_rsp, mu_theta_rsp[, ".chain"]) |>
  purrr::map(\(x) tidybayes::unspread_draws(x, mu_theta[i], drop_indices = TRUE) |>
  select(paste0("mu_theta[", 1:20, "]")) |>
  as.matrix())

### trace plots for reordered chains
trace_rsp <- mcmc_trace(mu_theta_draws_rsp)
ggsave(here("graphics", "trace_plots_1D_rsp.png"))

# rhat plots for mu_theta
theta_vars <- variables(draws)[grep("^theta\\[", variables(draws))]
theta_draws <- subset_draws(draws, variable = theta_vars)
rhat(theta_draws)

# boxplots for estimates
## extract all theta draws (switching to tidybayes syntax)
theta_draws <- spread_draws(fit, theta[i])
# rotation sign permutation
## transform theta draws
theta_rsp <- theta_draws |>
  group_by(i, .chain) |>
  mutate(chain_sign = ifelse(mean(theta) > 0, 1, -1)) |>
  ungroup(.chain) |>
  mutate(group_sign = ifelse(mean(theta) > 0, 1, -1)) |>
  ungroup() |>
  mutate(theta = case_when(chain_sign == group_sign ~ theta,
         .default = theta*(-1)))|>
  select(-c("chain_sign", "group_sign"))

## match up covariates
judge_covariates <- sim_data[!duplicated(sim_data[, "judge_id"]), ] |>
  arrange(case_id) |>
  select(party, year) |>
  mutate(i = seq_along(party))

theta_draws <- left_join(theta_draws, judge_covariates, by = "i")

## ggplot
theta_plot <- theta_draws |>
  mutate(year = as.factor(year)) |>
  ggplot(aes(x = year, y = theta, fill = as.factor(party))) +
  geom_boxplot() +
  scale_fill_manual(values = c("#1696d2", "#db2b27")) +
  theme_minimal() +
  theme(legend.position = "none") +
  ylab("Theta D=1") +
  xlab(NULL) +
  with(theta_draws, ylim(min(theta), max(theta))) +
  ggtitle("Distribution of Theta Estimates by Cohort")

ggsave(here("graphics", "theta_hat_1D_rsp.png"), theta_plot)
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
