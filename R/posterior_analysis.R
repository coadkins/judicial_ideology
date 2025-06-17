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
  group_by(.draw) |>
  mutate(draw_sign = ifelse(mean(mu_theta) > 0, 1, -1)) |>
  ungroup() |>
  mutate(mu_theta_id = mu_theta * draw_sign)

## transform back into mcmc array
mu_theta_rsp_array <- 
  split(mu_theta_rsp, mu_theta_rsp[, ".chain"]) |>
  purrr::map(\(x) tidybayes::unspread_draws(x, mu_theta_id[i], drop_indices = TRUE) |>
  select(paste0("mu_theta_id[", 1:20, "]")) |>
  as.matrix())

### trace plots for reordered chains
trace_rsp <- mcmc_trace(mu_theta_rsp_array)
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

ggsave(here("graphics", "mu_theta_hat_1D_rsp.png"), theta_plot)

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
