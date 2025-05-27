library(bayesplot)
library(cmdstanr)
library(dplyr)
library(ggplot2)
library(here)
library(patchwork)
library(posterior)
library(tidyr)
library(qs)

# load posterior draws from .qs
fit <- qs::qread(here("results", "stan_fit.qs"))

draws <- fit$draws()

# subset to only mu_theta draws 
mu_theta_vars <- variables(draws)[grep("mu_theta", variables(draws))]
mu_theta_draws <- subset_draws(draws, variable = mu_theta_vars)

# trace plots for mu_theta
trace_plots <- mcmc_trace(mu_theta_draws, pars=vars("mu_theta[1,1]":"mu_theta[3,1]", 
"mu_theta[1,2]":"mu_theta[3,2]"))
ggsave(here("graphics","trace_plots.png"), trace_plots)

# rhat plots for mu_theta
theta_vars <- variables(draws)[grep("^theta\\[", variables(draws))]
theta_draws <- subset_draws(draws, variable = theta_vars)
rhat(theta_draws)

# scale and rotate beta (not neccessary)
beta_vars <- variables(draws)[grep("^beta\\[", variables(draws))]
mcmc_trace(draws, pars=beta_vars[1:10])

# boxplots for estimates
## load covariates
sim_data <- qread(here("results", "sim_data_2D.qs"))

## extract all theta draws (switching to tidybayes syntax)
theta_draws <- spread_draws(fit, theta[i, ..])

## match up covariates 
judge_covariates <- sim_data[!duplicated(sim_data[,"judge_id"]), ] |>
  arrange(case_id) |>
  select(party, year) |>
  mutate(i = seq_along(party))

theta_draws <- left_join(theta_draws, judge_covariates, 
  by = "i")

## ggplot
theta_1_plot <- theta_draws|>
  mutate(year = as.factor(year)) |>
  ggplot(aes(x = year, y = theta.1, fill = as.factor(party))) +
  geom_boxplot() +
  scale_fill_manual(values = c("#1696d2", "#db2b27")) +
  theme_minimal() +
  theme(legend.position = "none") +
  ylab("Theta D=1") +
  xlab(NULL) +
  with(theta_draws, ylim(min(theta.2), max(theta.1))) +
  ggtitle("Distribution of Theta Estimates by Cohort")

theta_2_plot <- theta_draws |>
  mutate(year = as.factor(year)) |>
  ggplot(aes(x = year, y = theta.2, fill = as.factor(party))) +
  geom_boxplot() +
  scale_fill_manual(values = c("#1696d2", "#db2b27")) +
  theme_minimal() +
  theme(legend.position = "none") +
  ylab("Theta D=2") +
  xlab("Group Number") +
  with(theta_draws, ylim(min(theta.2), max(theta.1)))

theta_out <- theta_1_plot/theta_2_plot
ggsave(here("graphics", "theta_hat.png"), theta_out)
# posterior predictive check
prediction_draws <- spread_draws(fit, y_hat[..])[, -c(1:3)] |>
  as.matrix()

## match up covariates
## chart by year
post_pred_plot <- ppc_bars_grouped(y = sim_data[, "outcome"], 
    yrep = prediction_draws, group = sim_data[, "year"])

post_pred_plot <- post_pred_plot +
  ggtitle("PPC Check by Cohort")

ggsave(here("graphics", "ppc_2D.png"), post_pred_plot)