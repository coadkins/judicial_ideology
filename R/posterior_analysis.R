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

# reference model date
model_date <- "08092025466"
# source functions.R and utils.R
walk(here::here("R", c("functions.R", "utils.R")), source)

fit_array <- load_posterior_draws(
  path = here::here("results", model_date),
  variables = "mu_theta"
)

# load the simulated data
sim_df <- qs::qread(here("results", model_date, "sim_data_1D.qs"))
# from the simulated data, get
# 1. a data frame matching judges to groups
# 2. a vector of group_ids in order they appear in the data
group_ids <- get_group_ids(sim_df, judge_id, year)
group_order <- unique(group_ids[, "g"])

id_array <- identify_chains(
  post_array = fit_array,
  param_hat = mu_theta[i],
  sign = -1
)

# identify signs for beta

id_array <- identify_chains(
  post_array = id_array,
  param = beta[i],
  sign = -1
)

# check convergence plots
mu_g_trace_plots <- bayesplot::mcmc_trace(
  id_array,
  pars = paste0("mu_theta[", 1:20, "]")
)
ggsave(here("graphics", model_date, "mu_g_trace_plots.png"), create.dir = TRUE)

# check convergence plots
beta_j_trace_plots <- bayesplot::mcmc_trace(
  id_array,
  pars = paste0("beta[", 1:20, "]")
)
ggsave(
  here("graphics", model_date, "beta_j_trace_plots.png"),
  create.dir = TRUE
)

# posterior predictive check
prediction_draws <- spread_draws(fit_array, y_hat[..])[, -c(1:3)] |>
  as.matrix()

## plot predictions by group
post_pred_plot <- ppc_bars_grouped(
  y = sim_df[, "outcome"],
  yrep = prediction_draws,
  group = sim_df[, "year"]
)
## update plot title and save
post_pred_plot <- post_pred_plot +
  ggtitle("PPC Check by Cohort")
ggsave(here("graphics", model_date, "ppc.png"), post_pred_plot)

# Plot model results versus simulated data

validation_plot <- reshape_posterior(id_array, mu_theta[i], group_order) |>
  ggplot(aes(x = id, y = mu_theta)) + # boxplot for "true" theta draws geom_boxplot(data = sim_data, alpha = 0.5) +
  # boxplot for mu_theta
  geom_boxplot(alpha = 0.5, fill = "lightgrey") +
  # use transparency so both sets of box plots are visible
  # boxplot for observed data
  geom_boxplot(
    aes(x = year, y = theta, fill = factor(party), alpha = .5),
    data = sim_df
  ) +
  scale_fill_manual(values = c("#1696d2", "#db2b27")) +
  # boxplot for "true" theta draws
  theme_minimal() +
  theme(legend.position = "none") +
  ylab("Mu Theta") +
  xlab(NULL) +
  ggtitle("Distribution of Theta Estimates by Cohort")

ggsave(here("graphics", model_date, "validation_plot.png"), validation_plot)
