library(bayesplot)
library(cmdstanr)
library(dplyr)
library(ggplot2)
library(here)
library(mirai)
library(patchwork)
library(posterior)
library(purrr)
library(targets)
library(tidybayes)
library(tidyr)
library(qs)
library(qs2)

# load results from targets pipeline
fit_array <- tar_read("mcmc_mcmc_hirt.hom")$draws()

# examine trace plots
mu_g_trace_plots <- bayesplot::mcmc_trace(
  fit_array,
  pars = paste0("mu_theta[", 1:20, "]")
)

ggsave(here("graphics", model_date, "mu_g_trace_plots.png"), create.dir = TRUE)

# posterior predictive check
prediction_draws <- spread_draws(fit_array, y_hat[..])[, -c(1:3)] |>
  as.matrix()

## plot predictions by group
post_pred_plot <- ppc_bars_grouped(
  y = sim_data$outcome,
  yrep = prediction_draws,
  group = sim_data$
)
## update plot title and save
post_pred_plot <- post_pred_plot +
ggtitle("PPC Check by Cohort")
ggsave(here("graphics", model_date, "ppc.png"), post_pred_plot)

# Coverage Check
# calculate "true" means and arrange in the same
# order as the Stan data
true_group_means <- sim_df |>
  group_by(year) |>
  summarize(mu_theta = mean(theta)) |>
  slice(as.numeric(group_order)) |>
  pull(mu_theta)

# calculate coverage by group
calculate_coverage(
  reshaped_posterior = reshape_posterior(fit_array, mu_theta[i], group_order),
  true_values = true_group_means,
  param_hat = mu_theta_hat,
  prob = .95
)

# Plot model results versus simulated data
validation_plot <- reshape_posterior(fit_array, mu_theta[i], group_order) |>
  ggplot(aes(x = id, y = mu_theta_hat)) +
  # boxplot for mu_theta
  geom_boxplot(alpha = 0.5, fill = "lightgrey") +
  # use transparency so both sets of box plots are visible
  # boxplot for observed data
  geom_boxplot(
    aes(x = year, y = theta, fill = factor(party), alpha = .5),
    data = sim_df,
    coef = 0,
    outlier.shape = NA
  ) +
  scale_fill_manual(values = c("#1696d2", "#db2b27")) +
  # boxplot for "true" theta draws
  theme_minimal() +
  theme(legend.position = "none") +
  ylab("Mu Theta") +
  xlab(NULL) +
  ggtitle("Distribution of Theta Estimates by Cohort")

ggsave(here("graphics", model_date, "validation_plot.png"), validation_plot)
