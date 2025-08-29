# Created by use_targets().
# Follow the comments below to fill in this target script.
# Then follow the manual to check and run the pipeline:
#   https://books.ropensci.org/targets/walkthrough.html#inspect-the-pipeline

# Load packages required to define the pipeline:
library(here)
library(stantargets)
library(stringi)
library(tidybayes)
library(tarchetypes)
library(targets)
library(qs2)
library(paws.storage)

tar_option_set(
  # Use memory more sparingly
  memory = "transient",
  garbage_collection = TRUE,
  repository = "aws",
  repository_meta = "aws",
  format = "qs",
  controller = crew::crew_controller_local(workers = 2),
  resources = tar_resources(
    tar_resources_aws(
      bucket = Sys.getenv("S3_BUCKET"),
      prefix = "ordered_outcome",
      endpoint = Sys.getenv("S3_ENDPOINT"),
      region = Sys.getenv("S3_REGION")
    )
  )
)

# Set target options:
tar_option_set(
  packages = c(
    "bayesplot",
    "cmdstanr",
    "dplyr",
    "ggplot2",
    "posterior",
    "purrr",
    "rlang",
    "stringi",
    "tidybayes",
    "tidyr",
    "qs2"
  )
)

# Run the R scripts in the R/ folder with your custom functions:
tar_source(here("R", "simulation.R"))
tar_source(here("R", "post_processing.R"))
tar_source(here("R", "utils.R"))

# Replace the target list below with your own:
list(
  tar_stan_mcmc(
    name = mcmc,
    stan_files = here("stan", "hirt-hom.stan"),
    data = simulate_data(
      cohort_g = 20,
      judge_gi = 40,
      case_ij = 40,
      types_b = 20
    ),
    chains = 4,
    parallel_chains = 4,
    iter_warmup = 1500,
    iter_sampling = 1500,
    format = "qs",
    format_df = "qs",
    stdout = R.utils::nullfile(),
    stderr = R.utils::nullfile(),
    return_summary = FALSE
  ),
  # format results as a posterior package draws_array
  tar_target(
    mcmc_draws_array,
    mcmc_mcmc_hirt.hom$draws(variables = "mu_theta", format = "draws_array")
  ),
  tar_target(
    outcome_distribution,
    visualize_variation_outcome(
      mcmc_data[["outcome"]],
      mcmc_data[[c(".join_data", "g_ij")]]
    )
  ),
  # RSP
  tar_target(
    identified_draws_array_1,
    identify_chains(
      post_array = mcmc_draws_array,
      param_hat = mu_theta[i],
      sign_d = -1
    )
  ),
  tar_target(
    identified_draws_array_2,
    identify_chains(
      post_array = mcmc_draws_array,
      param_hat = mu_theta[i],
      sign_d = 1
    )
  ),
  tar_target(
    identified_draws_array_3,
    identify_draws(
      post_array = mcmc_draws_array,
      param_hat = mu_theta[i]
    )
  ),
  # trace plots
  tar_target(
    mcmc_trace_plots,
    bayesplot::mcmc_trace(
      mcmc_draws_array,
      pars = paste0("mu_theta[", 1:20, "]")
    )
  ),
  tar_target(
    id_trace_plots,
    bayesplot::mcmc_trace(
      identified_draws_array_1,
      pars = paste0("mu_theta[", 1:20, "]")
    )
  ),
  # targets related to validation plot
  tar_target(
    reshaped_posterior,
    reshape_posterior(
      post_array = mcmc_draws_array,
      param_hat = mu_theta[i],
      order = mcmc_data[[c(".join_data", "g")]]
    )
  ),
  tar_target(
    mcmc_validation_plot,
    {
      # reorder data frame of judge info to match model order
      mcmc_data
      judge_order <- unique(mcmc_data[["ii"]])
      dgp_raw <- mcmc_data[[c(".join_data", "theta_df")]]
      validation_plot(
        data = reshaped_posterior,
        id = id,
        param = mu_theta_hat,
        dgp_df = dgp_raw[judge_order, ]
      )
    }
  ),
  tar_target(
    reshaped_id_1,
    reshape_posterior(
      post_array = identified_draws_array_1,
      param_hat = mu_theta[i],
      order = mcmc_data[[c(".join_data", "g")]]
    )
  ),
  tar_target(id_validation_plot_1, {
    # reorder data frame of judge info to match model order
    mcmc_data
    judge_order <- unique(mcmc_data[["ii"]])
    dgp_raw <- mcmc_data[[c(".join_data", "theta_df")]]
    validation_plot(
      data = reshaped_id_1,
      id = id,
      param = mu_theta_hat,
      dgp_df = dgp_raw[judge_order, ]
    )
  }),
  tar_target(
    reshaped_id_2,
    reshape_posterior(
      post_array = identified_draws_array_2,
      param_hat = mu_theta[i],
      order = mcmc_data[[c(".join_data", "g")]]
    )
  ),
  tar_target(id_validation_plot_2, {
    # reorder data frame of judge info to match model order
    mcmc_data
    judge_order <- unique(mcmc_data[["ii"]])
    dgp_raw <- mcmc_data[[c(".join_data", "theta_df")]]
    validation_plot(
      data = reshaped_id_2,
      id = id,
      param = mu_theta_hat,
      dgp_df = dgp_raw[judge_order, ]
    )
  }),
  tar_target(
    reshaped_id_3,
    reshape_posterior(
      post_array = identified_draws_array_3,
      param_hat = mu_theta[i],
      order = mcmc_data[[c(".join_data", "g")]]
    )
  ),
  tar_target(id_validation_plot_3, {
    # reorder data frame of judge info to match model order
    mcmc_data
    judge_order <- unique(mcmc_data[["ii"]])
    dgp_raw <- mcmc_data[[c(".join_data", "theta_df")]]
    validation_plot(
      data = reshaped_id_3,
      id = id,
      param = mu_theta_hat,
      dgp_df = dgp_raw[judge_order, ]
    )
  })
)
