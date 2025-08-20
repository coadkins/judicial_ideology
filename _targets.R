# Created by use_targets().
# Follow the comments below to fill in this target script.
# Then follow the manual to check and run the pipeline:
#   https://books.ropensci.org/targets/walkthrough.html#inspect-the-pipeline

# Load packages required to define the pipeline:
library(here)
library(stantargets)
library(stringi)
library(tarchetypes)
library(targets)
library(qs2)
library(paws.storage)

# Use memory more sparingly
tar_option_set(
  memory = "transient",
  garbage_collection = TRUE,
  repository_meta = "aws",
  resources = tar_resources(
    tar_resources_aws(
      bucket = Sys.getenv("S3_BUCKET"),
      prefix = "standardize",
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
tar_source(here("R", "functions.R"))
tar_source(here("R", "sim_functions.R"))
tar_source(here("R", "utils.R"))

# Replace the target list below with your own:
list(
  tar_stan_mcmc(
    name = mcmc,
    stan_files = here("stan", "hirt-hom.stan"),
#    cpp_options = list(stan_threads = TRUE),
    data = simulate_data(
      cohort_g = 20,
      judge_gi = 50,
      case_ij = 50,
      types_b = 50
    ),
    chains = 4,
    parallel_chains = 4,
#    threads_per_chain = 2,
    iter_warmup = 1000,
    iter_sampling = 1000,
    format = "qs",
    format_df = "qs",
    stdout = R.utils::nullfile(),
    stderr = R.utils::nullfile()
  )
)
