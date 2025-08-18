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

# Use memory more sparingly
tar_option_set(memory = "transient", garbage_collection = TRUE)

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

# set up the output dir for the .csv files
model_id <- stringi::stri_c(format(Sys.Date(), "%m%d%Y"), sample(100:999, 1))
results_path <- here("results", model_id)
if (!dir.exists(results_path)) {
  dir.create(results_path, recursive = TRUE)
}
cmdstanr::save_output_files(
  dir = results_path,
  basename = model_id,
  timestamp = FALSE,
  random = FALSE 
)

# Replace the target list below with your own:
list(
  tar_stan_mcmc(
    name = mcmc,
    stan_files = here("stan", "hirt-hom.stan"),
    data = simulate_data(
      cohort_g = 20,
      judge_gi = 50,
      case_ij = 50,
      types_b = 50
    ),
    chains = 4,
    parallel_chains = 4,
    iter_warmup = 1000,
    iter_sampling = 1000,
    format = "qs",
    format_df = "qs",
    stdout = R.utils::nullfile(),
    stderr = R.utils::nullfile()
  )
)
