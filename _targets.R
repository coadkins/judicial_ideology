library(targets)

source("functions.R")
source("_stan_models.R")

tar_option_set(
  packages = renv::dependencies() |> dplyr::pull(Package), # packages that your targets need to run
  format = "rds", 
  memory = "transient",
  garbage_collection = TRUE
)

# tar_make_clustermq() configuration
options(clustermq.scheduler = "multicore")

# End this file with a list of target objects.
list(
  tar_target(
    name = fit_2PL,
    command = stan(
      model_code = stan_code_2PL,
      data = model_data_2PL,
      iter = 2000,
      save_warmup = FALSE
      )
  ),
  
  tar_target(
  ),
  
  tar_target(
    name = fit_2PL_constrained,
    command = stan(
      model_code = stan_code_2PL_constrained,
      data = model_data_constrained,
      iter = 2000,
      save_warmup = FALSE
      )
),
tar_target(
  name = fit_2PL_hurdle_constrained,
  command = stan(
    model_code = stan_code_hurdle_constrained,
    data = model_data_hurdle_constrained,
    iter = 2000,
    save_warmup = FALSE
    )
  ),
tar_target(
  name = fit_2PL_hurdle_2d,
  command = stan(
    model_code = stan_code_hurdle_2d,
    data = model_data_2d,
    iter = 1000,
    save_warmup = FALSE)
  ),
tar_target(
  name = fit_2PL_hurdle_2d_constrained,
  command =  stan(
    model_code = stan_code_hurdle_2d_con,
    data = model_data_2d_constrained,
    iter = 2000,
    save_warmup = FALSE
    )
  ),
)
