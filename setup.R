pak::pak("renv")
renv::activate()
renv::restore(exclude = c("rstan", "rstanarm"))
cmdstanr::install_cmdstan("/usr/cmdstan", version = "2.36.0")
