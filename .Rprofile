options(
  repos = c(
    CRAN = "https://packagemanager.posit.co/cran/__linux__/rhel9/latest", 
    standev = "https://stan-dev.r-universe.dev")
)
source("renv/activate.R")
renv::settings$snapshot.type("explicit")
renv::settings$ignored.packages("cmdstanr")
renv::config$pak.enabled(TRUE)
Sys.setenv(CMDSTAN = here::here(".cmdstan"))

