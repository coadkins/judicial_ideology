source("renv/activate.R")
options(
  repos = c(
    CRAN = "https://packagemanager.posit.co/cran/__linux__/manylinux_2_28/latest", 
    standev = "https://stan-dev.r-universe.dev")
)
renv::settings$snapshot.type("explicit")
renv::config$pak.enabled(TRUE)
Sys.setenv(CMDSTAN = here::here(".cmdstan"))

