#' Strip a tidybayes style variable of indices
#'
#' @param var_in quosure, a tidybayes style variable name
#'
#' @returns expression, a variable name with no indices
extract_var_name <- function(var_in) {
  out <- rlang::as_label(var_in) |>
    gsub("^\\~|\\[.*", "", x = _) |>
    rlang::parse_expr()
  return(out)
}

extract_index <- function(var_in) {
  out <- rlang::as_label(var_in) |>
    stringi::stri_extract(str = _, regex = "(?<=\\[)[^\\]]+(?=\\])") |>
    rlang::parse_expr()

  return(out)
}

# ------------------------------------------------------------------------------
#
#' Use qs::qs_read or cmdstanr::read_cmdstan_csv() depending on the supplied path
#'
#' @param path either the path to a single .qs file, or a directory containing
#'  .csv files for each chain of the cmdstan sampler
#' @param ... optionally, supply arguments to read_cmdstan_csv(), useful for
#' reading in only a subset of variables
#'
#' @returns a draws array object
load_posterior_draws <- function(path, ...) {
  withr::defer(rm(fit))
  if (tools::file_ext(path) == "qs") {
    fit <- qs::qsread(path)
    return(fit$draws())
  } else if (tools::file_ext(path) == "qs2") {
    fit <- qs2::qs_read(path)
    return(fit$draws())
  } else {
    # OR load from .csv files if saving to .qs failed
    fit <- cmdstanr::read_cmdstan_csv(
      list.files(
        path,
        pattern = "*.csv",
        full.names = TRUE
      ),
      variables = ...
    )
    return(fit[["post_warmup_draws"]])
  }
}
