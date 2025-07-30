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

# ------------------------------------------------------------------------------