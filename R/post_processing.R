###############################################################################
################### FUNCTIONS RELATED TO POST-PROCESSING MCMC DRAWS ###########
###############################################################################reshape_posterior <- function(
reshape_posterior <- function(
  post_array,
  param_hat,
  order
) {
  param_hat <- rlang::enquo(param_hat)
  old_var_col_name <- extract_var_name(param_hat)
  new_var_col_name <- old_var_col_name |>
    rlang::as_label() |>
    paste0("_hat") |>
    rlang::parse_expr()
  index_label <- extract_index(param_hat) |>
    rlang::expr_deparse() # deparse() required b/c join takes string arg
  # create a data frame from the draws array
  subset_df <- tidybayes::spread_draws(post_array, !!param_hat) |>
    rename(!!new_var_col_name := !!old_var_col_name)
  # match `i` in subset_df to id var in simulated data
  order_df <- tibble::tibble(id = order, !!index_label := seq_along(order))

  out <- dplyr::left_join(subset_df, order_df, by = index_label)
  return(out)
}
