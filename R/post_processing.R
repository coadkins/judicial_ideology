###############################################################################
################### FUNCTIONS RELATED TO POST-PROCESSING MCMC DRAWS ###########
###############################################################################
validation_plot <- function(data, id, param, dgp_df) {
  ggplot2::ggplot(data, aes(x = {{ id }}, y = {{ param }})) +
    ggplot2::geom_boxplot(alpha = 0.5, fill = "lightgrey") +
    ggplot2::geom_boxplot(
      aes(x = year, y = theta, fill = factor(party), alpha = .5),
      data = dgp_df,
      coef = 0,
      outlier.shape = NA
    ) +
    ggplot2::scale_fill_manual(values = c("#1696d2", "#db2b27")) +
    ggplot2::theme_minimal() +
    ggplot2::theme(legend.position = "none") +
    ggplot2::ylab("Mu Theta") +
    ggplot2::xlab(NULL) +
    ggplot2::ggtitle("Distribution of Theta Estimates by Cohort")
}


reshape_posterior <- function(
  post_array,
  param_hat,
  order # vector of length `n_groups`
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
    dplyr::rename(!!new_var_col_name := !!old_var_col_name)
  # match `i` in subset_df to id_var in simulated data
  order_df <- tibble::tibble(id = order, !!index_label := seq_along(order))
  out <- dplyr::left_join(subset_df, order_df, by = index_label)
  return(out)
}
