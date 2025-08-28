###############################################################################
################### FUNCTIONS RELATED TO POST-PROCESSING MCMC DRAWS ###########
###############################################################################
validation_plot <- function(data, id, param, dgp_df) {
  ggplot2::ggplot(data, ggplot2::aes(x = {{ id }}, y = {{ param }})) +
    ggplot2::geom_boxplot(alpha = 0.5, fill = "lightgrey") +
    ggplot2::geom_boxplot(
      ggplot2::aes(x = year, y = theta, fill = factor(party), alpha = .5),
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

identify_chains <- function(
  post_array = fit_array,
  param_hat = mu_theta[i],
  sign_d = -1
) {
  param_hat <- rlang::enquo(param_hat)
  # transform the input array into a `draws_df` objects
  post_long_df <- tidybayes::spread_draws(post_array, !!param_hat) |>
    dplyr::ungroup()
  # convert tidybayes syntax for variable names
  # into a quosure that identifies the column
  # containing the parameter values
  value_hat_col <- rlang::as_label(param_hat) |>
    gsub("^\\~|\\[.*", "", x = _) |>
    rlang::parse_expr()
  # figure out which draws to flip
  # use chains/iterations since that is how draws_array is structured
  draw_flips <- post_long_df |>
    dplyr::group_by(.chain) |>
    dplyr::mutate(
      mean_p = mean(!!value_hat_col),
      flip = sign(mean_p) * sign(sign_d) < 0,
      !!value_hat_col := case_when(
        flip == TRUE ~ -1 * !!value_hat_col,
        .default = !!value_hat_col
      )
    ) |>
    dplyr::ungroup()
  sub_id_array <- tidybayes::unspread_draws(draw_flips, !!param_hat) |>
    posterior::as_draws_array()
  # copy the original array and return it with flipped values
  id_array <- post_array
  # create a logical vector for every element col
  # that needs to be replaced in the original
  vars_lgl <- stringr::str_detect(
    unlist(dimnames(id_array)["variable"]), #var name for evey element
    paste0(
      rlang::as_label(value_hat_col), # regexp made from `value_col` expression
      "\\[.*"
    )
  )
  suppressWarnings(
    id_array[,, vars_lgl] <- sub_id_array
  )
  # return in the same format as input - draws_array
  # this is a valid input for posterior::mcmc_trace()
  return(id_array)
}
