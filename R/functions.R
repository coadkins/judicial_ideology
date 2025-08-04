
#' Return dataframe matching individuals to groups, in the right order 
#'
#' @param data_df data frame of observations
#' @param i_var expression, a data frame column designating individuals
#' @param g_var expression, a data frame column designating groups
#'
#' @returns a dataframe with `length(unique(data_df$g_var))` rows
get_group_ids <- function(data_df, i_var, g_var) {
    i_var <- rlang::enquo(i_var)
    g_var <- rlang::enquo(g_var)
    # get group_ids and preserve input order
    out <- data_df |>
        dplyr::distinct(!!i_var, !!g_var) |>
        dplyr::select(g = !!g_var) |>   # assign i so that its order
        dplyr::mutate(i = row_number()) # matches the order that
                                 # data entered the model
    return(out)
}

# -----------------------------------------------------------------------------

#' Compute parameter means for each draw in an array of posterior draws
#'
#' @param post_array array of posterior draws from a Stan model
#' @param param_hat expression, the parameter to generate grouped means for
#'  in `tidybayes` parameter syntax
#' @param group_ids a data frame matching individuals to groups, output by `get_group_ids()`
#'
#' @returns draws of group means, formatted as a posterior draws array.
draw_post_group_means <- function(post_array, param_hat, group_ids) {
    param_hat <- rlang::enquo(param_hat)
    value_hat_col <- extract_var_name(param_hat) # name of estimator column
    mu_value_hat_col <- stringi::stri_c( # name of group means column
        "mu_",
        rlang::as_label(value_hat_col),
        "[g]"
    ) |>
        rlang::parse_expr()

    # tidy the array
    subset_df <- tidybayes::spread_draws(post_array, !!param_hat)
    # join grouping variable
    mu_g_df <- subset_df |>
        dplyr::left_join(group_ids, by = "i") |>
        dplyr::group_by(g, .draw, .iteration, .chain) |>
        dplyr::summarize(mu_theta = mean(!!value_hat_col)) |> # get the mean for each group
        tidybayes::unspread_draws(!!mu_value_hat_col)
    # transform into draws array
    mu_g_array <- posterior::as_draws_array(mu_g_df, .nchains = 4)
    return(mu_g_array)
}

# -----------------------------------------------------------------------------

#' Reorder posterior to match order of stan data 
#'
#' @param post_array a draws array object, returned by `draws()` method
#' @param param_hat expression, the parameter estimated by the draws from the posterior distribution, 
#' specified in `tidybayes()` syntax.
#' @param order a vector of unit ids, in the order they appear in the model data
#' 
#' @returns a dataframe
reshape_posterior <- function(
    post_array, 
    param_hat,
    order) {
  param_hat <- enquo(param_hat)
  index_label <- extract_index(param_hat) |> 
      rlang::expr_deparse()
  # create a data frame from the draws array  
  subset_df <- tidybayes::spread_draws(post_array, !!param_hat)
  # match `i` in subset_df to id var in simulated data
  order_df <- tibble(id = order, 
    !!index_label := seq_along(order))

  out <- left_join(subset_df, order_df, by = index_label)
  return(out)
}

# -----------------------------------------------------------------------------

# TO DO: Rework these to use "param_hat" instead of parameter
# identify_sign <- function(
#     post_array = fit_array,
#     param_hat = theta[i],
#     sign = -1
# ) {
#     out <- identify_draws(fit_array, param_hat) |>
#         identify_chains(param_hat, sign)
#     return(out)
# }

identify_draws <- function(
    post_array = fit_array,
    param_hat = theta[i]
) {
    param_hat <- rlang::enquo(param_hat)
    # transform the input array into a `draws_df` objects
    post_long_df <- tidybayes::spread_draws(post_array, !!param_hat) |>
        ungroup()
    # convert tidybayes syntax for variable names
    # into a quosure that identifies the column
    # containing the parameter values
    value_hat_col <- rlang::as_label(param_hat) |>
        gsub("^\\~|\\[.*", "", x = _) |>
        rlang::parse_expr()
    # figure out which draws to flip
    draw_flips <- post_long_df |>
        dplyr::group_by(.draw) |>
        dplyr::summarise(
            flip = (sign * mean.default(!!value_hat_col)) < 0,
            .groups = "drop"
        ) |>
        dplyr::select(.draw, flip) |>
        dplyr::filter(flip == TRUE) |>
        pull(.chain)
    # copy the original array and return it with flipped values
    id_array <- post_array
    # create a logical vector for for every element row
    # that needs to be flipped
    cols <- draw_flips
    # create a logical vector for every element col
    # that needs to be flipped
    vars <- stringr::str_detect(
        unlist(dimnames(id_array)["variable"]), #var name for evey element
        paste0(
            rlang::as_label(value_hat_col), # regexp made from `value_col` expression
            "\\[.*"
        )
    )
    suppressWarnings(
        id_array[, cols, vars] <- -1 * id_array[, cols, vars]
    )
    # return in the same format as input - draws_array
    # this is a valid input for posterior::mcmc_trace()
    return(id_array)
}
}

identify_chains <- function(
    post_array = fit_array,
    param_hat = theta[i], # formatted for tidybayes::spread_draws()
    sign = -1
) {
    # capture the tidybayes syntax in a quosure
    param_hat <- rlang::enquo(param_hat)
    # transform the input array into a `draws_df` objects
    post_long_df <- tidybayes::spread_draws(post_array, !!param_hat) |>
        ungroup()
    value_hat_col <- rlang::as_label(param_hat) |>
        gsub("^\\~|\\[.*", "", x = _) |>
        rlang::parse_expr()
    # figure out which chains to flip
    chain_flips <- post_long_df |>
        dplyr::group_by(.chain) |>
        dplyr::summarise(
            flip = (sign * mean.default(!!value_hat_col)) < 0,
            .groups = "drop"
        ) |>
        dplyr::select(.chain, flip) |>
        dplyr::filter(flip == TRUE) |>
        pull(.chain)
    # copy the original array and return it with flipped values
    id_array <- post_array
    # create a logical vector for for every element row
    # that needs to be flipped
    cols <- chain_flips
    # create a logical vector for every element col
    # that needs to be flipped
    vars <- stringr::str_detect(
        unlist(dimnames(id_array)["variable"]), #var name for evey element
        paste0(
            rlang::as_label(value_hat_col), # regexp made from `value_col` expression
            "\\[.*"
        )
    )
    suppressWarnings(
        id_array[, cols, vars] <- -1 * id_array[, cols, vars]
    )
    # return in the same format as input - draws_array
    # this is a valid input for posterior::mcmc_trace()
    return(id_array)
}

# -----------------------------------------------------------------------------