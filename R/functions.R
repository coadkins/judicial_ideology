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
        dplyr::select(g = !!g_var) |> # assign i so that its order
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
    mu_value_hat_col <- stringi::stri_c(
        # name of group means column
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
    order
) {
    param_hat <- enquo(param_hat)
    index_label <- extract_index(param_hat) |>
        rlang::expr_deparse()
    # create a data frame from the draws array
    subset_df <- tidybayes::spread_draws(post_array, !!param_hat)
    # match `i` in subset_df to id var in simulated data
    order_df <- tibble::tibble(id = order, !!index_label := seq_along(order))

    out <- dplyr::left_join(subset_df, order_df, by = index_label)
    return(out)
}

# -----------------------------------------------------------------------------

identify_signs <- function(
    post_array = fit_array,
    param_hat = mu_theta[i],
    sign = -1
) {
    id_chain <- identify_chains(fit_array, {{ param_hat }})
    out <- identify_draws(id_chain, {{ param_hat }})
    return(out)
}

#' Identify and rotate draws for a draws_array object 
#'
#' @param post_array a draws_array object
#' @param param_hat expression, the parameter to identify, passed as an
#' argument to tidybayes::spread_draws()
#' @param sign the default rotation for a given draw (1 or -1)
#'
#' @returns a draws_array object
identify_draws <- function(
    post_array = fit_array,
    param_hat = theta[i]
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
        dplyr::group_by(.iteration, .chain) |>
        dplyr::mutate(
            mean_d = mean.default(!!value_hat_col),
            flip = (sign(!!value_hat_col) * sign(mean_d)) < 0,
            !!value_hat_col := case_when(
                flip == TRUE ~ -1 * !!value_hat_col,
                .default = !!value_hat_col
            )
        )
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
        id_array[, , vars_lgl] <- sub_id_array
    )
    # return in the same format as input - draws_array
    # this is a valid input for posterior::mcmc_trace()
    return(id_array)
}

identify_chains <- function(
    post_array = fit_array,
    # param_hat is formatted for use with tidybayes::spread_draws()
    param_hat = theta[i],
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
    vars_lgl <- stringr::str_detect(
        unlist(dimnames(id_array)["variable"]), #var name for evey element
        paste0(
            rlang::as_label(value_hat_col), # regexp made from `value_col` expression
            "\\[.*"
        )
    )
    suppressWarnings(
        id_array[, cols, vars_lgl] <- -1 * id_array[, cols, vars_lgl]
    )
    # return in the same format as input - draws_array
    # this is a valid input for posterior::mcmc_trace()
    return(id_array)
}

# -----------------------------------------------------------------------------
