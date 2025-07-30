
#' Return dataframe matching individuals to groups, in the right order 
#'
#' @param data_df data frame of observations
#' @param i_var expression, a data frame column designating individuals
#' @param g_var expression, a data frame column designating groups
#'
#' @returns a dataframe with `length(unique(data_df$g_var))` rows
get_group_ids <- function(data_df, i_var, g_var) {
    i_var <- enquo(i_var)
    g_var <- enquo(g_var)
    # get group_ids and preserve input order
    out <- data_df |>
        distinct(!!i_var, !!g_var) |>
        select(g = !!g_var) |>   # assign i so that its order
        mutate(i = row_number()) # matches the order that
                                 # data entered the model
    return(out)
}

# -----------------------------------------------------------------------------

#' Compute parameter means for each draw in an array of posterior draws
#'
#' @param post_array array of posterior draws from a Stan model
#' @param estimator expression, the parameter to generate grouped means for
#'  in `tidybayes` parameter syntax
#' @param group_ids a data frame matching individuals to groups, output by `get_group_ids()`
#'
#' @returns draws of group means, formatted as a posterior draws array.
draw_post_group_means <- function(post_array, estimator, group_ids) {
    estimator <- rlang::enquo(estimator)
    value_col <- extract_var_name(estimator) # name of estimator column
    mu_value_col <- stringi::stri_c( # name of group means column
        "mu_",
        rlang::as_label(value_col),
        "[g]"
    ) |>
        rlang::parse_expr()

    # tidy the array
    subset_df <- tidybayes::spread_draws(post_array, !!estimator)
    # join grouping variable
    mu_g_df <- subset_df |>
        dplyr::left_join(group_ids, by = "i") |>
        group_by(g, .draw, .iteration, .chain) |>
        summarize(mu_theta = mean(!!value_col)) |> # get the mean for each group
        tidybayes::unspread_draws(!!mu_value_col)
    # transform into draws array
    mu_g_array <- posterior::as_draws_array(mu_g_df, .nchains = 4)
    return(mu_g_array)
}

# -----------------------------------------------------------------------------

#' Compare the posterior draws to the original simulated data 
#'
#' @param post_array a draws array object, returned by `draws()` method
#' @param estimator expression, the parameter estimated by the draws from the posterior distribution
#' @param stan_data_df tidy data frame of (simulated) model data, returned by `stan_data_to_df()`
#' @param parameter expression, the column in stan_data_df corresponding to "true" parameter values
#'
#' @returns a dataframe meant to use with plot_validate_posterior()
validate_posterior <- function(
    post_array, 
    estimator, 
    stan_data_df,
    parameter) { 
}

# -----------------------------------------------------------------------------

#' Plot distribution of estimated versus true parameter values  
#'
#' @param validated_df
#'
#' @returns
plot_validate_posterior <- function(validated_df) {

}
# -----------------------------------------------------------------------------

# TO DO: Rework these to use "estimator" instead of parameter
identify_sign <- function(
    post_array = fit_array,
    param = theta[i],
    sign = -1
) {
    out <- identify_draws(fit_array, param) |>
        identify_chains(param, sign)
    return(out)
}

identify_draws <- function(
    post_array = fit_array,
    param = theta[i]
) {
    param <- rlang::enquo(param)
    # transform the input array into a `draws_df` objects
    post_long_df <- tidybayes::spread_draws(post_array, !!param) |>
        ungroup()
    # convert tidybayes syntax for variable names
    # into a quosure that identifies the column
    # containing the parameter values
    value_col <- rlang::as_label(param) |>
        gsub("^\\~|\\[.*", "", x = _) |>
        rlang::parse_expr()

    # copy the original array and return it with flipped values
}

identify_chains <- function(
    post_array = fit_array,
    param = theta[i], # formatted for tidybayes::spread_draws()
    sign = -1
) {
    # capture the tidybayes syntax in a quosure
    param <- rlang::enquo(param)
    # transform the input array into a `draws_df` objects
    post_long_df <- tidybayes::spread_draws(post_array, !!param) |>
        ungroup()
    value_col <- rlang::as_label(param) |>
        gsub("^\\~|\\[.*", "", x = _) |>
        rlang::parse_expr()
    # figure out which chains to flip
    chain_flips <- post_long_df |>
        dplyr::group_by(.chain) |>
        dplyr::summarise(
            flip = (sign * mean.default(!!value_col)) < 0,
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
            rlang::as_label(value_col), # regexp made from `value_col` expression
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
