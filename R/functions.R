get_group_ids <- function(data_df, i_var, g_var) {
    i_var <- enquo(i_var)
    g_var <- enquo(g_var)
    # get group_ids and preserve input order
    data_df |>
        distinct(!!i_var, !!g_var) |>
        select(g = !!g_var) |> # assign i so that its order
        mutate(i = row_number()) # matches the order that
    # data entered the model
}

draw_group_means <- function(post_array, param, group_ids) {
    param <- rlang::enquo(param)
    # create a label to id input params in
    # tidybayes' spread_draws() df
    value_col <- rlang::as_label(param) |>
        gsub("^\\~|\\[.*", "", x = _) |>
        rlang::parse_expr()
    mu_value_col <- stringi::stri_c(
        "mu_",
        rlang::as_label(value_col),
        "[g]") |>
        rlang::parse_expr()

    # tidy the array
    subset_df <- tidybayes::spread_draws(post_array, !!param)
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

validate_posterior <- function() {
    # compare the simulated posterior to the estimated one
}

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
