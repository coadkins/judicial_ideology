# TO DO: Update the simulation functions so that one set works for both
# 1D and 2D simulated data sets
# Right now, this file just contains functions that are applicable are
# applicable across simulation types

# this function creates secondary data strucutures which are used to create
# indexes in Stan, which facilitate vectorized sampling within groups
#' Generate group indices for my stan model
#'
#' @param df a dataframe that matches individuals to groups
#' @param x string, column containing individual id indicators
#' @param y string, column containing group id indicators
#' @param names vector, optional, names for elements of the output list
#'
#' @returns a list, the first element is a vector of individuals ordered by group,
#' the second element is a vector of group start indices,
#' and the third element is a vector of group end indices
#'
gen_group_idx <- function(
  df,
  x,
  y,
  names = c("judges_by_group", "group_start", "group_end")
) {
  # Load required library
  # Get unique individuals by group, arranged by year
  x_by_group <- df[!duplicated(df[[x]]), ] |>
    dplyr::arrange(year)

  # Count group sizes
  group_sizes <- x_by_group |>
    dplyr::count(.data[[y]]) |>
    dplyr::pull(n)

  # Calculate n_groups, handling factor columns
  if (is.factor(df[[y]])) {
    n_groups <- max(as.numeric(levels(df[[y]])), na.rm = TRUE)
  } else {
    n_groups <- max(df[[y]], na.rm = TRUE)
  }

  # Calculate group start positions
  group_start <- cumsum(c(1, group_sizes[-n_groups]))

  # Calculate group end positions
  group_end <- c(
    (group_start - 1)[-1], # for groups 1:(n_groups - 1)
    group_start[n_groups] + group_sizes[n_groups] - 1 # final group
  )

  # Return list with three elements using custom names
  result <- list(
    x_by_group[[x]],
    group_start,
    group_end
  )
  names(result) <- names
  return(result)
}

#' Get idx for judge with theta closest to a target value
#'
#' @param df data frame of case outcomes
#' @param x data frame column of individual ids; the data frame is filtered to
#' include one entry per individual
#' @param y data frame column of parameter values for each individual
#' @param target the target value for the parameter y
#'
#' @returns an individual id (i.e. judge id) id'ing the individual with the
#' score closest to the target value
#'
fix_judge_idx <- function(df, x, y, target) {
  # narrow to unique individual ids
  df <- with(df, df[!duplicated(x)], )
  # calculate absolute distance to target value
  df[, "distance_to_target"] <- abs(df[, y] - target)
  # get index
  idx <- which.min(df[, "distance_to_target"])
  return(df[idx, x])
}
