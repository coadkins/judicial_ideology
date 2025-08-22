###############################################################################
################### FUNCTIONS RELATED TO SIMULATION #############################
###############################################################################

simulate_data <- function(
  cohort_g = 20,
  judge_gi = 50,
  case_ij = 50,
  types_b = 50
) {
  # simulate data
  set.seed(02474)

  ## define constants
  n_cohort <- cohort_g
  year <- factor(1:n_cohort)
  n_party <- 2
  party <- rbinom(n_cohort, size = 1, prob = .5)
  n_judge <- judge_gi * n_cohort
  n_cases <- n_judge * case_ij
  n_case_types <- 50
  n_cov <- (n_party - 1) + (n_cohort) + (n_cohort - 1) * (n_party - 1)

  gamma_sim <- construct_gamma(party, year)

  sigma_theta <- rlnorm(1, 0, .25)
  # vectorize draw_theta_ij() over party and year
  theta_raw_list <- Map(
    draw_theta_ij_raw,
    party = party,
    year = year,
    MoreArgs = list(
      n = n_judge / n_cohort,
      gamma = gamma_sim,
      sigma_theta = sigma_theta
    )
  )

  # reference group center the raw simulated theta
  theta_reference <- theta_raw_list[[1]]
  theta_vector <- (do.call(c, theta_raw_list) - mean(theta_reference)) /
    sd(theta_reference)

  # combine the values into a data.frame
  theta_df <- theta_vector |>
    as.data.frame() |>
    dplyr::mutate(
      judge_id = 1:length(theta_vector),
      year = factor(
        rep(1:n_cohort, each = n_judge / n_cohort)
      )
    ) |>
    dplyr::left_join(
      data.frame(year = factor(1:n_cohort), party = party),
      by = "year"
    )

  colnames(theta_df) <- c("theta", "judge_id", "year", "party")

  case_params <- data.frame(
    type = 1:n_case_types,
    mu_beta = rnorm(n_case_types, 0, 1),
    mu_alpha = rnorm(n_case_types, 0, 1)
  )

  sigma_alpha <- rlnorm(1, 0, .25)
  sigma_beta <- rlnorm(1, 0, .25)

  cases_df <- Map(
    draw_panel,
    case_id = 1:n_cases,
    case_type = sample(1:n_case_types, n_cases, replace = TRUE),
    MoreArgs = list(
      theta_df = theta_df,
      mu_case_df = case_params,
      sigma_alpha = sigma_alpha,
      sigma_beta = sigma_beta
    )
  ) |>
    purrr::list_rbind()
  # transform data for stan
  judge_covariates <- cases_df |>
    dplyr::group_by(party, year) |>
    dplyr::summarize(mu_theta = mean(theta), .groups = "drop") |>
    dplyr::slice(
      # make sure mu_theta order tracks order that Stan reads the data
      with(cases_df, cases_df[!duplicated(year), "year"]) |>
        as.numeric()
    )

  x <- with(judge_covariates, model.matrix(~ 0 + party + year + party * year))

  stan_data <- list(
    N = nrow(cases_df),
    N_case_id = length(unique(cases_df$case_id)),
    B = length(unique(cases_df$case_type)),
    N_judge = length(unique(cases_df$judge_id)),
    G = length(unique(cases_df[, "year"])),
    K = ncol(x),
    outcome = with(cases_df, outcome[order(case_id)]),
    ii = with(cases_df, judge_id[order(case_id)]), # judge for each obs.
    jj = with(cases_df, case_id[order(case_id)]), # case for each obs.
    x = x,
    # .join_data is returned in "mcmc_data" and is useful for post-processing
    .join_data = list(
      mu_theta = dplyr::pull(judge_covariates, mu_theta),
      sigma_theta = sigma_theta,
      theta_df = theta_df,
      party = party,
      g_ij = cases_df[, "year"], # group membership for each obs.
      g = unique(cases_df[, "year"]) # groups ordered by obs.
    )
  )

  # Append additional data that defines indices that facilitate
  # vectorized sampling in Stan
  stan_data <- c(
    stan_data,
    gen_group_idx(cases_df, "judge_id", "year"),
    gen_group_idx(
      cases_df,
      "case_id",
      "case_type",
      names = c("cases_by_type", "type_start", "type_end")
    )
  )

  # Append additional data to facilitate identifcation
  stan_data <- append(
    stan_data,
    list(mu_theta_ref_group = 1),
  )
  return(stan_data)
}

construct_gamma <- function(party, year) {
  # construct matrix of gamma parameters consistent with my theory
  # this is onerous beceause year is "one-hot encoded"
  x <- model.matrix(~ 0 + party + year + party * year)
  year_cols <- grep("^year\\d+$", colnames(x))
  party_year_cols <- grep("^party:year\\d+$", colnames(x))
  # subtract 1 from dem_years to match
  dem_years <- which(party == 0)
  gamma <- matrix(NA, ncol = ncol(x), nrow = 1)
  gamma[, 1] <- (-1) # gamma for party
  gamma[, year_cols] <- seq(
    from = -.2,
    by = -.1,
    length.out = length(year_cols)
  ) # gamma for each year (theta1)
  # gamma for each party*year(theta)
  gamma[, party_year_cols[-dem_years]] <- seq(
    from = -.2,
    by = -.4, # reps.
    length.out = length(party_year_cols[-dem_years])
  )
  gamma[, party_year_cols[dem_years]] <-
    rep(0, length(party_year_cols[dem_years]))
  return(gamma)
}

draw_theta_ij_raw <- function(n, party, year, gamma, sigma_theta) {
  # variables predicting mu
  x <- model.matrix(~ 0 + party + year + party * year) # 1xk row vector
  mu <- x %*% t(gamma)
  # draw theta_ij
  out <- rnorm(n, mu, sigma_theta)
  return(out)
}

theta_ij_standardize <- function(theta_vector) {
  # de-mean each theta
  theta_standardized <- (theta_vector - mean(theta_vector)) / sd(theta_vector)
  return(theta_standardized)
}


## simulate judges and cases
draw_case <- function(thetas, mu_beta, mu_alpha, sigma_alpha, sigma_beta) {
  alpha <- rnorm(1, mu_alpha, sigma_alpha)
  beta <- rnorm(1, mu_beta, sigma_beta)
  linear_func <- alpha + t(beta) * thetas
  link_func <- 1 / (1 + exp(-(linear_func)))
  y_out <- rbinom(prob = link_func, n = 1, size = 1)
  return(y_out)
}

draw_panel <- function(
  case_id,
  theta_df,
  mu_case_df,
  case_type,
  sigma_alpha,
  sigma_beta
) {
  panel <- theta_df[sample(1:nrow(theta_df), size = 3), ] # 3 judges per panel
  thetas <- as.matrix(panel[, 1]) # 1 thetas per judge
  mu_case_df <- with(mu_case_df, mu_case_df[type == case_type, ]) # 1 case type per panel
  case <- apply(
    thetas,
    MARGIN = 1,
    FUN = draw_case,
    mu_beta = with(mu_case_df, mu_case_df[type == case_type, "mu_beta"]),
    mu_alpha = with(mu_case_df, mu_case_df[type == case_type, "mu_alpha"]),
    sigma_alpha = sigma_alpha,
    sigma_beta = sigma_beta
  )
  panel$outcome <- case
  panel$case_id <- case_id
  panel$case_type <- mu_case_df[, "type"]
  return(panel)
}

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
