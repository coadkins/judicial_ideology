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

  ## basic quantities
  n_cohort <- cohort_g
  year <- 1:n_cohort
  n_party <- 2
  party <- rbinom(n_cohort, size = 1, prob = .5)
  n_judge <- judge_gi * n_cohort
  n_cases <- n_judge * case_ij
  n_case_types <- 50

  ## derived quantities
  knots <- find_knots(party)
  n_knots <- length(knots)
  x <- model.matrix(
    ~ 1 +
      splines::bs(
        year,
        knots = knots,
        degree = 3,
        Boundary.knots = c(1, length(year))
      )
  )
  gamma_sim <- simulate_gamma(x, knots, party)

  sigma_theta <- rlnorm(1, 0, .25)
  # vectorize draw_theta_ij() over rows of x
  theta_raw_list <- apply(
    x,
    MARGIN = 1,
    \(x) {
      draw_theta_ij_raw(
        x = x,
        n = n_judge / n_cohort,
        gamma = gamma_sim,
        sigma_theta = sigma_theta
      )
    },
    simplify = FALSE
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

  # Sample covariance matrix from Inverse Wishart
  Sigma <- MCMCpack::riwish(v = 3, S = diag(2))

  # Sample bivariate normal for each case type
  mu_ab_matrix <- MASS::mvrnorm(n = n_case_types, mu = c(0, 0), Sigma = Sigma)

  case_params <- data.frame(
    type = 1:n_case_types,
    mu_beta = mu_ab_matrix[, 2], # second column for beta
    mu_alpha = mu_ab_matrix[, 1] # first column for alpha
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

simulate_gamma <- function(
  x,
  knots,
  party,
  trend_strength = -0.1,
  shift_0_to_1 = -0.5,
  shift_1_to_0 = 0.5
) {
  # construct matrix of gamma parameters consistent with my theory
  # need to use a basis spline to avoid multicollinearity in linear model
  # Initialize coefficients
  n_basis <- ncol(x)
  gamma <- matrix(NA, ncol = n_basis, nrow = 1)

  # Set intercept
  gamma[, 1] <- 0 # starting point

  # Add negative trend over time for non-intercept terms
  basis_weights <- seq(0, trend_strength, length.out = n_basis - 1)
  gamma[, -1] <- basis_weights

  # Identify knot types and add shifts
  # Because each knot is influenced by multiple asis functions
  # this loop iteratively updates the knot coefficients

  for (i in seq_along(knots)) {
    knot_pos <- knots[i]

    # Determine if this is a 0->1 or 1->0 transition
    if (knot_pos > 1 && knot_pos <= length(party)) {
      transition_type <- if (party[knot_pos - 1] == 0 && party[knot_pos] == 1) {
        "0_to_1"
      } else if (party[knot_pos - 1] == 1 && party[knot_pos] == 0) {
        "1_to_0"
      } else {
        "other"
      }

      # Find which basis functions are most active around this knot
      # and adjust their coefficients (non active knots are 0)
      knot_influence <- x[knot_pos, ]
      knot_influence[1] <- 0 # don't modify intercept

      if (transition_type == "0_to_1") {
        # Shift towards negative
        gamma[1, ] <- gamma[1, ] + shift_0_to_1 * knot_influence
      } else if (transition_type == "1_to_0") {
        # Shift towards positive
        gamma[1, ] <- gamma[1, ] + shift_1_to_0 * knot_influence
      }
    }
  }
  return(gamma)
}

draw_theta_ij_raw <- function(n, x, gamma, sigma_theta) {
  # variables predicting mu
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
find_knots <- function(party) {
  # Compare each element with the next one
  knots <- party[-length(party)] != party[-1]
  # Get indices where transitions occur (add 1 to get the higher index)
  which(knots) + 1
}

visualize_variation_theta <- function(dgp_df) {
  ggplot2::geom_boxplot(
    ggplot2::aes(x = year, y = theta, fill = factor(party), alpha = .5),
    data = dgp_df,
    coef = 0,
    outlier.shape = NA
  ) +
    ggplot2::scale_fill_manual(values = c("#1696d2", "#db2b27")) +
    ggplot2::theme_minimal() +
    ggplot2::theme(legend.position = "none") +
    ggplot2::ylab("Theta") +
    ggplot2::xlab(NULL) +
    ggplot2::ggtitle("Distribution of Theta Estimates by Cohort")
}

validation_plot <- function(data, id, param, dgp_df) {
  ggplot2::ggplot(data, aes(x = {{ id }}, y = {{ param }})) +
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

visualize_variation_outcome <- function(outcome, g_ij) {
  data <- tibble::tibble(outcome = outcome, groups = g_ij)
  ggplot2::ggplot(data, ggplot2::aes(x = outcome)) +
    ggplot2::geom_bar(fill = "steelblue", alpha = 0.7) +
    ggplot2::facet_wrap(~groups, ncol = 5) +
    ggplot2::labs(
      title = "Outcomes by Cohort",
      y = "",
      x = ""
    ) +
    ggplot2::theme_minimal() +
    ggplot2::theme(
      axis.text.x = ggplot2::element_blank(),
      axis.ticks.x = ggplot2::element_blank(),
      strip.text = ggplot2::element_text(size = 10)
    )
}
