# Load packages
library(tidyverse)
library(data.table)
library(rstan)
library(tidybayes)
library(janitor)
library(purrr)
library(forcats)

options(mc.cores = parallel::detectCores())
rstan_options(auto_write = TRUE)

model_data <- readRDS("model_data.RDS")

stan_code_hurdle_2d <- "
functions {
  matrix whiten(matrix XX) {
    /* De-means and 'whitens' (cov = I) XX */
    matrix[rows(XX), cols(XX)] DM;
    matrix[cols(XX), cols(XX)] SS;
    matrix[cols(XX), cols(XX)] PP;
    matrix[cols(XX), cols(XX)] WW;
    for (d in 1:cols(XX)) {
      DM[, d] = XX[, d] - mean(XX[, d]); /* de-mean each column */
    }
    SS = crossprod(DM) ./ (rows(XX) - 1.0); /* covariance of XX */
    PP = inverse_spd(SS); /* precision of XX */
    WW = cholesky_decompose(PP); /* Cholesky decomposition of precision */
    return DM * WW; /* de-meaned and whitened XX */
  }
}
data {
    int<lower=0> N;
    int<lower=0> N_case_id;
    int<lower=1> N_judge;
    int<lower=-1> outcome[N];
    int<lower=1> case_id[N];
    int<lower=1> judge[N];
    int<lower=1> party_id[N];
    int<lower=1> N_party_id;
    int<lower=1> N_dim;
    matrix[N_case_id, N_dim] beta_sign;
    matrix[N_case_id, N_dim] beta_nonzero;
}
parameters {
    vector[N_case_id] alpha; // difficulty
    matrix[N_judge, N_dim] eta_raw;
    matrix[N_case_id, N_dim] beta_free;
    matrix<lower=0>[N_case_id, N_dim] beta_pos;
    row_vector[N_dim] gamma_1;
    real gamma_0;
}
transformed parameters {
  /* IDENTIFICATION */
  /* Normalize etas [D + D + D(D - 1)/2 restrictions] */
  matrix[N_judge, N_dim] eta = whiten(eta_raw);
  /* Fix and restrict betas [D(D - 1)/2 + D restictions] */
  matrix[N_case_id, N_dim] beta;
  for (i in 1:N_case_id) {
    for (d in 1:N_dim) {
      /* Fix D(D - 1)/2 betas to 0 */
      if (beta_nonzero[i, d] == 0) beta[i, d] = 0;
      /* Restrict sign of D betas */
      else if (beta_sign[i, d] == 0) beta[i, d] = beta_free[i, d];
      else if (beta_sign[i, d] > 0) beta[i, d] = beta_pos[i, d];
      else if (beta_sign[i, d] <0) beta[i,d] = -1 * beta_pos[i,d];
    }
  }
}
model {
    for (n in 1:N) {
        if (outcome[n] == -1)
            1 ~ bernoulli(Phi_approx(gamma_0 + gamma_1*eta[judge[n],]'));
        else {
            0 ~ bernoulli(Phi_approx(gamma_0 + gamma_1*eta[judge[n],]'));
            outcome[n] ~ bernoulli_logit(beta[case_id[n],] * eta[judge[n],]' - alpha[case_id[n]]);
        }
    }
     alpha ~ std_normal();
     gamma_0 ~ std_normal();
     to_vector(gamma_1) ~ std_normal();
     to_vector(beta_free) ~ std_normal();
     to_vector(beta_pos) ~ lognormal(1,1);
     to_vector(eta_raw) ~ std_normal();
}
generated quantities {
    int y_hat[N];
    real gamma_hat[N];
    for (n in 1:N) {
        gamma_hat[n] = Phi_approx(gamma_0 + gamma_1*eta[judge[n],]');
        if (bernoulli_rng(gamma_hat[n]) == 1)
            y_hat[n] = -1;
        else {
            y_hat[n] = bernoulli_logit_rng(beta[case_id[n],] * eta[judge[n],]' - alpha[case_id[n]]);
        }
    }
}
"
# Data for 2D Model with minimal constraints

model_data_2d <- model_data %>%
                 mutate(case_id = fct_drop(case_id),
                 judge = fct_drop(judge)) %>%
                 tidybayes::compose_data(.n_name = n_prefix("N")) %>%
                 append(., list(N_dim = 2))

model_data_2d$beta_sign <- matrix(
    data = 0,
    nrow = model_data_2d$N_case_id,
    ncol = model_data_2d$N_dim
)

model_data_2d$beta_nonzero <- matrix(
    data = 1,
    nrow = nrow(model_data_2d$beta_sign),
    ncol = ncol(model_data_2d$beta_sign)
)

model_data_2d$beta_nonzero[1, 1] <- 0
model_data_2d$beta_nonzero[1:2, 2] <- 0

# Fit 2D Model with Minimal Constraints

fit_2PL_hurdle_2d <- stan(
    model_code = stan_code_hurdle_2d,
    data = model_data_2d,
    iter = 1000,
    save_warmup = FALSE
)

saveRDS(fit_2PL_hurdle_2d, paste(save, "fit_2PL_hurdle_2d.RDS", sep = ""))

# Fit 2D Hurdle Model with Constraints

stan_code_hurdle_2d_con <- "
functions {
  matrix whiten(matrix XX) {
    /* De-means and 'whitens' (cov = I) XX */
    matrix[rows(XX), cols(XX)] DM;
    matrix[cols(XX), cols(XX)] SS;
    matrix[cols(XX), cols(XX)] PP;
    matrix[cols(XX), cols(XX)] WW;
    for (d in 1:cols(XX)) {
      DM[, d] = XX[, d] - mean(XX[, d]); /* de-mean each column */
    }
    SS = crossprod(DM) ./ (rows(XX) - 1.0); /* covariance of XX */
    PP = inverse_spd(SS); /* precision of XX */
    WW = cholesky_decompose(PP); /* Cholesky decomposition of precision */
    return DM * WW; /* de-meaned and whitened XX */
  }
}
data {
    int<lower=0> N;
    int<lower=0> N_case_id;
    int<lower=1> N_judge;
    int<lower=-1> outcome[N];
    int<lower=1> case_id[N];
    int<lower=1> judge[N];
    int<lower=1> party_id[N];
    int<lower=1> N_party_id;
    int<lower=1> N_dim;
    matrix[N_case_id, N_dim] beta_sign;
    matrix[N_case_id, N_dim] beta_nonzero;
    matrix[N_judge, N_dim] eta_sign;
    matrix[N_judge, N_dim] eta_nonzero;
}
parameters {
    vector[N_case_id] alpha; // difficulty
    matrix<lower = 0>[N_judge, N_dim] eta_pos;
    matrix[N_judge, N_dim] eta_free;
    matrix[N_case_id, N_dim] beta_free;
    matrix<lower=0>[N_case_id, N_dim] beta_pos;
    row_vector[N_dim] gamma_1;
    real gamma_0;
}
transformed parameters {
  /* IDENTIFICATION */
  /* Fix and restrict betas [D(D - 1)/2 + D restictions] */
  matrix[N_case_id, N_dim] beta;
  matrix[N_judge, N_dim] eta_raw;
  matrix[N_judge, N_dim] eta;
  for (i in 1:N_case_id) {
    for (d in 1:N_dim) {
      /* Fix D(D - 1)/2 betas to 0 */
      if (beta_nonzero[i, d] == 0) beta[i, d] = 0;
      /* Restrict sign of D betas */
      else if (beta_sign[i, d] == 0) beta[i, d] = beta_free[i, d];
      else if (beta_sign[i, d] > 0) beta[i, d] = beta_pos[i, d];
      else if (beta_sign[i, d] <0) beta[i,d] = -1 * beta_pos[i,d];
    }
  }
    for (j in 1:N_judge) {
    for (d in 1:N_dim) {
      if (eta_nonzero[j, d] == 0) eta_raw[j, d] = 0;
      else if (eta_sign[j, d] == 0) eta_raw[j, d] = eta_free[j,d];
      else if (eta_sign[j, d] > 0) eta_raw[j,d] = eta_pos[j,d];
      else if (eta_sign[j,d] < 0) eta_raw[j,d] = -1 * eta_pos[j,d];
    }
   }
  eta = whiten(eta_raw);
}
model {
    for (n in 1:N) {
        if (outcome[n] == -1)
            1 ~ bernoulli(Phi_approx(gamma_0 + gamma_1*eta[judge[n],]'));
        else {
            0 ~ bernoulli(Phi_approx(gamma_0 + gamma_1*eta[judge[n],]'));
            outcome[n] ~ bernoulli_logit(beta[case_id[n],] * eta[judge[n],]' - alpha[case_id[n]]);
        }
    }
     alpha ~ std_normal();
     gamma_0 ~ std_normal();
     to_vector(gamma_1) ~ std_normal();
     to_vector(beta_free) ~ std_normal();
     to_vector(beta_pos) ~ lognormal(1,1);
     to_vector(eta_pos) ~ lognormal(1,1);
     to_vector(eta_free) ~ std_normal();
}
generated quantities {
    int y_hat[N];
    real gamma_hat[N];
    for (n in 1:N) {
        gamma_hat[n] = Phi_approx(gamma_0 + gamma_1*eta[judge[n],]');
        if (bernoulli_rng(gamma_hat[n]) == 1)
            y_hat[n] = -1;
        else {
            y_hat[n] = bernoulli_logit_rng(beta[case_id[n],] * eta[judge[n],]' - alpha[case_id[n]]);
        }
    }
}
"

## Add constraints
sign_function <- function(x, ...) {
    ifelse(x$.lower > quantile(x$.lower, probs = .05) &
     x$.upper < quantile(x$.upper, probs =  .9), 0,
        ifelse(x$.lower <= quantile(x$.lower, probs = .05), -1,
            ifelse(x$.upper >= quantile(x$.upper, probs = .95), 1, NA)))
}

beta_sign_hurdle <- as.data.frame(fit_2PL_hurdle_2d,
              pars = "beta") %>%
              pivot_longer(starts_with("beta"),
                           names_to = c("n", "dim"),
                           names_pattern = "(.*[0-9])+,([1-2]{1})]",
                           names_transform = list(n = readr::parse_number)) %>%
              mutate(dim = as.numeric(dim)) %>%
              group_by(n, dim) %>%
              group_modify(~data.frame(
                               .lower = quantile(.x$value, .025),
                               .upper = quantile(.x$value, .975)),
                           .keep = TRUE) %>%
              ungroup(n) %>%
              group_map(sign_function) %>%
              as.data.frame(col.names = c("a", "b"))


eta_post_hurdle <- as.data.frame(fit_2PL_hurdle_2d,
              pars = "eta") %>%
              pivot_longer(starts_with("eta"),
                           names_to = c("j", "dim"),
                           names_pattern = "(.*[0-9])+,([1-2]{1})]",
                           names_transform = list(j = readr::parse_number)) %>%
              mutate(dim = as.numeric(dim)) %>%
              group_by(j, dim) %>%
              group_modify(~data.frame(
                                eta = median(.x$value),
                               .lower = quantile(.x$value, .025),
                               .upper = quantile(.x$value, .975)),
                           .keep = TRUE) %>%
              ungroup(j) 

eta_sign_hurdle <- eta_post_hurdle %>%
              group_map(sign_function) %>%
              as.data.frame(col.names = c("a", "b"))

## Compose Data

### Constrain Betas
model_data_2d_constrained <-  model_data %>%
                              mutate(case_id = fct_drop(case_id),
                                     judge = fct_drop(judge)) %>%
                              tidybayes::compose_data(.n_name = n_prefix("N")) %>%
                              append(., list(N_dim = 2))

model_data_2d_constrained$beta_sign <- as.matrix(beta_sign_hurdle)

model_data_2d_constrained$beta_nonzero <- matrix(
    data = 1,
    nrow = model_data_2d_constrained$N_case_id,
    ncol = model_data_2d_constrained$N_dim
)

model_data_2d_constrained$beta_nonzero[1, 1] <- 0
model_data_2d_constrained$beta_nonzero[1:2, 2] <- 0

### Constrain Etas

eta_post_hurdle <- eta_post_hurdle %>% 
select(dim, eta) %>%
pivot_wider(names_from = "dim",
            values_from = "eta",
            values_fn = list) %>%
            unnest_longer(everything())

model_data_2d_constrained$eta_sign <- as.matrix(eta_sign_hurdle)

model_data_2d_constrained$eta_nonzero <- matrix(
    data = 1,
    nrow = model_data_2d_constrained$N_judge,
    ncol = model_data_2d_constrained$N_dim
)

model_data_2d_constrained$eta_nonzero[which(abs(eta_post_hurdle$'1') < .01),
1] <- 0

model_data_2d_constrained$eta_nonzero[which(abs(eta_post_hurdle$'2') < .01),
2] <- 0

saveRDS(model_data_2d_constrained, paste(save,
        "model_data_2d_constrained.RDS", sep = ""))

# Run constrained model

fit_2PL_hurdle_2d_constrained <- stan(
    model_code = stan_code_hurdle_2d_con,
    data = model_data_2d_constrained,
    iter = 2000,
    save_warmup = FALSE
)

saveRDS(fit_2PL_hurdle_2d_constrained, "data/fit_2PL_hurdle_2d_constrained.RDS")