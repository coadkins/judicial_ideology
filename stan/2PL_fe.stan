// generated with brms 2.20.4
functions {
}
data {
  int<lower=1> N;  // total number of observations
  vector[N] Y;  // response variable
  int<lower=1> K_eta;  // number of population-level effects
  matrix[N, K_eta] X_eta;  // population-level design matrix
  int<lower=1> K_alpha;  // number of population-level effects
  matrix[N, K_alpha] X_alpha;  // population-level design matrix
  int<lower=1> K_logbeta;  // number of population-level effects
  matrix[N, K_logbeta] X_logbeta;  // population-level design matrix
  // covariates for non-linear functions
  matrix[N, 26] C_1;
  // data for group-level effects of ID 1
  int<lower=1> N_1;  // number of grouping levels
  int<lower=1> M_1;  // number of coefficients per level
  array[N] int<lower=1> J_1;  // grouping indicator per observation
  // group-level predictor values
  vector[N] Z_1_eta_1;
  // data for group-level effects of ID 2
  int<lower=1> N_2;  // number of grouping levels
  int<lower=1> M_2;  // number of coefficients per level
  array[N] int<lower=1> J_2;  // grouping indicator per observation
  // group-level predictor values
  vector[N] Z_2_alpha_1;
  // data for group-level effects of ID 3
  int<lower=1> N_3;  // number of grouping levels
  int<lower=1> M_3;  // number of coefficients per level
  array[N] int<lower=1> J_3;  // grouping indicator per observation
  // group-level predictor values
  vector[N] Z_3_logbeta_1;
  int prior_only;  // should the likelihood be ignored?
}
transformed data {
}
parameters {
  vector[K_eta] b_eta;  // regression coefficients
  vector[K_alpha] b_alpha;  // regression coefficients
  vector[K_logbeta] b_logbeta;  // regression coefficients
  real<lower=0> sigma;  // dispersion parameter
  vector<lower=0>[M_1] sd_1;  // group-level standard deviations
  array[M_1] vector[N_1] z_1;  // standardized group-level effects
  vector<lower=0>[M_2] sd_2;  // group-level standard deviations
  array[M_2] vector[N_2] z_2;  // standardized group-level effects
  vector<lower=0>[M_3] sd_3;  // group-level standard deviations
  array[M_3] vector[N_3] z_3;  // standardized group-level effects
}
transformed parameters {
  vector[N_1] r_1_eta_1;  // actual group-level effects
  vector[N_2] r_2_alpha_1;  // actual group-level effects
  vector[N_3] r_3_logbeta_1;  // actual group-level effects
  real lprior = 0;  // prior contributions to the log posterior
  r_1_eta_1 = (sd_1[1] * (z_1[1]));
  r_2_alpha_1 = (sd_2[1] * (z_2[1]));
  r_3_logbeta_1 = (sd_3[1] * (z_3[1]));
  lprior += student_t_lpdf(sigma | 3, 0, 2.5)
    - 1 * student_t_lccdf(0 | 3, 0, 2.5);
  lprior += student_t_lpdf(sd_1 | 3, 0, 2.5)
    - 1 * student_t_lccdf(0 | 3, 0, 2.5);
  lprior += student_t_lpdf(sd_2 | 3, 0, 2.5)
    - 1 * student_t_lccdf(0 | 3, 0, 2.5);
  lprior += student_t_lpdf(sd_3 | 3, 0, 2.5)
    - 1 * student_t_lccdf(0 | 3, 0, 2.5);
}
model {
  // likelihood including constants
  if (!prior_only) {
    // initialize linear predictor term
    vector[N] nlp_eta = rep_vector(0.0, N);
    // initialize linear predictor term
    vector[N] nlp_alpha = rep_vector(0.0, N);
    // initialize linear predictor term
    vector[N] nlp_logbeta = rep_vector(0.0, N);
    // initialize non-linear predictor term
    vector[N] mu;
    nlp_eta += X_eta * b_eta;
    nlp_alpha += X_alpha * b_alpha;
    nlp_logbeta += X_logbeta * b_logbeta;
    for (n in 1:N) {
      // add more terms to the linear predictor
      nlp_eta[n] += r_1_eta_1[J_1[n]] * Z_1_eta_1[n];
    }
    for (n in 1:N) {
      // add more terms to the linear predictor
      nlp_alpha[n] += r_2_alpha_1[J_2[n]] * Z_2_alpha_1[n];
    }
    for (n in 1:N) {
      // add more terms to the linear predictor
      nlp_logbeta[n] += r_3_logbeta_1[J_3[n]] * Z_3_logbeta_1[n];
    }
    for (n in 1:N) {
      // compute non-linear predictor values
      mu[n] = (C_1[n] + exp(nlp_logbeta[n]) * nlp_eta[n] + nlp_alpha[n]);
    }
    target += normal_lpdf(Y | mu, sigma);
  }
  // priors including constants
  target += lprior;
  target += std_normal_lpdf(z_1[1]);
  target += std_normal_lpdf(z_2[1]);
  target += std_normal_lpdf(z_3[1]);
}
generated quantities {
}

