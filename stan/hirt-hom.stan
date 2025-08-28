data {
  int<lower=1> N;
  int<lower=1> N_case_id;
  int<lower=1> B; // number of case types
  int<lower=1> N_judge;
  int<lower=1> G; // number of groups
  int<lower=1> K_d; // number of covariates to model group mean for dem.
  int<lower=1> K_r; // number of covariates to model group mean for rep.
  array[N] int<lower=0, upper=1> outcome; // binomial outcome (judge votes)
  array[N] int<lower=1, upper=N_judge> ii; //tracks judge for obs. n
  array[N] int<lower=1, upper=N_case_id> jj; //tracks case for obs. n
  // data to calculate the group means for theta
  int<lower=0> N_d; // number of dem cohorts 
  int<lower=0> N_r; // number of rep cohorts
  matrix[N_d, K_d] x_d; // model matrix for dems.
  matrix[N_r, K_r] x_r; // model matrix for reps.
  array[N_d] int<lower=1, upper=G> idx_d; // idx for dems in mu_theta vector
  array[N_r] int<lower=1, upper=G> idx_r; // idx for reps in mu_theta vector
  // These data structures facilitate identification by creating 
  // constraints on mu_theta, gammma and mu_beta
  // I use these data structures to mimic ragged arrays of judges and cases
  // This allows me to vectorize sampling within groups, which is much
  // faster than looping across all judges and individuals
  array[N_judge] int judges_by_group;
  array[G] int group_start;
  array[G] int group_end;
  array[N_case_id] int cases_by_type;
  array[B] int type_start;
  array[B] int type_end;
}
parameters {
  // parameters related to ability scores
  vector[N_judge] theta_raw;
  real<lower=0> mu_sigma_theta; // std. deviation of group means from grand mean
  real<lower=0> sigma_theta; // homoskedastic variance for all groups of judges
  vector[K_d] gamma_d; // predicts mu_theta dem 
  vector[K_r] gamma_r; // predicts mu_theta rep
  // other parameters
  vector[N_case_id] alpha_raw; // intercept for each case
  vector[N_case_id] beta_raw; // discrimination score
  // mu_alpha and mu_beta sampled from bivariate normal distribution
  matrix[B, 2] mu_ab; // raw parameters for bivariate normal
  cov_matrix[2] Sigma; // covariance matrix for bivariate normal
  real<lower=0> sigma_beta;
  real<lower=0> sigma_alpha;
}
transformed parameters {
  // Extract mu_alpha and mu_beta from bivariate normal sampling
  vector[B] mu_alpha = mu_ab[ : , 1];
  vector[B] mu_beta = mu_ab[ : , 2];
  
  // non-centered parametrizaion for case parameters 
  vector[N_case_id] alpha;
  vector[N_case_id] beta_nc;
  vector[N_case_id] beta;
  
  for (b in 1 : B) {
    int start = type_start[b];
    int end = type_end[b];
    alpha[cases_by_type[start : end]] = mu_alpha[b]
                                        + sigma_alpha
                                          * alpha_raw[cases_by_type[start : end]];
    beta_nc[cases_by_type[start : end]] = mu_beta[b]
                                          + sigma_beta
                                            * beta_raw[cases_by_type[start : end]];
  }
  // scale and shift beta to normal(0,1)
  beta = (beta_nc - mean(beta_nc)) / sd(beta_nc);
  
  // construct mu_theta
  vector[G] mu_theta;
  vector[N_d] mu_theta_dem;
  vector[N_r] mu_theta_rep;
  // reference group
  // calculate mean ability for each group
  mu_theta[idx_d] = x_d * gamma_d;
  mu_theta[idx_r] = x_r * gamma_r;
  // Non-centered parameterization for theta
  vector[N_judge] theta_nc;
  // Stan won't vectorize e.g. `theta ~ normal(mu_theta[group_id], sigma_theta)` :/
  // but vectorizing within groups is possible 
  for (g in 1 : G) {
    int start = group_start[g];
    int end = group_end[g];
    theta_nc[judges_by_group[start : end]] = mu_theta[g]
                                             + sigma_theta
                                               * theta_raw[judges_by_group[start : end]];
  }
  // mean center and scale theta 
  vector[N_judge] theta;
  theta = (theta_nc - mean(theta_nc)) / sd(theta_nc);
}
model {
  // See "https://mc-stan.org/docs/2_36/stan-users-guide/regression"
  // prior for theta and related parameters
  theta_raw ~ std_normal();
  sigma_theta ~ lognormal(0, .25);
  gamma_d ~ normal(0, 1);
  gamma_r ~ normal(-1, 1);
  // Priors for case-specific parameters
  //  Bivariate normal prior for mu_alpha and mu_beta
  for (b in 1 : B) {
    mu_ab[b,  : ] ~ multi_normal([0, 0], Sigma);
  }
  // Inverse Wishart prior for covariance matrix
  Sigma ~ inv_wishart(3, diag_matrix(rep_vector(1.0, 2)));
  
  sigma_alpha ~ lognormal(0, .25);
  sigma_beta ~ lognormal(0, .25);
  alpha_raw ~ std_normal();
  beta_raw ~ std_normal();
  // sample likelihood 
  outcome ~ bernoulli_logit(beta[jj] .* theta[ii] + alpha[jj]);
}
generated quantities {
  array[N] int<lower=0, upper=1> y_hat;
  y_hat = bernoulli_rng(inv_logit((beta[jj] .* theta[ii] + alpha[jj])));
}
