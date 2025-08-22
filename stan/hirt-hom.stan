data {
  int<lower=1> N;
  int<lower=1> N_case_id;
  int<lower=1> B; // number of case types
  int<lower=1> N_judge;
  int<lower=1> G; // number of groups
  int<lower=1> K; // number of covariates to model group mean
  array[N] int<lower=0, upper=1> outcome; // binomial outcome (judge votes)
  array[N] int<lower=1, upper=N_judge> ii; //tracks judge for obs. n
  array[N] int<lower=1, upper=N_case_id> jj; //tracks case for obs. n
  matrix[G, K] x; // party, cohort and intercept to model group mean
  // These data structures facilitate identification by creating 
  // constraints on mu_theta, gammma and mu_beta
  int<lower=1, upper=G> mu_theta_ref_group;
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
  real<lower=0> sigma_theta; // homoskedastic variance for all groups of judges
  vector[K] gamma; // coef for mu predictors 
  // other parameters
  vector[N_case_id] alpha_raw; // intercept for each case
  vector[N_case_id] beta_raw; // discrimination score
  vector[B] mu_alpha;
  vector[B] mu_beta;
  real<lower=0> sigma_beta;
  real<lower=0> sigma_alpha;
}
transformed parameters {
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
  // reference group
  // calculate mean ability for each group
  mu_theta = x * gamma;
  // Non-centered parameterization for theta
  vector[N_judge] theta_nc;
  vector[N_judge] theta;
  // Stan won't vectorize e.g. `theta ~ normal(mu_theta[group_id], sigma_theta)` :/
  // but vectorizing within groups is possible 
  for (g in 1 : G) {
    int start = group_start[g];
    int end = group_end[g];
    theta_nc[judges_by_group[start : end]] = mu_theta[g]
                                             + sigma_theta
                                               * theta_raw[judges_by_group[start : end]];
  }
  
  // center theta around reference group
  real theta_shift = mean(theta_nc[judges_by_group[group_start[mu_theta_ref_group] : group_end[mu_theta_ref_group]]]);
  theta = theta_nc - theta_shift;
}
model {
  // See "https://mc-stan.org/docs/2_36/stan-users-guide/regression"
  // prior for theta and related parameters
  theta_raw ~ std_normal();
  sigma_theta ~ lognormal(0, .25);
  gamma ~ normal(-1, 1);
  // Priors for case-specific parameters
  mu_alpha ~ std_normal();
  mu_beta ~ std_normal();
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
