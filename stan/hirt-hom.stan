data {
    int<lower=1> N;
    int<lower=1> N_case_id;
    int<lower=1> B;                                      // number of case types
    int<lower=1> N_judge;
    int<lower=1> G;                                      // number of groups
    int<lower=1> K;                                      // number of covariates to model group mean
    array[N] int<lower=0, upper=1> outcome;              // binomial outcome (judge votes)
    array[N] int<lower=1, upper=N_judge> ii;             //tracks judge for obs. n
    array[N] int<lower=1, upper=N_case_id> jj;           //tracks case for obs. n
    matrix[G, K] x;                                      // party, cohort and intercept to model group mean
    // These data structures facilitate identification by creating 
    // constraints on mu_theta, gammma and mu_beta
    int<lower=1, upper=G> mu_theta_fixed_idx;
    array[G-1] int<lower=1, upper=G> mu_theta_free_idx;
    int<lower=1, upper=K> gamma_fixed_idx;
    array[K-1] int<lower=1, upper=K> gamma_free_idx;
    int<lower=1, upper=B> mu_beta_pos_idx;
    int<lower=1, upper=B> mu_beta_neg_idx;
    array[B-2] int<lower=1, upper=B> mu_beta_free_idx;
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
    real<lower=0> sigma_theta;           // homoskedastic variance for all groups of judges
    vector[K-1] gamma_free;                // coef for mu predictors 
    vector[B-2] mu_beta_free;            // group means for discrimination
    real<lower=0> mu_beta_pos;           // constraints on mu beta coefs
    real<upper=0> mu_beta_neg;
    // other parameters
    vector[N_case_id] alpha;            // intercept for each case
    vector[N_case_id] beta;             // discrimination score
    vector[B] mu_alpha;
    real<lower=0> sigma_beta;
    real<lower=0> sigma_alpha; 
}

transformed parameters {
// construct gamma, incorporating the fixed coefficient
vector[K] gamma;
gamma[gamma_fixed_idx] = 0;
gamma[gamma_free_idx] = gamma_free;

// construct mu_beta, incorporating constraints
  vector[B] mu_beta;
  mu_beta[mu_beta_pos_idx] = mu_beta_pos;
  mu_beta[mu_beta_neg_idx] = mu_beta_neg;
  mu_beta[mu_beta_free_idx] = mu_beta_free;
  // construct mu_theta
  vector[G] mu_theta;
  // reference group
  mu_theta[mu_theta_fixed_idx] = 0;
  // calculate mean ability for each group
  mu_theta[mu_theta_free_idx] = x[mu_theta_free_idx, ]
    *gamma;
// Non-centered parameterization for theta
  vector[N_judge] theta;
// Stan won't vectorize e.g. `theta ~ normal(mu_theta[group_id], sigma_theta)` :/
// but vectorizing within groups is possible 
  for (g in 1:G) {
    theta[judges_by_group[group_start[g]:group_end[g]]] = 
      mu_theta[g] + sigma_theta * theta_raw[judges_by_group[group_start[g]:group_end[g]]];
  }
}

model {
// See "https://mc-stan.org/docs/2_36/stan-users-guide/regression"
// define variables that do not need to sampled
// prior for theta and related parameters
  theta_raw ~ std_normal();
  sigma_theta ~ lognormal(0, 1);
  gamma_free ~ normal(0,2); 
   // Truncated normal priors for sign constraints
  mu_beta_pos ~ normal(1.0, 0.5) T[0,];    // Truncated below at 0
  mu_beta_neg ~ normal(-1.0, 0.5) T[,-0];  // Truncated above at 0
  // Priors for case-specific parameters
  mu_alpha ~ std_normal();
  mu_beta_free ~ std_normal();
  sigma_alpha ~ lognormal(-1, .5);
  sigma_beta ~ lognormal(-1, 1);
// iterate over case_types for alpha and beta hierarchical prior 
for (b in 1:B) {
  // sample alpha
  alpha[cases_by_type[type_start[b]:type_end[b]]] ~ 
  normal(mu_alpha[b], sigma_alpha);
  // sample beta
  beta[cases_by_type[type_start[b]:type_end[b]]] ~ 
  normal(mu_beta[b], sigma_beta);
}
// iterate over observations for likelihood
// sample likelihood 
  outcome ~ bernoulli_logit(beta[jj] .* theta[ii] + alpha[jj]);
}
generated quantities {
  vector[N] y_hat;
  for (n in 1:N) {
   y_hat[n] = bernoulli_rng(inv_logit((beta[jj[n]] * theta[ii[n]] + alpha[jj[n]])));
  }
}
