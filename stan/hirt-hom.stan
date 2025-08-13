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
    // I use these data structures to facilitate identification
    int<lower=1, upper=K> gamma_fixed_idx;
    int<lower=1, upper=K> gamma_pos_idx;
    int<lower=1, upper=K> gamma_neg_idx;
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
    vector[N_judge] theta;               // ability score for each judge
    real<lower=0> sigma_theta;           // homoskedastic variance for all groups of judges
    vector[K] gamma_free;                // coef for mu predictors 
    real<lower=0> gamma_pos;             // constraints on mu coefs
    real gamma_fixed;
    real<upper=0> gamma_neg;
    // other parameters
    vector[N_case_id] alpha;            // intercept for each case
    vector[N_case_id] beta;             // discrimination score
    vector[B] mu_beta;
    vector[B] mu_alpha;
    real<lower=0> sigma_beta;
    real<lower=0> sigma_alpha; 
}

transformed parameters {
// construct gama
  vector[K] gamma;                     // coef. for mu_theta predictors
  {
  int counter = 1;
  for (k in 1:K) {
    if(k==gamma_pos_idx) {
      gamma[k] = gamma_pos; 
  }
    if (k==gamma_neg_idx) {
      gamma[k] = gamma_neg;
  }
    if (k==gamma_fixed_idx) {
      gamma[k] = gamma_fixed;
  } else{
    gamma[k] = gamma_free[counter];
    counter += 1;
} 
  }
}

// construct theta, including constrained values
// group means
  vector[G] mu_theta;
  // calculate mean ability for each group
  vector[G] mu_theta_raw = x*gamma;

  // standardize and scale mu_theta draws
  real mu_theta_mean = mean(mu_theta_raw);
  real mu_theta_sd = sd(mu_theta_raw);

  for(g in 1:G){
    mu_theta[g] = (mu_theta_raw[g] - mu_theta_mean) / mu_theta_sd;
   }
}

model {
// See "https://mc-stan.org/docs/2_36/stan-users-guide/regression"
// define variables that do not need to sampled
// prior for theta and related parameters
  sigma_theta ~ lognormal(0, 1);
  gamma_free ~ normal(0,2); 
  // Very tight prior around 0 for fixed coefficient
  gamma_fixed ~ normal(0, 0.1);
   // Truncated normal priors for sign constraints
  gamma_pos ~ normal(1.0, 0.5) T[0,];  // Truncated below at 0
  gamma_neg ~ normal(-1.0, 0.5) T[,-0];  // Truncated above at 0
  // Priors for case-specific parameters
  mu_alpha ~ std_normal();
  mu_beta ~ std_normal();
  sigma_alpha ~ lognormal(-1, 1);
  sigma_beta ~ lognormal(-1, 1);
  
// Stan won't vectorize e.g. `theta ~ normal(mu_theta[group_id], sigma_theta)` :/
// but vectorizing within groups is possible 

// iterate over groups for theta hierarchical prior
for (g in 1:G) {
  theta[judges_by_group[group_start[g]:group_end[g]]] ~ 
  normal(mu_theta[g], sigma_theta);
}

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
for(n in 1:N) {
// sample likelihood 
  outcome[n] ~ bernoulli_logit(beta[jj[n]] * theta[ii[n]] + alpha[jj[n]]);
 }
}
generated quantities {
  vector[N] y_hat;
  for (n in 1:N) {
   y_hat[n] = bernoulli_rng(inv_logit((beta[jj[n]] * theta[ii[n]] + alpha[jj[n]])));
  }
}
