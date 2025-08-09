data {
    int<lower=1> N;
    int<lower=1> N_case_id;
    int<lower=1> B;                                    // number of case types
    int<lower=1> N_judge;
    int<lower=1> G;                                    // number of groups
    int<lower=1> K;                                    // number of covariates to model group mean
    array[N] int<lower=0, upper=1> outcome;            // binomial outcome (judge votes)
    array[N] int<lower=1, upper=N_judge> ii;           //tracks judge for obs. n
    array[N] int<lower=1, upper=N_case_id> jj;         //tracks case for obs. n
    array[N] int<lower=1, upper=B> nn;                 // tracks case type for obs. n 
    array[N_judge] int<lower=1, upper=G> group_id;     // tracks group for judge i 
    matrix[G, K] x;                                    // party, cohort and intercept to model group mean
}

parameters {
    // parameters related to ability scores
    vector[N_judge] theta;               // ability score for each judge
    real<lower=0> sigma_theta;           // homoskedastic variance for all groups of judges
    vector[K] gamma;                     // coef. for mu_theta predictors
    // other parameters
    vector[N_case_id] alpha;            // intercept for each case
    vector[N_case_id] beta;             // discrimination score
    vector[B] mu_beta;
    vector[B] mu_alpha;
    real<lower=0> sigma_beta;
    real<lower=0> sigma_alpha; 
}

transformed parameters {
// group means
  vector[G] mu_theta;
  // calculate mean ability for each group
  vector[G] mu_theta_raw = x*gamma;
  real mu_theta_mean = mean(mu_theta_raw);
  real mu_theta_sd = sd(mu_theta_raw);
  
  // standardize and scale mu_theta draws
  for(g in 1:G){
    mu_theta[g] = (mu_theta_raw[g] - mu_theta_mean) / mu_theta_sd;
}
}
model {
// See "https://mc-stan.org/docs/2_36/stan-users-guide/regression"
// define variables that do not need to sampled
// prior for theta and related parameters
  sigma_theta ~ lognormal(0, 1);
  gamma ~ normal(0,2); 
  // Priors for case-specific parameters
  mu_alpha ~ std_normal();
  mu_beta ~ std_normal();
  sigma_alpha ~ lognormal(-1, 1);
  sigma_beta ~ lognormal(-1, 1);
  
// Stan won't vectorize e.g. `theta ~ normal(mu_theta[group_id], sigma_theta)` :/
for(n in 1:N) {
// sample hierarchical priors for judge ability parameter
  theta[ii[n]] ~ normal(mu_theta[group_id[ii[n]]], sigma_theta);
  alpha[jj[n]] ~ normal(mu_alpha[nn[n]], sigma_alpha);
  beta[jj[n]] ~ normal(mu_beta[nn[n]], sigma_beta);
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
