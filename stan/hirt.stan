functions {
    vector standardize(vector x) {
      real mu = mean(x);
      real sigma = sd(x);
      return (x - mu) / sigma;
    }
  }

data {
    int<lower=1> N;
    int<lower=1> N_case_id;
    int<lower=1> N_judge;
    int<lower=1> G;                                 // number of groups
    int<lower=1> K;                                 // number of covariates to model group mean
    int<lower=1> J;                                 // number of covariates to model group sd
    array[N] int<lower=0, upper=1> outcome;
    array[N] int<lower=1, upper=N_judge> ii;        //tracks judge for obs. n
    array[N] int<lower=1, upper=N_case_id> jj;      //tracks case for obs. n
    array[N_judge] int<lower=1, upper=G> group_id;  // tracks group for judge i 
    matrix[G, K] x;                                 // party, cohort and intercept to model group mean
    matrix[G, J] z;                                 // cohort and intercept to model group sd
}

parameters {
    vector[N_judge] theta_raw;            // ability score
    vector[K] gamma;                      // coef. for mu_theta predictors
    vector[J] lambda;                     // coef. for sigma_theta predictors 
    vector[N_case_id] alpha_raw;          // difficulty score
    real mu_alpha;
    real<lower=0> sigma_alpha;
    vector[N_case_id] beta_raw;           // discrimination score
    real mu_beta;
    real<lower=0> sigma_beta;
}

transformed parameters {
// non-centered parameterization   
vector[N_judge] theta;
vector[N_case_id] alpha;
vector[N_case_id] beta;
vector[G] mu_theta;
vector[G] sigma_theta;
  for (g in 1:G) {
    mu_theta[g] = x[g, ]*gamma;  
  }
  // covariance matrix is modeled using Cholesky factor
  // construct the Cholesky factor of  cov. matrix for each group
for (g in 1:G) {
  sigma_theta[g] = exp(z[g]*lambda);
}
// demean and standardize theta
  theta = standardize(theta_raw); 
// implies: beta ~ normal(mu_beta, sigma_beta)
beta = mu_beta + sigma_beta * beta_raw;
alpha = mu_alpha + sigma_alpha * alpha_raw;
}

model {
// See "https://mc-stan.org/docs/2_36/stan-users-guide/regression"
// ideology (theta) and related parameters
// Compute group-level parameters per judge
// prior for group level mean predictors
  gamma ~ normal(0,1); 

// prior for group level SD predictors
  lambda ~ normal(0,1);

  for (i in 1:N_judge) {
    theta_raw[i] ~ normal(mu_theta[group_id[i]],
					 sigma_theta[group_id[i]]); //sample judge-level ability scores
  } 

// Priors for case-specific parameters
alpha_raw ~ std_normal();
beta_raw ~ std_normal();
theta_raw ~ std_normal();
mu_alpha ~ std_normal();
mu_beta ~ std_normal();
sigma_alpha ~ lognormal(1, .25);
sigma_beta ~ lognormal(1, .25);
// Model of outcomes 
for (n in 1:N) {
    outcome[n] ~ bernoulli_logit(beta[jj[n]] *
                       theta[ii[n]] + alpha[jj[n]]);
}
}

generated quantities {
    vector[N] y_hat;
    for (n in 1:N) {
     y_hat[n] = bernoulli_rng(inv_logit((beta[jj[n]] * theta[ii[n]] + alpha[jj[n]])));
    }
}
