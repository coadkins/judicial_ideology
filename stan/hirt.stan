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
    array[N] int<lower=0, upper=1> outcome;         // binomial outcome (judge votes)
    array[N] int<lower=1, upper=N_judge> ii;        //tracks judge for obs. n
    array[N] int<lower=1, upper=N_case_id> jj;      //tracks case for obs. n
    array[N_judge] int<lower=1, upper=G> group_id;  // tracks group for judge i 
    matrix[G, K] x;                                 // party, cohort and intercept to model group mean
}

parameters {
    vector[N_judge] theta_raw;            // ability score
    real <lower=0> sigma_theta;           // homoskedastic variance for all groups 
    vector[K] gamma;                      // coef. for mu_theta predictors
    vector[N_case_id] alpha;              // intercept
    vector[N_case_id] beta;               // discrimination score
}

transformed parameters {
vector[N_judge] theta;
vector[G] mu_theta;
// calculate mean ability for each group
  for (g in 1:G) {
    mu_theta[g] = x[g, ]*gamma;  
  }

// force sigma_theta to be positive
// demean and standardize theta
  theta = standardize(theta_raw); 
}

model {
// See "https://mc-stan.org/docs/2_36/stan-users-guide/regression"
// ideology (theta) and related parameters
// Compute group-level parameters per judge
// prior for group level mean predictors
  gamma ~ normal(0,1); 

  for (i in 1:N_judge) {
    theta_raw[i] ~ normal(mu_theta[group_id[i]],
					 sigma_theta); //sample judge-level ability scores
  } 

// Priors for case-specific parameters
alpha ~ normal(0,3);
beta ~ normal(0,3);
theta_raw ~ std_normal();
sigma_theta ~ std_normal(); 

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
