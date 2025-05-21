functions {
  matrix center_columns(matrix X) {
    int N = rows(X);
    int K = cols(X);
    matrix[N, K] X_centered;
    
    // First column is a vector of ones
    X_centered[, 1] = rep_vector(1, N);

    for (k in 2:K) {
      real col_mean = mean(to_vector(X[, k])); // Correct way to compute column mean
      for (n in 1:N) {
        X_centered[n, k] = X[n, k] - col_mean; // Subtract mean for each row element
      }
    }
    return X_centered;
  }
}

data {
    int<lower=1> N;
    int<lower=1> N_case_id;
    int<lower=1> N_judge;
    array[N] int<lower=0, upper=1> outcome;
    array[N] int<lower=1, upper=N_judge> ii;           //tracks judge for obs. n
    array[N] int<lower=1, upper=N_case_id> jj;         //tracks case for obs. n
    array[N_judge] int<lower=1, upper=N_judge> judge;  // tracks judge for theta_i
    matrix[N_judge, 3] x;                              //party, cohort and intercept
    matrix[N_judge, 2] z;                              //cohort and intercept
}

transformed data {
    // identification constraints from Zhao 2019
    matrix[3, N_judge] x_centered;
    matrix[2, N_judge] z_centered;
    // sum of gamma' * mu_theta_predictors = 0
    x_centered = center_columns(x)';
    // sum of lambda' * sigma_theta_predictors = 0
    z_centered = center_columns(z)';
    }

parameters {
    vector[N_judge] theta_raw;            // ability score
    vector[N_judge] mu_theta;             // group-level mean of ability score
    vector<lower=0>[N_judge] sigma_theta; // group-level sd of ability scores
    vector[3] gamma;                      // coef. for mu_theta predictors
    vector[2] lambda;                     // coef. for sigma_theta predictors 
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
// implies: beta ~ normal(mu_beta, sigma_beta)
beta = mu_beta + sigma_beta * beta_raw;
alpha = mu_alpha + sigma_alpha * alpha_raw;
for (i in 1:N_judge) {
theta[i] = mu_theta[i] + sigma_theta[i] * theta_raw[i];
}
}

model {
// See "https://mc-stan.org/docs/2_36/stan-users-guide/regression"
// ideology (theta) and related parameters
// Compute group-level parameters per judge
for (i in 1:N_judge) {
    mu_theta[i] ~ normal(gamma' * x_centered[,i], 1);
    sigma_theta[i] ~ lognormal((lambda' * z_centered[,i]/2), 1);
    theta_raw[i] ~ std_normal();
}
    gamma ~ std_normal();;
    lambda ~ std_normal();

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
    vector[N_judge] theta_hat;
    for (n in 1:N) {
     y_hat[n] = bernoulli_rng(inv_logit((beta[jj[n]] * theta[ii[n]] + alpha[jj[n]])));
    }
}
