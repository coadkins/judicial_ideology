data {
    int<lower=1> N;
    int<lower=1> N_case_id;
    int<lower=1> B;                                 // number of case types
    int<lower=1> N_judge;
    int<lower=1> G;                                 // number of groups
    int<lower=1> K;                                 // number of covariates to model group mean
    array[N] int<lower=0, upper=1> outcome;         // binomial outcome (judge votes)
    array[N] int<lower=1, upper=N_judge> ii;        //tracks judge for obs. n
    array[N] int<lower=1, upper=N_case_id> jj;      //tracks case for obs. n
    array[N_judge] int<lower=1, upper=G> group_id;  // tracks group for judge i 
    array[N_case_id] int<lower=1, upper=B> type;    // tracks type for case j 
    matrix[G, K] x;                                 // party, cohort and intercept to model group mean
}

parameters {
    // parameters related to ability scores
    vector[N_judge] theta_raw;                // ability score for each judge
    real<lower=0> sigma_theta;                // homoskedastic variance for all groups of judges
    vector[K] gamma;                          // coef. for mu_theta predictors
    // other parameters
    vector[N_case_id] alpha_raw;              // intercept for each case
    vector[N_case_id] beta_raw;               // discrimination score
}

transformed parameters {
vector[N_judge] theta;
vector[N_case_id] beta;
vector[N_case_id] alpha;
// group means
vector[G] mu_theta;
vector[B] mu_beta;
vector[B] mu_alpha;
// group sds
real<lower=0> sigma_beta;
real<lower=9> sigma_alpha; 

// Non-centered parametrization for beta
for (i in 1:N_case_id) {
beta[i] = mu_beta[type[i]] + sigma_beta * beta_raw[i];
}

// Non-centered parametrization for alpha
for (i in 1:N_case_id) {
alpha[i] = mu_alpha[type[i]] + sigma_alpha * alpha_raw[i];
}

// calculate mean ability for each group
vector[G] mu_theta_raw = x*gamma;
real mu_theta_mean = mean(mu_theta_raw);
real mu_theta_sd = sd(mu_theta_raw);

// standardize and scale mu_theta draws
for (g in 1:G) {
        mu_theta[g] = (mu_theta[g] - mu_theta_mean) / mu_theta_sd;
    }
    
// Non-centered parametrization for theta
for (i in 1:N_judge) {
theta[i] = mu_theta[group_id[i]] + sigma_theta * theta_raw[i];
}

}
model {
// See "https://mc-stan.org/docs/2_36/stan-users-guide/regression"
// ideology (theta) and related parameters
sigma_theta ~ lognormal(-1, 0.5);
// prior for group level mean predictors
gamma ~ normal(0,2); 
// Priors for case-specific parameters
alpha_raw ~ std_normal();
beta_raw ~ std_normal();
theta_raw ~ std_normal();
mu_alpha ~ std_normal();
mu_beta ~ std_normal();
sigma_alpha ~ cauchy(0, 5);
sigma_beta ~ cauchy(0, 5);

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
