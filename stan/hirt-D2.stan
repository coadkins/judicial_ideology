functions {
  matrix whiten(matrix XX) {
    /* De-means and 'whitens' (cov = I) XX */
    matrix[rows(XX), cols(XX)] DM;
    matrix[cols(XX), cols(XX)] SS;
    matrix[cols(XX), cols(XX)] PP;
    matrix[cols(XX), cols(XX)] WW;
    for (d in 1:cols(XX)) {
      DM[, d] = XX[, d] - mean(XX[, d]); /* de-mean each column */
    }
    SS = crossprod(DM) ./ (rows(XX) - 1.0); /* covariance of XX */
    PP = inverse_spd(SS); /* precision of XX */
    WW = cholesky_decompose(PP); /* Cholesky decomposition of precision */
    return DM * WW; /* de-meaned and whitened XX */
  }
}

data {
  int<lower=1> N;                                 // number of outcomes
  int<lower=1> N_case_id;                         // number of cases
  int<lower=1> N_judge;                           // number of judges
  int<lower=1> D;                                 // number of dimensions for theta
  int<lower=1> G;                                 // number of groups
  int<lower=1> K;                                 // number of covariates to model group mean
  int<lower=1> J;                                 // number of covariates to model group sd
  array[N] int<lower=0, upper=1> outcome;
  array[N] int<lower=1, upper=N_judge>  ii;       // tracks judge for obs. n
  array[N]  int<lower=1, upper=N_case_id> jj;     // tracks case for obs. n
  array[N_judge] int<lower=1, upper=G> group_id;  // tracks group for judge i 
  matrix[G, K] x;                                // party, cohort and intercept to model group mean
  matrix[G, J] z;                                // cohort and intercept to model group sd
}

parameters { 
  matrix[N_judge, D] theta_raw;               // ability score 
  matrix[D, K] gamma;                         // coef. for mu_theta predictors 
  matrix[D, J] lambda;                        // coef. for sigma_theta predictors 
  vector[N_case_id] alpha_raw;                // difficulty score 
  real mu_alpha; 
  real<lower=0> sigma_alpha; 
  array[N_case_id] row_vector[D] beta_raw;    // discrimination score
  row_vector[D] mu_beta;                      // common mean for all disc. score 
  cov_matrix[D] sigma_beta;                   // common vcov matrix for all disc. score 
  cholesky_factor_corr[D] L_corr;             // cholesky factor of correlation matrices
}

transformed parameters {
  matrix[N_judge, D] theta;
  vector[N_case_id] alpha;
  array[N_case_id] row_vector[D] beta;
  array[G] row_vector[D]  mu_theta;                  // calc. group-level mean of ability score
  array[G] vector[D] sigma_theta;                // calc. group-level vcov for ability score
  array[G] cholesky_factor_cov[D] L_Sigma_theta; // array of cholesky factors of variance matrices

  for (g in 1:G) {
    mu_theta[g] = x[g, ]*gamma';  
  }
  // covariance matrix is modeled using Cholesky factor
  // construct the Cholesky factor of  cov. matrix for each group
for (g in 1:G) {
  sigma_theta[g] = exp(lambda*z[g]');
  L_Sigma_theta[g] = diag_pre_multiply(sigma_theta[g], L_corr);
}
// "whiten" theta for first group only
  theta = whiten(theta_raw); 
// non-centered parameterization   
// implies: beta ~ normal(mu_beta, sigma_beta)
  for (n in 1:N_case_id) {
beta[n] = mu_beta + beta_raw[n]*sigma_beta;  
  }
alpha = mu_alpha + sigma_alpha * alpha_raw;
}

model {
// Priors for group and judge parameters
// prior for group-level mean predictors
  for (d in 1:D) {
  gamma[d, ] ~ normal(0,1); 
}
// prior for group level SD predictors
  L_corr ~ lkj_corr_cholesky(2.0); 
  for (d in 1:D) {
  lambda[d, ] ~ normal(0,1); 
} 

  for (i in 1:N_judge) {
    theta_raw[i] ~ multi_normal_cholesky(mu_theta[group_id[i], ],
					 L_Sigma_theta[group_id[i]]); //sample judge-level ability scores
  } 

// Priors for case-specific parameters
beta_raw ~ multi_normal(mu_beta, sigma_beta);
alpha_raw ~ std_normal();
mu_alpha ~ std_normal();
sigma_alpha ~ cauchy(0, 2.5);
sigma_beta ~ inv_wishart(D, diag_matrix(rep_vector(1.0, 2))); 
mu_beta ~ std_normal();
// Model of outcomes 
for (n in 1:N) {
    outcome[n] ~ bernoulli_logit(beta[jj[n]] *
                       theta[ii[n]]' + alpha[jj[n]]);
}
}
generated quantities {
    vector[N] y_hat;
    for (n in 1:N) {
     y_hat[n] = bernoulli_rng(inv_logit((beta[jj[n]] * theta[ii[n]]' + alpha[jj[n]])));
    }
}
