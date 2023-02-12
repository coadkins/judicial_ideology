stan_code_2PL <- "data {
    int<lower=0> N;
    int<lower=0> N_case_id;
    int<lower=1> N_judge;
    int<lower=0> outcome[N];
    int<lower=1> case_id[N];
    int<lower=1> judge[N];
    int<lower=1> party_id[N];
    int<lower=1> N_party_id;
}

parameters {
    vector[N_case_id] alpha; // intercepts/difficulty
    vector[N_judge] eta;
// ability score
   vector[N_case_id] beta; // discrimination
}
model {
    vector[N] theta;
    for (n in 1:N) {
        theta[n] = beta[case_id[n]] * eta[judge[n]] - alpha[case_id[n]];
    }

    outcome ~ bernoulli_logit(theta);

    alpha ~ std_normal();
    eta ~ std_normal();
    beta ~ std_normal();
}
"

stan_code_2PL_constrained <- "data {
    int<lower=0> N;
    int<lower=0> N_case_id;
    int<lower=1> N_judge;
    int<lower=0> outcome[N];
    int<lower=1> case_id[N];
    int<lower=1> judge[N];
    int<lower=1> party_id[N];
    int<lower=1> N_party_id;
    int<lower = -1, upper = 1> beta_sign[N_case_id]; //sign
    int<lower = -1, upper =1> eta_sign[N_judge];     // eta sign
}

parameters {
    vector[N_case_id] alpha; // difficulty
    real<lower=0> eta_pos[N_judge];     // ability score (+ constraint)
    real eta_open[N_judge];
    real<lower=0> beta_pos[N_case_id]; // discrimination (+ constraint)
    real beta_open[N_case_id]; // discrimination (no constraint)
}

transformed parameters {
    real beta[N_case_id]; // discrimination
    real eta[N_judge];    // ability
    for (i in 1:N_case_id) {
         if (beta_sign[i] == 0) beta[i] = beta_open[i];
         else if (beta_sign[i] == 1) beta[i] = beta_pos[i];
         else beta[i] = beta_pos[i] * -1;
     }
    for (i in 1:N_judge) {
         if (eta_sign[i] == 0) eta[i] = eta_open[i];
         else if (eta_sign[i] == 1) eta[i] = eta_pos[i];
         else eta[i] = eta_pos[i] * -1;
     }
}

model {
    vector[N] theta;
    for (n in 1:N) {
        theta[n] = beta[case_id[n]] * eta[judge[n]] - alpha[case_id[n]];
    }

    outcome ~ bernoulli_logit(theta);

    alpha ~ std_normal();
    eta_pos ~ lognormal(1,1);
    eta_open ~ std_normal();
    beta_pos ~ lognormal(1,1);
    beta_open ~ std_normal();
}
generated quantities {
    int<lower=0> y_hat[N];
    vector[N] theta_hat;
    for (n in 1:N) {
        theta_hat[n] = beta[case_id[n]] * eta[judge[n]] - alpha[case_id[n]];
    }
      y_hat = bernoulli_logit_rng(theta_hat);
}
"

stan_code_hurdle <- "data {
    int<lower=0> N;
    int<lower=0> N_case_id;
    int<lower=1> N_judge;
    int<lower=-1> outcome[N];
    int<lower=1> case_id[N];
    int<lower=1> judge[N];
    int<lower=1> party_id[N];
    int<lower=1> N_party_id;
}
parameters {
    vector[N_case_id] alpha; // difficulty
    vector[N_judge] eta;     // ability score
    vector[N_case_id] beta;  // discrimination
    real gamma_0;       // settlement
    real gamma_1;       // settlement
}
model {
    for (n in 1:N) {
        if (outcome[n] == -1)
            1 ~ bernoulli(Phi_approx(gamma_0 + gamma_1*eta[judge[n]]));
        else {
            0 ~ bernoulli(Phi_approx(gamma_0 + gamma_1*eta[judge[n]]));
            outcome[n] ~ bernoulli_logit(beta[case_id[n]] * eta[judge[n]] - alpha[case_id[n]]);
        }
    }
    alpha ~ std_normal();
    eta ~ std_normal();
    beta ~ std_normal();
    gamma_0 ~ std_normal();
    gamma_1 ~ std_normal();
}
"

stan_code_hurdle_constrained <- "data {
    int<lower=0> N;
    int<lower=0> N_case_id;
    int<lower=1> N_judge;
    int<lower=-1> outcome[N];
    int<lower=1> case_id[N];
    int<lower=1> judge[N];
    int<lower=1> party_id[N];
    int<lower=1> N_party_id;
    int<lower = -1, upper = 1> beta_sign[N_case_id]; //sign
    int<lower = -1, upper = 1> eta_sign[N_judge];
    int<lower = 0> eta_nonzero[N_judge];
}
parameters {
    vector[N_case_id] alpha;            // difficulty
    real<lower=0> eta_pos[N_judge];     // discrimination (+ constraint)
    real eta_open[N_judge];
    real<lower=0> beta_pos[N_case_id];  // discrimination (+ constraint)
    real beta_open[N_case_id];          // discrimination (no constraint)
    real gamma_0;
    real gamma_1;
}
transformed parameters {
    real beta[N_case_id]; // discrimination
    vector[N_judge] eta;                // ability score
    for (i in 1:N_case_id) {
         if (beta_sign[i] == 0) beta[i] = beta_open[i];
         else if (beta_sign[i] == 1) beta[i] = beta_pos[i];
         else beta[i] = beta_pos[i] * -1;
     }
    for (j in 1:N_judge) {
         if (eta_nonzero[j] == 0) eta[j] = 0;
         else if (eta_sign[j] == 0) eta[j] = eta_open[j];
         else if (eta_sign[j] == 1) eta[j] = eta_pos[j];
         else eta[j] = eta_pos[j] * -1;
    }
}
model {
    for (n in 1:N) {
        if (outcome[n] == -1)
            1 ~ bernoulli(Phi_approx(gamma_0 + gamma_1*eta[judge[n]]));
        else {
            0 ~ bernoulli(Phi_approx(gamma_0 + gamma_1*eta[judge[n]]));
            outcome[n] ~ bernoulli_logit(beta[case_id[n]] * eta[judge[n]] - alpha[case_id[n]]);
        }
    }
    alpha ~ std_normal();
    eta_pos ~ lognormal(1,1);
    eta_open ~ std_normal();
    beta_pos ~ lognormal(1,1);
    beta_open ~ std_normal();
    gamma_0 ~ std_normal();
    gamma_1 ~ std_normal();
}
generated quantities {
    int y_hat[N];
    real gamma_hat[N];
    for (n in 1:N) {
        gamma_hat[n] = Phi_approx(gamma_0 + gamma_1*eta[judge[n]]);
        if (bernoulli_rng(gamma_hat[n]) == 1)
            y_hat[n] = -1;
        else {
            y_hat[n] = bernoulli_logit_rng(beta[case_id[n]] * eta[judge[n]]- alpha[case_id[n]]);
        }
    }
}
"

stan_code_hurdle_2d <- "
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
    int<lower=0> N;
    int<lower=0> N_case_id;
    int<lower=1> N_judge;
    int<lower=-1> outcome[N];
    int<lower=1> case_id[N];
    int<lower=1> judge[N];
    int<lower=1> party_id[N];
    int<lower=1> N_party_id;
    int<lower=1> N_dim;
    matrix[N_case_id, N_dim] beta_sign;
    matrix[N_case_id, N_dim] beta_nonzero;
}
parameters {
    vector[N_case_id] alpha; // difficulty
    matrix[N_judge, N_dim] eta_raw;
    matrix[N_case_id, N_dim] beta_free;
    matrix<lower=0>[N_case_id, N_dim] beta_pos;
    row_vector[N_dim] gamma_1;
    real gamma_0;
}
transformed parameters {
  /* IDENTIFICATION */
  /* Normalize etas [D + D + D(D - 1)/2 restrictions] */
  matrix[N_judge, N_dim] eta = whiten(eta_raw);
  /* Fix and restrict betas [D(D - 1)/2 + D restictions] */
  matrix[N_case_id, N_dim] beta;
  for (i in 1:N_case_id) {
    for (d in 1:N_dim) {
      /* Fix D(D - 1)/2 betas to 0 */
      if (beta_nonzero[i, d] == 0) beta[i, d] = 0;
      /* Restrict sign of D betas */
      else if (beta_sign[i, d] == 0) beta[i, d] = beta_free[i, d];
      else if (beta_sign[i, d] > 0) beta[i, d] = beta_pos[i, d];
      else if (beta_sign[i, d] <0) beta[i,d] = -1 * beta_pos[i,d];
    }
  }
}
model {
    for (n in 1:N) {
        if (outcome[n] == -1)
            1 ~ bernoulli(Phi_approx(gamma_0 + gamma_1*eta[judge[n],]'));
        else {
            0 ~ bernoulli(Phi_approx(gamma_0 + gamma_1*eta[judge[n],]'));
            outcome[n] ~ bernoulli_logit(beta[case_id[n],] * eta[judge[n],]' - alpha[case_id[n]]);
        }
    }
     alpha ~ std_normal();
     gamma_0 ~ std_normal();
     to_vector(gamma_1) ~ std_normal();
     to_vector(beta_free) ~ std_normal();
     to_vector(beta_pos) ~ lognormal(1,1);
     to_vector(eta_raw) ~ std_normal();
}
generated quantities {
    int y_hat[N];
    real gamma_hat[N];
    for (n in 1:N) {
        gamma_hat[n] = Phi_approx(gamma_0 + gamma_1*eta[judge[n],]');
        if (bernoulli_rng(gamma_hat[n]) == 1)
            y_hat[n] = -1;
        else {
            y_hat[n] = bernoulli_logit_rng(beta[case_id[n],] * eta[judge[n],]' - alpha[case_id[n]]);
        }
    }
}
"

stan_code_hurdle_2d_con <- "
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
    int<lower=0> N;
    int<lower=0> N_case_id;
    int<lower=1> N_judge;
    int<lower=-1> outcome[N];
    int<lower=1> case_id[N];
    int<lower=1> judge[N];
    int<lower=1> party_id[N];
    int<lower=1> N_party_id;
    int<lower=1> N_dim;
    matrix[N_case_id, N_dim] beta_sign;
    matrix[N_case_id, N_dim] beta_nonzero;
    matrix[N_judge, N_dim] eta_sign;
    matrix[N_judge, N_dim] eta_nonzero;
}
parameters {
    vector[N_case_id] alpha; // difficulty
    matrix<lower = 0>[N_judge, N_dim] eta_pos;
    matrix[N_judge, N_dim] eta_free;
    matrix[N_case_id, N_dim] beta_free;
    matrix<lower=0>[N_case_id, N_dim] beta_pos;
    row_vector[N_dim] gamma_1;
    real gamma_0;
}
transformed parameters {
  /* IDENTIFICATION */
  /* Fix and restrict betas [D(D - 1)/2 + D restictions] */
  matrix[N_case_id, N_dim] beta;
  matrix[N_judge, N_dim] eta_raw;
  matrix[N_judge, N_dim] eta;
  for (i in 1:N_case_id) {
    for (d in 1:N_dim) {
      /* Fix D(D - 1)/2 betas to 0 */
      if (beta_nonzero[i, d] == 0) beta[i, d] = 0;
      /* Restrict sign of D betas */
      else if (beta_sign[i, d] == 0) beta[i, d] = beta_free[i, d];
      else if (beta_sign[i, d] > 0) beta[i, d] = beta_pos[i, d];
      else if (beta_sign[i, d] <0) beta[i,d] = -1 * beta_pos[i,d];
    }
  }
    for (j in 1:N_judge) {
    for (d in 1:N_dim) {
      if (eta_nonzero[j, d] == 0) eta_raw[j, d] = 0;
      else if (eta_sign[j, d] == 0) eta_raw[j, d] = eta_free[j,d];
      else if (eta_sign[j, d] > 0) eta_raw[j,d] = eta_pos[j,d];
      else if (eta_sign[j,d] < 0) eta_raw[j,d] = -1 * eta_pos[j,d];
    }
   }
  eta = whiten(eta_raw);
}
model {
    for (n in 1:N) {
        if (outcome[n] == -1)
            1 ~ bernoulli(Phi_approx(gamma_0 + gamma_1*eta[judge[n],]'));
        else {
            0 ~ bernoulli(Phi_approx(gamma_0 + gamma_1*eta[judge[n],]'));
            outcome[n] ~ bernoulli_logit(beta[case_id[n],] * eta[judge[n],]' - alpha[case_id[n]]);
        }
    }
     alpha ~ std_normal();
     gamma_0 ~ std_normal();
     to_vector(gamma_1) ~ std_normal();
     to_vector(beta_free) ~ std_normal();
     to_vector(beta_pos) ~ lognormal(1,1);
     to_vector(eta_pos) ~ lognormal(1,1);
     to_vector(eta_free) ~ std_normal();
}
generated quantities {
    int y_hat[N];
    real gamma_hat[N];
    for (n in 1:N) {
        gamma_hat[n] = Phi_approx(gamma_0 + gamma_1*eta[judge[n],]');
        if (bernoulli_rng(gamma_hat[n]) == 1)
            y_hat[n] = -1;
        else {
            y_hat[n] = bernoulli_logit_rng(beta[case_id[n],] * eta[judge[n],]' - alpha[case_id[n]]);
        }
    }
}
"