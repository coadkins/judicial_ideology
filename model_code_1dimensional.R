 # Load packages
library(rstan)
library(tidyverse)
library(tidybayes)
library(bayesplot)
library(janitor)
library(purrr)
library(forcats)
library(vroom)

options(mc.cores = parallel::detectCores())
rstan_options(auto_write = TRUE)

# path for external storage
save <- "/run/media/coryadkins/expansion/bayes_project/"

# Clean Data for Model

fjc_cl_dt <- vroom("/run/media/coryadkins/expansion/adkins_data/cl_fjc_dt_seventh.csv") %>%
             clean_names()

## Identify Relevant Cases Types (including duplicate codes)

cultural_cases <- c("550 Prisoner - civil rights",
                    "441 Civil Rights: Voting",
                    "441 Civil rights voting",
                    "442 Civil rights jobs",
                    "442 Civil Rights: Jobs",
                    "Civil Rights: Jobs",
                    "Civil Rights: Voting",
                    "465 Other immigration actions",
                    "463 Habeas corpus - alien detainee",
                    "462 Naturalization, petition for hearing of denial",
                    "465 Other Immigration Actions",
                    "462 Naturalization Application",
                    "530 Prisoner petitions - habeas corpus",
                    "Habeas Corpus - Alien Detainee",
                     "Naturalization Application")

economic_cases <- c("365 Personal injury - Product liability",
                    "444 Civil rights welfare",
                    "710 Fair Labor Standards Act",
                    "355 Motor vehicle product liability",
                    "893 Environmental matters",
                    "385 Property damage - Product liability",
                    "790 Other labor litigation",
                    "195 Contract product liability",
                    "245 Tort product liability",
                    "Personal Inj. Prod. Liability",
                    "P.I. : Asbestos",
                    "365 Personal Inj. Prod. Liability",
                    "Labor: Other",
                    "Prop. Damage Prod. Liability",
                    "Labor: Fair Standards",
                    "371 Truth in Lending",
                    "160 Stockholders Suits",
                    "Civil Rights: Welfare (DISCONTINUED AFTER 1/1/2012)",
                    "Consumer Credit",
                    "480 Consumer credit",
                    "Civil Rights: Americans with Disabilities - Employment")

civil_rights <- c("550 Prisoner - civil rights",
                    "441 Civil Rights: Voting",
                    "441 Civil rights voting",
                    "442 Civil rights jobs",
                    "442 Civil Rights: Jobs",
                    "Civil Rights: Jobs",
                    "Civil Rights: Voting",
                    "444 Civil rights welfare",
                    "440 Civil rights other",
                    "510 Prisoner petitions - vacate sentence",
                    "560 Prisoner Petitions: Civil Detainee: Conditions of Confinement",
                    "444 Civil Rights: Welfare",
                    "895 Freedom of Information Act",
                    "448 Civil Rights: Education",
                    "530 Habeas Corpus (General) Jurisdiction: Federal Question",
                    "Habeas Corpus (Prison Condition)",
                    "555 Habeas Corpus (Prison Condition)",
                    "445 Civil Rights: Americans with Disabilities - Employment",
                    "446 Civil Rights: Americans with Disabilities - Other",
                    "446 Civil rights ADA other",
                    "Prisoner Petitions - Prison Conditions",
                    "555 Prisoner Petitions - Prison Conditions",
                    "443 Civil Rights: Accomodations",
                    "Freedom of Information Act",
                    "Prisoner: Conditions of Confinement",
                    "Prison Condition",
                    "555 Civil Rights (Prison Condition)",
                    "Death Penalty - Habeas Corpus",
                    "Prisoner: Habeas Corpus")

immigration <- c("465 Other immigration actions",
                    "463 Habeas corpus - alien detainee",
                    "462 Naturalization, petition for hearing of denial",
                    "465 Other Immigration Actions",
                    "462 Naturalization Application")

product_liability <- c("365 Personal injury - Product liability",
                        "355 Motor vehicle product liability",
                        "195 Contract product liability",
                        "245 Tort product liability",
                        "Personal Inj. Prod. Liability",
                        "P.I. : Asbestos",
                        "365 Personal Inj. Prod. Liability",
                        "Personal Injury: Health Care/Pharmaceutical Personal Injury Product Liability",
                        "245 Tort Product Liability",
                        "Tort Product Liability",
                        "355 P.I.: Motor Vehicle Prod. Liability",
                        "P.I.: Motor Vehicle Prod. Liability",
                        "Labor: Railway Labor Act",
                        "740 Labor: Railway Labor Act")

labor_litigation <- c("710 Fair Labor Standards Act",
                      "Labor: Fair Standards",
                      "790 Other labor litigation",
                      "Labor: Other",
                      "Civil Rights: Americans with Disabilities - Employment",
                      "730 Labor Management Report & Disclosure",
                      "Labor: Reporting/Disclosure",
                      "730 Labor: Reporting/Disclosure",
                      "Labor: Labor/Mgt. Relations",
                      "Labor: E.R.I.S.A.",
                      "Labor: Other",
                      "720 Labor: Labor/Mgt. Relations",
                      "660 Occupational safety/health",
                      "791 Labor: E.R.I.S.A.")

consumer_credit <- c("Consumer Credit",
                    "480 Consumer credit",
                    "371 Truth in Lending",
                    "480 Consumer credit")

antitrust <- c("Anti-trust",
               "410 Anti-Trust")

environmental <- c("893 Environmental matters")

all_cases <- c(cultural_cases, labor_litigation, 
               consumer_credit, product_liability,environmental)

## Clean Data for Model

# Recode Responses 0,1 for Conservative, Liberal and -1 for Settlement

# Function

recode_outcome <- function(judgment, disp, ...) {
if (disp %in% c (13)) {
	return(-1)
} else if (judgment == 1 & disp %in% c(2, 3, 8, 9, 6, 3, 14, 5, 17)) {
	return(1)
} else if (judgment == 2 & disp %in% c(2, 3, 8, 9, 6, 3, 14, 5, 17)) {
    return(0)
} else return(NA)
}

## Apply Function

fjc_cl_dt$outcome <- pmap_dbl(fjc_cl_dt, recode_outcome)


## filter only case types that can be interpreted As liberal/conservative 


model_data <- fjc_cl_dt %>%
              filter(nature_of_suit %in% all_cases) %>%
              filter(is.na(outcome) == FALSE) %>%
              select(fjc_id, name_first, name_last, docket, outcome,
                     party_of_appointing_president_1,
                     judgment, disp, nature_of_suit, court_id, court_name_1) %>%
              mutate(party = ifelse(party_of_appointing_president_1 ==
                     "Democratic", 2, 1)) %>% 
              transmute(judge = factor(fjc_id),
                        case_id = factor(docket),
                        outcome = outcome,
                        party_id = factor(party),
                        name = paste(name_first, name_last, sep = " ")) %>%
              na.omit()

# Reduce dataset because of system memory

sample <- sample(fct_unique(model_data$case_id),
                    20000, replace = FALSE)

model_data <- model_data %>%
              filter(case_id %in% sample) %>%
              mutate(case_id = fct_drop(case_id),
                     judge = fct_drop(judge))

# save key for names
judge_key <- model_data %>%
             distinct(judge, name, party_id) %>%
             mutate(judge = as.numeric(judge))

write_csv(judge_key, "judge_key.csv")

# save data
model_data <- model_data %>%
select(judge, case_id, outcome, party_id)

saveRDS(model_data, "model_data.RDS")

# BINARY OUTCOME DATA
# Compose Data for STAN and Removes Settled Cases (Outcome = -1)

model_data_2PL <- model_data %>%
filter(!(outcome == -1)) %>%
mutate(case_id = fct_drop(case_id),
       judge = fct_drop(judge)) %>%
tidybayes::compose_data(.n_name = n_prefix("N"))

saveRDS(model_data_2PL, "model_data_2PL.RDS")

# 1. One Dimensional 2PL Model (unconstrained)

model_data_2PL <- readRDS("model_data_2PL.RDS")

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
## Run Model

fit_2PL <- stan(
    model_code = stan_code_2PL,
    data = model_data_2PL,
    iter = 2000,
    save_warmup = FALSE
)

saveRDS(fit_2PL, paste(save, "fit_2PL.RDS", sep = ""))

# 2. One Dimensional 2PL Model (constrained)

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

## Constrain Betas (1 = positive, 0 = free, -1 = negative)

sign_function <- function(x, lower, upper, ...) {
    ifelse(x$.lower > quantile(x$.lower, probs = lower) &
     x$.upper < quantile(x$.upper, probs =  upper), 0,
        ifelse(x$.lower <= quantile(x$.lower, probs = lower), -1,
            ifelse(x$.upper >= quantile(x$.upper, probs = upper), 1, NA)))
}

beta_sign <- as.data.frame(fit_2PL,
            pars = "beta") %>%
            pivot_longer(starts_with("beta"),
               names_to = c("case_id"),
               names_pattern = "([\\d]+)",
               names_transform = list(
               case_id = readr::parse_number)) %>%
            group_by(case_id) %>%
            group_modify(~data.frame(
                beta = median(.x$value),
               .lower = quantile(.x$value, .025),
               .upper = quantile(.x$value, .975)),
            .keep = TRUE) %>%
            ungroup(case_id) %>%
            mutate(beta_sign = sign_function(., lower = .10, upper = .90)) %>%
            mutate(case_id = factor(case_id)) %>%
            select(beta_sign)

# Constrain Etas

eta_sign <- as.data.frame(fit_2PL,
            pars = "eta") %>%
            pivot_longer(starts_with("eta"),
               names_to = c("judge"),
               names_pattern = "([\\d]{1,3})",
               names_transform = list(
               judge = readr::parse_number)) %>%
            group_by(judge) %>%
            group_modify(~data.frame(
                eta = median(.x$value),
               .lower = quantile(.x$value, .025),
               .upper = quantile(.x$value, .975)),
            .keep = TRUE) %>%
            ungroup(judge) %>%
            mutate(eta_sign = sign_function(., lower = .01, upper = .90)) %>%
         mutate(judge = factor(judge)) %>%
         select(eta_sign)

model_data_constrained <- model_data %>%
                         filter(!(outcome == -1)) %>%
                         mutate(case_id = fct_drop(case_id),
                         judge = fct_drop(judge)) %>%
                         tidybayes::compose_data(.n_name = n_prefix("N")) %>%
                         append(., beta_sign) %>%
                         append(., eta_sign)

saveRDS(model_data_constrained, paste(save, "model_data_constrained.RDS", sep = ""))

## Fit constrained 1 Dimensional 2PL model

fit_2PL_constrained <- stan(
    model_code = stan_code_2PL_constrained,
    data = model_data_constrained,
    iter = 3000,
    save_warmup = FALSE
)

saveRDS(fit_2PL_constrained, paste(save, "fit_2PL_constrained.RDS", sep = ""))

# 3. Hurdle Model 2PL (unconstrained)

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

## Compose Data for 2PL Hurdle
## Three Outcomes

model_data_hurdle <- model_data %>%
       mutate(case_id = fct_drop(case_id),
              judge = fct_drop(judge)) %>%
tidybayes::compose_data(.n_name = n_prefix("N"))

## Fit Data for 2 PL Hurdle

fit_2PL_hurdle <- stan(
    model_code = stan_code_hurdle,
    data = model_data_hurdle,
    iter = 2000,
    save_warmup = FALSE
)

saveRDS(fit_2PL_hurdle, paste(save, "fit_2PL_hurdle.RDS", sep = ""))

# 4. Hurdle Model 1 D (constrained)

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

## Constrain Betas

beta_sign <- as.data.frame(fit_2PL_hurdle,
            pars = "beta") %>%
            pivot_longer(starts_with("beta"),
               names_to = c("case_id"),
               names_pattern = "([\\d]+)",
               names_transform = list(
               case_id = readr::parse_number)) %>%
            group_by(case_id) %>%
            group_modify(~data.frame(
                beta = median(.x$value),
               .lower = quantile(.x$value, .025),
               .upper = quantile(.x$value, .975)),
            .keep = TRUE) %>%
            ungroup(case_id) %>%
            mutate(beta_sign = sign_function(., lower = .10, upper = .70)) %>%
            mutate(case_id = factor(case_id)) %>%
            select(beta_sign)

# Constrain Etas

eta_post <- as.data.frame(fit_2PL_hurdle,
            pars = "eta") %>%
            pivot_longer(starts_with("eta"),
               names_to = c("judge"),
               names_pattern = "([\\d]{1,3})",
               names_transform = list(
               judge = readr::parse_number)) %>%
            group_by(judge) %>%
            group_modify(~data.frame(
                eta = median(.x$value),
               .lower = quantile(.x$value, .025),
               .upper = quantile(.x$value, .975)),
            .keep = TRUE) %>%
            ungroup(judge)

eta_sign <- eta_post %>%
            mutate(eta_sign = sign_function(., lower = .01, upper = .80)) %>%
            mutate(judge = factor(judge)) %>%
            select(eta_sign)

## Compose Data

model_data_hurdle_constrained <- model_data %>%
              mutate(case_id = fct_drop(case_id),
                     judge = fct_drop(judge)) %>%
              tidybayes::compose_data(.n_name = n_prefix("N")) %>%
              append(., list(beta_sign = pull(beta_sign,beta_sign),
                             eta_sign = pull(eta_sign, eta_sign)))

model_data_hurdle_constrained$eta_nonzero <- replicate(n = model_data_hurdle_constrained$N_judge, 
                                             1)
model_data_hurdle_constrained$eta_nonzero[which(abs(eta_post$eta) == min(abs(eta_post$eta)))] <- 0

saveRDS(model_data_hurdle_constrained, paste(save,
        "model_data_hurdle_constrained.RDS", sep = ""))

# Run 2PL Hurdle Constrained

fit_2PL_hurdle_constrained <- stan(
    model_code = stan_code_hurdle_constrained,
    data = model_data_hurdle_constrained,
    iter = 3000,
    save_warmup = FALSE
)

saveRDS(fit_2PL_hurdle_constrained, paste(save,
                                    "fit_2PL_hurdle_constrained.RDS", sep = ""))
