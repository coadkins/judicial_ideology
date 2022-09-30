library(tidyverse)
library(tidybayes)
library(bayesplot)
library(rstan)
library(forcats)

options(mc.cores = parallel::detectCores())
rstan_options(auto_write = TRUE)

save <- "/run/media/coryadkins/expansion/bayes_project/"

# Load key for judge names

judge_key <- read_csv("judge_key.csv", col_types = "ffc")

model_data <- readRDS("model_data.RDS")

# Posterior Predictive Checks

color_scheme_set("brightblue")

## 2PL Constrained

fit_2PL_constrained <- readRDS(paste(save, "fit_2PL_constrained.RDS", sep = ""))

y_hat_2PL_constrained <- as.matrix(fit_2PL_constrained, pars = "y_hat")

### vs. 0, 1 outcomes

y_binary <- model_data %>%
            filter(!(outcome == -1)) %>%
            pull(outcome)

ppc_1 <- ppc_hist(y_binary,
                 y_hat_2PL_constrained[1:8, ],
                 binwidth = 1) +
             labs(title = "Posterior Predictive Distribution",
                  subtitle = "2PL Model (Settlements Excluded)") +
             scale_x_continuous(breaks = c(0, 1))

### vs. -1, 0, 1 outcomes

ppc_2 <- ppc_hist(model_data$outcome,
         y_hat_2PL_constrained[1:8, ],
         binwidth = 1) +
         labs(title = "Posterior Predictive Distribution",
              subtitle = "2PL Model (Settlements Included)")

ggsave("ppc_fit_2PL_constrained.png", ppc_1)
ggsave("ppc_2.png", ppc_2)

## 2PL Constrained with Hurdle

fit_2PL_hurdle_constrained <- readRDS(paste(save,
                              "fit_2PL_hurdle_constrained.RDS", sep = ""))

# vs. -1, 0, 1 outcomes

y_hat_2PL_hurdle <- as.matrix(fit_2PL_hurdle_constrained, pars = "y_hat")

ppc_2PL_h <- ppc_hist(model_data$outcome, y_hat_2PL_hurdle[1:8,], binwidth = 1) +
             labs(title = "Posterior Predictive Distribution",
                  subtitle = "2PL Model with Hurdle")

ggsave("ppc_2PL_hurdle_constrained.png", ppc_2PL_h)

# Credibility Intervals for Eta

## One Dimension with No Hurdle

etas_pc <- as.data.frame(fit_2PL_constrained,
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
            mutate(judge = factor(judge, judge)) %>%
            left_join(judge_key) %>%
            mutate(party_id = fct_relabel(factor(party_id),
                  ~ifelse(. == 2, "Democrat", "Republican"))) %>%
            na.omit()

eta_1d <- etas_pc %>%
    ggplot(aes(x = fct_reorder(judge, eta, min),
               y = eta, ymin = .lower, ymax = .upper,
               color = party_id)) +
    geom_pointinterval() +
    scale_color_manual(values = c("firebrick", "dodgerblue3"),
                       name = "Party of Appointing President") +
    coord_flip() +
    theme_minimal() +
    theme(
    legend.position = "none",
    ) +
    facet_wrap(~party_id) +
    xlab("Judge ID") +
    ylab("Eta (95% Credible Interval)") +
    labs(title = "Eta (One Dimension, No Hurdle)")

ggsave("ci_constrained.png", eta_1d)

judge_order <- etas_pc %>%
               arrange(eta) %>%
               select(judge)

## One Dimension with Hurdle

etas_pc_h <- as.data.frame(fit_2PL_hurdle_constrained,
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
            mutate(judge = factor(judge, judge)) %>%
            left_join(judge_key) %>%
            mutate(party_id = fct_relabel(factor(party_id),
                  ~ifelse(. == 2, "Democrat", "Republican"))) %>%
            na.omit()


eta_1d_hurdle <- etas_pc_h %>%
    ggplot(aes(x = fct_reorder(judge, eta, min),
               y = eta, ymin = .lower, ymax = .upper,
               color = party_id)) +
    geom_pointinterval() +
    scale_color_manual(values = c("firebrick", "dodgerblue3"),
                       name = "Party of Appointing President") +
    coord_flip(ylim = c(-3,4)) + 
    theme_minimal() +
    facet_wrap(~party_id) +
    xlab("Judge ID") +
    ylab("Eta (95% Credible Interval)") +
    labs(title = "Eta (One Dimension)")

ggsave("ci_hurdle_constrained.png", eta_1d_hurdle)

## Multidimensional 2PL Constrained with Hurdle

fit_2PL_hurdle_2d_constrained <- readRDS(
    paste(save, "fit_2PL_hurdle_2d_constrained.RDS", sep = ""))

y_hat_hurdle_2d <- as.matrix(fit_2PL_hurdle_2d_constrained,
                             pars = "y_hat")

ppc_2d <- ppc_hist(model_data$outcome, y_hat_hurdle_2d[1:8,], binwidth = 1)

ggsave("ppc_2d.png", ppc_2d)

etas_pc1 <- as.data.frame(fit_2PL_hurdle_2d_constrained,
              pars = "eta") %>%
              pivot_longer(starts_with("eta"),
                           names_to = c("judge", "dim"),
                           names_pattern = "(.*[0-9])+,([1-2]{1})]",
                           names_transform = list(
                            judge = readr::parse_number)) %>%
              mutate(dim = as.numeric(dim)) %>%
              group_by(judge, dim) %>%
              group_modify(~data.frame(
                                eta = median(.x$value),
                               .lower = quantile(.x$value, .025),
                               .upper = quantile(.x$value, .975)),
                           .keep = TRUE) %>%
              ungroup(judge) %>%
              mutate(judge = factor(judge, judge)) %>%
              left_join(judge_key) %>%
              mutate(party_id = fct_relabel(factor(party_id),
                  ~ifelse(. == 2, "Democrat", "Republican"))) %>%
              filter(dim == 1)

eta1_2d <- etas_pc1 %>%
    ggplot(aes(y = eta, x = fct_reorder(judge,eta, min),
              ymin = .lower, ymax = .upper,
              color = party_id)) +
    geom_pointinterval() +
    scale_color_manual(values = c("firebrick", "dodgerblue3"),
                       name = "Party of Appointing President") +
    coord_flip() +
    theme_minimal() +
    facet_wrap(~party_id) +
    xlab("Judge") +
    ylab("Eta (95% Credible Interval)") +
    labs(title = "Eta for Dimension 1")

ggsave("2d_eta1.png", eta1_2d)

### Dimension 2 with Hurdle

etas_pc2 <- as.data.frame(fit_2PL_hurdle_2d_constrained,
              pars = "eta") %>%
              pivot_longer(starts_with("eta"),
                           names_to = c("judge", "dim"),
                           names_pattern = "(.*[0-9])+,([1-2]{1})]",
                           names_transform = list(judge = readr::parse_number)) %>%
              mutate(dim = as.numeric(dim)) %>%
              group_by(judge, dim) %>%
              group_modify(~data.frame(
                                eta = median(.x$value),
                               .lower = quantile(.x$value, .025),
                               .upper = quantile(.x$value, .975)),
                           .keep = TRUE) %>%
              ungroup(judge) %>%
              mutate(judge = factor(judge, judge)) %>%
              left_join(judge_key) %>%
              mutate(party_id = fct_relabel(factor(party_id),
              ~ifelse(. == 2, "Democrat", "Republican"))) %>%
              filter(dim == 2)

eta2_2d <- etas_pc2 %>%
    ggplot(aes(y = eta, x = fct_reorder(judge, eta, min),
               ymin = .lower, ymax = .upper,
               color = party_id)) +
    geom_pointinterval() +
    scale_color_manual(values = c("firebrick", "dodgerblue3"),
                       name = "Party of Appointing President") +
    coord_flip()+
    theme_minimal() +
    theme(
    legend.position = "none",
    ) +
    xlab("Judge ID") +
    ylab("Eta (95% Credible Interval)") +
    labs(title = "Eta for Dimension 2")

ggsave("2d_eta2.png", eta2_2d)

# Trace Plots

plot_3 <- traceplot(fit_rasch_2d, nrow = 2,
                    pars = c("eta[1,1]", "eta[2,1]", "eta[3,1]", "eta[1,2]", "eta[2,2]", "eta[3,2]")) +
          labs(caption = "Iterations = 100", title = "Traceplots for Eta")

ggsave("plot_3.png", plot_3)
