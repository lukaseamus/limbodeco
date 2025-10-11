#### limbodeco: a model of macroalgal decomposition ####
#### Part 2: Statistical model and examples         ####
#### Luka Seamus Wright                             ####

# 1. Prepare data ####
require(tidyverse)
require(magrittr)
require(extraDistr) # R has no native beta prime distribution
set.seed(100)
data <- read_csv("Examples.csv") %>%
  mutate(species = species %>% fct(),
         treatment = treatment %>% fct(),
         p_mean = if_else(p_mean == 0, 1e-5, p_mean)) %>%
  rowwise() %>%
  mutate(p = if( !is.na(p_sd) ) {
    list(
      rbetapr( n , p_mean * ( 1 + p_mean * (1 + p_mean) / p_sd^2 ) , 
                   2 + p_mean * (1 + p_mean) / p_sd^2 )
    )
  } else {
    list( p_mean )
  }) %>%
  unnest(p) %T>%
  print()

require(ggh4x)
data %>%
  ggplot() +
    geom_point(aes(t, p), shape = 16, alpha = 0.2) +
    facet_nested_wrap(~ reference + species + treatment,
                      nest_line = T) +
    theme_minimal()

# 2. Model parameterisation ####
# 2.1 Brouwer 1996 ####
# 2.1.1 Visualisation ####
data %>%
  filter(reference == "Brouwer 1996") %>%
  droplevels() %>%
  ggplot() +
    geom_point(aes(t, p), shape = 16, alpha = 0.5) +
    geom_pointrange(data = . %>% 
                      distinct(t, p_mean, p_sd, treatment),
                    aes(t, p_mean, 
                        ymin = p_mean - p_sd,
                        ymax = p_mean + p_sd)) +
    facet_grid(~ treatment) +
    theme_minimal()

# 2.1.2 Prior simulation ####
# R doesn't have a built-in log1p_exp function
log1p_exp <- function(x) {
  ifelse(
    x > 0, 
    x + log1p(exp(-x)),
    log1p(exp(x))
  )
}

# Constant model
tibble(n = 1:1e3,
       alpha = rnorm( 1e3 , 0 , 0.01 ), 
       mu = rexp( 1e3 , 0.01 ),
       tau = rexp( 1e3 , 10 )) %>%
  expand_grid(t = data %>%
                filter(reference == "Brouwer 1996") %$% 
                seq(min(t), max(t), length.out = 100)) %>%
  mutate(
    p = exp(
      t * alpha - ( alpha + tau ) * (
        log1p_exp( t - mu ) - log1p_exp( -mu )
      )
    )
  ) %>%
  ggplot(aes(t, p, group = n)) +
    geom_hline(yintercept = data %>%
                 filter(reference == "Brouwer 1996") %$%
                 range(p)) +
    geom_line(alpha = 0.05) +
    coord_cartesian(ylim = c(-0.1, 1.5), expand = F, clip = "off") +
    theme_minimal() +
    theme(panel.grid = element_blank())

# Relative model
tibble(n = 1:1e3,
       alpha = rnorm( 1e3 , 0 , 0.01 ), 
       mu = rexp( 1e3 , 0.01 ),
       tau = rexp( 1e3 , 10 )) %>%
  expand_grid(t = data %>%
                filter(reference == "Brouwer 1996") %$% 
                seq(min(t), max(t), length.out = 100)) %>%
  mutate(
    p = exp(
      t * alpha - ( alpha + tau ) * mu / 5 * (
        log1p_exp( 5 / mu * ( t - mu ) ) - log1p_exp( -5 )
      )
    )
  ) %>%
  ggplot(aes(t, p, group = n)) +
    geom_hline(yintercept = data %>%
                 filter(reference == "Brouwer 1996") %$%
                 range(p)) +
    geom_line(alpha = 0.05) +
    coord_cartesian(ylim = c(-0.1, 1.5), expand = F, clip = "off") +
    theme_minimal() +
    theme(panel.grid = element_blank())

# 2.1.3 Stan models ####
require(cmdstanr)
require(here)
Brouwer_constant_model <- here("Stan", "Brouwer_constant.stan") %>% 
  read_file() %>%
  write_stan_file() %>%
  cmdstan_model()

Brouwer_relative_model <- here("Stan", "Brouwer_relative.stan") %>% 
  read_file() %>%
  write_stan_file() %>%
  cmdstan_model()

require(tidybayes)
options(cmdstanr_max_rows = 100)
Brouwer_constant_samples <- Brouwer_constant_model$sample(
          data = data %>%
            filter(reference == "Brouwer 1996") %>% 
            droplevels() %>%
            select(t, p, treatment) %>%
            compose_data(),
          chains = 8,
          parallel_chains = parallel::detectCores(),
          iter_warmup = 1e4,
          iter_sampling = 1e4
        ) %T>%
  print()

Brouwer_relative_samples <- Brouwer_relative_model$sample(
          data = data %>%
            filter(reference == "Brouwer 1996") %>%
            droplevels() %>%
            select(t, p, treatment) %>%
            compose_data(),
          chains = 8,
          parallel_chains = parallel::detectCores(),
          iter_warmup = 1e4,
          iter_sampling = 1e4
        ) %T>%
  print()
# NAs introduced in Rhat and effective sample size estimates
# of p_mu because p_mu = 1 at t = 0 is pre-determined.

# 2.1.4 Model checks ####
# Rhat
Brouwer_constant_samples$summary() %>%
  drop_na() %>% # remove NAs to allow summary of Rhat
  mutate(rhat_check = rhat > 1.001) %>%
  summarise(rhat_1.001 = sum(rhat_check) / length(rhat),
            rhat_mean = mean(rhat),
            rhat_sd = sd(rhat))
# No of rhat above 1.001. rhat = 1.00 ± 0.0000630. Great.

Brouwer_relative_samples$summary() %>%
  drop_na() %>%
  mutate(rhat_check = rhat > 1.001) %>%
  summarise(rhat_1.001 = sum(rhat_check) / length(rhat),
            rhat_mean = mean(rhat),
            rhat_sd = sd(rhat))
# No rhat above 1.001. rhat = 1.00 ± 0.0000581. Great.

# Chains
require(bayesplot)
Brouwer_constant_samples$draws(format = "df") %>%
  mcmc_rank_overlay(pars = c("alpha[1]", "alpha[2]",
                             "mu[1]", "mu[2]", 
                             "tau", "epsilon",
                             "lambda[1]", "lambda[2]",
                             "theta[1]", "theta[2]"))
# Chains are good.

Brouwer_relative_samples$draws(format = "df") %>%
  mcmc_rank_overlay(pars = c("alpha[1]", "alpha[2]",
                             "mu[1]", "mu[2]", 
                             "tau", "epsilon",
                             "lambda[1]", "lambda[2]",
                             "theta[1]", "theta[2]"))
# Chains are good.

# Pairs
Brouwer_constant_samples$draws(format = "df") %>%
  mcmc_pairs(pars = c("alpha[1]", "mu[1]", "tau", 
                      "epsilon", "lambda[1]", "theta[1]"))
Brouwer_constant_samples$draws(format = "df") %>%
  mcmc_pairs(pars = c("alpha[2]", "mu[2]", "tau", 
                      "epsilon", "lambda[2]", "theta[2]"))
# Pairs look ok. But some bimodality and non-identifiability.

Brouwer_relative_samples$draws(format = "df") %>%
  mcmc_pairs(pars = c("alpha[1]", "mu[1]", "tau", 
                      "epsilon", "lambda[1]", "theta[1]"))
Brouwer_relative_samples$draws(format = "df") %>%
  mcmc_pairs(pars = c("alpha[2]", "mu[2]", "tau", 
                      "epsilon", "lambda[2]", "theta[2]"))
# Some weak positive correlation between mu and tau, 
# and weak negative correlation between alpha and mu, 
# but not concerning. Generally looks more stable.

# 2.1.5 Prior-posterior comparison ####
source("functions.R")
Brouwer_constant_prior <- prior_samples(
  model = Brouwer_constant_model,
  data = data %>% 
    filter(reference == "Brouwer 1996") %>%
    droplevels() %>%
    select(t, p, treatment) %>%
    compose_data()
  )

Brouwer_relative_prior <- prior_samples(
  model = Brouwer_relative_model,
  data = data %>% 
    filter(reference == "Brouwer 1996") %>%
    droplevels() %>%
    select(t, p, treatment) %>%
    compose_data()
)

Brouwer_constant_prior %>% 
  prior_posterior_draws(
    posterior_samples = Brouwer_constant_samples,
    group = data %>% 
      filter(reference == "Brouwer 1996") %>%
      droplevels() %>%
      select(treatment),
    parameters = c("alpha[treatment]", "mu[treatment]", 
                   "tau", "epsilon", "lambda[treatment]",
                   "theta[treatment]"),
    format = "long"
    ) %>%
  prior_posterior_plot(group_name = "treatment", ridges = FALSE)
# Some bimodality. mu has a strange sharp posterior.
# Generally looks unstable.

Brouwer_relative_prior %>% 
  prior_posterior_draws(
    posterior_samples = Brouwer_relative_samples,
    group = data %>% 
      filter(reference == "Brouwer 1996") %>%
      droplevels() %>%
      select(treatment),
    parameters = c("alpha[treatment]", "mu[treatment]", 
                   "tau", "epsilon", "lambda[treatment]",
                   "theta[treatment]"),
    format = "long"
  ) %>%
  prior_posterior_plot(group_name = "treatment", ridges = FALSE)
# Looks much more stable.

# 2.1.6 Prediction ####
# Parameter posteriors
Brouwer_constant_prior_posterior <- Brouwer_constant_prior %>% 
  prior_posterior_draws(
    posterior_samples = Brouwer_constant_samples,
    group = data %>% 
      filter(reference == "Brouwer 1996") %>%
      droplevels() %>%
      select(treatment),
    parameters = c("alpha[treatment]", "mu[treatment]", 
                   "tau", "epsilon", "lambda[treatment]",
                   "theta[treatment]"),
    format = "short"
  ) %>% 
  # Since I want only one grouping variable, there is redundancy in distribution.
  filter(!(treatment == "Pre-killed" & 
             distribution == "prior")) %>% # Remove one redundant prior.
  mutate(treatment = if_else(distribution == "prior", # Add Prior to treatment
                             "Prior", treatment) %>% fct()) %>%
  select(-distribution) %T>%
  print()

Brouwer_relative_prior_posterior <- Brouwer_relative_prior %>% 
  prior_posterior_draws(
    posterior_samples = Brouwer_relative_samples,
    group = data %>% 
      filter(reference == "Brouwer 1996") %>%
      droplevels() %>%
      select(treatment),
    parameters = c("alpha[treatment]", "mu[treatment]", 
                   "tau", "epsilon", "lambda[treatment]",
                   "theta[treatment]"),
    format = "short"
  ) %>% 
  # Since I want only one grouping variable, there is redundancy in distribution.
  filter(!(treatment == "Pre-killed" & 
             distribution == "prior")) %>% # Remove one redundant prior.
  mutate(treatment = if_else(distribution == "prior", # Add Prior to treatment
                             "Prior", treatment) %>% fct()) %>%
  select(-distribution) %T>%
  print()

# Predict across predictor range
Brouwer_constant_prediction <- Brouwer_constant_prior_posterior %>%
  spread_continuous(data = data %>%
                      filter(reference == "Brouwer 1996") %>%
                      droplevels(), 
                    predictor_name = "t",
                    group_name = "treatment") %>%
  mutate(
    p_mu = exp(
      t * alpha - ( alpha + tau ) * (
        log1p_exp( t - mu ) -
          log1p_exp( -mu )
      )
    ),
    k = ( alpha + tau ) / ( 1 + exp( t - mu ) ) - tau,
    nu = ( epsilon - theta ) * exp( -lambda * t ) + theta,
    p = rbetapr( n() , p_mu * ( 1 + nu ) , 2 + nu )
  ) %T>%
  print()

Brouwer_relative_prediction <- Brouwer_relative_prior_posterior %>%
  spread_continuous(data = data %>% 
                      filter(reference == "Brouwer 1996") %>%
                      droplevels(), 
                    predictor_name = "t",
                    group_name = "treatment") %>%
  mutate(
    p_mu = exp(
      t * alpha - ( alpha + tau ) * mu / 5 * (
        log1p_exp( 5 / mu * ( t - mu ) ) -
          log1p_exp( -5 )
      )
    ),
    k = ( alpha + tau ) / ( 1 + exp( 5 / mu * ( t - mu ) ) ) - tau,
    nu = ( epsilon - theta ) * exp( -lambda * t ) + theta,
    p = rbetapr( n() , p_mu * ( 1 + nu ) , 2 + nu )
  ) %T>%
  print()
# Some NAs produced in p
Brouwer_relative_prediction %>%
  group_by(treatment) %>%
  summarise(p %>% is.na() %>% any())
# Only Prior is affected indicating that some prior values are impossible 
# to start with, so not concerning.

# Summarise predictions
Brouwer_constant_prediction_summary <- Brouwer_constant_prediction %>%
  group_by(t, treatment) %>%
  median_qi(p_mu, k, nu, p, .width = c(.5, .8, .9)) %T>%
  print()

Brouwer_relative_prediction_summary <- Brouwer_relative_prediction %>%
  drop_na() %>% # NAs affect summary
  group_by(t, treatment) %>%
  median_qi(p_mu, k, nu, p, .width = c(.5, .8, .9)) %T>%
  print()

# Viusalise mean predictions
data %>%
  filter(reference == "Brouwer 1996") %>%
  droplevels() %>%
  ggplot() +
  geom_point(aes(t, p), 
             shape = 16, alpha = 0.5) +
  geom_pointrange(data = . %>% 
                    distinct(t, p_mean, p_sd, treatment),
                  aes(t, p_mean,
                      ymin = p_mean - p_sd,
                      ymax = p_mean + p_sd)) +
  geom_line(data = Brouwer_constant_prediction_summary %>%
              filter(treatment != "Prior"),
            aes(t, p_mu)) +
  geom_ribbon(data = Brouwer_constant_prediction_summary %>%
                filter(treatment != "Prior"),
              aes(t, ymin = p_mu.lower, ymax = p_mu.upper, 
                  alpha = factor(.width))) +
  scale_alpha_manual(values = c(0.5, 0.4, 0.3), guide = "none") +
  facet_grid(~ treatment) +
  theme_minimal()

data %>%
  filter(reference == "Brouwer 1996") %>%
  droplevels() %>%
  ggplot() +
  geom_point(aes(t, p), 
             shape = 16, alpha = 0.5) +
  geom_pointrange(data = . %>% 
                    distinct(t, p_mean, p_sd, treatment),
                  aes(t, p_mean,
                      ymin = p_mean - p_sd,
                      ymax = p_mean + p_sd)) +
  geom_line(data = Brouwer_relative_prediction_summary %>%
              filter(treatment != "Prior"),
            aes(t, p_mu)) +
  geom_ribbon(data = Brouwer_relative_prediction_summary %>%
                filter(treatment != "Prior"),
              aes(t, ymin = p_mu.lower, ymax = p_mu.upper, 
                  alpha = factor(.width))) +
  scale_alpha_manual(values = c(0.5, 0.4, 0.3), guide = "none") +
  facet_grid(~ treatment) +
  theme_minimal()

# Visualise predictions of new observations
data %>%
  filter(reference == "Brouwer 1996") %>%
  droplevels() %>%
  ggplot() +
  geom_point(aes(t, p), 
             shape = 16, alpha = 0.5) +
  geom_pointrange(data = . %>% 
                    distinct(t, p_mean, p_sd, treatment),
                  aes(t, p_mean,
                      ymin = p_mean - p_sd,
                      ymax = p_mean + p_sd)) +
  geom_line(data = Brouwer_constant_prediction_summary %>%
              filter(treatment != "Prior"),
            aes(t, p)) +
  geom_ribbon(data = Brouwer_constant_prediction_summary %>%
                filter(treatment != "Prior"),
              aes(t, ymin = p.lower, ymax = p.upper, 
                  alpha = factor(.width))) +
  scale_alpha_manual(values = c(0.5, 0.4, 0.3), guide = "none") +
  facet_grid(~ treatment) +
  theme_minimal()

data %>%
  filter(reference == "Brouwer 1996") %>%
  droplevels() %>%
  ggplot() +
  geom_point(aes(t, p), 
             shape = 16, alpha = 0.5) +
  geom_pointrange(data = . %>% 
                    distinct(t, p_mean, p_sd, treatment),
                  aes(t, p_mean,
                      ymin = p_mean - p_sd,
                      ymax = p_mean + p_sd)) +
  geom_line(data = Brouwer_relative_prediction_summary %>%
              filter(treatment != "Prior"),
            aes(t, p)) +
  geom_ribbon(data = Brouwer_relative_prediction_summary %>%
                filter(treatment != "Prior"),
              aes(t, ymin = p.lower, ymax = p.upper, 
                  alpha = factor(.width))) +
  scale_alpha_manual(values = c(0.5, 0.4, 0.3), guide = "none") +
  facet_grid(~ treatment) +
  theme_minimal()

# Visualise predictions of k
Brouwer_constant_prediction_summary %>%
  filter(treatment != "Prior") %>%
  ggplot() +
  geom_line(aes(t, k)) +
  geom_ribbon(aes(t, ymin = k.lower, ymax = k.upper, 
                  alpha = factor(.width))) +
  scale_alpha_manual(values = c(0.5, 0.4, 0.3), guide = "none") +
  facet_grid(~ treatment) +
  theme_minimal()

Brouwer_relative_prediction_summary %>%
  filter(treatment != "Prior") %>%
  ggplot() +
  geom_line(aes(t, k)) +
  geom_ribbon(aes(t, ymin = k.lower, ymax = k.upper,
                  alpha = factor(.width))) +
  scale_alpha_manual(values = c(0.5, 0.4, 0.3), guide = "none") +
  facet_grid(~ treatment) +
  theme_minimal()

# Visualise predictions of nu
Brouwer_constant_prediction_summary %>%
  filter(treatment != "Prior") %>%
  ggplot() +
  geom_line(aes(t, nu)) +
  geom_ribbon(aes(t, ymin = nu.lower, ymax = nu.upper, 
                  alpha = factor(.width))) +
  scale_alpha_manual(values = c(0.5, 0.4, 0.3), guide = "none") +
  facet_grid(~ treatment) +
  theme_minimal()

Brouwer_relative_prediction_summary %>%
  filter(treatment != "Prior") %>%
  ggplot() +
  geom_line(aes(t, nu)) +
  geom_ribbon(aes(t, ymin = nu.lower, ymax = nu.upper,
                  alpha = factor(.width))) +
  scale_alpha_manual(values = c(0.5, 0.4, 0.3), guide = "none") +
  facet_grid(~ treatment) +
  theme_minimal()

# While the constant model seems to fit the data better,
# especially the gradual decline in the control, it 
# generally looks less stable, the transition being very 
# jagged, almost like a piecewise model. Let's look at LOO.

# 2.1.7 Leave-one-out cross-validation ####
require(loo)
loo_compare(
  list(
    constant = Brouwer_constant_samples$loo(cores = parallel::detectCores()),
    relative = Brouwer_relative_samples$loo(cores = parallel::detectCores())
  )
) %>%
  as.data.frame() %>%
  rownames_to_column("model") %>%
  as_tibble()
# The models are not distinguishable based on LOO with a mean ± s.e.m.
# difference of 3.17 ± 6.59. Let's try a very different dataset to be sure.

# 2.2 Frontier et al. 2022 ####
# 2.2.1 Visualisation ####
data %>%
  filter(reference == "Frontier et al. 2022") %>%
  droplevels() %>%
  ggplot() +
    geom_point(aes(t, p), shape = 16, alpha = 0.3) +
    facet_grid(treatment ~ species) +
    theme_minimal()

# 2.2.2 Prior simulation ####
# Constant model
tibble(n = 1:1e3,
       alpha = rnorm( 1e3 , 0 , 0.01 ), 
       mu = rgamma( 1e3 , 30^2 / 20^2 , 30 / 20^2 ),
       tau = rexp( 1e3 , 10 )) %>%
  expand_grid(t = data %>%
                filter(reference == "Frontier et al. 2022") %$% 
                seq(min(t), max(t), length.out = 100)) %>%
  mutate(
    p = exp(
      t * alpha - ( alpha + tau ) * (
        log1p_exp( t - mu ) - log1p_exp( -mu )
      )
    )
  ) %>%
  ggplot(aes(t, p, group = n)) +
    geom_hline(yintercept = data %>%
                 filter(reference == "Frontier et al. 2022") %$%
                 range(p)) +
    geom_line(alpha = 0.05) +
    coord_cartesian(ylim = c(-0.1, 1.5), expand = F, clip = "off") +
    theme_minimal() +
    theme(panel.grid = element_blank())

# Relative model
tibble(n = 1:1e3,
       alpha = rnorm( 1e3 , 0 , 0.01 ), 
       mu = rgamma( 1e3 , 30^2 / 20^2 , 30 / 20^2 ),
       tau = rexp( 1e3 , 10 )) %>%
  expand_grid(t = data %>%
                filter(reference == "Frontier et al. 2022") %$% 
                seq(min(t), max(t), length.out = 100)) %>%
  mutate(
    p = exp(
      t * alpha - ( alpha + tau ) * mu / 5 * (
        log1p_exp( 5 / mu * ( t - mu ) ) - log1p_exp( -5 )
      )
    )
  ) %>%
  ggplot(aes(t, p, group = n)) +
    geom_hline(yintercept = data %>%
                 filter(reference == "Frontier et al. 2022") %$%
                 range(p)) +
    geom_line(alpha = 0.05) +
    coord_cartesian(ylim = c(-0.1, 1.5), expand = F, clip = "off") +
    theme_minimal() +
    theme(panel.grid = element_blank())

# 2.2.3 Stan models ####
Frontier_constant_model <- here("Stan", "Frontier_constant.stan") %>% 
  read_file() %>%
  write_stan_file() %>%
  cmdstan_model()

Frontier_relative_model <- here("Stan", "Frontier_relative.stan") %>% 
  read_file() %>%
  write_stan_file() %>%
  cmdstan_model()

Frontier_constant_samples <- Frontier_constant_model$sample(
          data = data %>%
            filter(reference == "Frontier et al. 2022" &
                     t != 0) %>% # t0 = 1 is pre-determined
            droplevels() %>%
            select(t, p, species, treatment) %>%
            compose_data(),
          chains = 8,
          parallel_chains = parallel::detectCores(),
          iter_warmup = 1e4,
          iter_sampling = 1e4
        ) %T>%
  print()

Frontier_relative_samples <- Frontier_relative_model$sample(
          data = data %>%
            filter(reference == "Frontier et al. 2022" &
                     t != 0) %>%
            droplevels() %>%
            select(t, p, species, treatment) %>%
            compose_data(),
          chains = 8,
          parallel_chains = parallel::detectCores(),
          iter_warmup = 1e4,
          iter_sampling = 1e4
        ) %T>%
  print()

# 2.2.4 Model checks ####
# Rhat
Frontier_constant_samples$summary() %>%
  mutate(rhat_check = rhat > 1.001) %>%
  summarise(rhat_1.001 = sum(rhat_check) / length(rhat),
            rhat_mean = mean(rhat),
            rhat_sd = sd(rhat))
# No rhat above 1.001. rhat = 1.00 ± 0.0000899. Great.

Frontier_relative_samples$summary() %>%
  mutate(rhat_check = rhat > 1.001) %>%
  summarise(rhat_1.001 = sum(rhat_check) / length(rhat),
            rhat_mean = mean(rhat),
            rhat_sd = sd(rhat))
# No rhat above 1.001. rhat = 1.00 ± 0.0000822. Great.

# Chains
Frontier_constant_samples$draws(format = "df") %>%
  mcmc_rank_overlay(pars = c("alpha[1]", "alpha[2]",
                             "mu[1,1]", "mu[1,2]", "mu[1,3]",
                             "mu[2,1]", "mu[2,2]", "mu[2,3]",
                             "tau[1]", "tau[2]", "epsilon",
                             "lambda[1,1]", "lambda[1,2]", "lambda[1,3]",
                             "lambda[2,1]", "lambda[2,2]", "lambda[2,3]",
                             "theta[1,1]", "theta[1,2]", "theta[1,3]",
                             "theta[2,1]", "theta[2,2]", "theta[2,3]"))
# Chains look good.

Frontier_relative_samples$draws(format = "df") %>%
  mcmc_rank_overlay(pars = c("alpha[1]", "alpha[2]",
                             "mu[1,1]", "mu[1,2]", "mu[1,3]",
                             "mu[2,1]", "mu[2,2]", "mu[2,3]",
                             "tau[1]", "tau[2]", "epsilon",
                             "lambda[1,1]", "lambda[1,2]", "lambda[1,3]",
                             "lambda[2,1]", "lambda[2,2]", "lambda[2,3]",
                             "theta[1,1]", "theta[1,2]", "theta[1,3]",
                             "theta[2,1]", "theta[2,2]", "theta[2,3]"))
# Chains look good.

# Pairs
Frontier_constant_samples$draws(format = "df") %>%
  mcmc_pairs(pars = c("alpha[1]", "mu[1,1]", "tau[1]", 
                      "epsilon", "lambda[1,1]", "theta[1,1]"))
Frontier_constant_samples$draws(format = "df") %>%
  mcmc_pairs(pars = c("alpha[1]", "mu[1,2]", "tau[1]", 
                      "epsilon", "lambda[1,2]", "theta[1,2]"))
Frontier_constant_samples$draws(format = "df") %>%
  mcmc_pairs(pars = c("alpha[1]", "mu[1,3]", "tau[1]", 
                      "epsilon", "lambda[1,3]", "theta[1,3]"))

Frontier_constant_samples$draws(format = "df") %>%
  mcmc_pairs(pars = c("alpha[2]", "mu[2,1]", "tau[2]", 
                      "epsilon", "lambda[2,1]", "theta[2,1]"))
Frontier_constant_samples$draws(format = "df") %>%
  mcmc_pairs(pars = c("alpha[2]", "mu[2,2]", "tau[2]", 
                      "epsilon", "lambda[2,2]", "theta[2,2]"))
Frontier_constant_samples$draws(format = "df") %>%
  mcmc_pairs(pars = c("alpha[2]", "mu[2,3]", "tau[2]", 
                      "epsilon", "lambda[2,3]", "theta[2,3]"))
# Pairs look fine.

Frontier_relative_samples$draws(format = "df") %>%
  mcmc_pairs(pars = c("alpha[1]", "mu[1,1]", "tau[1]", 
                      "epsilon", "lambda[1,1]", "theta[1,1]"))
Frontier_relative_samples$draws(format = "df") %>%
  mcmc_pairs(pars = c("alpha[1]", "mu[1,2]", "tau[1]", 
                      "epsilon", "lambda[1,2]", "theta[1,2]"))
Frontier_relative_samples$draws(format = "df") %>%
  mcmc_pairs(pars = c("alpha[1]", "mu[1,3]", "tau[1]", 
                      "epsilon", "lambda[1,3]", "theta[1,3]"))

Frontier_relative_samples$draws(format = "df") %>%
  mcmc_pairs(pars = c("alpha[2]", "mu[2,1]", "tau[2]", 
                      "epsilon", "lambda[2,1]", "theta[2,1]"))
Frontier_relative_samples$draws(format = "df") %>%
  mcmc_pairs(pars = c("alpha[2]", "mu[2,2]", "tau[2]", 
                      "epsilon", "lambda[2,2]", "theta[2,2]"))
Frontier_relative_samples$draws(format = "df") %>%
  mcmc_pairs(pars = c("alpha[2]", "mu[2,3]", "tau[2]", 
                      "epsilon", "lambda[2,3]", "theta[2,3]"))
# In some cases there is more correlation between mu and tau
# but posteriors generally look smoother.

# 2.2.5 Prior-posterior comparison ####
Frontier_constant_prior <- prior_samples(
  model = Frontier_constant_model,
  data = data %>%
    filter(reference == "Frontier et al. 2022" &
             t != 0) %>%
    droplevels() %>%
    select(t, p, species, treatment) %>%
    compose_data()
  )

Frontier_relative_prior <- prior_samples(
  model = Frontier_relative_model,
  data = data %>%
    filter(reference == "Frontier et al. 2022" &
             t != 0) %>%
    droplevels() %>%
    select(t, p, species, treatment) %>%
    compose_data()
)

Frontier_constant_prior %>% 
  prior_posterior_draws(
    posterior_samples = Frontier_constant_samples,
    group = data %>%
      filter(reference == "Frontier et al. 2022") %>%
      droplevels() %>%
      select(species, treatment),
    parameters = c("alpha[species]", "mu[species, treatment]", 
                   "tau[species]", "epsilon", "lambda[species, treatment]", 
                   "theta[species, treatment]"),
    format = "long"
  ) %T>% {
    prior_posterior_plot(., group_name = "species", ridges = FALSE) %>%
      print()
    } %>%
  prior_posterior_plot(group_name = "treatment", ridges = FALSE)
# Some strange sharp posteriors. But generally looks acceptable.

Frontier_relative_prior %>% 
  prior_posterior_draws(
    posterior_samples = Frontier_relative_samples,
    group = data %>%
      filter(reference == "Frontier et al. 2022") %>%
      droplevels() %>%
      select(species, treatment),
    parameters = c("alpha[species]", "mu[species, treatment]", 
                   "tau[species]", "epsilon", "lambda[species, treatment]", 
                   "theta[species, treatment]"),
    format = "long"
  ) %T>% {
    prior_posterior_plot(., group_name = "species", ridges = FALSE) %>%
      print()
    } %>%
  prior_posterior_plot(group_name = "treatment", ridges = FALSE)
# Looks more stable.

# 2.2.6 Prediction ####
# Parameter posteriors
Frontier_constant_prior_posterior <- Frontier_constant_prior %>% 
  prior_posterior_draws(
    posterior_samples = Frontier_constant_samples,
    group = data %>%
      filter(reference == "Frontier et al. 2022") %>%
      droplevels() %>%
      select(species, treatment),
    parameters = c("alpha[species]", "mu[species, treatment]", 
                   "tau[species]", "epsilon", "lambda[species, treatment]", 
                   "theta[species, treatment]"),
    format = "short"
  ) %>% 
  filter(!(treatment %in% c("1.5m", "3m") &
             distribution == "prior")) %>% 
  mutate(treatment = if_else(distribution == "prior",
                             "Prior", treatment) %>% fct()) %>%
  select(-distribution) %T>%
  print()

Frontier_relative_prior_posterior <- Frontier_relative_prior %>% 
  prior_posterior_draws(
    posterior_samples = Frontier_relative_samples,
    group = data %>%
      filter(reference == "Frontier et al. 2022") %>%
      droplevels() %>%
      select(species, treatment),
    parameters = c("alpha[species]", "mu[species, treatment]", 
                   "tau[species]", "epsilon", "lambda[species, treatment]", 
                   "theta[species, treatment]"),
    format = "short"
  ) %>% 
  filter(!(treatment %in% c("1.5m", "3m") &
             distribution == "prior")) %>% 
  mutate(treatment = if_else(distribution == "prior",
                             "Prior", treatment) %>% fct()) %>%
  select(-distribution) %T>%
  print()

# Predict across predictor range
Frontier_constant_prediction <- Frontier_constant_prior_posterior %>%
  spread_continuous(data = data %>% # Note that I include t0
                      filter(reference == "Frontier et al. 2022") %>%
                      droplevels(), 
                    # Same t range for all variables so no grouping needed
                    predictor_name = "t") %>%
  mutate(
    p_mu = exp(
      t * alpha - ( alpha + tau ) * (
        log1p_exp( t - mu ) -
          log1p_exp( -mu )
      )
    ),
    k = ( alpha + tau ) / ( 1 + exp( t - mu ) ) - tau,
    nu = ( epsilon - theta ) * exp( -lambda * t ) + theta,
    p = rbetapr( n() , p_mu * ( 1 + nu ) , 2 + nu )
  ) %T>%
  print()

Frontier_relative_prediction <- Frontier_relative_prior_posterior %>%
  spread_continuous(data = data %>% 
                      filter(reference == "Frontier et al. 2022") %>%
                      droplevels(),
                    predictor_name = "t") %>%
  mutate(
    p_mu = exp(
      t * alpha - ( alpha + tau ) * mu / 5 * (
        log1p_exp( 5 / mu * ( t - mu ) ) -
          log1p_exp( -5 )
      )
    ),
    k = ( alpha + tau ) / ( 1 + exp( 5 / mu * ( t - mu ) ) ) - tau,
    nu = ( epsilon - theta ) * exp( -lambda * t ) + theta,
    p = rbetapr( n() , p_mu * ( 1 + nu ) , 2 + nu )
  ) %T>%
  print()

# Summarise predictions
Frontier_constant_prediction_summary <- Frontier_constant_prediction %>%
  group_by(t, species, treatment) %>%
  mean_qi(p_mu, k, nu, p, .width = c(.5, .8, .9)) %T>%
  print()

Frontier_relative_prediction_summary <- Frontier_relative_prediction %>%
  group_by(t, species, treatment) %>%
  median_qi(p_mu, k, nu, p, .width = c(.5, .8, .9)) %T>%
  print()

# Visualise mean predictions
data %>%
  filter(reference == "Frontier et al. 2022") %>%
  droplevels() %>%
  ggplot() +
    geom_point(aes(t, p), shape = 16, alpha = 0.3) +
    geom_line(data = Frontier_constant_prediction_summary %>%
                filter(treatment != "Prior"),
              aes(t, p_mu)) +
    geom_ribbon(data = Frontier_constant_prediction_summary %>%
                  filter(treatment != "Prior"),
                aes(t, ymin = p_mu.lower, ymax = p_mu.upper, 
                    alpha = factor(.width))) +
    scale_alpha_manual(values = c(0.5, 0.4, 0.3), guide = "none") +
    facet_grid(treatment ~ species) +
    theme_minimal()

data %>%
  filter(reference == "Frontier et al. 2022") %>%
  droplevels() %>%
  ggplot() +
    geom_point(aes(t, p), shape = 16, alpha = 0.3) +
    geom_line(data = Frontier_relative_prediction_summary %>%
                filter(treatment != "Prior"),
              aes(t, p_mu)) +
    geom_ribbon(data = Frontier_relative_prediction_summary %>%
                  filter(treatment != "Prior"),
                aes(t, ymin = p_mu.lower, ymax = p_mu.upper, 
                    alpha = factor(.width))) +
    scale_alpha_manual(values = c(0.5, 0.4, 0.3), guide = "none") +
    facet_grid(treatment ~ species) +
    theme_minimal()

# Visualise predictions for new observations
data %>%
  filter(reference == "Frontier et al. 2022") %>%
  droplevels() %>%
  ggplot() +
    geom_point(aes(t, p), shape = 16, alpha = 0.3) +
    geom_line(data = Frontier_constant_prediction_summary %>%
                filter(treatment != "Prior"),
              aes(t, p)) +
    geom_ribbon(data = Frontier_constant_prediction_summary %>%
                  filter(treatment != "Prior"),
                aes(t, ymin = p.lower, ymax = p.upper, 
                    alpha = factor(.width))) +
    scale_alpha_manual(values = c(0.5, 0.4, 0.3), guide = "none") +
    facet_grid(treatment ~ species) +
    theme_minimal()

data %>%
  filter(reference == "Frontier et al. 2022") %>%
  droplevels() %>%
  ggplot() +
    geom_point(aes(t, p), shape = 16, alpha = 0.3) +
    geom_line(data = Frontier_relative_prediction_summary %>%
                filter(treatment != "Prior"),
              aes(t, p)) +
    geom_ribbon(data = Frontier_relative_prediction_summary %>%
                  filter(treatment != "Prior"),
                aes(t, ymin = p.lower, ymax = p.upper, 
                    alpha = factor(.width))) +
    scale_alpha_manual(values = c(0.5, 0.4, 0.3), guide = "none") +
    facet_grid(treatment ~ species) +
    theme_minimal()

# Visualise predictions of k
Frontier_constant_prediction_summary %>%
  filter(treatment != "Prior") %>%
  ggplot() +
  geom_line(aes(t, k)) +
  geom_ribbon(aes(t, ymin = k.lower, ymax = k.upper, 
                  alpha = factor(.width))) +
  scale_alpha_manual(values = c(0.5, 0.4, 0.3), guide = "none") +
  facet_grid(treatment ~ species) +
  theme_minimal()

Frontier_relative_prediction_summary %>%
  filter(treatment != "Prior") %>%
  ggplot() +
  geom_line(aes(t, k)) +
  geom_ribbon(aes(t, ymin = k.lower, ymax = k.upper,
                  alpha = factor(.width))) +
  scale_alpha_manual(values = c(0.5, 0.4, 0.3), guide = "none") +
  facet_grid(treatment ~ species) +
  theme_minimal()

# Visualise predictions of nu
Frontier_constant_prediction_summary %>%
  filter(treatment != "Prior") %>%
  ggplot() +
  geom_line(aes(t, nu)) +
  geom_ribbon(aes(t, ymin = nu.lower, ymax = nu.upper, 
                  alpha = factor(.width))) +
  scale_alpha_manual(values = c(0.5, 0.4, 0.3), guide = "none") +
  facet_grid(treatment ~ species) +
  theme_minimal()

Frontier_relative_prediction_summary %>%
  filter(treatment != "Prior") %>%
  ggplot() +
  geom_line(aes(t, nu)) +
  geom_ribbon(aes(t, ymin = nu.lower, ymax = nu.upper,
                  alpha = factor(.width))) +
  scale_alpha_manual(values = c(0.5, 0.4, 0.3), guide = "none") +
  facet_grid(treatment ~ species) +
  theme_minimal()

# The constant model generally looks less stable, and the
# relative model seemingly gives better predictions. 
# Let's look at LOO.

# 2.2.7 Leave-one-out cross-validation ####
loo_compare(
  list(
    constant = Frontier_constant_samples$loo(cores = parallel::detectCores()),
    relative = Frontier_relative_samples$loo(cores = parallel::detectCores())
  )
) %>%
  as.data.frame() %>%
  rownames_to_column("model") %>%
  as_tibble()
# The relative model wins here too, with a difference of 4.57 ± 3.33. This
# is still marginal but clearer than the previous LOO comparison.

# Clean up
rm(list = ls(pattern = "^(Brouwer|Frontier)"))

# 3. Examples ####
# 3.1 Birch et al. 1983 ####
# Birch et al. 1983 is a difficult one. I first tried modelling as is but
# (1) the control treatment has too strong a suggestion of an asymptote > 0
# and (2) there is an initial drop of ~12% from t0 to the first timepoint.
# These characteristics essentially caused massive sampling issues because
# 1 suggests that there should be an additional offset parameter and 2
# suggests that k is more negative to begin with. Given that the paper
# states there was essentially no decomposition in the first phase (up to
# timepoint 4), I applied a +12% offset. This solved issue 2 and resulted
# in a decent model (see below) but issue 1 remains. I have decided to 
# abandon this example because I failed but also because it is somewhat 
# of an oddity among macroalgal decomposition stories.

# 3.1.1 Visualisation ####
data %>%
  filter(reference == "Birch et al. 1983" & t != 0) %>%
  droplevels() %>%
  mutate(p = p + 0.12, # +12% offset
         p_mean = p_mean + 0.12) %>%
  ggplot() +
  geom_point(aes(t, p), shape = 16, alpha = 0.5) +
  geom_pointrange(data = . %>% 
                    distinct(t, p_mean, p_sd, treatment),
                  aes(t, p_mean, 
                      ymin = p_mean - p_sd,
                      ymax = p_mean + p_sd)) +
  facet_grid(~ treatment) +
  theme_minimal()

# 3.1.2 Prior simulation ####
tibble(n = 1:1e3,
       alpha = rnorm( 1e3 , 0 , 0.005 ), 
       mu = rgamma( 1e3 , 150^2 / 100^2 , 150 / 100^2 ),
       tau = rgamma( 1e3 , 0.1^2 / 0.05^2 , 0.1 / 0.05^2 )) %>%
  expand_grid(t = data %>%
                filter(reference == "Birch et al. 1983") %$% 
                seq(min(t), max(t), length.out = 100)) %>%
  mutate(
    p = exp(
      t * alpha - ( alpha + tau ) * mu / 5 * (
        log1p_exp( 5 / mu * ( t - mu ) ) - log1p_exp( -5 )
      )
    )
  ) %>%
  ggplot(aes(t, p, group = n)) +
    geom_hline(yintercept = data %>%
                 filter(reference == "Birch et al. 1983") %$%
                 range(p)) +
    geom_line(alpha = 0.05) +
    coord_cartesian(ylim = c(-0.1, 1.5), expand = F, clip = "off") +
    theme_minimal() +
    theme(panel.grid = element_blank())

# 3.1.3 Stan model ####
Birch_model <- here("Stan", "Birch.stan") %>% 
  read_file() %>%
  write_stan_file() %>%
  cmdstan_model()

Birch_samples <- Birch_model$sample(
          data = data %>% # t0 = 1 is predetermined
            filter(reference == "Birch et al. 1983" & t != 0) %>% 
            droplevels() %>%
            select(t, p, treatment) %>%
            mutate(p = p + 0.12) %>% # apply offset
            compose_data(),
          chains = 8,
          parallel_chains = parallel::detectCores(),
          iter_warmup = 1e4,
          iter_sampling = 1e4
        ) %T>%
  print()

# 3.1.4 Model checks ####
# Rhat
Birch_samples$summary() %>%
  mutate(rhat_check = rhat > 1.001) %>%
  summarise(rhat_1.001 = sum(rhat_check) / length(rhat),
            rhat_mean = mean(rhat),
            rhat_sd = sd(rhat))
# No rhat above 1.001. rhat = 1.00 ± 0.0000755. Great.

# Chains
Birch_samples$draws(format = "df") %>%
  mcmc_rank_overlay()
# Chains are good.

# Pairs
Birch_samples$draws(format = "df") %>%
  mcmc_pairs(pars = c("alpha[1]", "mu[1]", "tau[1]"))
Birch_samples$draws(format = "df") %>%
  mcmc_pairs(pars = c("alpha[2]", "mu[2]", "tau[1]"))
# Looks fine.

# 3.1.5 Prior-posterior comparison ####
Birch_prior <- prior_samples(
  model = Birch_model,
  data = data %>%
    filter(reference == "Birch et al. 1983" & t != 0) %>% 
    droplevels() %>%
    select(t, p, treatment) %>%
    mutate(p = p + 0.12) %>%
    compose_data()
  )

Birch_prior %>% 
  prior_posterior_draws(
    posterior_samples = Birch_samples,
    group = data %>% 
      filter(reference == "Birch et al. 1983") %>%
      droplevels() %>%
      select(treatment),
    parameters = c("alpha[treatment]", "mu[treatment]", 
                   "tau[treatment]", "epsilon", "lambda[treatment]",
                   "theta[treatment]"),
    format = "long"
    ) %>%
  prior_posterior_plot(group_name = "treatment", ridges = FALSE)

# 3.1.6 Prediction ####
# Parameter posteriors
Birch_prior_posterior <- Birch_prior %>% 
  prior_posterior_draws(
    posterior_samples = Birch_samples,
    group = data %>% 
      filter(reference == "Birch et al. 1983") %>%
      droplevels() %>%
      select(treatment),
    parameters = c("alpha[treatment]", "mu[treatment]", 
                   "tau[treatment]", "epsilon", "lambda[treatment]",
                   "theta[treatment]"),
    format = "short"
  ) %>% 
  filter(!(treatment == "Pre-killed" & distribution == "prior")) %>%
  mutate(
    treatment = if_else(
      distribution == "prior", "Prior", treatment
    ) %>% fct()
  ) %>%
  select(-distribution) %T>%
  print()

# Predict across predictor range
Birch_prediction <- Birch_prior_posterior %>%
  spread_continuous(data = data %>% 
                      filter(reference == "Birch et al. 1983") %>%
                      droplevels(), 
                    predictor_name = "t", length = 200,
                    group_name = "treatment") %>%
  mutate(
    p_mu = exp(
      t * alpha - ( alpha + tau ) * mu / 5 * (
        log1p_exp( 5 / mu * ( t - mu ) ) -
          log1p_exp( -5 )
      )
    ),
    k = ( alpha + tau ) / ( 1 + exp( 5 / mu * ( t - mu ) ) ) - tau,
    nu = ( epsilon - theta ) * exp( -lambda * t ) + theta,
    p = rbetapr( n() , p_mu * ( 1 + nu ) , 2 + nu )
  ) %T>%
  print()

# Summarise predictions
Birch_prediction_summary <- Birch_prediction %>%
  group_by(t, treatment) %>%
  median_qi(p_mu, k, nu, p, .width = c(.5, .8, .9)) %T>%
  print()

# Clean up raw predictions
rm(Birch_prediction)

# Viusalise mean predictions
data %>%
  filter(reference == "Birch et al. 1983" & t != 0) %>%
  droplevels() %>%
  mutate(p_mean = p_mean + 0.12) %>%
  ggplot() +
  geom_pointrange(data = . %>% 
                    distinct(t, p_mean, p_sd, treatment),
                  aes(t, p_mean,
                      ymin = p_mean - p_sd,
                      ymax = p_mean + p_sd)) +
  geom_line(data = Birch_prediction_summary %>%
              filter(treatment != "Prior"),
            aes(t, p_mu)) +
  geom_ribbon(data = Birch_prediction_summary %>%
                filter(treatment != "Prior"),
              aes(t, ymin = p_mu.lower, ymax = p_mu.upper, 
                  alpha = factor(.width))) +
  scale_alpha_manual(values = c(0.5, 0.4, 0.3), guide = "none") +
  facet_grid(~ treatment) +
  theme_minimal()

# Visualise predictions of new observations
data %>%
  filter(reference == "Birch et al. 1983" & t != 0) %>%
  droplevels() %>%
  mutate(p_mean = p_mean + 0.12) %>%
  ggplot() +
  geom_pointrange(data = . %>% 
                    distinct(t, p_mean, p_sd, treatment),
                  aes(t, p_mean, colour = treatment,
                      ymin = p_mean - p_sd,
                      ymax = p_mean + p_sd)) +
  geom_line(data = Birch_prediction_summary %>%
              filter(treatment != "Prior"),
            aes(t, p, colour = treatment)) +
  geom_ribbon(data = Birch_prediction_summary %>%
                filter(treatment != "Prior"),
              aes(t, ymin = p.lower, ymax = p.upper, 
                  alpha = factor(.width), fill = treatment)) +
  scale_alpha_manual(values = c(0.5, 0.4, 0.3), guide = "none") +
  theme_minimal()

# Visualise predictions of k
Birch_prediction_summary %>%
  filter(treatment != "Prior") %>%
  ggplot() +
  geom_line(aes(t, k)) +
  geom_ribbon(aes(t, ymin = k.lower, ymax = k.upper, 
                  alpha = factor(.width))) +
  scale_alpha_manual(values = c(0.5, 0.4, 0.3), guide = "none") +
  facet_grid(~ treatment) +
  theme_minimal()

# Visualise predictions of nu
Birch_prediction_summary %>%
  filter(treatment != "Prior") %>%
  ggplot() +
  geom_line(aes(t, nu)) +
  geom_ribbon(aes(t, ymin = nu.lower, ymax = nu.upper, 
                  alpha = factor(.width))) +
  scale_alpha_manual(values = c(0.5, 0.4, 0.3), guide = "none") +
  facet_grid(~ treatment) +
  theme_minimal()

# 3.2 Brouwer 1996 ####
# 3.2.1 Visualisation ####
data %>%
  filter(reference == "Brouwer 1996") %>%
  droplevels() %>%
  ggplot() +
  geom_point(aes(t, p), shape = 16, alpha = 0.5) +
  geom_pointrange(data = . %>% 
                    distinct(t, p_mean, p_sd, treatment),
                  aes(t, p_mean, 
                      ymin = p_mean - p_sd,
                      ymax = p_mean + p_sd)) +
  facet_grid(~ treatment) +
  theme_minimal()

# 3.2.2 Prior simulation ####
tibble(n = 1:1e3,
       alpha = rnorm( 1e3 , -0.005 , 0.003 ), 
       mu = rgamma( 1e3 , 200^2 / 150^2 , 200 / 150^2 ),
       tau = rgamma( 1e3 , 0.1^2 / 0.05^2 , 0.1 / 0.05^2 )) %>%
  expand_grid(t = data %>%
                filter(reference == "Brouwer 1996") %$% 
                seq(min(t), max(t), length.out = 100)) %>%
  mutate(
    p = exp(
      t * alpha - ( alpha + tau ) * mu / 5 * (
        log1p_exp( 5 / mu * ( t - mu ) ) - log1p_exp( -5 )
      )
    )
  ) %>%
  ggplot(aes(t, p, group = n)) +
    geom_hline(yintercept = data %>%
                 filter(reference == "Brouwer 1996") %$%
                 range(p)) +
    geom_line(alpha = 0.05) +
    coord_cartesian(ylim = c(-0.1, 1.5), expand = F, clip = "off") +
    theme_minimal() +
    theme(panel.grid = element_blank())

# 3.2.3 Stan model ####
Brouwer_model <- here("Stan", "Brouwer.stan") %>% 
  read_file() %>%
  write_stan_file() %>%
  cmdstan_model()

Brouwer_samples <- Brouwer_model$sample(
          data = data %>%
            filter(reference == "Brouwer 1996") %>% 
            droplevels() %>%
            select(t, p, treatment) %>%
            compose_data(),
          chains = 8,
          parallel_chains = parallel::detectCores(),
          iter_warmup = 1e4,
          iter_sampling = 1e4
        ) %T>%
  print()

# 3.2.4 Model checks ####
# Rhat
Brouwer_samples$summary() %>%
  mutate(rhat_check = rhat > 1.001) %>%
  summarise(rhat_1.001 = sum(rhat_check) / length(rhat),
            rhat_mean = mean(rhat),
            rhat_sd = sd(rhat))
# No rhat above 1.001. rhat = 1.00 ± 0.000120.

# Chains
Brouwer_samples$draws(format = "df") %>%
  mcmc_rank_overlay()
# Chains are good.

# Pairs
Brouwer_samples$draws(format = "df") %>%
  mcmc_pairs(pars = c("alpha[1]", "mu[1]", "tau"))
Brouwer_samples$draws(format = "df") %>%
  mcmc_pairs(pars = c("alpha[2]", "mu[2]", "tau"))
# Some negative correlation between alpha and mu
# and positive correlation between mu and tau,
# but not dramatic.

# 3.2.5 Prior-posterior comparison ####
Brouwer_prior <- prior_samples(
  model = Brouwer_model,
  data = data %>%
    filter(reference == "Brouwer 1996") %>% 
    droplevels() %>%
    select(t, p, treatment) %>%
    compose_data()
  )

Brouwer_prior %>% 
  prior_posterior_draws(
    posterior_samples = Brouwer_samples,
    group = data %>% 
      filter(reference == "Brouwer 1996") %>%
      droplevels() %>%
      select(treatment),
    parameters = c("alpha[treatment]", "mu[treatment]", "tau", 
                   "epsilon", "lambda[treatment]", "theta"),
    format = "long"
    ) %>%
  prior_posterior_plot(group_name = "treatment", ridges = FALSE)

# 3.2.6 Prediction ####
# Parameter posteriors
Brouwer_prior_posterior <- Brouwer_prior %>% 
  prior_posterior_draws(
    posterior_samples = Brouwer_samples,
    group = data %>% 
      filter(reference == "Brouwer 1996") %>%
      droplevels() %>%
      select(treatment),
    parameters = c("alpha[treatment]", "mu[treatment]", "tau", 
                   "epsilon", "lambda[treatment]", "theta"),
    format = "short"
  ) %>% 
  filter(!(treatment == "Pre-killed" & distribution == "prior")) %>%
  mutate(
    treatment = if_else(
      distribution == "prior", "Prior", treatment
    ) %>% fct()
  ) %>%
  select(-distribution) %T>%
  print()

# Predict across predictor range
Brouwer_prediction <- Brouwer_prior_posterior %>%
  spread_continuous(data = data %>% 
                      filter(reference == "Brouwer 1996") %>%
                      droplevels(), 
                    predictor_name = "t", length = 200,
                    group_name = "treatment") %>%
  mutate(
    p_mu = exp(
      t * alpha - ( alpha + tau ) * mu / 5 * (
        log1p_exp( 5 / mu * ( t - mu ) ) -
          log1p_exp( -5 )
      )
    ),
    k = ( alpha + tau ) / ( 1 + exp( 5 / mu * ( t - mu ) ) ) - tau,
    nu = ( epsilon - theta ) * exp( -lambda * t ) + theta,
    p = rbetapr( n() , p_mu * ( 1 + nu ) , 2 + nu )
  ) %T>%
  print()

# Summarise predictions
Brouwer_prediction_summary <- Brouwer_prediction %>%
  group_by(t, treatment) %>%
  median_qi(p_mu, k, nu, p, .width = c(.5, .8, .9)) %T>%
  print()

# Clean up raw predictions
rm(Brouwer_prediction)

# Viusalise mean predictions
data %>%
  filter(reference == "Brouwer 1996") %>%
  droplevels() %>%
  ggplot() +
  geom_pointrange(data = . %>% 
                    distinct(t, p_mean, p_sd, treatment),
                  aes(t, p_mean, colour = treatment,
                      ymin = p_mean - p_sd,
                      ymax = p_mean + p_sd)) +
  geom_line(data = Brouwer_prediction_summary %>%
              filter(treatment != "Prior"),
            aes(t, p_mu, colour = treatment)) +
  geom_ribbon(data = Brouwer_prediction_summary %>%
                filter(treatment != "Prior"),
              aes(t, ymin = p_mu.lower, ymax = p_mu.upper, 
                  alpha = factor(.width), fill = treatment)) +
  scale_alpha_manual(values = c(0.5, 0.4, 0.3), guide = "none") +
  theme_minimal()

# Visualise predictions of new observations
data %>%
  filter(reference == "Brouwer 1996") %>%
  droplevels() %>%
  ggplot() +
  geom_pointrange(data = . %>% 
                    distinct(t, p_mean, p_sd, treatment),
                  aes(t, p_mean, colour = treatment,
                      ymin = p_mean - p_sd,
                      ymax = p_mean + p_sd)) +
  geom_line(data = Brouwer_prediction_summary %>%
              filter(treatment != "Prior"),
            aes(t, p, colour = treatment)) +
  geom_ribbon(data = Brouwer_prediction_summary %>%
                filter(treatment != "Prior"),
              aes(t, ymin = p.lower, ymax = p.upper, 
                  alpha = factor(.width), fill = treatment)) +
  scale_alpha_manual(values = c(0.5, 0.4, 0.3), guide = "none") +
  theme_minimal()

# Visualise predictions of k
Brouwer_prediction_summary %>%
  filter(treatment != "Prior") %>%
  ggplot() +
  geom_line(aes(t, k, colour = treatment)) +
  geom_ribbon(aes(t, ymin = k.lower, ymax = k.upper, 
                  alpha = factor(.width), fill = treatment)) +
  scale_alpha_manual(values = c(0.5, 0.4, 0.3), guide = "none") +
  theme_minimal()

# Visualise predictions of nu
Brouwer_prediction_summary %>%
  filter(treatment != "Prior") %>%
  ggplot() +
  geom_line(aes(t, nu, colour = treatment)) +
  geom_ribbon(aes(t, ymin = nu.lower, ymax = nu.upper, 
                  alpha = factor(.width), fill = treatment)) +
  scale_alpha_manual(values = c(0.5, 0.4, 0.3), guide = "none") +
  theme_minimal()

# 3.3 Hamersley et al. 2015 ####
# 3.3.1 Visualisation ####
data %>%
  filter(reference == "Hamersley et al. 2015") %>%
  droplevels() %>%
  ggplot() +
    geom_point(aes(t, p), shape = 16, alpha = 0.5) +
    geom_pointrange(data = . %>%
                      distinct(t, p_mean, p_sd, treatment),
                    aes(t, p_mean,
                        ymin = p_mean - p_sd,
                        ymax = p_mean + p_sd)) +
    facet_grid(~ treatment) +
    theme_minimal()
# Warning because t0 has no s.d.

# 3.3.2 Prior simulation ####
tibble(n = 1:1e3,
       alpha = rnorm( 1e3 , 0 , 0.01 ), 
       mu = rexp( 1e3 , 0.1 ),
       tau = rgamma( 1e3 , 0.2^2 / 0.1^2 , 0.2 / 0.1^2 )) %>%
  expand_grid(t = data %>%
                filter(reference == "Hamersley et al. 2015") %$% 
                seq(min(t), max(t), length.out = 100)) %>%
  mutate(
    p = exp(
      t * alpha - ( alpha + tau ) * mu / 5 * (
        log1p_exp( 5 / mu * ( t - mu ) ) - log1p_exp( -5 )
      )
    )
  ) %>%
  ggplot(aes(t, p, group = n)) +
    geom_hline(yintercept = data %>%
                 filter(reference == "Hamersley et al. 2015") %$%
                 range(p)) +
    geom_line(alpha = 0.05) +
    coord_cartesian(ylim = c(-0.1, 1.5), expand = F, clip = "off") +
    theme_minimal() +
    theme(panel.grid = element_blank())

# 3.3.3 Stan model ####
Hamersley_model <- here("Stan", "Hamersley.stan") %>% 
  read_file() %>%
  write_stan_file() %>%
  cmdstan_model()

Hamersley_samples <- Hamersley_model$sample(
          data = data %>%
            filter(reference == "Hamersley et al. 2015" & t != 0) %>%
            droplevels() %>%
            select(t, p, treatment) %>%
            compose_data(),
          chains = 8,
          parallel_chains = parallel::detectCores(),
          iter_warmup = 1e4,
          iter_sampling = 1e4
        ) %T>%
  print()

# 3.3.4 Model checks ####
# Rhat
Hamersley_samples$summary() %>%
  mutate(rhat_check = rhat > 1.001) %>%
  summarise(rhat_1.001 = sum(rhat_check) / length(rhat),
            rhat_mean = mean(rhat),
            rhat_sd = sd(rhat))
# No of rhat above 1.001. rhat = 1.00 ± 0.0000639.

# Chains
Hamersley_samples$draws(format = "df") %>%
  mcmc_rank_overlay()

# Pairs
Hamersley_samples$draws(format = "df") %>%
  mcmc_pairs(pars = c("alpha[1]", "mu[1]", "tau[1]"))
Hamersley_samples$draws(format = "df") %>%
  mcmc_pairs(pars = c("alpha[2]", "mu[2]", "tau[2]"))
Hamersley_samples$draws(format = "df") %>%
  mcmc_pairs(pars = c("alpha[3]", "mu[3]", "tau[3]"))
# Mostly mu and tau are somewhat non-identifiable (positive correlation).

# 3.3.5 Prior-posterior comparison ####
Hamersley_prior <- prior_samples(
  model = Hamersley_model,
  data = data %>%
    filter(reference == "Hamersley et al. 2015" & t != 0) %>%
    droplevels() %>%
    select(t, p, treatment) %>%
    compose_data()
  )

Hamersley_prior %>% 
  prior_posterior_draws(
    posterior_samples = Hamersley_samples,
    group = data %>% 
      filter(reference == "Hamersley et al. 2015") %>%
      droplevels() %>%
      select(treatment),
    parameters = c("alpha[treatment]", "mu[treatment]", 
                   "tau[treatment]", "epsilon", 
                   "lambda[treatment]", "theta[treatment]"),
    format = "long"
    ) %>%
  prior_posterior_plot(group_name = "treatment", ridges = FALSE)

# 3.3.6 Prediction ####
# Parameter posteriors
Hamersley_prior_posterior <- Hamersley_prior %>% 
  prior_posterior_draws(
    posterior_samples = Hamersley_samples,
    group = data %>% 
      filter(reference == "Hamersley et al. 2015") %>%
      droplevels() %>%
      select(treatment),
    parameters = c("alpha[treatment]", "mu[treatment]", 
                   "tau[treatment]", "epsilon", 
                   "lambda[treatment]", "theta[treatment]"),
    format = "short"
  ) %>% 
  filter(!(treatment %in% c("Fresh", "Senescent") & 
             distribution == "prior")) %>%
  mutate(
    treatment = if_else(
      distribution == "prior", "Prior", treatment
    ) %>% fct()
  ) %>%
  select(-distribution) %T>%
  print()

# Predict across predictor range
Hamersley_prediction <- Hamersley_prior_posterior %>%
  spread_continuous(data = data %>% 
                      filter(reference == "Hamersley et al. 2015") %>%
                      droplevels(), 
                    predictor_name = "t", length = 200,
                    group_name = "treatment") %>%
  mutate(
    p_mu = exp(
      t * alpha - ( alpha + tau ) * mu / 5 * (
        log1p_exp( 5 / mu * ( t - mu ) ) -
          log1p_exp( -5 )
      )
    ),
    k = ( alpha + tau ) / ( 1 + exp( 5 / mu * ( t - mu ) ) ) - tau,
    nu = ( epsilon - theta ) * exp( -lambda * t ) + theta,
    p = rbetapr( n() , p_mu * ( 1 + nu ) , 2 + nu )
  ) %T>%
  print()

# Summarise predictions
Hamersley_prediction_summary <- Hamersley_prediction %>%
  group_by(t, treatment) %>%
  median_qi(p_mu, k, nu, p, .width = c(.5, .8, .9)) %T>%
  print()

# Clean up raw predictions
rm(Hamersley_prediction)

# Viusalise mean predictions
data %>%
  filter(reference == "Hamersley et al. 2015" & t != 0) %>%
  droplevels() %>%
  ggplot() +
  geom_pointrange(data = . %>% 
                    distinct(t, p_mean, p_sd, treatment),
                  aes(t, p_mean, colour = treatment,
                      ymin = p_mean - p_sd,
                      ymax = p_mean + p_sd)) +
  geom_line(data = Hamersley_prediction_summary %>%
              filter(treatment != "Prior"),
            aes(t, p_mu, colour = treatment)) +
  geom_ribbon(data = Hamersley_prediction_summary %>%
                filter(treatment != "Prior"),
              aes(t, ymin = p_mu.lower, ymax = p_mu.upper, 
                  alpha = factor(.width), fill = treatment)) +
  scale_alpha_manual(values = c(0.5, 0.4, 0.3), guide = "none") +
  theme_minimal()

# Visualise predictions of new observations
data %>%
  filter(reference == "Hamersley et al. 2015" & t != 0) %>%
  droplevels() %>%
  ggplot() +
  geom_pointrange(data = . %>% 
                    distinct(t, p_mean, p_sd, treatment),
                  aes(t, p_mean, colour = treatment,
                      ymin = p_mean - p_sd,
                      ymax = p_mean + p_sd)) +
  geom_line(data = Hamersley_prediction_summary %>%
              filter(treatment != "Prior"),
            aes(t, p, colour = treatment)) +
  geom_ribbon(data = Hamersley_prediction_summary %>%
                filter(treatment != "Prior"),
              aes(t, ymin = p.lower, ymax = p.upper, 
                  alpha = factor(.width), fill = treatment)) +
  scale_alpha_manual(values = c(0.5, 0.4, 0.3), guide = "none") +
  theme_minimal()

# Visualise predictions of k
Hamersley_prediction_summary %>%
  filter(treatment != "Prior") %>%
  ggplot() +
  geom_line(aes(t, k, colour = treatment)) +
  geom_ribbon(aes(t, ymin = k.lower, ymax = k.upper, 
                  alpha = factor(.width), fill = treatment)) +
  scale_alpha_manual(values = c(0.5, 0.4, 0.3), guide = "none") +
  theme_minimal()

# Visualise predictions of nu
Hamersley_prediction_summary %>%
  filter(treatment != "Prior") %>%
  ggplot() +
  geom_line(aes(t, nu, colour = treatment)) +
  geom_ribbon(aes(t, ymin = nu.lower, ymax = nu.upper, 
                  alpha = factor(.width), fill = treatment)) +
  scale_alpha_manual(values = c(0.5, 0.4, 0.3), guide = "none") +
  theme_minimal()

# 3.4 de Bettignies et al. 2020 ####
# 3.4.1 Visualisation ####
data %>%
  filter(reference == "de Bettignies et al. 2020") %>%
  droplevels() %>%
  ggplot() +
    geom_point(aes(t, p), shape = 16, alpha = 0.5) +
    geom_pointrange(data = . %>%
                      distinct(t, p_mean, p_sd, treatment),
                    aes(t, p_mean,
                        ymin = p_mean - p_sd,
                        ymax = p_mean + p_sd)) +
    facet_grid(~ treatment) +
    theme_minimal()
# Warning because t0 and senescent treatment have no s.d.

# 3.3.2 Prior simulation ####
# de Bettignies et al. 2020 provide k values (0.0366 and 0.0107).
# the larger of which can be used as a prior for tau because I
# expect k for fresh tissue to be an underestimate.
tibble(n = 1:1e3,
       alpha = rnorm( 1e3 , 0 , 0.01 ), 
       mu = rexp( 1e3 , 0.1 ),
       tau = rgamma( 1e3 , 0.0366^2 / 0.02^2 , 0.0366 / 0.02^2 )) %>%
  expand_grid(t = data %>%
                filter(reference == "de Bettignies et al. 2020") %$% 
                seq(min(t), max(t), length.out = 100)) %>%
  mutate(
    p = exp(
      t * alpha - ( alpha + tau ) * mu / 5 * (
        log1p_exp( 5 / mu * ( t - mu ) ) - log1p_exp( -5 )
      )
    )
  ) %>%
  ggplot(aes(t, p, group = n)) +
    geom_hline(yintercept = data %>%
                 filter(reference == "de Bettignies et al. 2020") %$%
                 range(p)) +
    geom_line(alpha = 0.05) +
    coord_cartesian(ylim = c(-0.1, 1.5), expand = F, clip = "off") +
    theme_minimal() +
    theme(panel.grid = element_blank())

# 3.4.3 Stan model ####
Bettignies_model <- here("Stan", "Bettignies.stan") %>% 
  read_file() %>%
  write_stan_file() %>%
  cmdstan_model()

Bettignies_samples <- Bettignies_model$sample(
          data = data %>%
            filter(reference == "de Bettignies et al. 2020" & t != 0) %>%
            droplevels() %>%
            select(t, p, treatment) %>%
            compose_data(),
          chains = 8,
          parallel_chains = parallel::detectCores(),
          iter_warmup = 1e4,
          iter_sampling = 1e4
        ) %T>%
  print()

# 3.4.4 Model checks ####
# Rhat
Bettignies_samples$summary() %>%
  mutate(rhat_check = rhat > 1.001) %>%
  summarise(rhat_1.001 = sum(rhat_check) / length(rhat),
            rhat_mean = mean(rhat),
            rhat_sd = sd(rhat))
# No of rhat above 1.001. rhat = 1.00 ± 0.000145.

# Chains
Bettignies_samples$draws(format = "df") %>%
  mcmc_rank_overlay()

# Pairs
Bettignies_samples$draws(format = "df") %>%
  mcmc_pairs(pars = c("alpha[1]", "mu[1]", "tau[1]"))
Bettignies_samples$draws(format = "df") %>%
  mcmc_pairs(pars = c("alpha[2]", "mu[2]", "tau[2]"))

# 3.4.5 Prior-posterior comparison ####
Bettignies_prior <- prior_samples(
  model = Bettignies_model,
  data = data %>%
    filter(reference == "de Bettignies et al. 2020" & t != 0) %>%
    droplevels() %>%
    select(t, p, treatment) %>%
    compose_data()
  )

Bettignies_prior %>% 
  prior_posterior_draws(
    posterior_samples = Bettignies_samples,
    group = data %>% 
      filter(reference == "de Bettignies et al. 2020") %>%
      droplevels() %>%
      select(treatment),
    parameters = c("alpha[treatment]", "mu[treatment]", 
                   "tau[treatment]", "epsilon", 
                   "lambda[treatment]", "theta[treatment]"),
    format = "long"
    ) %>%
  prior_posterior_plot(group_name = "treatment", ridges = FALSE)

# 3.4.6 Prediction ####
# Parameter posteriors
Bettignies_prior_posterior <- Bettignies_prior %>% 
  prior_posterior_draws(
    posterior_samples = Bettignies_samples,
    group = data %>% 
      filter(reference == "de Bettignies et al. 2020") %>%
      droplevels() %>%
      select(treatment),
    parameters = c("alpha[treatment]", "mu[treatment]", 
                   "tau[treatment]", "epsilon", 
                   "lambda[treatment]", "theta[treatment]"),
    format = "short"
  ) %>% 
  filter(!(treatment == "Senescent" & distribution == "prior")) %>%
  mutate(
    treatment = if_else(
      distribution == "prior", "Prior", treatment
    ) %>% fct()
  ) %>%
  select(-distribution) %T>%
  print()

# Predict across predictor range
Bettignies_prediction <- Bettignies_prior_posterior %>%
  spread_continuous(data = data %>% 
                      filter(reference == "de Bettignies et al. 2020") %>%
                      droplevels(), 
                    predictor_name = "t", length = 200,
                    group_name = "treatment") %>%
  mutate(
    p_mu = exp(
      t * alpha - ( alpha + tau ) * mu / 5 * (
        log1p_exp( 5 / mu * ( t - mu ) ) -
          log1p_exp( -5 )
      )
    ),
    k = ( alpha + tau ) / ( 1 + exp( 5 / mu * ( t - mu ) ) ) - tau,
    nu = ( epsilon - theta ) * exp( -lambda * t ) + theta,
    p = rbetapr( n() , p_mu * ( 1 + nu ) , 2 + nu )
  ) %T>%
  print()

# Summarise predictions
Bettignies_prediction_summary <- Bettignies_prediction %>%
  group_by(t, treatment) %>%
  median_qi(p_mu, k, nu, p, .width = c(.5, .8, .9)) %T>%
  print()

# Clean up raw predictions
rm(Bettignies_prediction)

# Viusalise mean predictions
data %>%
  filter(reference == "de Bettignies et al. 2020" & t != 0) %>%
  droplevels() %>%
  ggplot() +
  geom_point(aes(t, p, colour = treatment), shape = 16, alpha = 0.5) +
  geom_pointrange(data = . %>% 
                    distinct(t, p_mean, p_sd, treatment),
                  aes(t, p_mean, colour = treatment,
                      ymin = p_mean - p_sd,
                      ymax = p_mean + p_sd)) +
  geom_line(data = Bettignies_prediction_summary %>%
              filter(treatment != "Prior"),
            aes(t, p_mu, colour = treatment)) +
  geom_ribbon(data = Bettignies_prediction_summary %>%
                filter(treatment != "Prior"),
              aes(t, ymin = p_mu.lower, ymax = p_mu.upper, 
                  alpha = factor(.width), fill = treatment)) +
  scale_alpha_manual(values = c(0.5, 0.4, 0.3), guide = "none") +
  theme_minimal()

# Visualise predictions of new observations
data %>%
  filter(reference == "de Bettignies et al. 2020" & t != 0) %>%
  droplevels() %>%
  ggplot() +
  geom_pointrange(data = . %>% 
                    distinct(t, p_mean, p_sd, treatment),
                  aes(t, p_mean, colour = treatment,
                      ymin = p_mean - p_sd,
                      ymax = p_mean + p_sd)) +
  geom_line(data = Bettignies_prediction_summary %>%
              filter(treatment != "Prior"),
            aes(t, p, colour = treatment)) +
  geom_ribbon(data = Bettignies_prediction_summary %>%
                filter(treatment != "Prior"),
              aes(t, ymin = p.lower, ymax = p.upper, 
                  alpha = factor(.width), fill = treatment)) +
  scale_alpha_manual(values = c(0.5, 0.4, 0.3), guide = "none") +
  theme_minimal()

# Visualise predictions of k
Bettignies_prediction_summary %>%
  filter(treatment != "Prior") %>%
  ggplot() +
  geom_line(aes(t, k, colour = treatment)) +
  geom_ribbon(aes(t, ymin = k.lower, ymax = k.upper, 
                  alpha = factor(.width), fill = treatment)) +
  scale_alpha_manual(values = c(0.5, 0.4, 0.3), guide = "none") +
  theme_minimal()

# Visualise predictions of nu
Bettignies_prediction_summary %>%
  filter(treatment != "Prior") %>%
  ggplot() +
  geom_line(aes(t, nu, colour = treatment)) +
  geom_ribbon(aes(t, ymin = nu.lower, ymax = nu.upper, 
                  alpha = factor(.width), fill = treatment)) +
  scale_alpha_manual(values = c(0.5, 0.4, 0.3), guide = "none") +
  theme_minimal()

# 3.5 Frontier et al. 2021 ####
# 3.5.1 Visualisation ####
data %>%
  filter(reference == "Frontier et al. 2021" & t != 0) %>%
  droplevels() %>%
  ggplot() +
    geom_point(aes(t, p), shape = 16, alpha = 0.5) +
    facet_grid(treatment ~ species) +
    theme_minimal()

# 3.5.2 Prior simulation ####
tibble(n = 1:1e3,
       alpha = rexp( 1e3 , 100 ), 
       mu = rgamma( 1e3 , 50^2 / 30^2 , 50 / 30^2 ),
       tau = rgamma( 1e3 , 0.1^2 / 0.05^2 , 0.1 / 0.05^2 )) %>%
  expand_grid(t = data %>%
                filter(reference == "Frontier et al. 2021") %$% 
                seq(min(t), max(t), length.out = 100)) %>%
  mutate(
    p = exp(
      t * alpha - ( alpha + tau ) * mu / 5 * (
        log1p_exp( 5 / mu * ( t - mu ) ) - log1p_exp( -5 )
      )
    )
  ) %>%
  ggplot(aes(t, p, group = n)) +
    geom_hline(yintercept = data %>%
                 filter(reference == "Frontier et al. 2021") %$%
                 range(p)) +
    geom_line(alpha = 0.05) +
    coord_cartesian(ylim = c(-0.1, 1.5), expand = F, clip = "off") +
    theme_minimal() +
    theme(panel.grid = element_blank())

# 3.5.3 Stan model ####
Frontier2021_model <- here("Stan", "Frontier2021.stan") %>% 
  read_file() %>%
  write_stan_file() %>%
  cmdstan_model()

Frontier2021_samples <- Frontier2021_model$sample(
          data = data %>%
            filter(reference == "Frontier et al. 2021" & t != 0) %>%
            droplevels() %>%
            select(t, p, species, treatment) %>%
            compose_data(),
          chains = 8,
          parallel_chains = parallel::detectCores(),
          iter_warmup = 1e4,
          iter_sampling = 1e4
        ) %T>%
  print()

# 3.5.4 Model checks ####
# Rhat
Frontier2021_samples$summary() %>%
  mutate(rhat_check = rhat > 1.001) %>%
  summarise(rhat_1.001 = sum(rhat_check) / length(rhat),
            rhat_mean = mean(rhat),
            rhat_sd = sd(rhat))
# No of rhat above 1.001. rhat = 1.00 ± 0.0000814.

# Chains
Frontier2021_samples$draws(format = "df") %>%
  mcmc_rank_overlay()

# Pairs
Frontier2021_samples$draws(format = "df") %>%
  mcmc_pairs(pars = c("alpha[1]", "mu[1,1]", "tau[1]"))
Frontier2021_samples$draws(format = "df") %>%
  mcmc_pairs(pars = c("alpha[1]", "mu[1,2]", "tau[1]"))
Frontier2021_samples$draws(format = "df") %>%
  mcmc_pairs(pars = c("alpha[1]", "mu[1,3]", "tau[1]"))

Frontier2021_samples$draws(format = "df") %>%
  mcmc_pairs(pars = c("alpha[2]", "mu[2,1]", "tau[2]"))
Frontier2021_samples$draws(format = "df") %>%
  mcmc_pairs(pars = c("alpha[2]", "mu[2,2]", "tau[2]"))
Frontier2021_samples$draws(format = "df") %>%
  mcmc_pairs(pars = c("alpha[2]", "mu[2,3]", "tau[2]"))

# 3.5.5 Prior-posterior comparison ####
Frontier2021_prior <- prior_samples(
  model = Frontier2021_model,
  data = data %>%
    filter(reference == "Frontier et al. 2021" & t != 0) %>%
    droplevels() %>%
    select(t, p, species, treatment) %>%
    compose_data()
  )

Frontier2021_prior %>% 
  prior_posterior_draws(
    posterior_samples = Frontier2021_samples,
    group = data %>% 
      filter(reference == "Frontier et al. 2021") %>%
      droplevels() %>%
      select(species, treatment),
    parameters = c("alpha[species]", "mu[species, treatment]", 
                   "tau[species]", "epsilon", 
                   "lambda[species, treatment]", 
                   "theta[species]"),
    format = "long"
    ) %T>% {
      prior_posterior_plot(., group_name = "species", ridges = FALSE) %>%
        print()
    } %>%
  prior_posterior_plot(group_name = "treatment", ridges = FALSE)

# 3.5.6 Prediction ####
# Parameter posteriors
Frontier2021_prior_posterior <- Frontier2021_prior %>% 
  prior_posterior_draws(
    posterior_samples = Frontier2021_samples,
    group = data %>% 
      filter(reference == "Frontier et al. 2021") %>%
      droplevels() %>%
      select(treatment),
    parameters = c("alpha[species]", "mu[species, treatment]", 
                   "tau[species]", "epsilon", 
                   "lambda[species, treatment]", 
                   "theta[species]"),
    format = "short"
  ) %>% 
  filter(!(treatment %in% c("15m", "30m") &
             distribution == "prior")) %>%
  mutate(
    treatment = if_else(
      distribution == "prior", "Prior", treatment
    ) %>% fct()
  ) %>%
  select(-distribution) %T>%
  print()

# Predict across predictor range
Frontier2021_prediction <- Frontier2021_prior_posterior %>%
  spread_continuous(data = data %>% 
                      filter(reference == "Frontier et al. 2021") %>%
                      droplevels(), # all groups have the same predictor range
                    predictor_name = "t", length = 200) %>%
  mutate(
    p_mu = exp(
      t * alpha - ( alpha + tau ) * mu / 5 * (
        log1p_exp( 5 / mu * ( t - mu ) ) -
          log1p_exp( -5 )
      )
    ),
    k = ( alpha + tau ) / ( 1 + exp( 5 / mu * ( t - mu ) ) ) - tau,
    nu = ( epsilon - theta ) * exp( -lambda * t ) + theta,
    p = rbetapr( n() , p_mu * ( 1 + nu ) , 2 + nu )
  ) %T>%
  print()

# Summarise predictions
Frontier2021_prediction_summary <- Frontier2021_prediction %>%
  group_by(t, species, treatment) %>%
  median_qi(p_mu, k, nu, p, .width = c(.5, .8, .9)) %T>%
  print()

# Clean up raw predictions
rm(Frontier2021_prediction)

# Viusalise mean predictions
data %>%
  filter(reference == "Frontier et al. 2021" & t != 0) %>%
  droplevels() %>%
  ggplot() +
  geom_point(aes(t, p), shape = 16, alpha = 0.5) +
  geom_line(data = Frontier2021_prediction_summary %>%
              filter(treatment != "Prior"),
            aes(t, p_mu)) +
  geom_ribbon(data = Frontier2021_prediction_summary %>%
                filter(treatment != "Prior"),
              aes(t, ymin = p_mu.lower, ymax = p_mu.upper, 
                  alpha = factor(.width))) +
  scale_alpha_manual(values = c(0.5, 0.4, 0.3), guide = "none") +
  facet_grid(treatment ~ species) +
  theme_minimal()

# Visualise predictions of new observations
data %>%
  filter(reference == "Frontier et al. 2021" & t != 0) %>%
  droplevels() %>%
  ggplot() +
  geom_point(aes(t, p), shape = 16, alpha = 0.5) +
  geom_line(data = Frontier2021_prediction_summary %>%
              filter(treatment != "Prior"),
            aes(t, p)) +
  geom_ribbon(data = Frontier2021_prediction_summary %>%
                filter(treatment != "Prior"),
              aes(t, ymin = p.lower, ymax = p.upper, 
                  alpha = factor(.width))) +
  scale_alpha_manual(values = c(0.5, 0.4, 0.3), guide = "none") +
  facet_grid(treatment ~ species) +
  theme_minimal()

# Visualise predictions of k
Frontier2021_prediction_summary %>%
  filter(treatment != "Prior") %>%
  ggplot() +
  geom_line(aes(t, k)) +
  geom_ribbon(aes(t, ymin = k.lower, ymax = k.upper, 
                  alpha = factor(.width))) +
  scale_alpha_manual(values = c(0.5, 0.4, 0.3), guide = "none") +
  facet_grid(treatment ~ species) +
  theme_minimal()

# Visualise predictions of nu
Frontier2021_prediction_summary %>%
  filter(treatment != "Prior") %>%
  ggplot() +
  geom_line(aes(t, nu)) +
  geom_ribbon(aes(t, ymin = nu.lower, ymax = nu.upper, 
                  alpha = factor(.width))) +
  scale_alpha_manual(values = c(0.5, 0.4, 0.3), guide = "none") +
  facet_grid(treatment ~ species) +
  theme_minimal()

# 3.6 Frontier et al. 2022 ####
# 3.6.1 Visualisation ####
data %>%
  filter(reference == "Frontier et al. 2022" & t != 0) %>%
  droplevels() %>%
  ggplot() +
    geom_point(aes(t, p), shape = 16, alpha = 0.5) +
    facet_grid(treatment ~ species) +
    theme_minimal()

# 3.6.2 Prior simulation ####
tibble(n = 1:1e3,
       alpha = rexp( 1e3 , 100 ), 
       mu = rgamma( 1e3 , 50^2 / 30^2 , 40 / 50^2 ),
       tau = rgamma( 1e3 , 0.1^2 / 0.05^2 , 0.1 / 0.05^2 )) %>%
  expand_grid(t = data %>%
                filter(reference == "Frontier et al. 2022") %$% 
                seq(min(t), max(t), length.out = 100)) %>%
  mutate(
    p = exp(
      t * alpha - ( alpha + tau ) * mu / 5 * (
        log1p_exp( 5 / mu * ( t - mu ) ) - log1p_exp( -5 )
      )
    )
  ) %>%
  ggplot(aes(t, p, group = n)) +
    geom_hline(yintercept = data %>%
                 filter(reference == "Frontier et al. 2022") %$%
                 range(p)) +
    geom_line(alpha = 0.05) +
    coord_cartesian(ylim = c(-0.1, 1.5), expand = F, clip = "off") +
    theme_minimal() +
    theme(panel.grid = element_blank())

# 3.6.3 Stan model ####
Frontier2022_model <- here("Stan", "Frontier2022.stan") %>% 
  read_file() %>%
  write_stan_file() %>%
  cmdstan_model()

Frontier2022_samples <- Frontier2022_model$sample(
          data = data %>%
            filter(reference == "Frontier et al. 2022" & t != 0) %>%
            droplevels() %>%
            select(t, p, species, treatment) %>%
            compose_data(),
          chains = 8,
          parallel_chains = parallel::detectCores(),
          iter_warmup = 1e4,
          iter_sampling = 1e4
        ) %T>%
  print()

# 3.6.4 Model checks ####
# Rhat
Frontier2022_samples$summary() %>%
  mutate(rhat_check = rhat > 1.001) %>%
  summarise(rhat_1.001 = sum(rhat_check) / length(rhat),
            rhat_mean = mean(rhat),
            rhat_sd = sd(rhat))
# No of rhat above 1.001. rhat = 1.00 ± 0.0000751.

# Chains
Frontier2022_samples$draws(format = "df") %>%
  mcmc_rank_overlay()

# Pairs
Frontier2022_samples$draws(format = "df") %>%
  mcmc_pairs(pars = c("alpha[1]", "mu[1,1]", "tau"))
Frontier2022_samples$draws(format = "df") %>%
  mcmc_pairs(pars = c("alpha[1]", "mu[1,2]", "tau"))
Frontier2022_samples$draws(format = "df") %>%
  mcmc_pairs(pars = c("alpha[1]", "mu[1,3]", "tau"))

Frontier2022_samples$draws(format = "df") %>%
  mcmc_pairs(pars = c("alpha[2]", "mu[2,1]", "tau"))
Frontier2022_samples$draws(format = "df") %>%
  mcmc_pairs(pars = c("alpha[2]", "mu[2,2]", "tau"))
Frontier2022_samples$draws(format = "df") %>%
  mcmc_pairs(pars = c("alpha[2]", "mu[2,3]", "tau"))

# 3.6.5 Prior-posterior comparison ####
Frontier2022_prior <- prior_samples(
  model = Frontier2022_model,
  data = data %>%
    filter(reference == "Frontier et al. 2022" & t != 0) %>%
    droplevels() %>%
    select(t, p, species, treatment) %>%
    compose_data()
  )

Frontier2022_prior %>% 
  prior_posterior_draws(
    posterior_samples = Frontier2022_samples,
    group = data %>% 
      filter(reference == "Frontier et al. 2022") %>%
      droplevels() %>%
      select(species, treatment),
    parameters = c("alpha[species]", "mu[species, treatment]", 
                   "tau", "epsilon", "lambda[species, treatment]", 
                   "theta"),
    format = "long"
    ) %T>% {
      prior_posterior_plot(., group_name = "species", ridges = FALSE) %>%
        print()
    } %>%
  prior_posterior_plot(group_name = "treatment", ridges = FALSE)

# 3.6.6 Prediction ####
# Parameter posteriors
Frontier2022_prior_posterior <- Frontier2022_prior %>% 
  prior_posterior_draws(
    posterior_samples = Frontier2022_samples,
    group = data %>% 
      filter(reference == "Frontier et al. 2022") %>%
      droplevels() %>%
      select(treatment),
    parameters = c("alpha[species]", "mu[species, treatment]", 
                   "tau", "epsilon", "lambda[species, treatment]", 
                   "theta"),
    format = "short"
  ) %>% 
  filter(!(treatment %in% c("1.5m", "3m") &
             distribution == "prior")) %>%
  mutate(
    treatment = if_else(
      distribution == "prior", "Prior", treatment
    ) %>% fct()
  ) %>%
  select(-distribution) %T>%
  print()

# Predict across predictor range
Frontier2022_prediction <- Frontier2022_prior_posterior %>%
  spread_continuous(data = data %>% 
                      filter(reference == "Frontier et al. 2022") %>%
                      droplevels(), # all groups have the same predictor range
                    predictor_name = "t", length = 200) %>%
  mutate(
    p_mu = exp(
      t * alpha - ( alpha + tau ) * mu / 5 * (
        log1p_exp( 5 / mu * ( t - mu ) ) -
          log1p_exp( -5 )
      )
    ),
    k = ( alpha + tau ) / ( 1 + exp( 5 / mu * ( t - mu ) ) ) - tau,
    nu = ( epsilon - theta ) * exp( -lambda * t ) + theta,
    p = rbetapr( n() , p_mu * ( 1 + nu ) , 2 + nu )
  ) %T>%
  print()

# Summarise predictions
Frontier2022_prediction_summary <- Frontier2022_prediction %>%
  group_by(t, species, treatment) %>%
  median_qi(p_mu, k, nu, p, .width = c(.5, .8, .9)) %T>%
  print()

# Clean up raw predictions
rm(Frontier2022_prediction)

# Viusalise mean predictions
data %>%
  filter(reference == "Frontier et al. 2022" & t != 0) %>%
  droplevels() %>%
  ggplot() +
  geom_point(aes(t, p), shape = 16, alpha = 0.5) +
  geom_line(data = Frontier2022_prediction_summary %>%
              filter(treatment != "Prior"),
            aes(t, p_mu)) +
  geom_ribbon(data = Frontier2022_prediction_summary %>%
                filter(treatment != "Prior"),
              aes(t, ymin = p_mu.lower, ymax = p_mu.upper, 
                  alpha = factor(.width))) +
  scale_alpha_manual(values = c(0.5, 0.4, 0.3), guide = "none") +
  facet_grid(treatment ~ species) +
  theme_minimal()

# Visualise predictions of new observations
data %>%
  filter(reference == "Frontier et al. 2022" & t != 0) %>%
  droplevels() %>%
  ggplot() +
  geom_point(aes(t, p), shape = 16, alpha = 0.5) +
  geom_line(data = Frontier2022_prediction_summary %>%
              filter(treatment != "Prior"),
            aes(t, p)) +
  geom_ribbon(data = Frontier2022_prediction_summary %>%
                filter(treatment != "Prior"),
              aes(t, ymin = p.lower, ymax = p.upper, 
                  alpha = factor(.width))) +
  scale_alpha_manual(values = c(0.5, 0.4, 0.3), guide = "none") +
  facet_grid(treatment ~ species) +
  theme_minimal()

# Visualise predictions of k
Frontier2022_prediction_summary %>%
  filter(treatment != "Prior") %>%
  ggplot() +
  geom_line(aes(t, k)) +
  geom_ribbon(aes(t, ymin = k.lower, ymax = k.upper, 
                  alpha = factor(.width))) +
  scale_alpha_manual(values = c(0.5, 0.4, 0.3), guide = "none") +
  facet_grid(treatment ~ species) +
  theme_minimal()

# Visualise predictions of nu
Frontier2022_prediction_summary %>%
  filter(treatment != "Prior") %>%
  ggplot() +
  geom_line(aes(t, nu)) +
  geom_ribbon(aes(t, ymin = nu.lower, ymax = nu.upper, 
                  alpha = factor(.width))) +
  scale_alpha_manual(values = c(0.5, 0.4, 0.3), guide = "none") +
  facet_grid(treatment ~ species) +
  theme_minimal()

# 3.7 Bourguès et al. 1996 ####
# 3.7.1 Visualisation ####
data %>%
  filter(reference == "Bourguès et al. 1996" & t != 0) %>%
  droplevels() %>%
  ggplot() +
    geom_point(aes(t, p), shape = 16, alpha = 0.5) +
    facet_grid(~ treatment) +
    theme_minimal()

# 3.7.2 Prior simulation ####
tibble(n = 1:1e3,
       alpha = rexp( 1e3 , 100 ), 
       mu = rgamma( 1e3 , 25^2 / 10^2 , 25 / 10^2 ),
       tau = rgamma( 1e3 , 0.1^2 / 0.05^2 , 0.1 / 0.05^2 )) %>%
  expand_grid(t = data %>%
                filter(reference == "Bourguès et al. 1996") %$% 
                seq(min(t), max(t), length.out = 100)) %>%
  mutate(
    p = exp(
      t * alpha - ( alpha + tau ) * mu / 5 * (
        log1p_exp( 5 / mu * ( t - mu ) ) - log1p_exp( -5 )
      )
    )
  ) %>%
  ggplot(aes(t, p, group = n)) +
    geom_hline(yintercept = data %>%
                 filter(reference == "Bourguès et al. 1996") %$%
                 range(p)) +
    geom_line(alpha = 0.05) +
    coord_cartesian(ylim = c(-0.1, 1.5), expand = F, clip = "off") +
    theme_minimal() +
    theme(panel.grid = element_blank())

# 3.7.3 Stan model ####
Bourguès_model <- here("Stan", "Bourguès.stan") %>% 
  read_file() %>%
  write_stan_file() %>%
  cmdstan_model()

Bourguès_samples <- Bourguès_model$sample(
          data = data %>%
            filter(reference == "Bourguès et al. 1996" & t != 0) %>%
            droplevels() %>%
            select(t, p, treatment) %>%
            compose_data(),
          chains = 8,
          parallel_chains = parallel::detectCores(),
          iter_warmup = 1e4,
          iter_sampling = 1e4
        ) %T>%
  print()

# 3.7.4 Model checks ####
# Rhat
Bourguès_samples$summary() %>%
  mutate(rhat_check = rhat > 1.001) %>%
  summarise(rhat_1.001 = sum(rhat_check) / length(rhat),
            rhat_mean = mean(rhat),
            rhat_sd = sd(rhat))
# No of rhat above 1.001. rhat = 1.00 ± 0.0000808.

# Chains
Bourguès_samples$draws(format = "df") %>%
  mcmc_rank_overlay()

# Pairs
Bourguès_samples$draws(format = "df") %>%
  mcmc_pairs(pars = c("alpha[1]", "mu[1]", "tau[1]"))
Bourguès_samples$draws(format = "df") %>%
  mcmc_pairs(pars = c("alpha[2]", "mu[2]", "tau[2]"))
Bourguès_samples$draws(format = "df") %>%
  mcmc_pairs(pars = c("alpha[3]", "mu[3]", "tau[3]"))
Bourguès_samples$draws(format = "df") %>%
  mcmc_pairs(pars = c("alpha[4]", "mu[4]", "tau[4]"))
# Some fairly strong positive banana-shaped correlation
# between mu and tau. Reduced by constraining priors.

# 3.7.5 Prior-posterior comparison ####
Bourguès_prior <- prior_samples(
  model = Bourguès_model,
  data = data %>%
    filter(reference == "Bourguès et al. 1996" & t != 0) %>%
    droplevels() %>%
    select(t, p, treatment) %>%
    compose_data()
  )

Bourguès_prior %>% 
  prior_posterior_draws(
    posterior_samples = Bourguès_samples,
    group = data %>% 
      filter(reference == "Bourguès et al. 1996") %>%
      droplevels() %>%
      select(treatment),
    parameters = c("alpha[treatment]", "mu[treatment]", 
                   "tau[treatment]", "epsilon", 
                   "lambda[treatment]", "theta[treatment]"),
    format = "long"
    ) %>%
  prior_posterior_plot(group_name = "treatment", ridges = FALSE)

# 3.7.6 Prediction ####
# Parameter posteriors
Bourguès_prior_posterior <- Bourguès_prior %>% 
  prior_posterior_draws(
    posterior_samples = Bourguès_samples,
    group = data %>% 
      filter(reference == "Bourguès et al. 1996") %>%
      droplevels() %>%
      select(treatment),
    parameters = c("alpha[treatment]", "mu[treatment]", 
                   "tau[treatment]", "epsilon", 
                   "lambda[treatment]", "theta[treatment]"),
    format = "short"
  ) %>% 
  filter(!(treatment %in% c("Spring", "Summer", "Autumn") & 
             distribution == "prior")) %>%
  mutate(
    treatment = if_else(
      distribution == "prior", "Prior", treatment
    ) %>% fct()
  ) %>%
  select(-distribution) %T>%
  print()

# Predict across predictor range
Bourguès_prediction <- Bourguès_prior_posterior %>%
  spread_continuous(data = data %>% 
                      filter(reference == "Bourguès et al. 1996") %>%
                      droplevels(), 
                    predictor_name = "t", length = 200,
                    group_name = "treatment") %>%
  mutate(
    p_mu = exp(
      t * alpha - ( alpha + tau ) * mu / 5 * (
        log1p_exp( 5 / mu * ( t - mu ) ) -
          log1p_exp( -5 )
      )
    ),
    k = ( alpha + tau ) / ( 1 + exp( 5 / mu * ( t - mu ) ) ) - tau,
    nu = ( epsilon - theta ) * exp( -lambda * t ) + theta,
    p = rbetapr( n() , p_mu * ( 1 + nu ) , 2 + nu )
  ) %T>%
  print()

# Summarise predictions
Bourguès_prediction_summary <- Bourguès_prediction %>%
  group_by(t, treatment) %>%
  median_qi(p_mu, k, nu, p, .width = c(.5, .8, .9)) %T>%
  print()

# Clean up raw predictions
rm(Bourguès_prediction)

# Viusalise mean predictions
data %>%
  filter(reference == "Bourguès et al. 1996" & t != 0) %>%
  droplevels() %>%
  ggplot() +
  geom_point(aes(t, p), shape = 16, alpha = 0.5) +
  geom_line(data = Bourguès_prediction_summary %>%
              filter(treatment != "Prior"),
            aes(t, p_mu)) +
  geom_ribbon(data = Bourguès_prediction_summary %>%
                filter(treatment != "Prior"),
              aes(t, ymin = p_mu.lower, ymax = p_mu.upper, 
                  alpha = factor(.width))) +
  scale_alpha_manual(values = c(0.5, 0.4, 0.3), guide = "none") +
  facet_grid(~ treatment) +
  theme_minimal()

# Visualise predictions of new observations
data %>%
  filter(reference == "Bourguès et al. 1996" & t != 0) %>%
  droplevels() %>%
  ggplot() +
  geom_point(aes(t, p), shape = 16, alpha = 0.5) +
  geom_line(data = Bourguès_prediction_summary %>%
              filter(treatment != "Prior"),
            aes(t, p)) +
  geom_ribbon(data = Bourguès_prediction_summary %>%
                filter(treatment != "Prior"),
              aes(t, ymin = p.lower, ymax = p.upper, 
                  alpha = factor(.width))) +
  scale_alpha_manual(values = c(0.5, 0.4, 0.3), guide = "none") +
  facet_grid(~ treatment) +
  theme_minimal()

# Visualise predictions of k
Bourguès_prediction_summary %>%
  filter(treatment != "Prior") %>%
  ggplot() +
  geom_line(aes(t, k)) +
  geom_ribbon(aes(t, ymin = k.lower, ymax = k.upper, 
                  alpha = factor(.width))) +
  scale_alpha_manual(values = c(0.5, 0.4, 0.3), guide = "none") +
  facet_grid(~ treatment) +
  theme_minimal()

# Visualise predictions of nu
Bourguès_prediction_summary %>%
  filter(treatment != "Prior") %>%
  ggplot() +
  geom_line(aes(t, nu)) +
  geom_ribbon(aes(t, ymin = nu.lower, ymax = nu.upper, 
                  alpha = factor(.width))) +
  scale_alpha_manual(values = c(0.5, 0.4, 0.3), guide = "none") +
  facet_grid(~ treatment) +
  theme_minimal()

# 4. Parameter estimates ####
# 4.1 Brouwer 1996 ####
Brouwer_parameters <- Brouwer_prior_posterior %>%
  group_by(treatment) %>%
  summarise(alpha_mean = mean(alpha),
            alpha_sd = sd(alpha),
            mu_mean = mean(mu),
            mu_sd = sd(mu),
            tau_mean = mean(tau),
            tau_sd = sd(tau),
            epsilon_mean = mean(epsilon),
            epsilon_sd = sd(epsilon),
            lambda_mean = mean(lambda),
            lambda_sd = sd(lambda),
            theta_mean = mean(theta),
            theta_sd = sd(theta),
            n = n()) %>%
  ungroup()

# 4.2 Hamersley et al. 2015 ####
Hamersley_parameters <- Hamersley_prior_posterior %>%
  group_by(treatment) %>%
  summarise(alpha_mean = mean(alpha),
            alpha_sd = sd(alpha),
            mu_mean = mean(mu),
            mu_sd = sd(mu),
            tau_mean = mean(tau),
            tau_sd = sd(tau),
            epsilon_mean = mean(epsilon),
            epsilon_sd = sd(epsilon),
            lambda_mean = mean(lambda),
            lambda_sd = sd(lambda),
            theta_mean = mean(theta),
            theta_sd = sd(theta),
            n = n()) %>%
  ungroup()

# 4.3 de Bettignies et al. 2020 ####
Bettignies_parameters <- Bettignies_prior_posterior %>%
  group_by(treatment) %>%
  summarise(alpha_mean = mean(alpha),
            alpha_sd = sd(alpha),
            mu_mean = mean(mu),
            mu_sd = sd(mu),
            tau_mean = mean(tau),
            tau_sd = sd(tau),
            epsilon_mean = mean(epsilon),
            epsilon_sd = sd(epsilon),
            lambda_mean = mean(lambda),
            lambda_sd = sd(lambda),
            theta_mean = mean(theta),
            theta_sd = sd(theta),
            n = n()) %>%
  ungroup()

# 4.4 Frontier et al. 2021 ####
Frontier2021_parameters <- Frontier2021_prior_posterior %>%
  group_by(species, treatment) %>%
  summarise(alpha_mean = mean(alpha),
            alpha_sd = sd(alpha),
            mu_mean = mean(mu),
            mu_sd = sd(mu),
            tau_mean = mean(tau),
            tau_sd = sd(tau),
            epsilon_mean = mean(epsilon),
            epsilon_sd = sd(epsilon),
            lambda_mean = mean(lambda),
            lambda_sd = sd(lambda),
            theta_mean = mean(theta),
            theta_sd = sd(theta),
            n = n()) %>%
  ungroup()

# 4.5 Frontier et al. 2022 ####
Frontier2022_parameters <- Frontier2022_prior_posterior %>%
  group_by(species, treatment) %>%
  summarise(alpha_mean = mean(alpha),
            alpha_sd = sd(alpha),
            mu_mean = mean(mu),
            mu_sd = sd(mu),
            tau_mean = mean(tau),
            tau_sd = sd(tau),
            epsilon_mean = mean(epsilon),
            epsilon_sd = sd(epsilon),
            lambda_mean = mean(lambda),
            lambda_sd = sd(lambda),
            theta_mean = mean(theta),
            theta_sd = sd(theta),
            n = n()) %>%
  ungroup()

# 4.6 Bourguès et al. 1996 ####
Bourguès_parameters <- Bourguès_prior_posterior %>%
  group_by(treatment) %>%
  summarise(alpha_mean = mean(alpha),
            alpha_sd = sd(alpha),
            mu_mean = mean(mu),
            mu_sd = sd(mu),
            tau_mean = mean(tau),
            tau_sd = sd(tau),
            epsilon_mean = mean(epsilon),
            epsilon_sd = sd(epsilon),
            lambda_mean = mean(lambda),
            lambda_sd = sd(lambda),
            theta_mean = mean(theta),
            theta_sd = sd(theta),
            n = n()) %>%
  ungroup()

# 4.7 Table 1 ####
require(glue)
Table_1 <- bind_rows(
  Brouwer_parameters %>% 
    filter(treatment != "Prior") %>%
    left_join(
      data %>% 
        filter(reference == "Brouwer 1996") %>%
        distinct(reference, species, treatment),
      by = "treatment"
    ),
  Hamersley_parameters %>% 
    filter(treatment != "Prior") %>%
    left_join(
      data %>% 
        filter(reference == "Hamersley et al. 2015") %>%
        distinct(reference, species, treatment),
      by = "treatment"
    ),
  Bettignies_parameters %>% 
    filter(treatment != "Prior") %>%
    left_join(
      data %>% 
        filter(reference == "de Bettignies et al. 2020") %>%
        distinct(reference, species, treatment),
      by = "treatment"
    ),
  Frontier2021_parameters %>% 
    filter(treatment != "Prior") %>%
    left_join(
      data %>% 
        filter(reference == "Frontier et al. 2021") %>%
        distinct(reference, species, treatment),
      by = c("species", "treatment")
    ),
  Frontier2022_parameters %>% 
    filter(treatment != "Prior") %>%
    left_join(
      data %>% 
        filter(reference == "Frontier et al. 2022") %>%
        distinct(reference, species, treatment),
      by = c("species", "treatment")
    ),
  Bourguès_parameters %>% 
    filter(treatment != "Prior") %>%
    left_join(
      data %>% 
        filter(reference == "Bourguès et al. 1996") %>%
        distinct(reference, species, treatment),
      by = "treatment"
    )
) %>%
  mutate( # here I am converting alpha and tau to % for readability
    alpha = glue("{signif(alpha_mean*100, 2)} ± {signif(alpha_sd*100, 2)}"),
    mu = glue("{if_else(mu_mean < 100, signif(mu_mean, 2), signif(mu_mean, 3))}
              ± {if_else(mu_sd < 100, signif(mu_sd, 2), signif(mu_sd, 3))}"),
    tau = glue("{signif(tau_mean*100, 2)} ± {signif(tau_sd*100, 2)}"),
    across(c(alpha, tau), ~str_replace_all(., "-", "−")) # replace hyphen with minus
  ) %>%
  select(reference, species, treatment, alpha, mu, tau)
  
Table_1 %>%
  write_csv(here("Tables", "Table_1.csv"))

require(officer)
read_docx() %>%
  body_add_table(value = Table_1) %>%
  print(target = here("Tables", "Table_1.docx"))

# 4.8 Text ####
Brouwer_prior_posterior %>%
  filter(treatment != "Prior") %>%
  select(-c(epsilon, lambda, theta)) %>%
  mutate(treatment = treatment %>% 
           fct_recode("C" = "Control",
                      "K" = "Pre-killed")) %>%
  pivot_wider(names_from = treatment,
              values_from = c(alpha, mu, tau)) %>%
  mutate(delta_mu = mu_C - mu_K,
         prop_mu = mu_K / mu_C) %>%
  summarise(delta_mu_mean = mean(delta_mu),
            delta_mu_sd = sd(delta_mu),
            delta_mu_P = mean(delta_mu > 0),
            prop_mu_mean = mean(prop_mu),
            prop_mu_sd = sd(prop_mu),
            alpha_C_P = mean(alpha_C > 0),
            alpha_K_P = mean(alpha_K < 0))

Hamersley_prior_posterior %>%
  filter(treatment != "Prior") %>%
  select(-c(epsilon, lambda, theta)) %>%
  mutate(treatment = treatment %>% 
           fct_recode("F" = "Fresh",
                      "S" = "Senescent",
                      "D" = "Detached")) %>%
  pivot_wider(names_from = treatment,
              values_from = c(alpha, mu, tau)) %T>%
  {
    summarise(., alpha_F_P = mean(alpha_F < 0),
              alpha_S_P = mean(alpha_S < 0),
              alpha_D_P = mean(alpha_D < 0)) %>%
      print()
  } %>%
  mutate(delta_alpha_F_S = alpha_F - alpha_S,
         delta_alpha_F_D = alpha_F - alpha_D,
         delta_alpha_S_D = alpha_S - alpha_D,
         delta_mu_F_S = mu_F - mu_S,
         delta_mu_F_D = mu_F - mu_D,
         delta_mu_S_D = mu_S - mu_D,
         delta_tau_F_S = tau_F - tau_S,
         delta_tau_F_D = tau_F - tau_D,
         delta_tau_S_D = tau_S - tau_D) %>%
  select(.chain, .iteration, .draw, starts_with("delta")) %>%
  pivot_longer(cols = -c(.chain, .iteration, .draw),
               names_to = "Contrast", values_to = "Difference", 
               names_prefix = "delta_") %>%
  group_by(Contrast) %>%
  summarise(mean = mean(Difference),
            sd = sd(Difference),
            P = pmax( mean(Difference < 0), mean(Difference > 0) )) %>%
  separate(Contrast, into = c("Parameter", "First", "Second"), sep = "_")

Bettignies_prior_posterior %>%
  filter(treatment != "Prior") %>%
  select(-c(epsilon, lambda, theta)) %>%
  mutate(treatment = treatment %>% 
           fct_recode("F" = "Fresh",
                      "S" = "Senescent")) %>%
  pivot_wider(names_from = treatment,
              values_from = c(alpha, mu, tau)) %T>%
  {
    summarise(., alpha_F_P = mean(alpha_F > 0),
              alpha_S_P = mean(alpha_S < 0)) %>%
      print()
  } %>%
  mutate(delta_alpha = alpha_F - alpha_S,
         delta_mu = mu_F - mu_S,
         delta_tau = tau_S - tau_F) %>%
  select(.chain, .iteration, .draw, starts_with("delta")) %>%
  pivot_longer(cols = -c(.chain, .iteration, .draw),
               names_to = "Parameter", values_to = "Difference", 
               names_prefix = "delta_") %>%
  group_by(Parameter) %>%
  summarise(mean = mean(Difference),
            sd = sd(Difference),
            P = pmax( mean(Difference < 0), mean(Difference > 0) ))

Frontier2021_prior_posterior %>%
  filter(treatment != "Prior") %>%
  select(-c(epsilon, lambda, theta)) %>%
  mutate(species = species %>% 
           fct_recode("H" = "Laminaria hyperborea",
                      "O" = "Laminaria ochroleuca")) %>%
  pivot_wider(names_from = c(species, treatment),
              values_from = c(alpha, mu, tau)) %>%
  mutate(delta_alpha = alpha_H_0m - alpha_O_0m,
         delta_mu_0m = mu_H_0m - mu_O_0m,
         delta_mu_15m = mu_H_15m - mu_O_15m,
         delta_mu_30m = mu_H_30m - mu_O_30m,
         delta_mu_H = mu_H_0m - mu_H_30m,
         delta_mu_O = mu_O_0m - mu_O_30m,
         delta_tau = tau_H_0m - tau_O_0m) %>%
  select(.chain, .iteration, .draw, starts_with("delta")) %>%
  pivot_longer(cols = -c(.chain, .iteration, .draw),
               names_to = "Parameter", values_to = "Difference", 
               names_prefix = "delta_") %>%
  group_by(Parameter) %>%
  summarise(mean = mean(Difference),
            sd = sd(Difference),
            P = pmax( mean(Difference < 0), mean(Difference > 0) ))

Frontier2022_prior_posterior %>%
  filter(treatment != "Prior") %>%
  select(-c(epsilon, lambda, theta)) %>%
  mutate(species = species %>% 
           fct_recode("H" = "Laminaria hyperborea",
                      "O" = "Laminaria ochroleuca")) %>%
  pivot_wider(names_from = c(species, treatment),
              values_from = c(alpha, mu, tau)) %>%
  mutate(delta_alpha = alpha_H_0.5m - alpha_O_0.5m,
         delta_mu_0.5m = mu_H_0.5m - mu_O_0.5m,
         delta_mu_1.5m = mu_H_1.5m - mu_O_1.5m,
         delta_mu_3m = mu_H_3m - mu_O_3m,
         delta_mu_H = mu_H_0.5m - mu_H_3m,
         delta_mu_O = mu_O_0.5m - mu_O_3m) %>%
  select(.chain, .iteration, .draw, starts_with("delta")) %>%
  pivot_longer(cols = -c(.chain, .iteration, .draw),
               names_to = "Parameter", values_to = "Difference", 
               names_prefix = "delta_") %>%
  group_by(Parameter) %>%
  summarise(mean = mean(Difference),
            sd = sd(Difference),
            P = pmax( mean(Difference < 0), mean(Difference > 0) ))

Bourguès_prior_posterior %>%
  filter(treatment != "Prior") %>%
  select(-c(epsilon, lambda, theta)) %>%
  mutate(treatment = treatment %>% 
           fct_recode("Sp" = "Spring",
                      "Su" = "Summer",
                      "A" = "Autumn",
                      "W" = "Winter")) %>%
  pivot_wider(names_from = treatment,
              values_from = c(alpha, mu, tau)) %>%
  mutate(delta_alpha_Sp_Su = alpha_Sp - alpha_Su,
         delta_alpha_Sp_A = alpha_Sp - alpha_A,
         delta_alpha_Sp_W = alpha_Sp - alpha_W,
         delta_alpha_Su_A = alpha_Su - alpha_A,
         delta_alpha_Su_W = alpha_Su - alpha_W,
         delta_alpha_A_W = alpha_A - alpha_W,
         delta_mu_Sp_Su = mu_Sp - mu_Su,
         delta_mu_Sp_A = mu_Sp - mu_A,
         delta_mu_Sp_W = mu_Sp - mu_W,
         delta_mu_Su_A = mu_Su - mu_A,
         delta_mu_Su_W = mu_Su - mu_W,
         delta_mu_A_W = mu_A - mu_W,
         delta_tau_Sp_Su = tau_Sp - tau_Su,
         delta_tau_Sp_A = tau_Sp - tau_A,
         delta_tau_Sp_W = tau_Sp - tau_W,
         delta_tau_Su_A = tau_Su - tau_A,
         delta_tau_Su_W = tau_Su - tau_W,
         delta_tau_A_W = tau_A - tau_W) %>%
  select(.chain, .iteration, .draw, starts_with("delta")) %>%
  pivot_longer(cols = -c(.chain, .iteration, .draw),
               names_to = "Contrast", values_to = "Difference", 
               names_prefix = "delta_") %>%
  group_by(Contrast) %>%
  summarise(mean = mean(Difference),
            sd = sd(Difference),
            P = pmax( mean(Difference < 0), mean(Difference > 0) )) %>%
  separate(Contrast, into = c("Parameter", "First", "Second"), sep = "_")

# 5. Visualisation ####
# 5.1 Dead or alive (Figure 2) ####
Brouwer_prediction_summary_longer <- Brouwer_prediction_summary %>%
  filter(treatment != "Prior") %>%
  pivot_longer(cols = -c(t, treatment, .width, .point, .interval),
               names_to = c("parameter", "name"),
               names_sep = "\\.") %>%
  replace_na(list(name = "median")) %>%
  pivot_wider(names_from = name,
              values_from = value) %T>%
  print()
# Can safely ignore warning, which just informs that NAs were 
# produced before I replaced them with "median".

Brouwer_prediction_summary_longer %>%
  ggplot() +
    geom_pointrange(data = data %>%
                      filter(reference == "Brouwer 1996") %>%
                      droplevels() %>% 
                      distinct(t, p_mean, p_sd, treatment),
                    aes(t, p_mean, colour = treatment,
                      ymin = p_mean - p_sd,
                      ymax = p_mean + p_sd)) +
    geom_line(aes(t, median, colour = treatment)) +
    geom_ribbon(aes(t, ymin = lower, ymax = upper,
                    alpha = factor(.width), fill = treatment)) +
    scale_alpha_manual(values = c(0.5, 0.4, 0.3), guide = "none") +
    facet_grid(rows = vars(parameter), scales = "free") +
    theme_minimal()
# Not quite what I imagined. Better proceed with separate plots
# and combine using patchwork.

# Create theme
mytheme <- theme(
  plot.margin = margin(0.5, 0.25, 0, 0, unit = "cm"),
  panel.background = element_blank(),
  panel.grid = element_blank(),
  panel.spacing = unit(0.5, units = "cm"),
  text = element_text(family = "Futura"),
  axis.line = element_line(colour = "black", lineend = "square"),
  axis.title = element_text(size = 10),
  axis.title.y = element_text(margin = margin(l = -0.3, unit = "cm")),
  axis.text = element_text(colour = "black", size = 8),
  axis.ticks = element_line(colour = "black", lineend = "square"),
  strip.background = element_blank(),
  strip.placement = "inside",
  strip.text = element_text(size = 8, margin = margin(0, 0, 0, 0.1, unit = "cm")),
  legend.position = "inside",
  legend.title = element_blank(),
  legend.text = element_text(size = 8),
  legend.background = element_blank(),
  legend.margin = margin(0, 0, -8, 0),
  legend.key.width = unit(.3, "cm"),
  legend.key.height = unit(.3, "cm"),
  legend.key.spacing.y = unit(.05, "cm")
)

Fig_2_k <- Brouwer_prediction_summary_longer %>%
  filter(parameter == "k") %>%
  ggplot() +
    geom_line(aes(t, median, colour = treatment)) +
    geom_ribbon(aes(t, ymin = lower, ymax = upper,
                    alpha = factor(.width), fill = treatment)) +
    geom_density(data = Brouwer_prior_posterior %>%
                   filter(treatment != "Prior"),
                 aes(x = mu, y = after_stat(density) * 0.2, fill = treatment),
                 alpha = 0.8, colour = NA, position = position_nudge(y = -0.09)) +
    scale_colour_manual(values = c("#81a512", "#5e5003")) +
    scale_fill_manual(values = c("#81a512", "#5e5003")) +
    scale_alpha_manual(values = c(0.5, 0.4, 0.3), guide = "none") +
    scale_x_continuous(breaks = seq(0, 320, 80),
                       limits = c(0, 320),
                       oob = scales::oob_keep) +
    scale_y_continuous(breaks = seq(-0.09, 0, 0.03),
                       labels = scales::label_number(accuracy = c(rep(0.01, 3), 1),
                                                     style_negative = "minus")) +
    labs(x = expression(italic("t")*" (d)"),
         y = expression(italic("k")*" (d"^-1*")")) +
    coord_cartesian(ylim = c(-0.09, 0),
                    expand = F, clip = "off") +
    mytheme +
    theme(legend.position = "none",
          axis.title.y = element_text(vjust = -1))

Fig_2_mu <- Brouwer_prediction_summary_longer %>%
  filter(parameter == "p_mu") %>%
  ggplot() +
    geom_hline(yintercept = 1) +
    geom_line(aes(t, median, colour = treatment)) +
    geom_ribbon(aes(t, ymin = lower, ymax = upper,
                    alpha = factor(.width), fill = treatment)) +
    geom_pointrange(data = data %>%
                      filter(reference == "Brouwer 1996") %>%
                      droplevels() %>% 
                      distinct(t, p_mean, p_sd, treatment),
                    aes(t, p_mean, colour = treatment,
                        ymin = p_mean - p_sd,
                        ymax = p_mean + p_sd),
                    size = 0.2, shape = 16,
                    linewidth = 0.4) +
    scale_colour_manual(values = c("#81a512", "#5e5003")) +
    scale_fill_manual(values = c("#81a512", "#5e5003")) +
    scale_alpha_manual(values = c(0.5, 0.4, 0.3), guide = "none") +
    scale_x_continuous(breaks = seq(0, 320, 80)) +
    scale_y_continuous(breaks = seq(0, 1.2, 0.4),
                       labels = scales::label_number(accuracy = c(1, rep(0.1, 3)))) +
    labs(x = expression(italic("t")*" (d)"),
         y = expression(italic("μ"["m"]))) +
    coord_cartesian(xlim = c(0, 320), ylim = c(0, 1.2),
                    expand = F, clip = "off") +
    mytheme +
    theme(axis.title.x = element_blank(),
          axis.text.x = element_blank(),
          axis.title.y = element_text(vjust = -.33),
          legend.position = "none")

Fig_2_nu <- Brouwer_prediction_summary_longer %>%
  filter(parameter == "nu") %>%
  ggplot() +
    geom_line(aes(t, median, colour = treatment)) +
    geom_ribbon(aes(t, ymin = lower, ymax = upper,
                    alpha = factor(.width), fill = treatment)) +
    scale_colour_manual(values = c("#81a512", "#5e5003")) +
    scale_fill_manual(values = c("#81a512", "#5e5003")) +
    scale_alpha_manual(values = c(0.5, 0.4, 0.3), guide = "none") +
    scale_x_continuous(breaks = seq(0, 320, 80)) +
    scale_y_continuous(breaks = seq(0, 1500, 500)) +
    labs(x = expression(italic("t")*" (d)"),
         y = expression(italic("ν"))) +
    coord_cartesian(xlim = c(0, 320), ylim = c(0, 1500),
                    expand = F, clip = "off") +
    mytheme +
    theme(axis.title.x = element_blank(),
          axis.text.x = element_blank(),
          axis.title.y = element_text(vjust = 0),
          legend.position = "none")

Fig_2_m <- Brouwer_prediction_summary_longer %>%
  filter(parameter == "p") %>%
  ggplot() +
    geom_hline(yintercept = 1) +
    geom_line(aes(t, median, colour = treatment)) +
    geom_ribbon(aes(t, ymin = lower, ymax = upper,
                    alpha = factor(.width), fill = treatment)) +
    geom_pointrange(data = data %>%
                      filter(reference == "Brouwer 1996") %>%
                      droplevels() %>% 
                      distinct(t, p_mean, p_sd, treatment),
                    aes(t, p_mean, colour = treatment,
                        ymin = p_mean - p_sd,
                        ymax = p_mean + p_sd),
                    size = 0.2, shape = 16,
                    linewidth = 0.4) +
    scale_colour_manual(values = c("#81a512", "#5e5003")) +
    scale_fill_manual(values = c("#81a512", "#5e5003")) +
    scale_alpha_manual(values = c(0.5, 0.4, 0.3), guide = "none") +
    scale_x_continuous(breaks = seq(0, 320, 80)) +
    scale_y_continuous(breaks = seq(0, 1.2, 0.4),
                       labels = scales::label_number(accuracy = c(1, rep(0.1, 3)))) +
    labs(x = expression(italic("t")*" (d)"),
         y = expression(italic(tilde("m")))) +
    coord_cartesian(xlim = c(0, 320), ylim = c(0, 1.2),
                    expand = F, clip = "off") +
    mytheme +
    theme(legend.position.inside = c(0.9, 0.15),
          axis.title.x = element_blank(),
          axis.text.x = element_blank(),
          axis.title.y = element_text(vjust = 0))

require(patchwork)
Fig_2 <- Fig_2_m / Fig_2_nu / Fig_2_mu / Fig_2_k
Fig_2 %>%
  ggsave(filename = "Figure_2.pdf", path = "Figures",
         device = cairo_pdf, width = 10, height = 20, units = "cm")

# 5.2 Senescence (Figure 3) ####
Hamersley_prediction_summary_longer <- Hamersley_prediction_summary %>%
  filter(treatment != "Prior") %>%
  pivot_longer(cols = -c(t, treatment, .width, .point, .interval),
               names_to = c("parameter", "name"),
               names_sep = "\\.") %>%
  replace_na(list(name = "median")) %>%
  pivot_wider(names_from = name,
              values_from = value) %T>%
  print()

Bettignies_prediction_summary_longer <- Bettignies_prediction_summary %>%
  filter(treatment != "Prior") %>%
  pivot_longer(cols = -c(t, treatment, .width, .point, .interval),
               names_to = c("parameter", "name"),
               names_sep = "\\.") %>%
  replace_na(list(name = "median")) %>%
  pivot_wider(names_from = name,
              values_from = value) %T>%
  print()

Fig_3a_k <- Hamersley_prediction_summary_longer %>%
  filter(parameter == "k") %>%
  ggplot() +
    geom_hline(yintercept = 0) +
    geom_line(aes(t, median, colour = treatment)) +
    geom_ribbon(aes(t, ymin = lower, ymax = upper,
                    alpha = factor(.width), fill = treatment)) +
    scale_colour_manual(values = c("#81a512", "#afab00", "#5e5003")) +
    scale_fill_manual(values = c("#81a512", "#afab00", "#5e5003")) +
    scale_alpha_manual(values = c(0.5, 0.4, 0.3), guide = "none") +
    scale_x_continuous(breaks = seq(0, 25, 5)) +
    scale_y_continuous(breaks = seq(-0.16, 0.02, 0.06),
                       labels = scales::label_number(accuracy = c(0.01, 0.1, 0.01, 0.01),
                                                     style_negative = "minus")) +
    labs(x = expression(italic("t")*" (d)"),
         y = expression(italic("k")*" (d"^-1*")")) +
    coord_cartesian(xlim = c(0, 25), ylim = c(-0.16, 0.02),
                    expand = F, clip = "off") +
    mytheme +
    theme(axis.title.y = element_text(vjust = -1),
          legend.position = "none")

Fig_3a_mu <- Hamersley_prediction_summary_longer %>%
  filter(parameter == "p_mu") %>%
  ggplot() +
    geom_line(aes(t, median, colour = treatment)) +
    geom_ribbon(aes(t, ymin = lower, ymax = upper,
                    alpha = factor(.width), fill = treatment)) +
    geom_pointrange(data = data %>%
                      filter(reference == "Hamersley et al. 2015" & t != 0) %>%
                      droplevels() %>% 
                      distinct(t, p_mean, p_sd, treatment),
                    aes(t, p_mean, colour = treatment,
                        ymin = p_mean - p_sd,
                        ymax = p_mean + p_sd),
                    size = 0.2, shape = 16,
                    linewidth = 0.4) +
    geom_density(data = Hamersley_prior_posterior %>%
                   filter(treatment != "Prior"),
                 aes(x = mu, y = after_stat(density) * 0.2, fill = treatment),
                 alpha = 0.8, colour = NA) +
    scale_colour_manual(values = c("#81a512", "#afab00", "#5e5003")) +
    scale_fill_manual(values = c("#81a512", "#afab00", "#5e5003")) +
    scale_alpha_manual(values = c(0.5, 0.4, 0.3), guide = "none") +
    scale_x_continuous(breaks = seq(0, 25, 5),
                       limits = c(0, 25),
                       oob = scales::oob_keep) +
    scale_y_continuous(breaks = seq(0, 1, 0.25),
                       labels = scales::label_number(accuracy = c(1, 0.01, 0.1, 0.01, 1))) +
    labs(x = expression(italic("t")*" (d)"),
         y = expression(italic("μ"["m"]))) +
    coord_cartesian(ylim = c(0, 1),
                    expand = F, clip = "off") +
    mytheme +
    theme(plot.margin = margin(0.2, 0.25, 0, 0, unit = "cm"),
          legend.position.inside = c(0.89, 0.89),
          axis.title.x = element_blank(),
          axis.text.x = element_blank(),
          axis.title.y = element_text(vjust = -.33))

Fig_3b_k <- Bettignies_prediction_summary_longer %>%
  filter(parameter == "k") %>%
  ggplot() +
    geom_hline(yintercept = 0) +
    geom_line(aes(t, median, colour = treatment)) +
    geom_ribbon(aes(t, ymin = lower, ymax = upper,
                    alpha = factor(.width), fill = treatment)) +
    scale_colour_manual(values = c("#81a512", "#afab00")) +
    scale_fill_manual(values = c("#81a512", "#afab00")) +
    scale_alpha_manual(values = c(0.5, 0.4, 0.3), guide = "none") +
    scale_x_continuous(breaks = seq(0, 180, 30)) +
    scale_y_continuous(breaks = seq(-0.04, 0.02, 0.02),
                       labels = scales::label_number(accuracy = c(rep(0.01, 2), 1, 0.01),
                                                     style_negative = "minus")) +
    labs(x = expression(italic("t")*" (d)"),
         y = expression(italic("k")*" (d"^-1*")")) +
    coord_cartesian(xlim = c(0, 180), ylim = c(-0.04, 0.02),
                    expand = F, clip = "off") +
    mytheme +
    theme(axis.title.y = element_text(vjust = -1),
          legend.position = "none")

Fig_3b_mu <- Bettignies_prediction_summary_longer %>%
  filter(parameter == "p_mu") %>%
  ggplot() +
    geom_hline(yintercept = 1) +
    geom_line(aes(t, median, colour = treatment)) +
    geom_ribbon(aes(t, ymin = lower, ymax = upper,
                    alpha = factor(.width), fill = treatment)) +
    geom_pointrange(data = data %>%
                      filter(reference == "de Bettignies et al. 2020" & t != 0) %>%
                      droplevels() %>% 
                      distinct(t, p_mean, p_sd, treatment),
                    aes(t, p_mean, colour = treatment,
                        ymin = p_mean - p_sd,
                        ymax = p_mean + p_sd),
                    size = 0.2, shape = 16,
                    linewidth = 0.4) +
    geom_density(data = Bettignies_prior_posterior %>%
                   filter(treatment != "Prior"),
                 aes(x = mu, y = after_stat(density) * 1.2, fill = treatment),
                 alpha = 0.8, colour = NA) +
    scale_colour_manual(values = c("#81a512", "#afab00")) +
    scale_fill_manual(values = c("#81a512", "#afab00")) +
    scale_alpha_manual(values = c(0.5, 0.4, 0.3), guide = "none") +
    scale_x_continuous(breaks = seq(0, 180, 30),
                       limits = c(0, 180),
                       oob = scales::oob_keep) +
    scale_y_continuous(breaks = seq(0, 1.2, 0.4),
                       labels = scales::label_number(accuracy = c(1, rep(0.1, 3)))) +
    labs(x = expression(italic("t")*" (d)"),
         y = expression(italic("μ"["m"]))) +
    coord_cartesian(ylim = c(0, 1.2),
                    expand = F, clip = "off") +
    mytheme +
    theme(plot.margin = margin(0.2, 0.25, 0, 0, unit = "cm"),
          legend.position = "none",
          axis.title.x = element_blank(),
          axis.text.x = element_blank(),
          axis.title.y = element_text(vjust = -.33))

Fig_3 <- ( Fig_3a_mu / Fig_3a_k / Fig_3b_mu / Fig_3b_k ) +
  plot_annotation(tag_levels = list(c("a", "", "b", ""))) &
  theme(plot.tag = element_text(family = "Futura", size = 12, face = "bold"),
        plot.tag.position = c(-0.002, 1.04))

Fig_3 %>%
  ggsave(filename = "Figure_3.pdf", path = "Figures",
         device = cairo_pdf, width = 10, height = 20, units = "cm")
# Safe to ignore warning due to missing s.d. for senescent treatment.

# 5.3 Light (Figure 4) ####
Fig_4a <- Frontier2021_prediction_summary %>%
  filter(treatment != "Prior") %>%
  ggplot() +
    geom_hline(yintercept = 1) +
    geom_line(aes(t, p, colour = treatment)) +
    geom_ribbon(aes(t, ymin = p.lower, ymax = p.upper,
                    alpha = factor(.width), fill = treatment)) +
    geom_point(data = data %>%
                 filter(reference == "Frontier et al. 2021" & t != 0) %>%
                 droplevels(),
               aes(t, p, colour = treatment), shape = 16, alpha = 0.5) +
    scale_colour_manual(values = c("#f5a54a", "#6a98b4", "#2e4a5b"),
                        guide = "none") +
    scale_fill_manual(values = c("#f5a54a", "#6a98b4", "#2e4a5b"),
                      guide = "none") +
    scale_alpha_manual(values = c(0.5, 0.4, 0.3), guide = "none") +
    scale_x_continuous(breaks = seq(0, 100, 20)) +
    scale_y_continuous(breaks = seq(0, 2.4, 0.8),
                       labels = scales::label_number(accuracy = c(1, rep(0.1, 3)))) +
    labs(x = expression(italic("t")*" (d)"),
         y = expression(italic(tilde("m")))) +
    coord_cartesian(xlim = c(0, 100), ylim = c(0, 2.4),
                    expand = F, clip = "off") +
    facet_grid2(treatment ~ species,
                switch = "y",
                strip = strip_nested(text_y = element_text(angle = 0, hjust = 0, vjust = 1)),
                labeller = labeller(
                  treatment = as_labeller(c(
                    "0m" = "0 m",
                    "15m" = "15 m",
                    "30m" = "30 m"
                  ))
                )) +
    mytheme +
    theme(strip.text.x = element_text(face = "italic", hjust = 0),
          plot.margin = margin(0, 0.5, 0, 0.2, unit = "cm"))

Fig_4b <- Frontier2022_prediction_summary %>%
  filter(treatment != "Prior") %>%
  ggplot() +
    geom_hline(yintercept = 1) +
    geom_line(aes(t, p, colour = treatment)) +
    geom_ribbon(aes(t, ymin = p.lower, ymax = p.upper,
                    alpha = factor(.width), fill = treatment)) +
    geom_point(data = data %>%
                 filter(reference == "Frontier et al. 2022" & t != 0) %>%
                 droplevels(),
               aes(t, p, colour = treatment), shape = 16, alpha = 0.5) +
    scale_colour_manual(values = c("#f5a54a", "#6a98b4", "#2e4a5b"),
                        guide = "none") +
    scale_fill_manual(values = c("#f5a54a", "#6a98b4", "#2e4a5b"),
                      guide = "none") +
    scale_alpha_manual(values = c(0.5, 0.4, 0.3), guide = "none") +
    scale_x_continuous(breaks = seq(0, 40, 20)) +
    scale_y_continuous(breaks = seq(0, 1.5, 0.5),
                       labels = scales::label_number(accuracy = c(1, 0.1, 1, 0.1))) +
    labs(x = expression(italic("t")*" (d)"),
         y = expression(italic(tilde("m")))) +
    coord_cartesian(xlim = c(0, 40), ylim = c(0, 1.5),
                    expand = F, clip = "off") +
    facet_grid2(treatment ~ species,
                switch = "y",
                strip = strip_nested(text_y = element_text(angle = 0, hjust = 0, vjust = 1)),
                labeller = labeller(
                  treatment = as_labeller(c(
                    "0.5m" = "0.5 m",
                    "1.5m" = "1.5 m",
                    "3m" = "3 m"
                  )),
                  species = as_labeller(c(
                    "Laminaria hyperborea" = "L. hyperborea",
                    "Laminaria ochroleuca" = "L. ochroleuca"
                  ))
                )) +
    mytheme +
    theme(strip.text.x = element_text(face = "italic", hjust = 0),
          plot.margin = margin(0, 0.2, 0, 0.2, unit = "cm"))

Fig_4 <- ( Fig_4a | Fig_4b ) +
  plot_layout(widths = c(1, 0.428)) +
  plot_annotation(tag_levels = c("a", "b")) &
  theme(plot.tag = element_text(family = "Futura", size = 12, face = "bold"),
        plot.tag.position = c(-0.018, 0.996))

Fig_4 %>%
  ggsave(filename = "Figure_4.pdf", path = "Figures",
         device = cairo_pdf, width = 20, height = 10, units = "cm")

# 5.4 Season (Figure 5) ####
Fig_5 <- Bourguès_prediction_summary %>%
  filter(treatment != "Prior") %>%
  ggplot() +
    geom_hline(yintercept = 1) +
    geom_line(aes(t, p, colour = treatment)) +
    geom_ribbon(aes(t, ymin = p.lower, ymax = p.upper,
                    alpha = factor(.width), fill = treatment)) +
    geom_point(data = data %>%
                 filter(reference == "Bourguès et al. 1996" & t != 0) %>%
                 droplevels(),
               aes(t, p, colour = treatment), shape = 16, alpha = 0.5) +
    geom_text(aes(x = 36, y = 1.1, label = treatment),
              check_overlap = T, size.unit = "pt", size = 8,
              family = "Futura", hjust = 1, vjust = 0) +
    scale_colour_manual(values = c("#81a512", "#f5a54a", "#5e5003", "#6a98b4"),
                        guide = "none") +
    scale_fill_manual(values = c("#81a512", "#f5a54a", "#5e5003", "#6a98b4"),
                      guide = "none") +
    scale_alpha_manual(values = c(0.5, 0.4, 0.3), guide = "none") +
    scale_x_continuous(breaks = seq(0, 36, 6)) +
    scale_y_continuous(breaks = seq(0, 1.2, 0.4),
                       labels = scales::label_number(accuracy = c(1, rep(0.1, 3)))) +
    labs(x = expression(italic("t")*" (d)"),
         y = expression(italic(tilde("m")))) +
    coord_cartesian(xlim = c(0, 36), ylim = c(0, 1.2),
                    expand = F, clip = "off") +
    facet_grid(rows = vars(treatment)) +
    mytheme +
    theme(strip.text = element_blank(),
          plot.margin = margin(0.45, 0.45, 0, 0.45, unit = "cm"))

Fig_5 %>%
  ggsave(filename = "Figure_5.pdf", path = "Figures",
         device = cairo_pdf, width = 10, height = 10, units = "cm")