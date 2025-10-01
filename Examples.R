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
      t * alpha - ( alpha + tau ) * 
        log( 
          ( 1 + exp( t - mu ) ) / 
            ( 1 + exp( -mu ) )
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
      t * alpha - ( alpha + tau ) * mu / 5 * 
        log( 
          ( 1 + exp( 5 / mu * ( t - mu ) ) ) / 
            ( 1 + exp( -5 ) )
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
      t * alpha - ( alpha + tau ) * 
        log( 
          ( 1 + exp( t - mu ) ) / 
            ( 1 + exp( -mu ) )
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
      t * alpha - ( alpha + tau ) * mu / 5 * 
        log( 
          ( 1 + exp( 5 / mu * ( t - mu ) ) ) / 
            ( 1 + exp( -5 ) )
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
      t * alpha - ( alpha + tau ) * 
        log( 
          ( 1 + exp( t - mu ) ) / 
            ( 1 + exp( -mu ) )
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
      t * alpha - ( alpha + tau ) * mu / 5 * 
        log( 
          ( 1 + exp( 5 / mu * ( t - mu ) ) ) / 
            ( 1 + exp( -5 ) )
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
      t * alpha - ( alpha + tau ) * 
        log( 
          ( 1 + exp( t - mu ) ) / 
            ( 1 + exp( -mu ) )
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
      t * alpha - ( alpha + tau ) * mu / 5 * 
        log( 
          ( 1 + exp( 5 / mu * ( t - mu ) ) ) / 
            ( 1 + exp( -5 ) )
        )
    ),
    k = ( alpha + tau ) / ( 1 + exp( 5 / mu * ( t - mu ) ) ) - tau,
    nu = ( epsilon - theta ) * exp( -lambda * t ) + theta,
    p = rbetapr( n() , p_mu * ( 1 + nu ) , 2 + nu )
  ) %T>%
  print()
# Again, some NAs produced in p
Frontier_relative_prediction %>%
  group_by(species, treatment) %>%
  summarise(p %>% is.na() %>% any())
# Mostly Prior is affected indicating that some prior values are impossible 
# to start with. But there are also some impossible values in the Laminaria
# hyperborea 3 m treatment. Trial and error suggests that this is due to the 
# exponential(10) prior on tau. This can be further constrained using gamma, 
# but this is not concerning since there are plenty of samples and the fraction
# of NAs is small:
Frontier_relative_prediction %>%
  group_by(species, treatment) %>%
  summarise(p %>% is.na() %>% sum() / n())
# So we can simply remove NAs.

# Summarise predictions
Frontier_constant_prediction_summary <- Frontier_constant_prediction %>%
  group_by(t, species, treatment) %>%
  mean_qi(p_mu, k, nu, p, .width = c(.5, .8, .9)) %T>%
  print()

Frontier_relative_prediction_summary <- Frontier_relative_prediction %>%
  drop_na() %>% # NAs affect summary
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
      t * alpha - ( alpha + tau ) * mu / 5 * 
        log( 
          ( 1 + exp( 5 / mu * ( t - mu ) ) ) / 
            ( 1 + exp( -5 ) )
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
                    predictor_name = "t",
                    group_name = "treatment") %>%
  mutate(
    p_mu = exp(
      t * alpha - ( alpha + tau ) * mu / 5 * 
        log( 
          ( 1 + exp( 5 / mu * ( t - mu ) ) ) / 
            ( 1 + exp( -5 ) )
        )
    ),
    k = ( alpha + tau ) / ( 1 + exp( 5 / mu * ( t - mu ) ) ) - tau,
    nu = ( epsilon - theta ) * exp( -lambda * t ) + theta,
    p = rbetapr( n() , p_mu * ( 1 + nu ) , 2 + nu )
  ) %T>%
  print()
# Some NAs produced by rbetapr due to arithmetic underflow
Birch_prediction %>%
  group_by(treatment) %>%
  summarise(p %>% is.na() %>% sum() / n())
# Essentially no NAs

# Summarise predictions
Birch_prediction_summary <- Birch_prediction %>%
  drop_na() %>% # NAs affect summary
  group_by(t, treatment) %>%
  median_qi(p_mu, k, nu, p, .width = c(.5, .8, .9)) %T>%
  print()

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
       alpha = rnorm( 1e3 , 0 , 0.01 ), 
       mu = rgamma( 1e3 , 300^2 / 200^2 , 300 / 200^2 ),
       tau = rgamma( 1e3 , 0.1^2 / 0.05^2 , 0.1 / 0.05^2 )) %>%
  expand_grid(t = data %>%
                filter(reference == "Brouwer 1996") %$% 
                seq(min(t), max(t), length.out = 100)) %>%
  mutate(
    p = exp(
      t * alpha - ( alpha + tau ) * mu / 5 * 
        log( 
          ( 1 + exp( 5 / mu * ( t - mu ) ) ) / 
            ( 1 + exp( -5 ) )
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
# No rhat above 1.001. rhat = 1.00 ± 0.0000937. Great.

# Chains
Brouwer_samples$draws(format = "df") %>%
  mcmc_rank_overlay()
# Chains are good.

# Pairs
Brouwer_samples$draws(format = "df") %>%
  mcmc_pairs(pars = c("alpha[1]", "mu[1]", "tau"))
Brouwer_samples$draws(format = "df") %>%
  mcmc_pairs(pars = c("alpha[2]", "mu[2]", "tau"))
# Looks fine.

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
                   "epsilon[treatment]", "lambda[treatment]",
                   "theta[treatment]"),
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
                   "epsilon[treatment]", "lambda[treatment]",
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
Brouwer_prediction <- Brouwer_prior_posterior %>%
  spread_continuous(data = data %>% 
                      filter(reference == "Brouwer 1996") %>%
                      droplevels(), 
                    predictor_name = "t",
                    group_name = "treatment") %>%
  mutate(
    p_mu = exp(
      t * alpha - ( alpha + tau ) * mu / 5 * 
        log( 
          ( 1 + exp( 5 / mu * ( t - mu ) ) ) / 
            ( 1 + exp( -5 ) )
        )
    ),
    k = ( alpha + tau ) / ( 1 + exp( 5 / mu * ( t - mu ) ) ) - tau,
    nu = ( epsilon - theta ) * exp( -lambda * t ) + theta,
    p = rbetapr( n() , p_mu * ( 1 + nu ) , 2 + nu )
  ) %T>%
  print()

Brouwer_prediction %>%
  group_by(treatment) %>%
  summarise(p %>% is.na() %>% sum() / n())
# Essentially no NAs

# Summarise predictions
Brouwer_prediction_summary <- Brouwer_prediction %>%
  drop_na() %>% # NAs affect summary
  group_by(t, treatment) %>%
  median_qi(p_mu, k, nu, p, .width = c(.5, .8, .9)) %T>%
  print()

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

#############################

# 3.3 Hamersley et al. 2015 ####
# 3.2.1 Visualisation ####
data %>%
  filter(reference == "Hamersley et al. 2015") %>%
  droplevels() %>%
  ggplot() +
    geom_pointrange(data = . %>%
                      distinct(t, p_mean, p_sd, treatment),
                    aes(t, p_mean,
                        ymin = p_mean - p_sd,
                        ymax = p_mean + p_sd)) +
    facet_grid(~ treatment) +
    theme_minimal()

# 2.1.2 Prior simulation ####
tibble(n = 1:1e3,
       alpha = rnorm( 1e3 , 0 , 0.01 ), 
       mu = rgamma( 1e3 , 10^2 / 6^2 , 10 / 6^2 ),
       tau = rgamma( 1e3 , 0.1^2 / 0.05^2 , 0.1 / 0.05^2 )) %>%
  expand_grid(t = data %>%
                filter(reference == "Hamersley et al. 2015") %$% 
                seq(min(t), max(t), length.out = 100)) %>%
  mutate(
    p = exp(
      t * alpha - ( alpha + tau ) * mu / 5 * 
        log( 
          ( 1 + exp( 5 / mu * ( t - mu ) ) ) / 
            ( 1 + exp( -5 ) )
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

# 2.1.3 Stan models ####
Hamersley_model <- here("Stan", "Hamersley.stan") %>% 
  read_file() %>%
  write_stan_file() %>%
  cmdstan_model()

Hamersley_samples <- Hamersley_model$sample(
          data = data %>%
            filter(reference == "Hamersley et al. 2015" &
                     t != 0) %>%
            droplevels() %>%
            distinct(t, p_mean, p_sd, treatment) %>%
            compose_data(),
          chains = 8,
          parallel_chains = parallel::detectCores(),
          iter_warmup = 1e4,
          iter_sampling = 1e4,
          adapt_delta = 0.99
        ) %T>%
  print()

# 2.1.4 Model checks ####
# Rhat
Hamersley_samples$summary() %>%
  mutate(rhat_check = rhat > 1.001) %>%
  summarise(rhat_1.001 = sum(rhat_check) / length(rhat),
            rhat_mean = mean(rhat),
            rhat_sd = sd(rhat))
# No of rhat above 1.001. rhat = 1.00 ± 0.000144. Good.

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
# Chains are great.

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
    filter(reference == "Brouwer 1996" &
             t != 0) %>%
    droplevels() %>%
    select(t, p, treatment) %>%
    compose_data()
  )

Brouwer_relative_prior <- prior_samples(
  model = Brouwer_relative_model,
  data = data %>% 
    filter(reference == "Brouwer 1996" &
             t != 0) %>%
    droplevels() %>%
    select(t, p, treatment) %>%
    compose_data()
)

Brouwer_constant_prior %>% 
  prior_posterior_draws(
    posterior_samples = Brouwer_constant_samples,
    group = data %>% 
      filter(reference == "Brouwer 1996" &
               t != 0) %>%
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
      filter(reference == "Brouwer 1996" &
               t != 0) %>%
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
      filter(reference == "Brouwer 1996" &
               t != 0) %>%
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
      filter(reference == "Brouwer 1996" &
               t != 0) %>%
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
require(extraDistr) # R doesn't have a native beta prime function
Brouwer_constant_prediction <- Brouwer_constant_prior_posterior %>%
  spread_continuous(data = data %>% # note full predictor range is used
                      filter(reference == "Brouwer 1996") %>%
                      droplevels(), 
                    predictor_name = "t",
                    group_name = "treatment") %>%
  mutate(
    p_mu = exp(
      t * alpha - ( alpha + tau ) * 
        log( 
          ( 1 + exp( t - mu ) ) / 
            ( 1 + exp( -mu ) )
        )
    ),
    k = ( alpha + tau ) / ( 1 + exp( t - mu ) ) - tau,
    nu = theta + exp( log( epsilon - theta ) - lambda * t ),
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
      t * alpha - ( alpha + tau ) * mu / 5 * 
        log( 
          ( 1 + exp( 5 / mu * ( t - mu ) ) ) / 
            ( 1 + exp( -5 ) )
        )
    ),
    k = ( alpha + tau ) / ( 1 + exp( 5 / mu * ( t - mu ) ) ) - tau,
    nu = theta + exp( log( epsilon - theta ) - lambda * t ),
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


# 3.4 de Bettignies et al. 2020 ####

# 3.5 Frontier et al. 2021 ####

# 3.6 Frontier et al. 2022 ####

# 3.7 Bourguès et al. 1996 ####









# 4. Visualisation ####
# 4.1 Dead or alive ####

# 4.2 Senescence ####

# 4.3 Light ####

# 4.4 Season ####



# Summarise parameters for annotation
require(glue)
ratio_annotation <- ratio_prior_posterior %>%
  group_by(Species) %>%
  summarise(beta_mean = mean(beta),
            beta_sd = sd(beta),
            cv_mean = mean(cv),
            cv_sd = sd(cv),
            n = n()) %>%
  mutate(label = glue(
    "μ = {signif(beta_mean, 2)} ± {signif(beta_sd, 2)} × x\n
         σ = {signif(cv_mean, 2)} ± {signif(cv_sd, 2)} × μ"
  )
  ) %T>%
  print()

# Define custom theme
mytheme <- theme(panel.background = element_blank(),
                 panel.grid.major = element_blank(),
                 panel.grid.minor = element_blank(),
                 panel.border = element_blank(),
                 plot.margin = margin(0.2, 0.5, 0.2, 0.2, unit = "cm"),
                 axis.line = element_line(),
                 axis.title = element_text(size = 12, hjust = 0),
                 axis.text = element_text(size = 10, colour = "black"),
                 axis.ticks.length = unit(.25, "cm"),
                 axis.ticks = element_line(colour = "black", lineend = "square"),
                 legend.key = element_blank(),
                 legend.key.width = unit(.25, "cm"),
                 legend.key.height = unit(.45, "cm"),
                 legend.key.spacing.x = unit(.5, "cm"),
                 legend.key.spacing.y = unit(.05, "cm"),
                 legend.background = element_blank(),
                 legend.position = "top",
                 legend.justification = 0,
                 legend.text = element_text(size = 12, hjust = 0),
                 legend.title = element_blank(),
                 legend.margin = margin(0, 0, 0, 0, unit = "cm"),
                 strip.background = element_blank(),
                 strip.text = element_text(size = 12, hjust = 0, face = "italic"),
                 panel.spacing = unit(1, "cm"),
                 text = element_text(family = "Futura"))

# Plot
require(geomtextpath)
require(ggh4x)
Fig_2b <- ggplot() + 
    geom_point(data = data,
               aes(t, p, colour = treatment),
               size = 2) +
    # geom_ribbon(data = ratio_prediction_summary %>%
    #               filter(Species == "Prior" &
    #                        .width == 0.9) %>%
    #               select(-Species),
    #             aes(Fresh, ymin = Dry.lower, ymax = Dry.upper),
    #             alpha = 0.1) +
    geom_line(data = Brouwer_prediction_summary %>%
                filter(treatment != "Prior"),
              aes(t, p_mu, colour = treatment)) +
    geom_ribbon(data = Brouwer_prediction_summary %>%
                  filter(treatment != "Prior"),
                aes(t, ymin = p.lower, ymax = p.upper,
                    fill = treatment, alpha = factor(.width))) +
    # geom_text(data = ratio_annotation %>%
    #             filter(!Species %in% c("Unobserved", "Prior")),
    #           aes(x = c(33, 4.95), y = c(2.5, 0.335), label = label),
    #           family = "Futura", size = 3.5, hjust = 0,
    #           lineheight = 0.8) +
    scale_fill_manual(values = c("#c3b300", "#4a7518"), guide = "none") +
    scale_colour_manual(values = c("#c3b300", "#4a7518"), guide = "none") +
    scale_alpha_manual(values = c(0.5, 0.4, 0.3), guide = "none") +
    labs(x = "t",
         y = "p") +
    coord_cartesian(expand = FALSE, clip = "off") +
    mytheme

Fig_S1 %>%
  ggsave(filename = "Fig_S1_unedited.pdf", path = "Figures",
         device = cairo_pdf, height = 10, width = 21, 
         units = "cm")

