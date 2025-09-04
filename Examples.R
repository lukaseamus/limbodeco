require(tidyverse)
require(magrittr)
set.seed(100)
data <- read_csv("Examples.csv") %>%
  mutate(species = species %>% fct(),
         treatment = treatment %>% fct(),
         p_mean = if_else(p_mean == 0, 1e-5, p_mean)) %>%
  rowwise() %>%
  mutate(p = if( !is.na(p_sd) ) {
    list( rgamma( n , p_mean^2 / p_sd^2 , p_mean / p_sd^2 ) )
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

data %>%
  filter(reference == "Brouwer 1996") %>%
  ggplot() +
    geom_point(aes(t, p), shape = 16, alpha = 0.5) +
    geom_pointrange(data = . %>% 
                      distinct(t, p_mean, p_sd, treatment),
                    aes(t, p_mean, 
                        ymin = p_mean - p_sd,
                        ymax = p_mean + p_sd)) +
    facet_grid(~ treatment) +
    theme_minimal()

# Constant model
tibble(n = 1:1e3,
       alpha = rnorm( 1e3 , 0 , 0.01 ), 
       mu = rexp( 1e3 , 0.01 ),
       tau = rexp( 1e3 , 10 )) %>%
  expand_grid(t = data %$% 
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
    geom_hline(yintercept = data %$%
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
  expand_grid(t = data %$% 
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
    geom_hline(yintercept = data %$%
                 range(p)) +
    geom_line(alpha = 0.05) +
    coord_cartesian(ylim = c(-0.1, 1.5), expand = F, clip = "off") +
    theme_minimal() +
    theme(panel.grid = element_blank())

# 1.2.3 Stan model ####
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
            select(t, p, treatment) %>%
            compose_data(),
          chains = 8,
          parallel_chains = parallel::detectCores(),
          iter_warmup = 1e4,
          iter_sampling = 1e4
        ) %T>%
  print()

# 1.2.4 Model checks ####
# Rhat
Brouwer_constant_samples$summary() %>%
  drop_na(rhat) %>%
  mutate(rhat_check = rhat > 1.001) %>%
  summarise(rhat_1.001 = sum(rhat_check) / length(rhat),
            rhat_mean = mean(rhat),
            rhat_sd = sd(rhat))
# 50% of rhat above 1.001. rhat = 1.00 ± 0.000510. Ok.

Brouwer_relative_samples$summary() %>%
  drop_na(rhat) %>%
  mutate(rhat_check = rhat > 1.001) %>%
  summarise(rhat_1.001 = sum(rhat_check) / length(rhat),
            rhat_mean = mean(rhat),
            rhat_sd = sd(rhat))
# No rhat above 1.001. rhat = 1.00 ± 0.0000662. Great.

# Chains
require(bayesplot)
Brouwer_constant_samples$draws(format = "df") %>%
  mcmc_rank_overlay(pars = c("alpha[1]", "alpha[2]",
                             "mu[1]", "mu[2]", "tau",
                             "theta"))
# Chains are ok.

Brouwer_relative_samples$draws(format = "df") %>%
  mcmc_rank_overlay(pars = c("alpha[1]", "alpha[2]",
                             "mu[1]", "mu[2]", "tau",
                             "theta"))
# Chains are good.

# Pairs
Brouwer_constant_samples$draws(format = "df") %>%
  mcmc_pairs(pars = c("alpha[1]", "mu[1]", "tau", "theta"))
Brouwer_constant_samples$draws(format = "df") %>%
  mcmc_pairs(pars = c("alpha[2]", "mu[2]", "tau", "theta"))
# Pairs don't look great. Some bimodality, and non-identifiability.

Brouwer_relative_samples$draws(format = "df") %>%
  mcmc_pairs(pars = c("alpha[1]", "mu[1]", "tau", "theta"))
Brouwer_relative_samples$draws(format = "df") %>%
  mcmc_pairs(pars = c("alpha[2]", "mu[2]", "tau", "theta"))
# Some positive correlation between mu and tau, and negative
# correlation between alpha and mu, but not as concerning. 
# No bimodality. Generally looks more stable.

# 1.2.5 Prior-posterior comparison ####
source("functions.R")
Brouwer_constant_prior <- prior_samples(
  model = Brouwer_constant_model,
  data = data %>% 
    filter(reference == "Brouwer 1996") %>%
    select(t, p, treatment) %>%
    compose_data()
  )

Brouwer_relative_prior <- prior_samples(
  model = Brouwer_relative_model,
  data = data %>% 
    filter(reference == "Brouwer 1996") %>%
    select(t, p, treatment) %>%
    compose_data()
)

Brouwer_constant_prior %>% 
  prior_posterior_draws(
    posterior_samples = Brouwer_constant_samples,
    group = data %>% 
      filter(reference == "Brouwer 1996") %>%
      select(treatment),
    parameters = c("alpha[treatment]", "mu[treatment]", 
                   "tau", "theta"),
    format = "long"
    ) %>%
  prior_posterior_plot(group_name = "treatment", ridges = FALSE)
# Some near-bimodality. mu has a strange sharp posterior.
# Generally looks unstable.

Brouwer_relative_prior %>% 
  prior_posterior_draws(
    posterior_samples = Brouwer_relative_samples,
    group = data %>% 
      filter(reference == "Brouwer 1996") %>%
      select(treatment),
    parameters = c("alpha[treatment]", "mu[treatment]", 
                   "tau", "theta"),
    format = "long"
  ) %>%
  prior_posterior_plot(group_name = "treatment", ridges = FALSE)
# Looks much more stable.

# 1.2.6 Prediction ####
Brouwer_constant_prior_posterior <- Brouwer_constant_prior %>% 
  prior_posterior_draws(
    posterior_samples = Brouwer_constant_samples,
    parameters = c("alpha[treatment]", "mu[treatment]", 
                   "tau", "theta"),
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
    parameters = c("alpha[treatment]", "mu[treatment]", 
                   "tau", "theta"),
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
                      filter(reference == "Brouwer 1996"), 
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
    p = rgamma( n() , p_mu / theta , 1 / theta )
  ) %T>%
  print()

Brouwer_relative_prediction <- Brouwer_relative_prior_posterior %>%
  spread_continuous(data = data %>% 
                      filter(reference == "Brouwer 1996"), 
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
    p = rgamma( n() , p_mu / theta , 1 / theta )
  ) %T>%
  print()

# Summarise predictions
Brouwer_constant_prediction_summary <- Brouwer_constant_prediction %>%
  # filter(is.finite(p_mu) & is.finite(p)) %>%
  group_by(t, treatment) %>%
  mean_qi(p_mu, p, .width = c(.5, .8, .9)) %T>%
  print()

Brouwer_relative_prediction_summary <- Brouwer_relative_prediction %>%
  # filter(is.finite(p_mu) & is.finite(p)) %>%
  group_by(t, treatment) %>%
  mean_qi(p_mu, p, .width = c(.5, .8, .9)) %T>%
  print()

# Mean prediction
data %>%
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


# Response prediction
data %>%
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

# While the constant model seems to fit the data better,
# it generally looks less stable, the transition being very 
# jagged, almost like a piecewise model. Let's look at LOO.

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
# The relative model wins here too. Let's try one more dataset.

data %>%
  filter(reference == "Frontier et al. 2022") %>%
  ggplot() +
    geom_point(aes(t, p), shape = 16, alpha = 0.3) +
    facet_grid(treatment ~ species) +
    theme_minimal()

# Constant model
tibble(n = 1:1e3,
       alpha = rnorm( 1e3 , 0 , 0.005 ), 
       mu = rgamma( 1e3 , 30^2 / 15^2 , 30 / 15^2 ),
       tau = rgamma( 1e3 , 0.1^2 / 0.05^2 , 0.1 / 0.05^2 )) %>%
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
       alpha = rnorm( 1e3 , 0 , 0.005 ), 
       mu = rgamma( 1e3 , 30^2 / 15^2 , 30 / 15^2 ),
       tau = rgamma( 1e3 , 0.1^2 / 0.05^2 , 0.1 / 0.05^2 )) %>%
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

# 1.2.3 Stan model ####
Frontier_constant_model <- here("Stan", "Frontier_constant.stan") %>% 
  read_file() %>%
  write_stan_file() %>%
  cmdstan_model()

Frontier_relative_model <- here("Stan", "Frontier_relative.stan") %>% 
  read_file() %>%
  write_stan_file() %>%
  cmdstan_model()

options(cmdstanr_max_rows = 100)
Frontier_constant_samples <- Frontier_constant_model$sample(
          data = data %>%
            filter(reference == "Frontier et al. 2022") %>%
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
            filter(reference == "Frontier et al. 2022") %>%
            droplevels() %>%
            select(t, p, species, treatment) %>%
            compose_data(),
          chains = 8,
          parallel_chains = parallel::detectCores(),
          iter_warmup = 1e4,
          iter_sampling = 1e4
        ) %T>%
  print()

# 1.2.4 Model checks ####
# Rhat
Frontier_constant_samples$summary() %>%
  drop_na(rhat) %>%
  mutate(rhat_check = rhat > 1.001) %>%
  summarise(rhat_1.001 = sum(rhat_check) / length(rhat),
            rhat_mean = mean(rhat),
            rhat_sd = sd(rhat))
# 80% of rhat above 1.001. rhat = 1.13 ± 0.103. Not good.

Frontier_relative_samples$summary() %>%
  drop_na(rhat) %>%
  mutate(rhat_check = rhat > 1.001) %>%
  summarise(rhat_1.001 = sum(rhat_check) / length(rhat),
            rhat_mean = mean(rhat),
            rhat_sd = sd(rhat))
# No rhat above 1.001. rhat = 1.00 ± 0.0000557. Great.

# Chains
Frontier_constant_samples$draws(format = "df") %>%
  mcmc_rank_overlay(pars = c("alpha[1]", "alpha[2]",
                             "mu[1,1]", "mu[1,2]", 
                             "mu[2,1]", "mu[2,2]",
                             "tau[1]", "tau[2]", 
                             "theta"))
# Chains are ok.

Frontier_relative_samples$draws(format = "df") %>%
  mcmc_rank_overlay(pars = c("alpha[1]", "alpha[2]",
                             "mu[1,1]", "mu[1,2]", 
                             "mu[2,1]", "mu[2,2]",
                             "tau[1]", "tau[2]", 
                             "theta"))
# Chains are good.

# Pairs
Frontier_constant_samples$draws(format = "df") %>%
  mcmc_pairs(pars = c("alpha[1]", "mu[1,1]", "tau[1]", "theta"))
Frontier_constant_samples$draws(format = "df") %>%
  mcmc_pairs(pars = c("alpha[1]", "mu[1,2]", "tau[1]", "theta"))
Frontier_constant_samples$draws(format = "df") %>%
  mcmc_pairs(pars = c("alpha[2]", "mu[2,1]", "tau[2]", "theta"))
Frontier_constant_samples$draws(format = "df") %>%
  mcmc_pairs(pars = c("alpha[2]", "mu[2,2]", "tau[2]", "theta"))
# Pairs don't look great. Some bimodality, and non-identifiability.

Frontier_relative_samples$draws(format = "df") %>%
  mcmc_pairs(pars = c("alpha[1]", "mu[1,1]", "tau[1]", "theta"))
Frontier_relative_samples$draws(format = "df") %>%
  mcmc_pairs(pars = c("alpha[1]", "mu[1,2]", "tau[1]", "theta"))
Frontier_relative_samples$draws(format = "df") %>%
  mcmc_pairs(pars = c("alpha[2]", "mu[2,1]", "tau[2]", "theta"))
Frontier_relative_samples$draws(format = "df") %>%
  mcmc_pairs(pars = c("alpha[2]", "mu[2,2]", "tau[2]", "theta"))
# Some positive correlation between mu and tau, and negative
# correlation between alpha and mu, but not as concerning. 
# No bimodality. Generally looks more stable.

# 1.2.5 Prior-posterior comparison ####
Frontier_constant_prior <- prior_samples(
  model = Frontier_constant_model,
  data = data %>%
    filter(reference == "Frontier et al. 2022") %>%
    droplevels() %>%
    select(t, p, species, treatment) %>%
    compose_data()
  )

Frontier_relative_prior <- prior_samples(
  model = Frontier_relative_model,
  data = data %>%
    filter(reference == "Frontier et al. 2022") %>%
    droplevels() %>%
    select(t, p, species, treatment) %>%
    compose_data()
)

Frontier_constant_prior %>% 
  prior_posterior_draws(
    posterior_samples = Frontier_constant_samples,
    group = data %>%
      filter(reference == "Frontier et al. 2022") %>%
      select(species, treatment),
    parameters = c("alpha[species]", "mu[species, treatment]", 
                   "tau[species]", "theta"),
    format = "long"
  ) %>% {
    prior_posterior_plot(., group_name = "species", ridges = FALSE) %>%
      print()
    } %>%
  prior_posterior_plot(group_name = "treatment", ridges = FALSE)


# Some near-bimodality. mu has a strange sharp posterior.
# Generally looks unstable.

Frontier_relative_prior %>% 
  prior_posterior_draws(
    posterior_samples = Frontier_relative_samples,
    group = data %>%
      filter(reference == "Frontier et al. 2022") %>%
      select(species),
    parameters = c("alpha[species]", "mu[species, treatment]", 
                   "tau[species]", "theta"),
    format = "long"
  ) %>%
  prior_posterior_plot(group_name = "species", ridges = FALSE)
# Looks much more stable.

# 1.2.6 Prediction ####
Brouwer_constant_prior_posterior <- Brouwer_constant_prior %>% 
  prior_posterior_draws(
    posterior_samples = Brouwer_constant_samples,
    parameters = c("alpha[treatment]", "mu[treatment]", 
                   "tau", "theta"),
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
    parameters = c("alpha[treatment]", "mu[treatment]", 
                   "tau", "theta"),
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
                      filter(reference == "Brouwer 1996"), 
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
    p = rgamma( n() , p_mu / theta , 1 / theta )
  ) %T>%
  print()

Brouwer_relative_prediction <- Brouwer_relative_prior_posterior %>%
  spread_continuous(data = data %>% 
                      filter(reference == "Brouwer 1996"), 
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
    p = rgamma( n() , p_mu / theta , 1 / theta )
  ) %T>%
  print()

# Summarise predictions
Brouwer_constant_prediction_summary <- Brouwer_constant_prediction %>%
  # filter(is.finite(p_mu) & is.finite(p)) %>%
  group_by(t, treatment) %>%
  mean_qi(p_mu, p, .width = c(.5, .8, .9)) %T>%
  print()

Brouwer_relative_prediction_summary <- Brouwer_relative_prediction %>%
  # filter(is.finite(p_mu) & is.finite(p)) %>%
  group_by(t, treatment) %>%
  mean_qi(p_mu, p, .width = c(.5, .8, .9)) %T>%
  print()

# Mean prediction
data %>%
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


# Response prediction
data %>%
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

# While the constant model seems to fit the data better,
# it generally looks less stable, the transition being very 
# jagged, almost like a piecewise model. Let's look at LOO.

loo_compare(
  list(
    constant = Frontier_constant_samples$loo(cores = parallel::detectCores()),
    relative = Frontier_relative_samples$loo(cores = parallel::detectCores())
  )
) %>%
  as.data.frame() %>%
  rownames_to_column("model") %>%
  as_tibble()
# The relative model wins here too.




# 1.2.7 Visualisation ####
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

