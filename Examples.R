#### limbodeco: a model of macroalgal decomposition ####
#### Part 2: Statistical model and examples         ####
#### Luka Seamus Wright                             ####

# 1. Prepare data ####
require(tidyverse)
require(magrittr)
data <- read_csv("Examples.csv", col_types = list( "f", "c", "f", "f", "f" )) %>%
  mutate(# Replace 0 with small constant within measurement error
         # because the model is undefined for y = 0
         m_mean = if_else(m_mean == 0, 1e-5, m_mean)) %T>%
  print()

# Separate data for which there is only mean and sd
data_mean_sd <- data %>%
  filter(n > 1) %>%
  droplevels() %T>%
  print()

# Simulate observations from mean and sd
set.seed(100)
data_sim <- data_mean_sd %>%
  rowwise() %>%
  mutate(m = if( !is.na(m_sd) ) {
    list( 
      # gamma ensures positivity but is closest to normal
      # assumption of mean and sd as reported in papers
      rgamma( n , m_mean^2 / m_sd^2 , m_mean / m_sd^2 )
    )
  } else {
    list( m_mean )
  }) %>%
  unnest(m) %T>%
  print()
# I can use data_sim for cases that only provided mean and sd where
# measurement error modelling is not feasible.

# Remove data_mean_sd from data
data %<>%
  filter(n == 1) %>%
  droplevels() %>%
  rename(m = m_mean) %>%
  select(-c(n, m_sd)) %T>%
  print()

require(ggh4x)
data_mean_sd %>%
  ggplot() +
    geom_pointrange(aes(t, m_mean, ymin = m_mean - m_sd, ymax = m_mean + m_sd), 
                    shape = 16) +
    facet_nested_wrap(~ reference + species + treatment,
                      nest_line = T) +
    theme_minimal()
# Warnings because point range error bars were dropped for initial value

data %>%
  ggplot() +
    geom_point(aes(t, m), shape = 16, alpha = 0.2) +
    facet_nested_wrap(~ reference + species + treatment,
                      nest_line = T) +
    theme_minimal()

# First I will compare parameterisations of the logistic describing k,
# likelihood functions (normal, gamma, lognormal and beta prime) and 
# modelling heteroskedasticity vs assuming homoskedasticity. In all
# these comparative cases I will use the most complete datasets:
# Frontier et al. 2022 and Vandendriessche et al. 2007. Because I
# cannot test everything at once, I will compare parameterisations
# with a normal likelihood assuming homoskedasticity, then compare
# various likelihoods with the better parameterisation, again
# assuming homoskedasticity, and finally test the assumption of
# homoskedasticity with the optimal parameterisation and likelihood.

# 2. Parameterisation ####
# 2.1 Frontier et al. 2022 ####
# 2.1.1 Visualisation ####
data %>%
  filter(reference == "Frontier et al. 2022") %>%
  droplevels() %>%
  ggplot() +
    geom_point(aes(t, m), shape = 16, alpha = 0.5) +
    geom_line(aes(t, m, group = replicate), alpha = 0.5) + 
    facet_grid(treatment ~ species) +
    theme_minimal()
# For simplicity, I will use complete pooling and ignore species, 
# treatments and replicates in the analysis for now. In the final 
# model I will use a multilevel structure that partially pools 
# across species, treatments and replicates and thus accounts for 
# pseudo-replication. 

# So the simple model will see data like this:
data %>%
  filter(reference == "Frontier et al. 2022") %>%
  droplevels() %>%
  ggplot() +
    geom_point(aes(t, m), shape = 16, alpha = 0.5) +
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

# Full four-parameter model
tibble(n = 1:1e3,
       r = rgamma( 1e3 , 1^2 / 0.5^2 , 1 / 0.5^2 ),
       alpha = rnorm( 1e3 , 0 , 0.02 ),
       mu = rgamma( 1e3 , 30^2 / 20^2 , 30 / 20^2 ),
       tau = rgamma( 1e3 , 0.1^2 / 0.05^2 , 0.1 / 0.05^2 ),
       sigma = rexp( 1e3 , 1 )) %>%
  expand_grid(t = data %>%
                filter(reference == "Frontier et al. 2022") %$% 
                seq(min(t), max(t), length.out = 100)) %>%
  mutate(
    m_mu = exp(
      t * alpha - ( alpha + tau ) / r * (
        log1p_exp( r * ( t - mu ) ) - log1p_exp( -r * mu )
      )
    ),
    m = rnorm( n() , m_mu , sigma )
  ) %>%
  pivot_longer(cols = c(m_mu, m),
               names_to = "parameter") %>%
  ggplot(aes(t, value, group = n)) +
    geom_line(alpha = 0.05) +
    coord_cartesian(expand = F, clip = "off") +
    facet_wrap(~parameter, scale = "free", nrow = 1) +
    theme_minimal() +
    theme(panel.grid = element_blank())

# Constant rate three-parameter model (r = 1)
tibble(n = 1:1e3,
       alpha = rnorm( 1e3 , 0 , 0.02 ),
       mu = rgamma( 1e3 , 30^2 / 20^2 , 30 / 20^2 ),
       tau = rgamma( 1e3 , 0.1^2 / 0.05^2 , 0.1 / 0.05^2 ),
       sigma = rexp( 1e3 , 1 )) %>%
  expand_grid(t = data %>%
                filter(reference == "Frontier et al. 2022") %$% 
                seq(min(t), max(t), length.out = 100)) %>%
  mutate(
    m_mu = exp(
      t * alpha - ( alpha + tau ) * (
        log1p_exp( t - mu ) - log1p_exp( -mu )
      )
    ),
    m = rnorm( n() , m_mu , sigma )
  ) %>%
  pivot_longer(cols = c(m_mu, m),
               names_to = "parameter") %>%
  ggplot(aes(t, value, group = n)) +
    geom_line(alpha = 0.05) +
    coord_cartesian(expand = F, clip = "off") +
    facet_wrap(~parameter, scale = "free", nrow = 1) +
    theme_minimal() +
    theme(panel.grid = element_blank())

# Relative rate (constant intercept) three-parameter model (r = 5/mu)
tibble(n = 1:1e3,
       alpha = rnorm( 1e3 , 0 , 0.02 ),
       mu = rgamma( 1e3 , 30^2 / 20^2 , 30 / 20^2 ),
       tau = rgamma( 1e3 , 0.1^2 / 0.05^2 , 0.1 / 0.05^2 ),
       sigma = rexp( 1e3 , 1 )) %>%
  expand_grid(t = data %>%
                filter(reference == "Frontier et al. 2022") %$% 
                seq(min(t), max(t), length.out = 100)) %>%
  mutate(
    m_mu = exp(
      t * alpha - ( alpha + tau ) * mu / 5 * (
        log1p_exp( 5 / mu * ( t - mu ) ) - log1p_exp( -5 )
      )
    ),
    m = rnorm( n() , m_mu , sigma )
  ) %>%
  pivot_longer(cols = c(m_mu, m),
               names_to = "parameter") %>%
  ggplot(aes(t, value, group = n)) +
    geom_line(alpha = 0.05) +
    coord_cartesian(expand = F, clip = "off") +
    facet_wrap(~parameter, scale = "free", nrow = 1) +
    theme_minimal() +
    theme(panel.grid = element_blank())

# 2.1.3 Stan models ####
# Load models
require(here)
require(cmdstanr)
Frontier_full_model <- here("Stan", "Frontier_full.stan") %>% 
  read_file() %>%
  write_stan_file() %>%
  cmdstan_model()

Frontier_constant_model <- here("Stan", "Frontier_constant.stan") %>% 
  read_file() %>%
  write_stan_file() %>%
  cmdstan_model()

Frontier_relative_model <- here("Stan", "Frontier_relative.stan") %>% 
  read_file() %>%
  write_stan_file() %>%
  cmdstan_model()

# Run models
require(tidybayes)
Frontier_full_samples <- Frontier_full_model$sample(
          data = data %>%
            filter(reference == "Frontier et al. 2022" &
                     t != 0) %>% # t0 = 1 is pre-determined
            droplevels() %>%
            select(t, m) %>%
            compose_data(),
          chains = 8,
          parallel_chains = parallel::detectCores(),
          iter_warmup = 1e3, # only use 1e3 iterations for diagnostic models
          iter_sampling = 1e3
        ) %T>%
  print()

Frontier_constant_samples <- Frontier_constant_model$sample(
          data = data %>%
            filter(reference == "Frontier et al. 2022" &
                     t != 0) %>%
            droplevels() %>%
            select(t, m) %>%
            compose_data(),
          chains = 8,
          parallel_chains = parallel::detectCores(),
          iter_warmup = 1e3,
          iter_sampling = 1e3
        ) %T>%
  print()

Frontier_relative_samples <- Frontier_relative_model$sample(
          data = data %>%
            filter(reference == "Frontier et al. 2022" &
                     t != 0) %>%
            droplevels() %>%
            select(t, m) %>%
            compose_data(),
          chains = 8,
          parallel_chains = parallel::detectCores(),
          iter_warmup = 1e3,
          iter_sampling = 1e3
        ) %T>%
  print()

# Save draws
Frontier_full_samples$draws() %>%
  write_rds(here("RDS", "Frontier_full_samples.rds"))
Frontier_full_samples$draws(format = "df") %>%
  write_rds(here("RDS", "Frontier_full_samples_df.rds"))

Frontier_constant_samples$draws() %>%
  write_rds(here("RDS", "Frontier_constant_samples.rds"))
Frontier_constant_samples$draws(format = "df") %>%
  write_rds(here("RDS", "Frontier_constant_samples_df.rds"))

Frontier_relative_samples$draws() %>%
  write_rds(here("RDS", "Frontier_relative_samples.rds"))
Frontier_relative_samples$draws(format = "df") %>%
  write_rds(here("RDS", "Frontier_relative_samples_df.rds"))

# 2.1.4 Rhat and effective sample size ####
Frontier_param_rhat_ess <- bind_rows(
  Frontier_full_samples$summary(
    variables = c("lp__", "r", "alpha", "mu", "tau", "sigma")
  ) %>%
    summarise(rhat_mean = mean(rhat),
              rhat_sd = sd(rhat),
              ess_mean = mean(ess_bulk),
              ess_sd = sd(ess_bulk)) %>%
    mutate(model = "full"),
  Frontier_constant_samples$summary(
    variables = c("lp__", "alpha", "mu", "tau", "sigma")
  ) %>%
    summarise(rhat_mean = mean(rhat),
              rhat_sd = sd(rhat),
              ess_mean = mean(ess_bulk),
              ess_sd = sd(ess_bulk)) %>%
    mutate(model = "constant"),
  Frontier_relative_samples$summary(
    variables = c("lp__", "alpha", "mu", "tau", "sigma")
  ) %>%
    summarise(rhat_mean = mean(rhat),
              rhat_sd = sd(rhat),
              ess_mean = mean(ess_bulk),
              ess_sd = sd(ess_bulk)) %>%
    mutate(model = "relative")
) %T>%
  print()

# 2.1.5 Chains ####
# Define custom plotting theme
mytheme <- theme(
  panel.background = element_blank(),
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
  strip.text = element_text(size = 12, hjust = 0),
  panel.spacing.x = unit(1, "cm"),
  panel.spacing.y = unit(0.6, "cm"),
  text = element_text(family = "Futura")
)

require(bayesplot)
Frontier_full_chains <- Frontier_full_samples$draws(format = "df") %>%
  mcmc_rank_overlay(pars = c("lp__", "r", "alpha", "mu", "tau", "sigma")) +
  guides(colour = guide_legend(nrow = 1)) +
  labs(title = "Full four-parameter model",
       y = "Frequency") +
  coord_cartesian(xlim = c(0, 8e3), 
                  ylim = c(0, 100),
                  expand = FALSE) +
  mytheme

Frontier_constant_chains <- Frontier_constant_samples$draws(format = "df") %>%
  mcmc_rank_overlay(pars = c("lp__", "alpha", "mu", "tau", "sigma")) +
  guides(colour = guide_legend(nrow = 1)) +
  labs(title = "Constant logistic rate three-parameter model",
       y = "Frequency") +
  coord_cartesian(xlim = c(0, 8e3), 
                  ylim = c(0, 100),
                  expand = FALSE) +
  mytheme

Frontier_relative_chains <- Frontier_relative_samples$draws(format = "df") %>%
  mcmc_rank_overlay(pars = c("lp__", "alpha", "mu", "tau", "sigma")) +
  guides(colour = guide_legend(nrow = 1)) +
  labs(title = "Constant logistic intercept three-parameter model",
       y = "Frequency") +
  coord_cartesian(xlim = c(0, 8e3), 
                  ylim = c(0, 100),
                  expand = FALSE) +
  mytheme

require(patchwork)
Frontier_param_chains <- Frontier_full_chains | 
  Frontier_constant_chains | 
  Frontier_relative_chains

Frontier_param_chains %>%
  ggsave(filename = "Frontier_param_chains.pdf", path = "Plots",
         device = cairo_pdf, width = 60, height = 15, units = "cm")

# 2.1.6 Pairs ####
# Pairs plots are not regular ggplot objects so cannot be combined
# as above. Instead each will have to be saved as a separate image.
Frontier_full_samples$draws(format = "df") %>%
  mcmc_pairs(
    pars = c("r", "alpha", "mu", "tau", "sigma"),
    grid_args = list(top = "Full four-parameter model")
  ) %>%
  # PDF is inefficient for pairs, so I use PNG
  ggsave(filename = "Frontier_full_pairs.png", path = "Plots",
         width = 20, height = 20, units = "cm", bg = "white")

Frontier_constant_samples$draws(format = "df") %>%
  mcmc_pairs(
    pars = c("alpha", "mu", "tau", "sigma"),
    grid_args = list(top = "Constant logistic rate three-parameter model")
  ) %>%
  ggsave(filename = "Frontier_constant_pairs.png", path = "Plots",
         width = 20, height = 20, units = "cm", bg = "white")

Frontier_relative_samples$draws(format = "df") %>%
  mcmc_pairs(
    pars = c("alpha", "mu", "tau", "sigma"),
    grid_args = list(top = "Constant logistic intercept three-parameter model")
  ) %>%
  ggsave(filename = "Frontier_relative_pairs.png", path = "Plots",
         width = 20, height = 20, units = "cm", bg = "white")

# 2.1.7 Leave-one-out cross-validation ####
require(loo)
Frontier_param_loo <- loo_compare(
  list(
    full = Frontier_full_samples$loo(cores = parallel::detectCores()),
    constant = Frontier_constant_samples$loo(cores = parallel::detectCores()),
    relative = Frontier_relative_samples$loo(cores = parallel::detectCores())
  )
) %>%
  as.data.frame() %>%
  rownames_to_column("model") %>%
  as_tibble() %T>%
  print()

# 2.1.8 Prior-posterior comparison ####
# Sample priors
source("functions.R")
Frontier_full_prior <- prior_samples(
  model = Frontier_full_model,
  data = data %>%
    filter(reference == "Frontier et al. 2022" &
             t != 0) %>%
    droplevels() %>%
    select(t, m) %>%
    compose_data(),
  samples = 1e3
)

Frontier_constant_prior <- prior_samples(
  model = Frontier_constant_model,
  data = data %>%
    filter(reference == "Frontier et al. 2022" &
             t != 0) %>%
    droplevels() %>%
    select(t, m) %>%
    compose_data(),
  samples = 1e3
  )

Frontier_relative_prior <- prior_samples(
  model = Frontier_relative_model,
  data = data %>%
    filter(reference == "Frontier et al. 2022" &
             t != 0) %>%
    droplevels() %>%
    select(t, m) %>%
    compose_data(),
  samples = 1e3
)

# Plot comparison
Frontier_full_prior_posterior <- Frontier_full_prior %>% 
  prior_posterior_draws(
    posterior_samples = Frontier_full_samples,
    parameters = c("r", "alpha", "mu", "tau", "sigma"),
    format = "long"
  ) %>%
  prior_posterior_plot() +
  scale_x_continuous(
    labels = scales::label_number(style_negative = "minus")
  ) +
  labs(title = "Full four-parameter model") +
  coord_cartesian(expand = FALSE) +
  mytheme +
  theme(axis.line.y = element_blank(),
        axis.ticks.y = element_blank(),
        axis.text.y = element_blank(),
        axis.title = element_blank())

Frontier_constant_prior_posterior <- Frontier_constant_prior %>% 
  prior_posterior_draws(
    posterior_samples = Frontier_constant_samples,
    parameters = c("alpha", "mu", "tau", "sigma"),
    format = "long"
  ) %>%
  prior_posterior_plot() +
  scale_x_continuous(
    labels = scales::label_number(style_negative = "minus")
  ) +
  labs(title = "Constant logistic rate three-parameter model") +
  coord_cartesian(expand = FALSE) +
  mytheme +
  theme(axis.line.y = element_blank(),
        axis.ticks.y = element_blank(),
        axis.text.y = element_blank(),
        axis.title = element_blank())

Frontier_relative_prior_posterior <- Frontier_relative_prior %>% 
  prior_posterior_draws(
    posterior_samples = Frontier_relative_samples,
    parameters = c("alpha", "mu", "tau", "sigma"),
    format = "long"
  ) %>%
  prior_posterior_plot() +
  scale_x_continuous(
    labels = scales::label_number(style_negative = "minus")
  ) +
  labs(title = "Constant logistic intercept three-parameter model") +
  coord_cartesian(expand = FALSE) +
  mytheme +
  theme(axis.line.y = element_blank(),
        axis.ticks.y = element_blank(),
        axis.text.y = element_blank(),
        axis.title = element_blank())

Frontier_param_prior_posterior <- (
  Frontier_full_prior_posterior | 
  Frontier_constant_prior_posterior | 
  Frontier_relative_prior_posterior
) + plot_layout(widths = c(1, 2/3, 2/3))

Frontier_param_prior_posterior %>%
  ggsave(filename = "Frontier_param_prior_posterior.pdf", path = "Plots",
         device = cairo_pdf, width = 60, height = 15, units = "cm")

# Clean up
rm(
  list = ls(
    pattern = "(?:_model|_prior|_posterior|_chains)$"
  )
)
# I don't want to remove the function prior_samples
# which is captured by _samples, so:
rm(
  list = ls(
    pattern = "(?:full_samples|constant_samples|relative_samples)$"
  )
)
gc()

# 2.2 Vandendriessche et al. 2007 ####
# 2.2.1 Visualisation ####
data %>%
  filter(reference == "Vandendriessche et al. 2007") %>%
  droplevels() %>%
  ggplot() +
    geom_point(aes(t, m), shape = 16, alpha = 0.5) +
    geom_line(aes(t, m, group = replicate), alpha = 0.5) + 
    facet_grid(treatment ~ species) +
    theme_minimal()
# I'll simplify as before, but will stratify by replicate
# because there is a lot of variation to describe for one 
# mean which can cause the model to fail.

data %>%
  filter(reference == "Vandendriessche et al. 2007") %>%
  droplevels() %>%
  ggplot() +
    geom_point(aes(t, m), shape = 16, alpha = 0.5) +
    geom_line(aes(t, m, group = replicate), alpha = 0.5) +
    theme_minimal()

# 2.2.2 Prior simulation ####
# Full four-parameter model
tibble(n = 1:1e3,
       r = rgamma( 1e3 , 1^2 / 0.5^2 , 1 / 0.5^2 ),
       alpha = rnorm( 1e3 , 0 , 0.02 ),
       mu = rgamma( 1e3 , 60^2 / 40^2 , 60 / 40^2 ), # increase prior for mu
       tau = rgamma( 1e3 , 0.1^2 / 0.05^2 , 0.1 / 0.05^2 ),
       sigma = rexp( 1e3 , 1 )) %>%
  expand_grid(t = data %>%
                filter(reference == "Vandendriessche et al. 2007") %$% 
                seq(min(t), max(t), length.out = 100)) %>%
  mutate(
    m_mu = exp(
      t * alpha - ( alpha + tau ) / r * (
        log1p_exp( r * ( t - mu ) ) - log1p_exp( -r * mu )
      )
    ),
    m = rnorm( n() , m_mu , sigma )
  ) %>%
  pivot_longer(cols = c(m_mu, m),
               names_to = "parameter") %>%
  ggplot(aes(t, value, group = n)) +
    geom_line(alpha = 0.05) +
    coord_cartesian(ylim = c(0, 10), expand = F, clip = "off") +
    facet_wrap(~parameter, scale = "free", nrow = 1) +
    theme_minimal() +
    theme(panel.grid = element_blank())

# Constant rate three-parameter model (r = 1)
tibble(n = 1:1e3,
       alpha = rnorm( 1e3 , 0 , 0.02 ),
       mu = rgamma( 1e3 , 60^2 / 40^2 , 60 / 40^2 ),
       tau = rgamma( 1e3 , 0.1^2 / 0.05^2 , 0.1 / 0.05^2 ),
       sigma = rexp( 1e3 , 1 )) %>%
  expand_grid(t = data %>%
                filter(reference == "Vandendriessche et al. 2007") %$% 
                seq(min(t), max(t), length.out = 100)) %>%
  mutate(
    m_mu = exp(
      t * alpha - ( alpha + tau ) * (
        log1p_exp( t - mu ) - log1p_exp( -mu )
      )
    ),
    m = rnorm( n() , m_mu , sigma )
  ) %>%
  pivot_longer(cols = c(m_mu, m),
               names_to = "parameter") %>%
  ggplot(aes(t, value, group = n)) +
    geom_line(alpha = 0.05) +
    coord_cartesian(ylim = c(0, 10), expand = F, clip = "off") +
    facet_wrap(~parameter, scale = "free", nrow = 1) +
    theme_minimal() +
    theme(panel.grid = element_blank())

# Relative rate (constant intercept) three-parameter model (r = 5/mu)
tibble(n = 1:1e3,
       alpha = rnorm( 1e3 , 0 , 0.02 ),
       mu = rgamma( 1e3 , 60^2 / 40^2 , 60 / 40^2 ),
       tau = rgamma( 1e3 , 0.1^2 / 0.05^2 , 0.1 / 0.05^2 ),
       sigma = rexp( 1e3 , 1 )) %>%
  expand_grid(t = data %>%
                filter(reference == "Vandendriessche et al. 2007") %$% 
                seq(min(t), max(t), length.out = 100)) %>%
  mutate(
    m_mu = exp(
      t * alpha - ( alpha + tau ) * mu / 5 * (
        log1p_exp( 5 / mu * ( t - mu ) ) - log1p_exp( -5 )
      )
    ),
    m = rnorm( n() , m_mu , sigma )
  ) %>%
  pivot_longer(cols = c(m_mu, m),
               names_to = "parameter") %>%
  ggplot(aes(t, value, group = n)) +
    geom_line(alpha = 0.05) +
    coord_cartesian(ylim = c(0, 10), expand = F, clip = "off") +
    facet_wrap(~parameter, scale = "free", nrow = 1) +
    theme_minimal() +
    theme(panel.grid = element_blank())

# 2.2.3 Stan models ####
# Load models
Vandendriessche_full_model <- here("Stan", "Vandendriessche_full.stan") %>% 
  read_file() %>%
  write_stan_file() %>%
  cmdstan_model()

Vandendriessche_constant_model <- here("Stan", "Vandendriessche_constant.stan") %>% 
  read_file() %>%
  write_stan_file() %>%
  cmdstan_model()

Vandendriessche_relative_model <- here("Stan", "Vandendriessche_relative.stan") %>% 
  read_file() %>%
  write_stan_file() %>%
  cmdstan_model()

# Run models
Vandendriessche_full_samples <- Vandendriessche_full_model$sample(
          data = data %>%
            filter(reference == "Vandendriessche et al. 2007" &
                     t != 0) %>%
            droplevels() %>%
            select(t, m, replicate) %>%
            compose_data(),
          chains = 8,
          parallel_chains = parallel::detectCores(),
          iter_warmup = 1e3,
          iter_sampling = 1e3
        ) %T>%
  print()

Vandendriessche_constant_samples <- Vandendriessche_constant_model$sample(
          data = data %>%
            filter(reference == "Vandendriessche et al. 2007" &
                     t != 0) %>%
            droplevels() %>%
            select(t, m, replicate) %>%
            compose_data(),
          chains = 8,
          parallel_chains = parallel::detectCores(),
          iter_warmup = 1e3,
          iter_sampling = 1e3
        ) %T>%
  print()

Vandendriessche_relative_samples <- Vandendriessche_relative_model$sample(
          data = data %>%
            filter(reference == "Vandendriessche et al. 2007" &
                     t != 0) %>%
            droplevels() %>%
            select(t, m, replicate) %>%
            compose_data(),
          chains = 8,
          parallel_chains = parallel::detectCores(),
          iter_warmup = 1e3,
          iter_sampling = 1e3
        ) %T>%
  print()

# Save draws
Vandendriessche_full_samples$draws() %>%
  write_rds(here("RDS", "Vandendriessche_full_samples.rds"))
Vandendriessche_full_samples$draws(format = "df") %>%
  write_rds(here("RDS", "Vandendriessche_full_samples_df.rds"))

Vandendriessche_constant_samples$draws() %>%
  write_rds(here("RDS", "Vandendriessche_constant_samples.rds"))
Vandendriessche_constant_samples$draws(format = "df") %>%
  write_rds(here("RDS", "Vandendriessche_constant_samples_df.rds"))

Vandendriessche_relative_samples$draws() %>%
  write_rds(here("RDS", "Vandendriessche_relative_samples.rds"))
Vandendriessche_relative_samples$draws(format = "df") %>%
  write_rds(here("RDS", "Vandendriessche_relative_samples_df.rds"))

# 2.2.4 Rhat and effective sample size ####
Vandendriessche_param_rhat_ess <- bind_rows(
  Vandendriessche_full_samples$summary(
    variables = c("lp__", "r", "alpha", "mu", "tau", "sigma")
  ) %>%
    summarise(rhat_mean = mean(rhat),
              rhat_sd = sd(rhat),
              ess_mean = mean(ess_bulk),
              ess_sd = sd(ess_bulk)) %>%
    mutate(model = "full"),
  Vandendriessche_constant_samples$summary(
    variables = c("lp__", "alpha", "mu", "tau", "sigma")
  ) %>%
    summarise(rhat_mean = mean(rhat),
              rhat_sd = sd(rhat),
              ess_mean = mean(ess_bulk),
              ess_sd = sd(ess_bulk)) %>%
    mutate(model = "constant"),
  Vandendriessche_relative_samples$summary(
    variables = c("lp__", "alpha", "mu", "tau", "sigma")
  ) %>%
    summarise(rhat_mean = mean(rhat),
              rhat_sd = sd(rhat),
              ess_mean = mean(ess_bulk),
              ess_sd = sd(ess_bulk)) %>%
    mutate(model = "relative")
) %T>%
  print()

# 2.2.5 Chains ####
# Pick three replicates
Vandendriessche_full_chains <- Vandendriessche_full_samples$draws(format = "df") %>%
  mcmc_rank_overlay(pars = c("lp__", "sigma", "r[1]", "alpha[1]", "mu[1]", "tau[1]", 
                             "r[20]", "alpha[20]", "mu[20]", "tau[20]",
                             "r[48]", "alpha[48]", "mu[48]", "tau[48]")) +
  guides(colour = guide_legend(nrow = 1)) +
  labs(title = "Full four-parameter model",
       y = "Frequency") +
  coord_cartesian(xlim = c(0, 8e3), 
                  ylim = c(0, 100),
                  expand = FALSE) +
  mytheme

Vandendriessche_constant_chains <- Vandendriessche_constant_samples$draws(format = "df") %>%
  mcmc_rank_overlay(pars = c("lp__", "sigma", "alpha[1]", "mu[1]", "tau[1]", 
                             "alpha[20]", "mu[20]", "tau[20]",
                             "alpha[48]", "mu[48]", "tau[48]")) +
  guides(colour = guide_legend(nrow = 1)) +
  labs(title = "Constant logistic rate three-parameter model",
       y = "Frequency") +
  coord_cartesian(xlim = c(0, 8e3), 
                  ylim = c(0, 100),
                  expand = FALSE) +
  mytheme

Vandendriessche_relative_chains <- Vandendriessche_relative_samples$draws(format = "df") %>%
  mcmc_rank_overlay(pars = c("lp__", "sigma", "alpha[1]", "mu[1]", "tau[1]", 
                             "alpha[20]", "mu[20]", "tau[20]",
                             "alpha[48]", "mu[48]", "tau[48]")) +
  guides(colour = guide_legend(nrow = 1)) +
  labs(title = "Constant logistic intercept three-parameter model",
       y = "Frequency") +
  coord_cartesian(xlim = c(0, 8e3), 
                  ylim = c(0, 100),
                  expand = FALSE) +
  mytheme

Vandendriessche_param_chains <- Vandendriessche_full_chains | 
  Vandendriessche_constant_chains | 
  Vandendriessche_relative_chains

Vandendriessche_param_chains %>%
  ggsave(filename = "Vandendriessche_param_chains.pdf", path = "Plots",
         device = cairo_pdf, width = 60, height = 15, units = "cm")

# 2.2.6 Pairs ####
Vandendriessche_full_samples$draws(format = "df") %>%
  mcmc_pairs(
    pars = c("r[1]", "alpha[1]", "mu[1]", "tau[1]", 
             "r[20]", "alpha[20]", "mu[20]", "tau[20]",
             "r[48]", "alpha[48]", "mu[48]", "tau[48]",
             "sigma"),
    grid_args = list(top = "Full four-parameter model")
  ) %>%
  ggsave(filename = "Vandendriessche_full_pairs.png", path = "Plots",
         width = 40, height = 40, units = "cm", bg = "white")

Vandendriessche_constant_samples$draws(format = "df") %>%
  mcmc_pairs(
    pars = c("alpha[1]", "mu[1]", "tau[1]", 
             "alpha[20]", "mu[20]", "tau[20]",
             "alpha[48]", "mu[48]", "tau[48]",
             "sigma"),
    grid_args = list(top = "Constant logistic rate three-parameter model")
  ) %>%
  ggsave(filename = "Vandendriessche_constant_pairs.png", path = "Plots",
         width = 40, height = 40, units = "cm", bg = "white")

Vandendriessche_relative_samples$draws(format = "df") %>%
  mcmc_pairs(
    pars = c("alpha[1]", "mu[1]", "tau[1]", 
             "alpha[20]", "mu[20]", "tau[20]",
             "alpha[48]", "mu[48]", "tau[48]",
             "sigma"),
    grid_args = list(top = "Constant logistic intercept three-parameter model")
  ) %>%
  ggsave(filename = "Vandendriessche_relative_pairs.png", path = "Plots",
         width = 40, height = 40, units = "cm", bg = "white")

# 2.2.7 Leave-one-out cross-validation ####
Vandendriessche_param_loo <- loo_compare(
  list(
    full = Vandendriessche_full_samples$loo(cores = parallel::detectCores()),
    constant = Vandendriessche_constant_samples$loo(cores = parallel::detectCores()),
    relative = Vandendriessche_relative_samples$loo(cores = parallel::detectCores())
  )
) %>%
  as.data.frame() %>%
  rownames_to_column("model") %>%
  as_tibble() %T>%
  print()
# Warning: cannot trust stats because Pareto k diagnostic values are too high.
Vandendriessche_relative_samples$loo(cores = parallel::detectCores())
# 99.7% are good, so it's fine.

# 2.2.8 Prior-posterior comparison ####
# Sample priors
Vandendriessche_full_prior <- prior_samples(
  model = Vandendriessche_full_model,
  data = data %>%
    filter(reference == "Vandendriessche et al. 2007" &
             t != 0) %>%
    droplevels() %>%
    select(t, m, replicate) %>%
    compose_data(),
  samples = 1e3
)

Vandendriessche_constant_prior <- prior_samples(
  model = Vandendriessche_constant_model,
  data = data %>%
    filter(reference == "Vandendriessche et al. 2007" &
             t != 0) %>%
    droplevels() %>%
    select(t, m, replicate) %>%
    compose_data(),
  samples = 1e3
  )

Vandendriessche_relative_prior <- prior_samples(
  model = Vandendriessche_relative_model,
  data = data %>%
    filter(reference == "Vandendriessche et al. 2007" &
             t != 0) %>%
    droplevels() %>%
    select(t, m, replicate) %>%
    compose_data(),
  samples = 1e3
)

# Plot comparison
Vandendriessche_full_prior_posterior <- Vandendriessche_full_prior %>% 
  prior_posterior_draws(
    posterior_samples = Vandendriessche_full_samples,
    group = data %>%
      filter(reference == "Vandendriessche et al. 2007") %>%
      droplevels() %>%
      select(replicate),
    parameters = c("r[replicate]", "alpha[replicate]", "mu[replicate]", 
                   "tau[replicate]", "sigma"),
    format = "long"
  ) %>%
  prior_posterior_plot(group_name = "replicate", ridges = TRUE) +
  scale_x_continuous(
    labels = scales::label_number(style_negative = "minus")
  ) +
  labs(title = "Full four-parameter model") +
  coord_cartesian(expand = FALSE) +
  mytheme +
  theme(axis.line.y = element_blank(),
        axis.ticks.y = element_blank(),
        axis.text.y = element_blank(),
        axis.title = element_blank())

Vandendriessche_constant_prior_posterior <- Vandendriessche_constant_prior %>% 
  prior_posterior_draws(
    posterior_samples = Vandendriessche_constant_samples,
    group = data %>%
      filter(reference == "Vandendriessche et al. 2007") %>%
      droplevels() %>%
      select(replicate),
    parameters = c("alpha[replicate]", "mu[replicate]", 
                   "tau[replicate]", "sigma"),
    format = "long"
  ) %>%
  prior_posterior_plot(group_name = "replicate", ridges = TRUE) +
  scale_x_continuous(
    labels = scales::label_number(style_negative = "minus")
  ) +
  labs(title = "Constant logistic rate three-parameter model") +
  coord_cartesian(expand = FALSE) +
  mytheme +
  theme(axis.line.y = element_blank(),
        axis.ticks.y = element_blank(),
        axis.text.y = element_blank(),
        axis.title = element_blank())

Vandendriessche_relative_prior_posterior <- Vandendriessche_relative_prior %>% 
  prior_posterior_draws(
    posterior_samples = Vandendriessche_relative_samples,
    group = data %>%
      filter(reference == "Vandendriessche et al. 2007") %>%
      droplevels() %>%
      select(replicate),
    parameters = c("alpha[replicate]", "mu[replicate]", 
                   "tau[replicate]", "sigma"),
    format = "long"
  ) %>%
  prior_posterior_plot(group_name = "replicate", ridges = TRUE) +
  scale_x_continuous(
    labels = scales::label_number(style_negative = "minus")
  ) +
  labs(title = "Constant logistic intercept three-parameter model") +
  coord_cartesian(expand = FALSE) +
  mytheme +
  theme(axis.line.y = element_blank(),
        axis.ticks.y = element_blank(),
        axis.text.y = element_blank(),
        axis.title = element_blank())

Vandendriessche_param_prior_posterior <- (
  Vandendriessche_full_prior_posterior | 
  Vandendriessche_constant_prior_posterior | 
  Vandendriessche_relative_prior_posterior
) + plot_layout(widths = c(1, 2/3, 2/3))

Vandendriessche_param_prior_posterior %>%
  ggsave(filename = "Vandendriessche_param_prior_posterior.pdf", path = "Plots",
         device = cairo_pdf, width = 60, height = 15, units = "cm")

# Clean up
rm(
  list = ls(
    pattern = "(?:_model|_prior|_posterior|_chains)$"
  )
)
rm(
  list = ls(
    pattern = "(?:full_samples|constant_samples|relative_samples)$"
  )
)
gc()

# 2.3 Save diagnostic tables ####
param_rhat_ess <- Frontier_param_rhat_ess %>%
  mutate(reference = "Frontier et al. 2022") %>%
  bind_rows(
    Vandendriessche_param_rhat_ess %>%
      mutate(reference = "Vandendriessche et al. 2007")
  ) %>%
  select(reference, model, 
         starts_with("rhat"),
         starts_with("ess")) %T>%
  print()

param_rhat_ess %>%
  write_csv(here("Tables", "Diagnostic", "param_rhat_ess.csv"))

param_loo <- Frontier_param_loo %>%
  mutate(reference = "Frontier et al. 2022") %>%
  bind_rows(
    Vandendriessche_param_loo %>%
      mutate(reference = "Vandendriessche et al. 2007")
  ) %>%
  select(reference, model, everything()) %T>%
  print()

param_loo %>%
  write_csv(here("Tables", "Diagnostic", "param_loo.csv"))

# Models are fairly similar when there are lots of data,
# but the relative parameterisation is best in most cases.
# Proceed with this parameterisation and vary the likelihood.

# 3. Likelihood ####
# 3.1 Frontier et al. 2022 ####
# 3.1.1 Prior simulation ####
# See above for normal likelihood

# Lognormal likelihood
tibble(n = 1:1e3,
       alpha = rnorm( 1e3 , 0 , 0.02 ),
       mu = rgamma( 1e3 , 30^2 / 20^2 , 30 / 20^2 ),
       tau = rgamma( 1e3 , 0.1^2 / 0.05^2 , 0.1 / 0.05^2 ),
       sigma = rexp( 1e3 , 1 )) %>%
  expand_grid(t = data %>%
                filter(reference == "Frontier et al. 2022") %$% 
                seq(min(t), max(t), length.out = 100)) %>%
  mutate(
    m_mu = exp(
      t * alpha - ( alpha + tau ) * mu / 5 * (
        log1p_exp( 5 / mu * ( t - mu ) ) - log1p_exp( -5 )
      )
    ), 
    m = rlnorm( n() , log(m_mu) , sigma )
    # In Stan I would not exponentiate and then take the log
    # but pass log_m_mu directly into the likelihood.
  ) %>%
  pivot_longer(cols = c(m_mu, m),
               names_to = "parameter") %>%
  ggplot(aes(t, value, group = n)) +
    geom_line(alpha = 0.05) +
    coord_cartesian(expand = F, clip = "off") +
    facet_wrap(~parameter, scale = "free", nrow = 1) +
    theme_minimal() +
    theme(panel.grid = element_blank())

# Gamma likelihood
tibble(n = 1:1e3,
       alpha = rnorm( 1e3 , 0 , 0.02 ),
       mu = rgamma( 1e3 , 30^2 / 20^2 , 30 / 20^2 ),
       tau = rgamma( 1e3 , 0.1^2 / 0.05^2 , 0.1 / 0.05^2 ),
       theta = rexp( 1e3 , 1 )) %>%
  expand_grid(t = data %>%
                filter(reference == "Frontier et al. 2022") %$% 
                seq(min(t), max(t), length.out = 100)) %>%
  mutate(
    m_mu = exp(
      t * alpha - ( alpha + tau ) * mu / 5 * (
        log1p_exp( 5 / mu * ( t - mu ) ) - log1p_exp( -5 )
      )
    ), 
    m = rgamma( n() , m_mu / theta , 1 / theta )
  ) %>%
  pivot_longer(cols = c(m_mu, m),
               names_to = "parameter") %>%
  ggplot(aes(t, value, group = n)) +
    geom_line(alpha = 0.05) +
    coord_cartesian(expand = F, clip = "off") +
    facet_wrap(~parameter, scale = "free", nrow = 1) +
    theme_minimal() +
    theme(panel.grid = element_blank())

# Beta prime likelihood
require(extraDistr) # No native beta prime likelihood
tibble(n = 1:1e3,
       alpha = rnorm( 1e3 , 0 , 0.02 ),
       mu = rgamma( 1e3 , 30^2 / 20^2 , 30 / 20^2 ),
       tau = rgamma( 1e3 , 0.1^2 / 0.05^2 , 0.1 / 0.05^2 ),
       nu = rgamma( 1e3 , 100^2 / 50^2 , 100 / 50^2 )) %>%
  expand_grid(t = data %>%
                filter(reference == "Frontier et al. 2022") %$% 
                seq(min(t), max(t), length.out = 100)) %>%
  mutate(
    m_mu = exp(
      t * alpha - ( alpha + tau ) * mu / 5 * (
        log1p_exp( 5 / mu * ( t - mu ) ) - log1p_exp( -5 )
      )
    ), 
    m = rbetapr( n() , m_mu * ( 1 + nu ) , 2 + nu )
  ) %>%
  pivot_longer(cols = c(m_mu, m),
               names_to = "parameter") %>%
  ggplot(aes(t, value, group = n)) +
    geom_line(alpha = 0.05) +
    coord_cartesian(expand = F, clip = "off") +
    facet_wrap(~parameter, scale = "free", nrow = 1) +
    theme_minimal() +
    theme(panel.grid = element_blank())

# 3.1.2 Stan models ####
# Load models
# The normal model is the same as the relative model used above.
Frontier_norm_model <- here("Stan", "Frontier_relative.stan") %>% 
  read_file() %>%
  write_stan_file() %>%
  cmdstan_model()

Frontier_lnorm_model <- here("Stan", "Frontier_lnorm.stan") %>% 
  read_file() %>%
  write_stan_file() %>%
  cmdstan_model()

Frontier_gamma_model <- here("Stan", "Frontier_gamma.stan") %>% 
  read_file() %>%
  write_stan_file() %>%
  cmdstan_model()

Frontier_betap_model <- here("Stan", "Frontier_betap.stan") %>% 
  read_file() %>%
  write_stan_file() %>%
  cmdstan_model()

# Run models
Frontier_norm_samples <- Frontier_norm_model$sample(
          data = data %>%
            filter(reference == "Frontier et al. 2022" &
                     t != 0) %>%
            droplevels() %>%
            select(t, m) %>%
            compose_data(),
          chains = 8,
          parallel_chains = parallel::detectCores(),
          iter_warmup = 1e3,
          iter_sampling = 1e3
        ) %T>%
  print()

Frontier_lnorm_samples <- Frontier_lnorm_model$sample(
          data = data %>%
            filter(reference == "Frontier et al. 2022" &
                     t != 0) %>%
            droplevels() %>%
            select(t, m) %>%
            compose_data(),
          chains = 8,
          parallel_chains = parallel::detectCores(),
          iter_warmup = 1e3,
          iter_sampling = 1e3
        ) %T>%
  print()

Frontier_gamma_samples <- Frontier_gamma_model$sample(
          data = data %>%
            filter(reference == "Frontier et al. 2022" &
                     t != 0) %>%
            droplevels() %>%
            select(t, m) %>%
            compose_data(),
          chains = 8,
          parallel_chains = parallel::detectCores(),
          iter_warmup = 1e3,
          iter_sampling = 1e3
        ) %T>%
  print()

Frontier_betap_samples <- Frontier_betap_model$sample(
          data = data %>%
            filter(reference == "Frontier et al. 2022" &
                     t != 0) %>%
            droplevels() %>%
            select(t, m) %>%
            compose_data(),
          chains = 8,
          parallel_chains = parallel::detectCores(),
          iter_warmup = 1e3,
          iter_sampling = 1e3
        ) %T>%
  print()

# Save draws (normal ones are already saved)
Frontier_lnorm_samples$draws() %>%
  write_rds(here("RDS", "Frontier_lnorm_samples.rds"))
Frontier_lnorm_samples$draws(format = "df") %>%
  write_rds(here("RDS", "Frontier_lnorm_samples_df.rds"))

Frontier_gamma_samples$draws() %>%
  write_rds(here("RDS", "Frontier_gamma_samples.rds"))
Frontier_gamma_samples$draws(format = "df") %>%
  write_rds(here("RDS", "Frontier_gamma_samples_df.rds"))

Frontier_betap_samples$draws() %>%
  write_rds(here("RDS", "Frontier_betap_samples.rds"))
Frontier_betap_samples$draws(format = "df") %>%
  write_rds(here("RDS", "Frontier_betap_samples_df.rds"))

# 3.1.3 Rhat and effective sample size ####
Frontier_lik_rhat_ess <- bind_rows(
  Frontier_norm_samples$summary(
    variables = c("lp__", "alpha", "mu", "tau", "sigma")
  ) %>%
    summarise(rhat_mean = mean(rhat),
              rhat_sd = sd(rhat),
              ess_mean = mean(ess_bulk),
              ess_sd = sd(ess_bulk)) %>%
    mutate(model = "normal"),
  Frontier_lnorm_samples$summary(
    variables = c("lp__", "alpha", "mu", "tau", "sigma")
  ) %>%
    summarise(rhat_mean = mean(rhat),
              rhat_sd = sd(rhat),
              ess_mean = mean(ess_bulk),
              ess_sd = sd(ess_bulk)) %>%
    mutate(model = "lognormal"),
  Frontier_gamma_samples$summary(
    variables = c("lp__", "alpha", "mu", "tau", "theta")
  ) %>%
    summarise(rhat_mean = mean(rhat),
              rhat_sd = sd(rhat),
              ess_mean = mean(ess_bulk),
              ess_sd = sd(ess_bulk)) %>%
    mutate(model = "gamma"),
  Frontier_betap_samples$summary(
    variables = c("lp__", "alpha", "mu", "tau", "nu")
  ) %>%
    summarise(rhat_mean = mean(rhat),
              rhat_sd = sd(rhat),
              ess_mean = mean(ess_bulk),
              ess_sd = sd(ess_bulk)) %>%
    mutate(model = "betaprime")
) %T>%
  print()

# 3.1.4 Chains ####
Frontier_norm_chains <- Frontier_norm_samples$draws(format = "df") %>%
  mcmc_rank_overlay(pars = c("lp__", "alpha", "mu", "tau", "sigma")) +
  guides(colour = guide_legend(nrow = 1)) +
  labs(title = "Normal likelihood",
       y = "Frequency") +
  coord_cartesian(xlim = c(0, 8e3), 
                  ylim = c(0, 100),
                  expand = FALSE) +
  mytheme

Frontier_lnorm_chains <- Frontier_lnorm_samples$draws(format = "df") %>%
  mcmc_rank_overlay(pars = c("lp__", "alpha", "mu", "tau", "sigma")) +
  guides(colour = guide_legend(nrow = 1)) +
  labs(title = "Lognormal likelihood",
       y = "Frequency") +
  coord_cartesian(xlim = c(0, 8e3), 
                  ylim = c(0, 100),
                  expand = FALSE) +
  mytheme

Frontier_gamma_chains <- Frontier_gamma_samples$draws(format = "df") %>%
  mcmc_rank_overlay(pars = c("lp__", "alpha", "mu", "tau", "theta")) +
  guides(colour = guide_legend(nrow = 1)) +
  labs(title = "Gamma likelihood",
       y = "Frequency") +
  coord_cartesian(xlim = c(0, 8e3), 
                  ylim = c(0, 100),
                  expand = FALSE) +
  mytheme

Frontier_betap_chains <- Frontier_betap_samples$draws(format = "df") %>%
  mcmc_rank_overlay(pars = c("lp__", "alpha", "mu", "tau", "nu")) +
  guides(colour = guide_legend(nrow = 1)) +
  labs(title = "Beta prime likelihood",
       y = "Frequency") +
  coord_cartesian(xlim = c(0, 8e3), 
                  ylim = c(0, 100),
                  expand = FALSE) +
  mytheme

Frontier_lik_chains <- ( Frontier_norm_chains | Frontier_lnorm_chains ) / 
  ( Frontier_gamma_chains | Frontier_betap_chains )

Frontier_lik_chains %>%
  ggsave(filename = "Frontier_lik_chains.pdf", path = "Plots",
         device = cairo_pdf, width = 40, height = 30, units = "cm")

# 3.1.5 Pairs ####
# A pairs plot for the normal likelihood already exists
Frontier_lnorm_samples$draws(format = "df") %>%
  mcmc_pairs(
    pars = c("alpha", "mu", "tau", "sigma"),
    grid_args = list(top = "Lognormal likelihood")
  ) %>%
  ggsave(filename = "Frontier_lnorm_pairs.png", path = "Plots",
         width = 20, height = 20, units = "cm", bg = "white")

Frontier_gamma_samples$draws(format = "df") %>%
  mcmc_pairs(
    pars = c("alpha", "mu", "tau", "theta"),
    grid_args = list(top = "Gamma likelihood")
  ) %>%
  ggsave(filename = "Frontier_gamma_pairs.png", path = "Plots",
         width = 20, height = 20, units = "cm", bg = "white")

Frontier_betap_samples$draws(format = "df") %>%
  mcmc_pairs(
    pars = c("alpha", "mu", "tau", "nu"),
    grid_args = list(top = "Beta prime likelihood")
  ) %>%
  ggsave(filename = "Frontier_betap_pairs.png", path = "Plots",
         width = 20, height = 20, units = "cm", bg = "white")

# 3.1.6 Leave-one-out cross-validation ####
Frontier_lik_loo <- loo_compare(
  list(
    normal = Frontier_norm_samples$loo(cores = parallel::detectCores()),
    lognormal = Frontier_lnorm_samples$loo(cores = parallel::detectCores()),
    gamma = Frontier_gamma_samples$loo(cores = parallel::detectCores()),
    betaprime = Frontier_betap_samples$loo(cores = parallel::detectCores())
  )
) %>%
  as.data.frame() %>%
  rownames_to_column("model") %>%
  as_tibble() %T>%
  print()
# Warning: cannot trust stats because Pareto k diagnostic values are too high.
Frontier_betap_samples$loo(cores = parallel::detectCores())
# 99.5% are good, so it's fine.

# 3.1.7 Prior-posterior comparison ####
# Sample priors
Frontier_norm_prior <- prior_samples(
  model = Frontier_norm_model,
  data = data %>%
    filter(reference == "Frontier et al. 2022" &
             t != 0) %>%
    droplevels() %>%
    select(t, m) %>%
    compose_data(),
  samples = 1e3
)

Frontier_lnorm_prior <- prior_samples(
  model = Frontier_lnorm_model,
  data = data %>%
    filter(reference == "Frontier et al. 2022" &
             t != 0) %>%
    droplevels() %>%
    select(t, m) %>%
    compose_data(),
  samples = 1e3
  )

Frontier_gamma_prior <- prior_samples(
  model = Frontier_gamma_model,
  data = data %>%
    filter(reference == "Frontier et al. 2022" &
             t != 0) %>%
    droplevels() %>%
    select(t, m) %>%
    compose_data(),
  samples = 1e3
)

Frontier_betap_prior <- prior_samples(
  model = Frontier_betap_model,
  data = data %>%
    filter(reference == "Frontier et al. 2022" &
             t != 0) %>%
    droplevels() %>%
    select(t, m) %>%
    compose_data(),
  samples = 1e3
)

# Plot comparison
Frontier_norm_prior_posterior <- Frontier_norm_prior %>% 
  prior_posterior_draws(
    posterior_samples = Frontier_norm_samples,
    parameters = c("alpha", "mu", "tau", "sigma"),
    format = "long"
  ) %>%
  prior_posterior_plot() +
  scale_x_continuous(
    labels = scales::label_number(style_negative = "minus")
  ) +
  labs(title = "Normal likelihood") +
  coord_cartesian(expand = FALSE) +
  mytheme +
  theme(axis.line.y = element_blank(),
        axis.ticks.y = element_blank(),
        axis.text.y = element_blank(),
        axis.title = element_blank())

Frontier_lnorm_prior_posterior <- Frontier_lnorm_prior %>% 
  prior_posterior_draws(
    posterior_samples = Frontier_lnorm_samples,
    parameters = c("alpha", "mu", "tau", "sigma"),
    format = "long"
  ) %>%
  prior_posterior_plot() +
  scale_x_continuous(
    labels = scales::label_number(style_negative = "minus")
  ) +
  labs(title = "Lognormal likelihood") +
  coord_cartesian(expand = FALSE) +
  mytheme +
  theme(axis.line.y = element_blank(),
        axis.ticks.y = element_blank(),
        axis.text.y = element_blank(),
        axis.title = element_blank())

Frontier_gamma_prior_posterior <- Frontier_gamma_prior %>% 
  prior_posterior_draws(
    posterior_samples = Frontier_gamma_samples,
    parameters = c("alpha", "mu", "tau", "theta"),
    format = "long"
  ) %>%
  prior_posterior_plot() +
  scale_x_continuous(
    labels = scales::label_number(style_negative = "minus")
  ) +
  labs(title = "Gamma likelihood") +
  coord_cartesian(expand = FALSE) +
  mytheme +
  theme(axis.line.y = element_blank(),
        axis.ticks.y = element_blank(),
        axis.text.y = element_blank(),
        axis.title = element_blank())

Frontier_betap_prior_posterior <- Frontier_betap_prior %>% 
  prior_posterior_draws(
    posterior_samples = Frontier_betap_samples,
    parameters = c("alpha", "mu", "tau", "nu"),
    format = "long"
  ) %>%
  prior_posterior_plot() +
  scale_x_continuous(
    labels = scales::label_number(style_negative = "minus")
  ) +
  labs(title = "Beta prime likelihood") +
  coord_cartesian(expand = FALSE) +
  mytheme +
  theme(axis.line.y = element_blank(),
        axis.ticks.y = element_blank(),
        axis.text.y = element_blank(),
        axis.title = element_blank())

Frontier_lik_prior_posterior <- ( Frontier_norm_prior_posterior | Frontier_lnorm_prior_posterior ) / 
  ( Frontier_gamma_prior_posterior | Frontier_betap_prior_posterior )

Frontier_lik_prior_posterior %>%
  ggsave(filename = "Frontier_lik_prior_posterior.pdf", path = "Plots",
         device = cairo_pdf, width = 40, height = 30, units = "cm")

# Clean up
rm(
  list = ls(
    pattern = "(?:_model|_prior|_posterior|_chains)$"
  )
)
rm(
  list = ls(
    pattern = "(?:norm_samples|lnorm_samples|gamma_samples|betap_samples)$"
  )
)
gc()

# 3.2 Vandendriessche et al. 2007 ####
# 3.2.1 Prior simulation ####
# See above for normal likelihood

# Lognormal likelihood
tibble(n = 1:1e3,
       alpha = rnorm( 1e3 , 0 , 0.02 ),
       mu = rgamma( 1e3 , 60^2 / 40^2 , 60 / 40^2 ),
       tau = rgamma( 1e3 , 0.1^2 / 0.05^2 , 0.1 / 0.05^2 ),
       sigma = rexp( 1e3 , 1 )) %>%
  expand_grid(t = data %>%
                filter(reference == "Vandendriessche et al. 2007") %$% 
                seq(min(t), max(t), length.out = 100)) %>%
  mutate(
    m_mu = exp(
      t * alpha - ( alpha + tau ) * mu / 5 * (
        log1p_exp( 5 / mu * ( t - mu ) ) - log1p_exp( -5 )
      )
    ), 
    m = rlnorm( n() , log(m_mu) , sigma )
  ) %>%
  pivot_longer(cols = c(m_mu, m),
               names_to = "parameter") %>%
  ggplot(aes(t, value, group = n)) +
    geom_line(alpha = 0.05) +
    coord_cartesian(ylim = c(0, 10), expand = F, clip = "off") +
    facet_wrap(~parameter, scale = "free", nrow = 1) +
    theme_minimal() +
    theme(panel.grid = element_blank())

# Gamma likelihood
tibble(n = 1:1e3,
       alpha = rnorm( 1e3 , 0 , 0.02 ),
       mu = rgamma( 1e3 , 60^2 / 40^2 , 60 / 40^2 ),
       tau = rgamma( 1e3 , 0.1^2 / 0.05^2 , 0.1 / 0.05^2 ),
       theta = rexp( 1e3 , 1 )) %>%
  expand_grid(t = data %>%
                filter(reference == "Vandendriessche et al. 2007") %$% 
                seq(min(t), max(t), length.out = 100)) %>%
  mutate(
    m_mu = exp(
      t * alpha - ( alpha + tau ) * mu / 5 * (
        log1p_exp( 5 / mu * ( t - mu ) ) - log1p_exp( -5 )
      )
    ), 
    m = rgamma( n() , m_mu / theta , 1 / theta )
  ) %>%
  pivot_longer(cols = c(m_mu, m),
               names_to = "parameter") %>%
  ggplot(aes(t, value, group = n)) +
    geom_line(alpha = 0.05) +
    coord_cartesian(ylim = c(0, 10), expand = F, clip = "off") +
    facet_wrap(~parameter, scale = "free", nrow = 1) +
    theme_minimal() +
    theme(panel.grid = element_blank())

# Beta prime likelihood
tibble(n = 1:1e3,
       alpha = rnorm( 1e3 , 0 , 0.02 ),
       mu = rgamma( 1e3 , 60^2 / 40^2 , 60 / 40^2 ),
       tau = rgamma( 1e3 , 0.1^2 / 0.05^2 , 0.1 / 0.05^2 ),
       nu = rgamma( 1e3 , 100^2 / 50^2 , 100 / 50^2 )) %>%
  expand_grid(t = data %>%
                filter(reference == "Vandendriessche et al. 2007") %$% 
                seq(min(t), max(t), length.out = 100)) %>%
  mutate(
    m_mu = exp(
      t * alpha - ( alpha + tau ) * mu / 5 * (
        log1p_exp( 5 / mu * ( t - mu ) ) - log1p_exp( -5 )
      )
    ), 
    m = rbetapr( n() , m_mu * ( 1 + nu ) , 2 + nu )
  ) %>%
  pivot_longer(cols = c(m_mu, m),
               names_to = "parameter") %>%
  ggplot(aes(t, value, group = n)) +
    geom_line(alpha = 0.05) +
    coord_cartesian(ylim = c(0, 10), expand = F, clip = "off") +
    facet_wrap(~parameter, scale = "free", nrow = 1) +
    theme_minimal() +
    theme(panel.grid = element_blank())

# 3.2.2 Stan models ####
# Load models
# The normal model is the same as the relative model used above.
Vandendriessche_norm_model <- here("Stan", "Vandendriessche_relative.stan") %>% 
  read_file() %>%
  write_stan_file() %>%
  cmdstan_model()

Vandendriessche_lnorm_model <- here("Stan", "Vandendriessche_lnorm.stan") %>% 
  read_file() %>%
  write_stan_file() %>%
  cmdstan_model()

Vandendriessche_gamma_model <- here("Stan", "Vandendriessche_gamma.stan") %>% 
  read_file() %>%
  write_stan_file() %>%
  cmdstan_model()

Vandendriessche_betap_model <- here("Stan", "Vandendriessche_betap.stan") %>% 
  read_file() %>%
  write_stan_file() %>%
  cmdstan_model()

# Run models
Vandendriessche_norm_samples <- Vandendriessche_norm_model$sample(
          data = data %>%
            filter(reference == "Vandendriessche et al. 2007" &
                     t != 0) %>%
            droplevels() %>%
            select(t, m, replicate) %>%
            compose_data(),
          chains = 8,
          parallel_chains = parallel::detectCores(),
          iter_warmup = 1e3,
          iter_sampling = 1e3
        ) %T>%
  print()

Vandendriessche_lnorm_samples <- Vandendriessche_lnorm_model$sample(
          data = data %>%
            filter(reference == "Vandendriessche et al. 2007" &
                     t != 0) %>%
            droplevels() %>%
            select(t, m, replicate) %>%
            compose_data(),
          chains = 8,
          parallel_chains = parallel::detectCores(),
          iter_warmup = 1e3,
          iter_sampling = 1e3
        ) %T>%
  print()

Vandendriessche_gamma_samples <- Vandendriessche_gamma_model$sample(
          data = data %>%
            filter(reference == "Vandendriessche et al. 2007" &
                     t != 0) %>%
            droplevels() %>%
            select(t, m, replicate) %>%
            compose_data(),
          chains = 8,
          parallel_chains = parallel::detectCores(),
          iter_warmup = 1e3,
          iter_sampling = 1e3
        ) %T>%
  print()

Vandendriessche_betap_samples <- Vandendriessche_betap_model$sample(
          data = data %>%
            filter(reference == "Vandendriessche et al. 2007" &
                     t != 0) %>%
            droplevels() %>%
            select(t, m, replicate) %>%
            compose_data(),
          chains = 8,
          parallel_chains = parallel::detectCores(),
          iter_warmup = 1e3,
          iter_sampling = 1e3
        ) %T>%
  print()

# Save draws (normal ones are already saved)
Vandendriessche_lnorm_samples$draws() %>%
  write_rds(here("RDS", "Vandendriessche_lnorm_samples.rds"))
Vandendriessche_lnorm_samples$draws(format = "df") %>%
  write_rds(here("RDS", "Vandendriessche_lnorm_samples_df.rds"))

Vandendriessche_gamma_samples$draws() %>%
  write_rds(here("RDS", "Vandendriessche_gamma_samples.rds"))
Vandendriessche_gamma_samples$draws(format = "df") %>%
  write_rds(here("RDS", "Vandendriessche_gamma_samples_df.rds"))

Vandendriessche_betap_samples$draws() %>%
  write_rds(here("RDS", "Vandendriessche_betap_samples.rds"))
Vandendriessche_betap_samples$draws(format = "df") %>%
  write_rds(here("RDS", "Vandendriessche_betap_samples_df.rds"))

# 3.2.3 Rhat and effective sample size ####
Vandendriessche_lik_rhat_ess <- bind_rows(
  Vandendriessche_norm_samples$summary(
    variables = c("lp__", "alpha", "mu", "tau", "sigma")
  ) %>%
    summarise(rhat_mean = mean(rhat),
              rhat_sd = sd(rhat),
              ess_mean = mean(ess_bulk),
              ess_sd = sd(ess_bulk)) %>%
    mutate(model = "normal"),
  Vandendriessche_lnorm_samples$summary(
    variables = c("lp__", "alpha", "mu", "tau", "sigma")
  ) %>%
    summarise(rhat_mean = mean(rhat),
              rhat_sd = sd(rhat),
              ess_mean = mean(ess_bulk),
              ess_sd = sd(ess_bulk)) %>%
    mutate(model = "lognormal"),
  Vandendriessche_gamma_samples$summary(
    variables = c("lp__", "alpha", "mu", "tau", "theta")
  ) %>%
    summarise(rhat_mean = mean(rhat),
              rhat_sd = sd(rhat),
              ess_mean = mean(ess_bulk),
              ess_sd = sd(ess_bulk)) %>%
    mutate(model = "gamma"),
  Vandendriessche_betap_samples$summary(
    variables = c("lp__", "alpha", "mu", "tau", "nu")
  ) %>%
    summarise(rhat_mean = mean(rhat),
              rhat_sd = sd(rhat),
              ess_mean = mean(ess_bulk),
              ess_sd = sd(ess_bulk)) %>%
    mutate(model = "betaprime")
) %T>%
  print()

# 3.2.4 Chains ####
Vandendriessche_norm_chains <- Vandendriessche_norm_samples$draws(format = "df") %>%
  mcmc_rank_overlay(pars = c("lp__", "sigma", "alpha[1]", "mu[1]", "tau[1]", 
                             "alpha[20]", "mu[20]", "tau[20]",
                             "alpha[48]", "mu[48]", "tau[48]")) +
  guides(colour = guide_legend(nrow = 1)) +
  labs(title = "Normal likelihood",
       y = "Frequency") +
  coord_cartesian(xlim = c(0, 8e3), 
                  ylim = c(0, 100),
                  expand = FALSE) +
  mytheme

Vandendriessche_lnorm_chains <- Vandendriessche_lnorm_samples$draws(format = "df") %>%
  mcmc_rank_overlay(pars = c("lp__", "sigma", "alpha[1]", "mu[1]", "tau[1]", 
                             "alpha[20]", "mu[20]", "tau[20]",
                             "alpha[48]", "mu[48]", "tau[48]")) +
  guides(colour = guide_legend(nrow = 1)) +
  labs(title = "Lognormal likelihood",
       y = "Frequency") +
  coord_cartesian(xlim = c(0, 8e3), 
                  ylim = c(0, 100),
                  expand = FALSE) +
  mytheme

Vandendriessche_gamma_chains <- Vandendriessche_gamma_samples$draws(format = "df") %>%
  mcmc_rank_overlay(pars = c("lp__", "theta", "alpha[1]", "mu[1]", "tau[1]", 
                             "alpha[20]", "mu[20]", "tau[20]",
                             "alpha[48]", "mu[48]", "tau[48]")) +
  guides(colour = guide_legend(nrow = 1)) +
  labs(title = "Gamma likelihood",
       y = "Frequency") +
  coord_cartesian(xlim = c(0, 8e3), 
                  ylim = c(0, 100),
                  expand = FALSE) +
  mytheme

Vandendriessche_betap_chains <- Vandendriessche_betap_samples$draws(format = "df") %>%
  mcmc_rank_overlay(pars = c("lp__", "nu", "alpha[1]", "mu[1]", "tau[1]", 
                             "alpha[20]", "mu[20]", "tau[20]",
                             "alpha[48]", "mu[48]", "tau[48]")) +
  guides(colour = guide_legend(nrow = 1)) +
  labs(title = "Beta prime likelihood",
       y = "Frequency") +
  coord_cartesian(xlim = c(0, 8e3), 
                  ylim = c(0, 100),
                  expand = FALSE) +
  mytheme

Vandendriessche_lik_chains <- ( Vandendriessche_norm_chains | Vandendriessche_lnorm_chains ) / 
  ( Vandendriessche_gamma_chains | Vandendriessche_betap_chains )

Vandendriessche_lik_chains %>%
  ggsave(filename = "Vandendriessche_lik_chains.pdf", path = "Plots",
         device = cairo_pdf, width = 40, height = 30, units = "cm")

# 3.2.5 Pairs ####
# A pairs plot for the normal likelihood already exists
Vandendriessche_lnorm_samples$draws(format = "df") %>%
  mcmc_pairs(
    pars = c("alpha[1]", "mu[1]", "tau[1]", 
             "alpha[20]", "mu[20]", "tau[20]",
             "alpha[48]", "mu[48]", "tau[48]",
             "sigma"),
    grid_args = list(top = "Lognormal likelihood")
  ) %>%
  ggsave(filename = "Vandendriessche_lnorm_pairs.png", path = "Plots",
         width = 40, height = 40, units = "cm", bg = "white")

Vandendriessche_gamma_samples$draws(format = "df") %>%
  mcmc_pairs(
    pars = c("alpha[1]", "mu[1]", "tau[1]", 
             "alpha[20]", "mu[20]", "tau[20]",
             "alpha[48]", "mu[48]", "tau[48]",
             "theta"),
    grid_args = list(top = "Gamma likelihood")
  ) %>%
  ggsave(filename = "Vandendriessche_gamma_pairs.png", path = "Plots",
         width = 40, height = 40, units = "cm", bg = "white")

Vandendriessche_betap_samples$draws(format = "df") %>%
  mcmc_pairs(
    pars = c("alpha[1]", "mu[1]", "tau[1]", 
             "alpha[20]", "mu[20]", "tau[20]",
             "alpha[48]", "mu[48]", "tau[48]",
             "nu"),
    grid_args = list(top = "Beta prime likelihood")
  ) %>%
  ggsave(filename = "Vandendriessche_betap_pairs.png", path = "Plots",
         width = 40, height = 40, units = "cm", bg = "white")

# 3.2.6 Leave-one-out cross-validation ####
Vandendriessche_lik_loo <- loo_compare(
  list(
    normal = Vandendriessche_norm_samples$loo(cores = parallel::detectCores()),
    lognormal = Vandendriessche_lnorm_samples$loo(cores = parallel::detectCores()),
    gamma = Vandendriessche_gamma_samples$loo(cores = parallel::detectCores()),
    betaprime = Vandendriessche_betap_samples$loo(cores = parallel::detectCores())
  )
) %>%
  as.data.frame() %>%
  rownames_to_column("model") %>%
  as_tibble() %T>%
  print()
# Warning: cannot trust stats because Pareto k diagnostic values are too high.
Vandendriessche_betap_samples$loo(cores = parallel::detectCores())
# 99.3% are good, so it's fine.

# 3.2.7 Prior-posterior comparison ####
# Sample priors
Vandendriessche_norm_prior <- prior_samples(
  model = Vandendriessche_norm_model,
  data = data %>%
    filter(reference == "Vandendriessche et al. 2007" &
             t != 0) %>%
    droplevels() %>%
    select(t, m, replicate) %>%
    compose_data(),
  samples = 1e3
)

Vandendriessche_lnorm_prior <- prior_samples(
  model = Vandendriessche_lnorm_model,
  data = data %>%
    filter(reference == "Vandendriessche et al. 2007" &
             t != 0) %>%
    droplevels() %>%
    select(t, m, replicate) %>%
    compose_data(),
  samples = 1e3
  )

Vandendriessche_gamma_prior <- prior_samples(
  model = Vandendriessche_gamma_model,
  data = data %>%
    filter(reference == "Vandendriessche et al. 2007" &
             t != 0) %>%
    droplevels() %>%
    select(t, m, replicate) %>%
    compose_data(),
  samples = 1e3
)

Vandendriessche_betap_prior <- prior_samples(
  model = Vandendriessche_betap_model,
  data = data %>%
    filter(reference == "Vandendriessche et al. 2007" &
             t != 0) %>%
    droplevels() %>%
    select(t, m, replicate) %>%
    compose_data(),
  samples = 1e3
)

# Plot comparison
Vandendriessche_norm_prior_posterior <- Vandendriessche_norm_prior %>% 
  prior_posterior_draws(
    posterior_samples = Vandendriessche_norm_samples,
    group = data %>%
      filter(reference == "Vandendriessche et al. 2007") %>%
      droplevels() %>%
      select(replicate),
    parameters = c("alpha[replicate]", "mu[replicate]", 
                   "tau[replicate]", "sigma"),
    format = "long"
  ) %>%
  prior_posterior_plot(group_name = "replicate", ridges = TRUE) +
  scale_x_continuous(
    labels = scales::label_number(style_negative = "minus")
  ) +
  labs(title = "Normal likelihood") +
  coord_cartesian(expand = FALSE) +
  mytheme +
  theme(axis.line.y = element_blank(),
        axis.ticks.y = element_blank(),
        axis.text.y = element_blank(),
        axis.title = element_blank())

Vandendriessche_lnorm_prior_posterior <- Vandendriessche_lnorm_prior %>% 
  prior_posterior_draws(
    posterior_samples = Vandendriessche_lnorm_samples,
    group = data %>%
      filter(reference == "Vandendriessche et al. 2007") %>%
      droplevels() %>%
      select(replicate),
    parameters = c("alpha[replicate]", "mu[replicate]", 
                   "tau[replicate]", "sigma"),
    format = "long"
  ) %>%
  prior_posterior_plot(group_name = "replicate", ridges = TRUE) +
  scale_x_continuous(
    labels = scales::label_number(style_negative = "minus")
  ) +
  labs(title = "Lognormal likelihood") +
  coord_cartesian(expand = FALSE) +
  mytheme +
  theme(axis.line.y = element_blank(),
        axis.ticks.y = element_blank(),
        axis.text.y = element_blank(),
        axis.title = element_blank())

Vandendriessche_gamma_prior_posterior <- Vandendriessche_gamma_prior %>% 
  prior_posterior_draws(
    posterior_samples = Vandendriessche_gamma_samples,
    group = data %>%
      filter(reference == "Vandendriessche et al. 2007") %>%
      droplevels() %>%
      select(replicate),
    parameters = c("alpha[replicate]", "mu[replicate]", 
                   "tau[replicate]", "theta"),
    format = "long"
  ) %>%
  prior_posterior_plot(group_name = "replicate", ridges = TRUE) +
  scale_x_continuous(
    labels = scales::label_number(style_negative = "minus")
  ) +
  labs(title = "Gamma likelihood") +
  coord_cartesian(expand = FALSE) +
  mytheme +
  theme(axis.line.y = element_blank(),
        axis.ticks.y = element_blank(),
        axis.text.y = element_blank(),
        axis.title = element_blank())

Vandendriessche_betap_prior_posterior <- Vandendriessche_betap_prior %>% 
  prior_posterior_draws(
    posterior_samples = Vandendriessche_betap_samples,
    group = data %>%
      filter(reference == "Vandendriessche et al. 2007") %>%
      droplevels() %>%
      select(replicate),
    parameters = c("alpha[replicate]", "mu[replicate]", 
                   "tau[replicate]", "nu"),
    format = "long"
  ) %>%
  prior_posterior_plot(group_name = "replicate", ridges = TRUE) +
  scale_x_continuous(
    labels = scales::label_number(style_negative = "minus")
  ) +
  labs(title = "Beta prime likelihood") +
  coord_cartesian(expand = FALSE) +
  mytheme +
  theme(axis.line.y = element_blank(),
        axis.ticks.y = element_blank(),
        axis.text.y = element_blank(),
        axis.title = element_blank())

Vandendriessche_lik_prior_posterior <- 
  ( Vandendriessche_norm_prior_posterior | Vandendriessche_lnorm_prior_posterior ) / 
  ( Vandendriessche_gamma_prior_posterior | Vandendriessche_betap_prior_posterior )

Vandendriessche_lik_prior_posterior %>%
  ggsave(filename = "Vandendriessche_lik_prior_posterior.pdf", path = "Plots",
         device = cairo_pdf, width = 40, height = 30, units = "cm")

# Clean up
rm(
  list = ls(
    pattern = "(?:_model|_prior|_posterior|_chains)$"
  )
)
rm(
  list = ls(
    pattern = "(?:norm_samples|lnorm_samples|gamma_samples|betap_samples)$"
  )
)
gc()

# 3.3 Save diagnostic tables ####
lik_rhat_ess <- Frontier_lik_rhat_ess %>%
  mutate(reference = "Frontier et al. 2022") %>%
  bind_rows(
    Vandendriessche_lik_rhat_ess %>%
      mutate(reference = "Vandendriessche et al. 2007")
  ) %>%
  select(reference, model, 
         starts_with("rhat"),
         starts_with("ess")) %T>%
  print()

lik_rhat_ess %>%
  write_csv(here("Tables", "Diagnostic", "lik_rhat_ess.csv"))

lik_loo <- Frontier_lik_loo %>%
  mutate(reference = "Frontier et al. 2022") %>%
  bind_rows(
    Vandendriessche_lik_loo %>%
      mutate(reference = "Vandendriessche et al. 2007")
  ) %>%
  select(reference, model, everything()) %T>%
  print()

lik_loo %>%
  write_csv(here("Tables", "Diagnostic", "lik_loo.csv"))

# Gamma and beta prime have emerged as the best likelihoods.
# Beta prime is the mot sensible likelihood given the nature
# of the data. I will test the effect of assuming homoskedasticity
# vs. modelling heteroskedasticity on both likelihoods. The
# last tests already assumed homoskedasticity, so all that remains
# to be done is modelling heteroskedasticity. I will use an
# exponential decay function on the beta prime precision and
# reparameterise the gamma likelihood in terms of rate, so I can 
# use the same exponential decay function.

# 4. Heteroskedasticity ####
# 4.1 Frontier et al. 2022 ####
# 4.1.1 Prior simulation ####
# See above for homoskedastic model.

# Gamma likelihood
# I will reparameterise gamma in terms of mean and rate (beta) rather
# than mean and scale (theta = 1/beta). This enables me to use the same
# exponential decay function as for beta prime to describe variance.
# beta is also the natural way gamma variance is parameterised in Stan. 
# Sticking with theta would mean I need an growth rather than a decay
# function, the most reasonable choice being exponential saturation.
tibble(n = 1:1e3,
       alpha = rnorm( 1e3 , 0 , 0.02 ),
       mu = rgamma( 1e3 , 30^2 / 20^2 , 30 / 20^2 ),
       tau = rgamma( 1e3 , 0.1^2 / 0.05^2 , 0.1 / 0.05^2 ),
       # Approximate equivalence between nu (beta prime) and beta (gamma)
       # is beta ~ nu / 2, so if I pick initial nu = 4e4, then beta = 2e4.
       epsilon = rgamma( 1e3 , 2e4^2 / 1e4^2 , 2e4 / 1e4^2 ),
       lambda = rexp( 1e3 , 1 ),
       theta = rgamma( 1e3 , 250^2 / 125^2 , 250 / 125^2 )) %>%
  expand_grid(t = data %>%
                filter(reference == "Frontier et al. 2022") %$% 
                seq(min(t), max(t), length.out = 100)) %>%
  mutate(
    m_mu = exp(
      t * alpha - ( alpha + tau ) * mu / 5 * (
        log1p_exp( 5 / mu * ( t - mu ) ) - log1p_exp( -5 )
      )
    ),
    beta = theta + (epsilon - theta) * exp( -lambda * t ),
    m = rgamma( n() , m_mu * beta , beta )
  ) %>%
  pivot_longer(cols = c(m_mu, m),
               names_to = "parameter") %>%
  ggplot(aes(t, value, group = n)) +
    geom_line(alpha = 0.05) +
    coord_cartesian(expand = F, clip = "off") +
    facet_wrap(~parameter, scale = "free", nrow = 1) +
    theme_minimal() +
    theme(panel.grid = element_blank())

# Beta prime likelihood
tibble(n = 1:1e3,
       alpha = rnorm( 1e3 , 0 , 0.02 ),
       mu = rgamma( 1e3 , 30^2 / 20^2 , 30 / 20^2 ),
       tau = rgamma( 1e3 , 0.1^2 / 0.05^2 , 0.1 / 0.05^2 ),
       # Since nu ~ 2 * beta, I pick 4e4 and 500 as initial and final.
       epsilon = rgamma( 1e3 , 4e4^2 / 2e4^2 , 4e4 / 2e4^2 ),
       lambda = rexp( 1e3 , 1 ),
       theta = rgamma( 1e3 , 500^2 / 250^2 , 500 / 250^2 )) %>%
  expand_grid(t = data %>%
                filter(reference == "Frontier et al. 2022") %$% 
                seq(min(t), max(t), length.out = 100)) %>%
  mutate(
    m_mu = exp(
      t * alpha - ( alpha + tau ) * mu / 5 * (
        log1p_exp( 5 / mu * ( t - mu ) ) - log1p_exp( -5 )
      )
    ),
    nu = theta + (epsilon - theta) * exp( -lambda * t ),
    m = rbetapr( n() , m_mu * ( 1 + nu ) , 2 + nu )
  ) %>%
  pivot_longer(cols = c(m_mu, m),
               names_to = "parameter") %>%
  ggplot(aes(t, value, group = n)) +
    geom_line(alpha = 0.05) +
    coord_cartesian(expand = F, clip = "off") +
    facet_wrap(~parameter, scale = "free", nrow = 1) +
    theme_minimal() +
    theme(panel.grid = element_blank())

# 4.1.2 Stan models ####
# Load models
# The homoskedastic models are the same as used above.
Frontier_gamma_hom_model <- here("Stan", "Frontier_gamma.stan") %>% 
  read_file() %>%
  write_stan_file() %>%
  cmdstan_model()

Frontier_gamma_het_model <- here("Stan", "Frontier_gamma_het.stan") %>% 
  read_file() %>%
  write_stan_file() %>%
  cmdstan_model()

Frontier_betap_hom_model <- here("Stan", "Frontier_betap.stan") %>% 
  read_file() %>%
  write_stan_file() %>%
  cmdstan_model()

Frontier_betap_het_model <- here("Stan", "Frontier_betap_het.stan") %>% 
  read_file() %>%
  write_stan_file() %>%
  cmdstan_model()

# Run models
Frontier_gamma_hom_samples <- Frontier_gamma_hom_model$sample(
          data = data %>%
            filter(reference == "Frontier et al. 2022" &
                     t != 0) %>%
            droplevels() %>%
            select(t, m) %>%
            compose_data(),
          chains = 8,
          parallel_chains = parallel::detectCores(),
          iter_warmup = 1e3,
          iter_sampling = 1e3
        ) %T>%
  print()

Frontier_gamma_het_samples <- Frontier_gamma_het_model$sample(
          data = data %>%
            filter(reference == "Frontier et al. 2022" &
                     t != 0) %>%
            droplevels() %>%
            select(t, m) %>%
            compose_data(),
          chains = 8,
          parallel_chains = parallel::detectCores(),
          iter_warmup = 1e3,
          iter_sampling = 1e3
        ) %T>%
  print()

Frontier_betap_hom_samples <- Frontier_betap_hom_model$sample(
          data = data %>%
            filter(reference == "Frontier et al. 2022" &
                     t != 0) %>%
            droplevels() %>%
            select(t, m) %>%
            compose_data(),
          chains = 8,
          parallel_chains = parallel::detectCores(),
          iter_warmup = 1e3,
          iter_sampling = 1e3
        ) %T>%
  print()

Frontier_betap_het_samples <- Frontier_betap_het_model$sample(
          data = data %>%
            filter(reference == "Frontier et al. 2022" &
                     t != 0) %>%
            droplevels() %>%
            select(t, m) %>%
            compose_data(),
          chains = 8,
          parallel_chains = parallel::detectCores(),
          iter_warmup = 1e3,
          iter_sampling = 1e3
        ) %T>%
  print()

# Save draws (homoskedastic ones are already saved)
Frontier_gamma_het_samples$draws() %>%
  write_rds(here("RDS", "Frontier_gamma_het_samples.rds"))
Frontier_gamma_het_samples$draws(format = "df") %>%
  write_rds(here("RDS", "Frontier_gamma_het_samples_df.rds"))

Frontier_betap_het_samples$draws() %>%
  write_rds(here("RDS", "Frontier_betap_het_samples.rds"))
Frontier_betap_het_samples$draws(format = "df") %>%
  write_rds(here("RDS", "Frontier_betap_het_samples_df.rds"))

# 4.1.3 Rhat and effective sample size ####
Frontier_het_rhat_ess <- bind_rows(
  Frontier_gamma_hom_samples$summary(
    variables = c("lp__", "alpha", "mu", "tau", "theta")
  ) %>%
    summarise(rhat_mean = mean(rhat),
              rhat_sd = sd(rhat),
              ess_mean = mean(ess_bulk),
              ess_sd = sd(ess_bulk)) %>%
    mutate(model = "gamma homoskedastic"),
  Frontier_gamma_het_samples$summary(
    variables = c("lp__", "alpha", "mu", "tau", 
                  "epsilon", "lambda", "theta")
  ) %>%
    summarise(rhat_mean = mean(rhat),
              rhat_sd = sd(rhat),
              ess_mean = mean(ess_bulk),
              ess_sd = sd(ess_bulk)) %>%
    mutate(model = "gamma heteroskedastic"),
  Frontier_betap_hom_samples$summary(
    variables = c("lp__", "alpha", "mu", "tau", "nu")
  ) %>%
    summarise(rhat_mean = mean(rhat),
              rhat_sd = sd(rhat),
              ess_mean = mean(ess_bulk),
              ess_sd = sd(ess_bulk)) %>%
    mutate(model = "betaprime homoskedastic"),
  Frontier_betap_het_samples$summary(
    variables = c("lp__", "alpha", "mu", "tau",
                  "epsilon", "lambda", "theta")
  ) %>%
    summarise(rhat_mean = mean(rhat),
              rhat_sd = sd(rhat),
              ess_mean = mean(ess_bulk),
              ess_sd = sd(ess_bulk)) %>%
    mutate(model = "betaprime heteroskedastic")
) %T>%
  print()

# 4.1.4 Chains ####
Frontier_gamma_hom_chains <- Frontier_gamma_hom_samples$draws(format = "df") %>%
  mcmc_rank_overlay(pars = c("lp__", "alpha", "mu", "tau", "theta")) +
  guides(colour = guide_legend(nrow = 1)) +
  labs(title = "Gamma likelihood with homogenous variance",
       y = "Frequency") +
  coord_cartesian(xlim = c(0, 8e3), 
                  ylim = c(0, 100),
                  expand = FALSE) +
  mytheme

Frontier_gamma_het_chains <- Frontier_gamma_het_samples$draws(format = "df") %>%
  mcmc_rank_overlay(pars = c("lp__", "alpha", "mu", "tau",
                             "epsilon", "lambda", "theta")) +
  guides(colour = guide_legend(nrow = 1)) +
  labs(title = "Gamma likelihood with heterogenous variance",
       y = "Frequency") +
  coord_cartesian(xlim = c(0, 8e3), 
                  ylim = c(0, 100),
                  expand = FALSE) +
  mytheme

Frontier_betap_hom_chains <- Frontier_betap_hom_samples$draws(format = "df") %>%
  mcmc_rank_overlay(pars = c("lp__", "alpha", "mu", "tau", "nu")) +
  guides(colour = guide_legend(nrow = 1)) +
  labs(title = "Beta prime likelihood with homogenous variance",
       y = "Frequency") +
  coord_cartesian(xlim = c(0, 8e3), 
                  ylim = c(0, 100),
                  expand = FALSE) +
  mytheme

Frontier_betap_het_chains <- Frontier_betap_het_samples$draws(format = "df") %>%
  mcmc_rank_overlay(pars = c("lp__", "alpha", "mu", "tau",
                             "epsilon", "lambda", "theta")) +
  guides(colour = guide_legend(nrow = 1)) +
  labs(title = "Beta prime likelihood with heterogenous variance",
       y = "Frequency") +
  coord_cartesian(xlim = c(0, 8e3), 
                  ylim = c(0, 100),
                  expand = FALSE) +
  mytheme

Frontier_het_chains <- ( Frontier_gamma_hom_chains | Frontier_gamma_het_chains ) / 
  ( Frontier_betap_hom_chains | Frontier_betap_het_chains )

Frontier_het_chains %>%
  ggsave(filename = "Frontier_het_chains.pdf", path = "Plots",
         device = cairo_pdf, width = 40, height = 30, units = "cm")

# 4.1.5 Pairs ####
# Pairs plots for the homogenous models already exist
Frontier_gamma_het_samples$draws(format = "df") %>%
  mcmc_pairs(
    pars = c("alpha", "mu", "tau",
             "epsilon", "lambda", "theta"),
    grid_args = list(top = "Gamma likelihood with heterogenous variance")
  ) %>%
  ggsave(filename = "Frontier_gamma_het_pairs.png", path = "Plots",
         width = 20, height = 20, units = "cm", bg = "white")

Frontier_betap_het_samples$draws(format = "df") %>%
  mcmc_pairs(
    pars = c("alpha", "mu", "tau",
             "epsilon", "lambda", "theta"),
    grid_args = list(top = "Beta prime likelihood with heterogenous variance")
  ) %>%
  ggsave(filename = "Frontier_betap_het_pairs.png", path = "Plots",
         width = 20, height = 20, units = "cm", bg = "white")

# 4.1.6 Leave-one-out cross-validation ####
Frontier_het_loo <- loo_compare(
  list(
    gamma_homoskedastic = 
      Frontier_gamma_hom_samples$loo(cores = parallel::detectCores()),
    gamma_heteroskedastic = 
      Frontier_gamma_het_samples$loo(cores = parallel::detectCores()),
    betaprime_homoskedastic = 
      Frontier_betap_hom_samples$loo(cores = parallel::detectCores()),
    betaprime_heteroskedastic = 
      Frontier_betap_het_samples$loo(cores = parallel::detectCores())
  )
) %>%
  as.data.frame() %>%
  rownames_to_column("model") %>%
  as_tibble() %T>%
  print()
# Warning: cannot trust stats because Pareto k diagnostic values are too high.
Frontier_betap_het_samples$loo(cores = parallel::detectCores())
# 99.5% are good, so it's fine.

# 4.1.7 Prior-posterior comparison ####
# Sample priors
Frontier_gamma_hom_prior <- prior_samples(
  model = Frontier_gamma_hom_model,
  data = data %>%
    filter(reference == "Frontier et al. 2022" &
             t != 0) %>%
    droplevels() %>%
    select(t, m) %>%
    compose_data(),
  samples = 1e3
)

Frontier_gamma_het_prior <- prior_samples(
  model = Frontier_gamma_het_model,
  data = data %>%
    filter(reference == "Frontier et al. 2022" &
             t != 0) %>%
    droplevels() %>%
    select(t, m) %>%
    compose_data(),
  samples = 1e3
  )

Frontier_betap_hom_prior <- prior_samples(
  model = Frontier_betap_hom_model,
  data = data %>%
    filter(reference == "Frontier et al. 2022" &
             t != 0) %>%
    droplevels() %>%
    select(t, m) %>%
    compose_data(),
  samples = 1e3
)

Frontier_betap_het_prior <- prior_samples(
  model = Frontier_betap_het_model,
  data = data %>%
    filter(reference == "Frontier et al. 2022" &
             t != 0) %>%
    droplevels() %>%
    select(t, m) %>%
    compose_data(),
  samples = 1e3
)

# Plot comparison
Frontier_gamma_hom_prior_posterior <- Frontier_gamma_hom_prior %>% 
  prior_posterior_draws(
    posterior_samples = Frontier_gamma_hom_samples,
    parameters = c("alpha", "mu", "tau", "theta"),
    format = "long"
  ) %>%
  prior_posterior_plot() +
  scale_x_continuous(
    labels = scales::label_number(style_negative = "minus")
  ) +
  labs(title = "Gamma likelihood with homogenous variance") +
  coord_cartesian(expand = FALSE) +
  mytheme +
  theme(axis.line.y = element_blank(),
        axis.ticks.y = element_blank(),
        axis.text.y = element_blank(),
        axis.title = element_blank())

Frontier_gamma_het_prior_posterior <- Frontier_gamma_het_prior %>% 
  prior_posterior_draws(
    posterior_samples = Frontier_gamma_het_samples,
    parameters = c("alpha", "mu", "tau", 
                   "epsilon", "lambda", "theta"),
    format = "long"
  ) %>%
  prior_posterior_plot() +
  scale_x_continuous(
    labels = scales::label_number(style_negative = "minus")
  ) +
  labs(title = "Gamma likelihood with heterogenous variance") +
  coord_cartesian(expand = FALSE) +
  mytheme +
  theme(axis.line.y = element_blank(),
        axis.ticks.y = element_blank(),
        axis.text.y = element_blank(),
        axis.title = element_blank())

Frontier_betap_hom_prior_posterior <- Frontier_betap_hom_prior %>% 
  prior_posterior_draws(
    posterior_samples = Frontier_betap_hom_samples,
    parameters = c("alpha", "mu", "tau", "nu"),
    format = "long"
  ) %>%
  prior_posterior_plot() +
  scale_x_continuous(
    labels = scales::label_number(style_negative = "minus")
  ) +
  labs(title = "Beta prime likelihood with homogenous variance") +
  coord_cartesian(expand = FALSE) +
  mytheme +
  theme(axis.line.y = element_blank(),
        axis.ticks.y = element_blank(),
        axis.text.y = element_blank(),
        axis.title = element_blank())

Frontier_betap_het_prior_posterior <- Frontier_betap_het_prior %>% 
  prior_posterior_draws(
    posterior_samples = Frontier_betap_het_samples,
    parameters = c("alpha", "mu", "tau",
                   "epsilon", "lambda", "theta"),
    format = "long"
  ) %>%
  prior_posterior_plot() +
  scale_x_continuous(
    labels = scales::label_number(style_negative = "minus")
  ) +
  labs(title = "Beta prime likelihood with heterogenous variance") +
  coord_cartesian(expand = FALSE) +
  mytheme +
  theme(axis.line.y = element_blank(),
        axis.ticks.y = element_blank(),
        axis.text.y = element_blank(),
        axis.title = element_blank())

Frontier_het_prior_posterior <- 
  ( Frontier_gamma_hom_prior_posterior | Frontier_gamma_het_prior_posterior ) / 
  ( Frontier_betap_hom_prior_posterior | Frontier_betap_het_prior_posterior )

Frontier_het_prior_posterior %>%
  ggsave(filename = "Frontier_het_prior_posterior.pdf", path = "Plots",
         device = cairo_pdf, width = 40, height = 30, units = "cm")

# Clean up
rm(
  list = ls(
    pattern = "(?:_model|_prior|_posterior|_chains|hom_samples|het_samples)$"
  )
)
gc()

# 4.2 Vandendriessche et al. 2007 ####
# 4.2.1 Prior simulation ####
# See above for homoskedastic model.

# Gamma likelihood
tibble(n = 1:1e3,
       alpha = rnorm( 1e3 , 0 , 0.02 ),
       mu = rgamma( 1e3 , 60^2 / 40^2 , 60 / 40^2 ),
       tau = rgamma( 1e3 , 0.1^2 / 0.05^2 , 0.1 / 0.05^2 ),
       epsilon = rgamma( 1e3 , 2e4^2 / 1e4^2 , 2e4 / 1e4^2 ),
       lambda = rexp( 1e3 , 1 ),
       theta = rgamma( 1e3 , 250^2 / 125^2 , 250 / 125^2 )) %>%
  expand_grid(t = data %>%
                filter(reference == "Vandendriessche et al. 2007") %$% 
                seq(min(t), max(t), length.out = 100)) %>%
  mutate(
    m_mu = exp(
      t * alpha - ( alpha + tau ) * mu / 5 * (
        log1p_exp( 5 / mu * ( t - mu ) ) - log1p_exp( -5 )
      )
    ),
    beta = theta + (epsilon - theta) * exp( -lambda * t ),
    m = rgamma( n() , m_mu * beta , beta )
  ) %>%
  pivot_longer(cols = c(m_mu, m),
               names_to = "parameter") %>%
  ggplot(aes(t, value, group = n)) +
    geom_line(alpha = 0.05) +
    coord_cartesian(ylim = c(0, 10), expand = F, clip = "off") +
    facet_wrap(~parameter, scale = "free", nrow = 1) +
    theme_minimal() +
    theme(panel.grid = element_blank())

# Beta prime likelihood
tibble(n = 1:1e3,
       alpha = rnorm( 1e3 , 0 , 0.02 ),
       mu = rgamma( 1e3 , 60^2 / 40^2 , 60 / 40^2 ),
       tau = rgamma( 1e3 , 0.1^2 / 0.05^2 , 0.1 / 0.05^2 ),
       epsilon = rgamma( 1e3 , 4e4^2 / 2e4^2 , 4e4 / 2e4^2 ),
       lambda = rexp( 1e3 , 1 ),
       theta = rgamma( 1e3 , 500^2 / 250^2 , 500 / 250^2 )) %>%
  expand_grid(t = data %>%
                filter(reference == "Vandendriessche et al. 2007") %$% 
                seq(min(t), max(t), length.out = 100)) %>%
  mutate(
    m_mu = exp(
      t * alpha - ( alpha + tau ) * mu / 5 * (
        log1p_exp( 5 / mu * ( t - mu ) ) - log1p_exp( -5 )
      )
    ),
    nu = theta + (epsilon - theta) * exp( -lambda * t ),
    m = rbetapr( n() , m_mu * ( 1 + nu ) , 2 + nu )
  ) %>%
  pivot_longer(cols = c(m_mu, m),
               names_to = "parameter") %>%
  ggplot(aes(t, value, group = n)) +
    geom_line(alpha = 0.05) +
    coord_cartesian(ylim = c(0, 10), expand = F, clip = "off") +
    facet_wrap(~parameter, scale = "free", nrow = 1) +
    theme_minimal() +
    theme(panel.grid = element_blank())

# 4.2.2 Stan models ####
# Load models
# The homoskedastic models are the same as used above.
Vandendriessche_gamma_hom_model <- here("Stan", "Vandendriessche_gamma.stan") %>% 
  read_file() %>%
  write_stan_file() %>%
  cmdstan_model()

Vandendriessche_gamma_het_model <- here("Stan", "Vandendriessche_gamma_het.stan") %>% 
  read_file() %>%
  write_stan_file() %>%
  cmdstan_model()

Vandendriessche_betap_hom_model <- here("Stan", "Vandendriessche_betap.stan") %>% 
  read_file() %>%
  write_stan_file() %>%
  cmdstan_model()

Vandendriessche_betap_het_model <- here("Stan", "Vandendriessche_betap_het.stan") %>% 
  read_file() %>%
  write_stan_file() %>%
  cmdstan_model()

# Run models
Vandendriessche_gamma_hom_samples <- Vandendriessche_gamma_hom_model$sample(
          data = data %>%
            filter(reference == "Vandendriessche et al. 2007" &
                     t != 0) %>%
            droplevels() %>%
            select(t, m, replicate) %>%
            compose_data(),
          chains = 8,
          parallel_chains = parallel::detectCores(),
          iter_warmup = 1e3,
          iter_sampling = 1e3
        ) %T>%
  print()

Vandendriessche_gamma_het_samples <- Vandendriessche_gamma_het_model$sample(
          data = data %>%
            filter(reference == "Vandendriessche et al. 2007" &
                     t != 0) %>%
            droplevels() %>%
            select(t, m, replicate) %>%
            compose_data(),
          chains = 8,
          parallel_chains = parallel::detectCores(),
          iter_warmup = 1e3,
          iter_sampling = 1e3
        ) %T>%
  print()

Vandendriessche_betap_hom_samples <- Vandendriessche_betap_hom_model$sample(
          data = data %>%
            filter(reference == "Vandendriessche et al. 2007" &
                     t != 0) %>%
            droplevels() %>%
            select(t, m, replicate) %>%
            compose_data(),
          chains = 8,
          parallel_chains = parallel::detectCores(),
          iter_warmup = 1e3,
          iter_sampling = 1e3
        ) %T>%
  print()

Vandendriessche_betap_het_samples <- Vandendriessche_betap_het_model$sample(
          data = data %>%
            filter(reference == "Vandendriessche et al. 2007" &
                     t != 0) %>%
            droplevels() %>%
            select(t, m, replicate) %>%
            compose_data(),
          chains = 8,
          parallel_chains = parallel::detectCores(),
          iter_warmup = 1e3,
          iter_sampling = 1e3
        ) %T>%
  print()

# Save draws (homoskedastic ones are already saved)
Vandendriessche_gamma_het_samples$draws() %>%
  write_rds(here("RDS", "Vandendriessche_gamma_het_samples.rds"))
Vandendriessche_gamma_het_samples$draws(format = "df") %>%
  write_rds(here("RDS", "Vandendriessche_gamma_het_samples_df.rds"))

Vandendriessche_betap_het_samples$draws() %>%
  write_rds(here("RDS", "Vandendriessche_betap_het_samples.rds"))
Vandendriessche_betap_het_samples$draws(format = "df") %>%
  write_rds(here("RDS", "Vandendriessche_betap_het_samples_df.rds"))

# 4.2.3 Rhat and effective sample size ####
Vandendriessche_het_rhat_ess <- bind_rows(
  Vandendriessche_gamma_hom_samples$summary(
    variables = c("lp__", "alpha", "mu", "tau", "theta")
  ) %>%
    summarise(rhat_mean = mean(rhat),
              rhat_sd = sd(rhat),
              ess_mean = mean(ess_bulk),
              ess_sd = sd(ess_bulk)) %>%
    mutate(model = "gamma homoskedastic"),
  Vandendriessche_gamma_het_samples$summary(
    variables = c("lp__", "alpha", "mu", "tau",
                  "epsilon", "lambda", "theta")
  ) %>%
    summarise(rhat_mean = mean(rhat),
              rhat_sd = sd(rhat),
              ess_mean = mean(ess_bulk),
              ess_sd = sd(ess_bulk)) %>%
    mutate(model = "gamma heteroskedastic"),
  Vandendriessche_betap_hom_samples$summary(
    variables = c("lp__", "alpha", "mu", "tau", "nu")
  ) %>%
    summarise(rhat_mean = mean(rhat),
              rhat_sd = sd(rhat),
              ess_mean = mean(ess_bulk),
              ess_sd = sd(ess_bulk)) %>%
    mutate(model = "betaprime homoskedastic"),
  Vandendriessche_betap_het_samples$summary(
    variables = c("lp__", "alpha", "mu", "tau",
                  "epsilon", "lambda", "theta")
  ) %>%
    summarise(rhat_mean = mean(rhat),
              rhat_sd = sd(rhat),
              ess_mean = mean(ess_bulk),
              ess_sd = sd(ess_bulk)) %>%
    mutate(model = "betaprime heteroskedastic")
) %T>%
  print()

# 4.2.4 Chains ####
Vandendriessche_gamma_hom_chains <- Vandendriessche_gamma_hom_samples$draws(format = "df") %>%
  mcmc_rank_overlay(pars = c("lp__", "theta", "alpha[1]", "mu[1]", "tau[1]", 
                             "alpha[20]", "mu[20]", "tau[20]",
                             "alpha[48]", "mu[48]", "tau[48]")) +
  guides(colour = guide_legend(nrow = 1)) +
  labs(title = "Gamma likelihood with homogenous variance",
       y = "Frequency") +
  coord_cartesian(xlim = c(0, 8e3), 
                  ylim = c(0, 100),
                  expand = FALSE) +
  mytheme

Vandendriessche_gamma_het_chains <- Vandendriessche_gamma_het_samples$draws(format = "df") %>%
  mcmc_rank_overlay(pars = c("lp__", "alpha[1]", "mu[1]", "tau[1]", 
                             "alpha[20]", "mu[20]", "tau[20]",
                             "alpha[48]", "mu[48]", "tau[48]",
                             "epsilon", "lambda", "theta")) +
  guides(colour = guide_legend(nrow = 1)) +
  labs(title = "Gamma likelihood with heterogenous variance",
       y = "Frequency") +
  coord_cartesian(xlim = c(0, 8e3), 
                  ylim = c(0, 100),
                  expand = FALSE) +
  mytheme

Vandendriessche_betap_hom_chains <- Vandendriessche_betap_hom_samples$draws(format = "df") %>%
  mcmc_rank_overlay(pars = c("lp__", "nu", "alpha[1]", "mu[1]", "tau[1]", 
                             "alpha[20]", "mu[20]", "tau[20]",
                             "alpha[48]", "mu[48]", "tau[48]")) +
  guides(colour = guide_legend(nrow = 1)) +
  labs(title = "Beta prime likelihood with homogenous variance",
       y = "Frequency") +
  coord_cartesian(xlim = c(0, 8e3), 
                  ylim = c(0, 100),
                  expand = FALSE) +
  mytheme

Vandendriessche_betap_het_chains <- Vandendriessche_betap_het_samples$draws(format = "df") %>%
  mcmc_rank_overlay(pars = c("lp__", "alpha[1]", "mu[1]", "tau[1]", 
                             "alpha[20]", "mu[20]", "tau[20]",
                             "alpha[48]", "mu[48]", "tau[48]",
                             "epsilon", "lambda", "theta")) +
  guides(colour = guide_legend(nrow = 1)) +
  labs(title = "Beta prime likelihood with heterogenous variance",
       y = "Frequency") +
  coord_cartesian(xlim = c(0, 8e3), 
                  ylim = c(0, 100),
                  expand = FALSE) +
  mytheme

Vandendriessche_het_chains <- 
  ( Vandendriessche_gamma_hom_chains | Vandendriessche_gamma_het_chains ) / 
  ( Vandendriessche_betap_hom_chains | Vandendriessche_betap_het_chains )

Vandendriessche_het_chains %>%
  ggsave(filename = "Vandendriessche_het_chains.pdf", path = "Plots",
         device = cairo_pdf, width = 40, height = 30, units = "cm")

# 4.2.5 Pairs ####
# Pairs plots for the homogenous models already exist
Vandendriessche_gamma_het_samples$draws(format = "df") %>%
  mcmc_pairs(
    pars = c("alpha[1]", "mu[1]", "tau[1]", 
             "alpha[20]", "mu[20]", "tau[20]",
             "alpha[48]", "mu[48]", "tau[48]",
             "epsilon", "lambda", "theta"),
    grid_args = list(top = "Gamma likelihood with heterogenous variance")
  ) %>%
  ggsave(filename = "Vandendriessche_gamma_het_pairs.png", path = "Plots",
         width = 40, height = 40, units = "cm", bg = "white")

Vandendriessche_betap_het_samples$draws(format = "df") %>%
  mcmc_pairs(
    pars = c("alpha[1]", "mu[1]", "tau[1]", 
             "alpha[20]", "mu[20]", "tau[20]",
             "alpha[48]", "mu[48]", "tau[48]",
             "epsilon", "lambda", "theta"),
    grid_args = list(top = "Beta prime likelihood with heterogenous variance")
  ) %>%
  ggsave(filename = "Vandendriessche_betap_het_pairs.png", path = "Plots",
         width = 40, height = 40, units = "cm", bg = "white")

# 4.2.6 Leave-one-out cross-validation ####
Vandendriessche_het_loo <- loo_compare(
  list(
    gamma_homoskedastic = 
      Vandendriessche_gamma_hom_samples$loo(cores = parallel::detectCores()),
    gamma_heteroskedastic = 
      Vandendriessche_gamma_het_samples$loo(cores = parallel::detectCores()),
    betaprime_homoskedastic = 
      Vandendriessche_betap_hom_samples$loo(cores = parallel::detectCores()),
    betaprime_heteroskedastic = 
      Vandendriessche_betap_het_samples$loo(cores = parallel::detectCores())
  )
) %>%
  as.data.frame() %>%
  rownames_to_column("model") %>%
  as_tibble() %T>%
  print()
# Warning: cannot trust stats because Pareto k diagnostic values are too high.
Vandendriessche_betap_het_samples$loo(cores = parallel::detectCores())
# 99.2% are good, so it's fine.

# 4.2.7 Prior-posterior comparison ####
# Sample priors
Vandendriessche_gamma_hom_prior <- prior_samples(
  model = Vandendriessche_gamma_hom_model,
  data = data %>%
    filter(reference == "Vandendriessche et al. 2007" &
             t != 0) %>%
    droplevels() %>%
    select(t, m, replicate) %>%
    compose_data(),
  samples = 1e3
)

Vandendriessche_gamma_het_prior <- prior_samples(
  model = Vandendriessche_gamma_het_model,
  data = data %>%
    filter(reference == "Vandendriessche et al. 2007" &
             t != 0) %>%
    droplevels() %>%
    select(t, m, replicate) %>%
    compose_data(),
  samples = 1e3
  )

Vandendriessche_betap_hom_prior <- prior_samples(
  model = Vandendriessche_betap_hom_model,
  data = data %>%
    filter(reference == "Vandendriessche et al. 2007" &
             t != 0) %>%
    droplevels() %>%
    select(t, m, replicate) %>%
    compose_data(),
  samples = 1e3
)

Vandendriessche_betap_het_prior <- prior_samples(
  model = Vandendriessche_betap_het_model,
  data = data %>%
    filter(reference == "Vandendriessche et al. 2007" &
             t != 0) %>%
    droplevels() %>%
    select(t, m, replicate) %>%
    compose_data(),
  samples = 1e3
)

# Plot comparison
Vandendriessche_gamma_hom_prior_posterior <- Vandendriessche_gamma_hom_prior %>% 
  prior_posterior_draws(
    posterior_samples = Vandendriessche_gamma_hom_samples,
    group = data %>%
      filter(reference == "Vandendriessche et al. 2007") %>%
      droplevels() %>%
      select(replicate),
    parameters = c("alpha[replicate]", "mu[replicate]", 
                   "tau[replicate]", "theta"),
    format = "long"
  ) %>%
  prior_posterior_plot(group_name = "replicate", ridges = TRUE) +
  scale_x_continuous(
    labels = scales::label_number(style_negative = "minus")
  ) +
  labs(title = "Gamma likelihood with homogenous variance") +
  coord_cartesian(expand = FALSE) +
  mytheme +
  theme(axis.line.y = element_blank(),
        axis.ticks.y = element_blank(),
        axis.text.y = element_blank(),
        axis.title = element_blank())

Vandendriessche_gamma_het_prior_posterior <- Vandendriessche_gamma_het_prior %>% 
  prior_posterior_draws(
    posterior_samples = Vandendriessche_gamma_het_samples,
    group = data %>%
      filter(reference == "Vandendriessche et al. 2007") %>%
      droplevels() %>%
      select(replicate),
    parameters = c("alpha[replicate]", "mu[replicate]", 
                   "tau[replicate]", "epsilon", "lambda", "theta"),
    format = "long"
  ) %>%
  prior_posterior_plot(group_name = "replicate", ridges = TRUE) +
  scale_x_continuous(
    labels = scales::label_number(style_negative = "minus")
  ) +
  labs(title = "Gamma likelihood with heterogenous variance") +
  coord_cartesian(expand = FALSE) +
  mytheme +
  theme(axis.line.y = element_blank(),
        axis.ticks.y = element_blank(),
        axis.text.y = element_blank(),
        axis.title = element_blank())

Vandendriessche_betap_hom_prior_posterior <- Vandendriessche_betap_hom_prior %>% 
  prior_posterior_draws(
    posterior_samples = Vandendriessche_betap_hom_samples,
    group = data %>%
      filter(reference == "Vandendriessche et al. 2007") %>%
      droplevels() %>%
      select(replicate),
    parameters = c("alpha[replicate]", "mu[replicate]", 
                   "tau[replicate]", "nu"),
    format = "long"
  ) %>%
  prior_posterior_plot(group_name = "replicate", ridges = TRUE) +
  scale_x_continuous(
    labels = scales::label_number(style_negative = "minus")
  ) +
  labs(title = "Beta prime likelihood with homogenous variance") +
  coord_cartesian(expand = FALSE) +
  mytheme +
  theme(axis.line.y = element_blank(),
        axis.ticks.y = element_blank(),
        axis.text.y = element_blank(),
        axis.title = element_blank())

Vandendriessche_betap_het_prior_posterior <- Vandendriessche_betap_het_prior %>% 
  prior_posterior_draws(
    posterior_samples = Vandendriessche_betap_het_samples,
    group = data %>%
      filter(reference == "Vandendriessche et al. 2007") %>%
      droplevels() %>%
      select(replicate),
    parameters = c("alpha[replicate]", "mu[replicate]", 
                   "tau[replicate]", "epsilon", "lambda", "theta"),
    format = "long"
  ) %>%
  prior_posterior_plot(group_name = "replicate", ridges = TRUE) +
  scale_x_continuous(
    labels = scales::label_number(style_negative = "minus")
  ) +
  labs(title = "Beta prime likelihood with heterogenous variance") +
  coord_cartesian(expand = FALSE) +
  mytheme +
  theme(axis.line.y = element_blank(),
        axis.ticks.y = element_blank(),
        axis.text.y = element_blank(),
        axis.title = element_blank())

Vandendriessche_het_prior_posterior <- 
  ( Vandendriessche_gamma_hom_prior_posterior | Vandendriessche_gamma_het_prior_posterior ) / 
  ( Vandendriessche_betap_hom_prior_posterior | Vandendriessche_betap_het_prior_posterior )

Vandendriessche_het_prior_posterior %>%
  ggsave(filename = "Vandendriessche_het_prior_posterior.pdf", path = "Plots",
         device = cairo_pdf, width = 40, height = 30, units = "cm")

# Clean up
rm(
  list = ls(
    pattern = "(?:_model|_prior|_posterior|_chains|hom_samples|het_samples)$"
  )
)
gc()

# 4.3 Save diagnostic tables ####
het_rhat_ess <- Frontier_het_rhat_ess %>%
  mutate(reference = "Frontier et al. 2022") %>%
  bind_rows(
    Vandendriessche_het_rhat_ess %>%
      mutate(reference = "Vandendriessche et al. 2007")
  ) %>%
  select(reference, model, 
         starts_with("rhat"),
         starts_with("ess")) %T>%
  print()

het_rhat_ess %>%
  write_csv(here("Tables", "Diagnostic", "het_rhat_ess.csv"))

het_loo <- Frontier_het_loo %>%
  mutate(reference = "Frontier et al. 2022") %>%
  bind_rows(
    Vandendriessche_het_loo %>%
      mutate(reference = "Vandendriessche et al. 2007")
  ) %>%
  select(reference, model, everything()) %T>%
  print()

het_loo %>%
  write_csv(here("Tables", "Diagnostic", "het_loo.csv"))

# Heteroskedastic models are harder to fit but definitely
# have better predictive performance.

# 5. Conventional model ####
# I fit the constant exponential decay model ( exp(-k*t) ) to each example
# dataset below to compare predictions of k to alpha, mu and tau and visually 
# compare predictions of m, but I cannot feasibly calculate the point-wise log 
# likelihood for all my models. This becomes computationally inefficient for 
# multilevel models with more MCMC samples. Therefore I am doing this here 
# for the two familiar examples. I am using a normal likelihood with 
# homogenous variance for the constant exponential decay model since this 
# aligns with conventional assumptions.

# 5.1 Frontier et al. 2022 ####
# 5.1.1 Prior simulation ####
# See above for macroalgal model.
tibble(n = 1:1e3,
       k = rgamma( 1e3 , 0.1^2 / 0.05^2 , 0.1 / 0.05^2 ),
       sigma = rexp( 1e3 , 1 )) %>%
  expand_grid(t = data %>%
                filter(reference == "Frontier et al. 2022") %$% 
                seq(min(t), max(t), length.out = 100)) %>%
  mutate(
    m_mu = exp( -k * t ),
    m = rnorm( n() , m_mu , sigma )
  ) %>%
  pivot_longer(cols = c(m_mu, m),
               names_to = "parameter") %>%
  ggplot(aes(t, value, group = n)) +
    geom_line(alpha = 0.05) +
    coord_cartesian(expand = F, clip = "off") +
    facet_wrap(~parameter, scale = "free", nrow = 1) +
    theme_minimal() +
    theme(panel.grid = element_blank())

# 5.1.2 Stan models ####
# Load models
# The macroalgal model is the same as used above.
Frontier_macro_model <- here("Stan", "Frontier_betap_het.stan") %>% 
  read_file() %>%
  write_stan_file() %>%
  cmdstan_model()

Frontier_conv_model <- here("Stan", "Frontier_conv.stan") %>% 
  read_file() %>%
  write_stan_file() %>%
  cmdstan_model()

# Run models
Frontier_macro_samples <- Frontier_macro_model$sample(
          data = data %>%
            filter(reference == "Frontier et al. 2022" &
                     t != 0) %>%
            droplevels() %>%
            select(t, m) %>%
            compose_data(),
          chains = 8,
          parallel_chains = parallel::detectCores(),
          iter_warmup = 1e3,
          iter_sampling = 1e3
        ) %T>%
  print()

Frontier_conv_samples <- Frontier_conv_model$sample(
          data = data %>%
            filter(reference == "Frontier et al. 2022" &
                     t != 0) %>%
            droplevels() %>%
            select(t, m) %>%
            compose_data(),
          chains = 8,
          parallel_chains = parallel::detectCores(),
          iter_warmup = 1e3,
          iter_sampling = 1e3
        ) %T>%
  print()

# Save draws (macroalgal model is already saved)
Frontier_conv_samples$draws() %>%
  write_rds(here("RDS", "Frontier_conv_samples.rds"))
Frontier_conv_samples$draws(format = "df") %>%
  write_rds(here("RDS", "Frontier_conv_samples_df.rds"))

# 5.1.3 Rhat and effective sample size ####
Frontier_conv_rhat_ess <- bind_rows(
  Frontier_macro_samples$summary(
    variables = c("lp__", "alpha", "mu", "tau", 
                  "epsilon", "lambda", "theta")
  ) %>%
    summarise(rhat_mean = mean(rhat),
              rhat_sd = sd(rhat),
              ess_mean = mean(ess_bulk),
              ess_sd = sd(ess_bulk)) %>%
    mutate(model = "macroalgal"),
  Frontier_conv_samples$summary(
    variables = c("lp__", "k", "sigma")
  ) %>%
    summarise(rhat_mean = mean(rhat),
              rhat_sd = sd(rhat),
              ess_mean = mean(ess_bulk),
              ess_sd = sd(ess_bulk)) %>%
    mutate(model = "conventional")
) %T>%
  print()

# 5.1.4 Chains ####
Frontier_macro_chains <- Frontier_macro_samples$draws(format = "df") %>%
  mcmc_rank_overlay(pars = c("lp__", "alpha", "mu", "tau",
                             "epsilon", "lambda", "theta")) +
  guides(colour = guide_legend(nrow = 1)) +
  labs(title = "Macroalgal model",
       y = "Frequency") +
  coord_cartesian(xlim = c(0, 8e3), 
                  ylim = c(0, 100),
                  expand = FALSE) +
  mytheme

Frontier_conv_chains <- Frontier_conv_samples$draws(format = "df") %>%
  mcmc_rank_overlay(pars = c("lp__", "k", "sigma")) +
  guides(colour = guide_legend(nrow = 1)) +
  labs(title = "Conventional model",
       y = "Frequency") +
  coord_cartesian(xlim = c(0, 8e3), 
                  ylim = c(0, 100),
                  expand = FALSE) +
  mytheme

Frontier_conv_chains <- ( Frontier_macro_chains / Frontier_conv_chains ) +
  plot_layout(heights = c(1, 1/4))

Frontier_conv_chains %>%
  ggsave(filename = "Frontier_conv_chains.pdf", path = "Plots",
         device = cairo_pdf, width = 20, height = 20, units = "cm")

# 5.1.5 Pairs ####
# Pairs plot for the macroalgal model already exists
Frontier_conv_samples$draws(format = "df") %>%
  mcmc_pairs(
    pars = c("k", "sigma"),
    grid_args = list(top = "Conventional model")
  ) %>%
  ggsave(filename = "Frontier_conv_pairs.png", path = "Plots",
         width = 10, height = 10, units = "cm", bg = "white")

# 5.1.6 Leave-one-out cross-validation ####
Frontier_conv_loo <- loo_compare(
  list(
    macroalgal = Frontier_macro_samples$loo(cores = parallel::detectCores()),
    conventional = Frontier_conv_samples$loo(cores = parallel::detectCores())
  )
) %>%
  as.data.frame() %>%
  rownames_to_column("model") %>%
  as_tibble() %T>%
  print()
# Warning: cannot trust stats because Pareto k diagnostic values are too high.
Frontier_macro_samples$loo(cores = parallel::detectCores())
# 99.5% are good, so it's fine.

# 5.1.7 Prior-posterior comparison ####
# Sample priors
Frontier_macro_prior <- prior_samples(
  model = Frontier_macro_model,
  data = data %>%
    filter(reference == "Frontier et al. 2022" &
             t != 0) %>%
    droplevels() %>%
    select(t, m) %>%
    compose_data(),
  samples = 1e3
)

Frontier_conv_prior <- prior_samples(
  model = Frontier_conv_model,
  data = data %>%
    filter(reference == "Frontier et al. 2022" &
             t != 0) %>%
    droplevels() %>%
    select(t, m) %>%
    compose_data(),
  samples = 1e3
  )

# Plot comparison
Frontier_macro_prior_posterior <- Frontier_macro_prior %>% 
  prior_posterior_draws(
    posterior_samples = Frontier_macro_samples,
    parameters = c("alpha", "mu", "tau",
                   "epsilon", "lambda", "theta"),
    format = "long"
  ) %>%
  prior_posterior_plot() +
  scale_x_continuous(
    labels = scales::label_number(style_negative = "minus")
  ) +
  labs(title = "Macroalgal model") +
  coord_cartesian(expand = FALSE) +
  mytheme +
  theme(axis.line.y = element_blank(),
        axis.ticks.y = element_blank(),
        axis.text.y = element_blank(),
        axis.title = element_blank())

Frontier_conv_prior_posterior <- Frontier_conv_prior %>% 
  prior_posterior_draws(
    posterior_samples = Frontier_conv_samples,
    parameters = c("k", "sigma"),
    format = "long"
  ) %>%
  prior_posterior_plot() +
  scale_x_continuous(
    labels = scales::label_number(style_negative = "minus")
  ) +
  labs(title = "Conventional model") +
  coord_cartesian(expand = FALSE) +
  mytheme +
  theme(axis.line.y = element_blank(),
        axis.ticks.y = element_blank(),
        axis.text.y = element_blank(),
        axis.title = element_blank())

Frontier_conv_prior_posterior <- 
  ( Frontier_macro_prior_posterior / Frontier_conv_prior_posterior ) +
  plot_layout(heights = c(1, 1/3))

Frontier_conv_prior_posterior %>%
  ggsave(filename = "Frontier_conv_prior_posterior.pdf", path = "Plots",
         device = cairo_pdf, width = 20, height = 20, units = "cm")

# Clean up
rm(
  list = ls(
    pattern = "(?:_model|_prior|_posterior|_chains|macro_samples|conv_samples)$"
  )
)
gc()

# 5.2 Vandendriessche et al. 2007 ####
# 5.2.1 Prior simulation ####
# See above for macroalgal model.
tibble(n = 1:1e3,
       k = rgamma( 1e3 , 0.1^2 / 0.05^2 , 0.1 / 0.05^2 ),
       sigma = rexp( 1e3 , 1 )) %>%
  expand_grid(t = data %>%
                filter(reference == "Vandendriessche et al. 2007") %$% 
                seq(min(t), max(t), length.out = 100)) %>%
  mutate(
    m_mu = exp( -k * t ),
    m = rnorm( n() , m_mu , sigma )
  ) %>%
  pivot_longer(cols = c(m_mu, m),
               names_to = "parameter") %>%
  ggplot(aes(t, value, group = n)) +
    geom_line(alpha = 0.05) +
    coord_cartesian(expand = F, clip = "off") +
    facet_wrap(~parameter, scale = "free", nrow = 1) +
    theme_minimal() +
    theme(panel.grid = element_blank())

# 5.2.2 Stan models ####
# Load models
# The macroalgal model is the same as used above.
Vandendriessche_macro_model <- here("Stan", "Vandendriessche_betap_het.stan") %>% 
  read_file() %>%
  write_stan_file() %>%
  cmdstan_model()

Vandendriessche_conv_model <- here("Stan", "Vandendriessche_conv.stan") %>% 
  read_file() %>%
  write_stan_file() %>%
  cmdstan_model()

# Run models
Vandendriessche_macro_samples <- Vandendriessche_macro_model$sample(
          data = data %>%
            filter(reference == "Vandendriessche et al. 2007" &
                     t != 0) %>%
            droplevels() %>%
            select(t, m, replicate) %>%
            compose_data(),
          chains = 8,
          parallel_chains = parallel::detectCores(),
          iter_warmup = 1e3,
          iter_sampling = 1e3
        ) %T>%
  print()

Vandendriessche_conv_samples <- Vandendriessche_conv_model$sample(
          data = data %>%
            filter(reference == "Vandendriessche et al. 2007" &
                     t != 0) %>%
            droplevels() %>%
            select(t, m, replicate) %>%
            compose_data(),
          chains = 8,
          parallel_chains = parallel::detectCores(),
          iter_warmup = 1e3,
          iter_sampling = 1e3
        ) %T>%
  print()

# Save draws (macroalgal one is already saved)
Vandendriessche_conv_samples$draws() %>%
  write_rds(here("RDS", "Vandendriessche_conv_samples.rds"))
Vandendriessche_conv_samples$draws(format = "df") %>%
  write_rds(here("RDS", "Vandendriessche_conv_samples_df.rds"))

# 5.2.3 Rhat and effective sample size ####
Vandendriessche_conv_rhat_ess <- bind_rows(
  Vandendriessche_macro_samples$summary(
    variables = c("lp__", "alpha", "mu", "tau",
                  "epsilon", "lambda", "theta")
  ) %>%
    summarise(rhat_mean = mean(rhat),
              rhat_sd = sd(rhat),
              ess_mean = mean(ess_bulk),
              ess_sd = sd(ess_bulk)) %>%
    mutate(model = "macroalgal"),
  Vandendriessche_conv_samples$summary(
    variables = c("lp__", "k", "sigma")
  ) %>%
    summarise(rhat_mean = mean(rhat),
              rhat_sd = sd(rhat),
              ess_mean = mean(ess_bulk),
              ess_sd = sd(ess_bulk)) %>%
    mutate(model = "conventional")
) %T>%
  print()

# 5.2.4 Chains ####
Vandendriessche_macro_chains <- Vandendriessche_macro_samples$draws(format = "df") %>%
  mcmc_rank_overlay(pars = c("lp__", "alpha[1]", "mu[1]", "tau[1]", 
                             "alpha[20]", "mu[20]", "tau[20]",
                             "alpha[48]", "mu[48]", "tau[48]",
                             "epsilon", "lambda", "theta")) +
  guides(colour = guide_legend(nrow = 1)) +
  labs(title = "Macroalgal model",
       y = "Frequency") +
  coord_cartesian(xlim = c(0, 8e3), 
                  ylim = c(0, 100),
                  expand = FALSE) +
  mytheme

Vandendriessche_conv_chains <- Vandendriessche_conv_samples$draws(format = "df") %>%
  mcmc_rank_overlay(pars = c("lp__", "k[1]", "k[5]", "k[10]", 
                             "k[15]", "k[20]", "k[25]",
                             "k[30]", "k[35]", "k[40]",
                             "k[48]", "k[50]", "sigma")) +
  guides(colour = guide_legend(nrow = 1)) +
  labs(title = "Conventional model",
       y = "Frequency") +
  coord_cartesian(xlim = c(0, 8e3), 
                  ylim = c(0, 100),
                  expand = FALSE) +
  mytheme

Vandendriessche_conv_chains <- Vandendriessche_macro_chains | Vandendriessche_conv_chains

Vandendriessche_conv_chains %>%
  ggsave(filename = "Vandendriessche_conv_chains.pdf", path = "Plots",
         device = cairo_pdf, width = 40, height = 15, units = "cm")

# 5.2.5 Pairs ####
# Pairs plot for the macroalgal model already exists
Vandendriessche_conv_samples$draws(format = "df") %>%
  mcmc_pairs(
    pars = c("k[1]", "k[5]", "k[10]", 
             "k[15]", "k[20]", "k[25]",
             "k[30]", "k[35]", "k[40]",
             "k[48]", "k[50]", "sigma"),
    grid_args = list(top = "Conventional model")
  ) %>%
  ggsave(filename = "Vandendriessche_conv_pairs.png", path = "Plots",
         width = 40, height = 40, units = "cm", bg = "white")

# 5.2.6 Leave-one-out cross-validation ####
Vandendriessche_conv_loo <- loo_compare(
  list(
    macroalgal = 
      Vandendriessche_macro_samples$loo(cores = parallel::detectCores()),
    conventional = 
      Vandendriessche_conv_samples$loo(cores = parallel::detectCores())
  )
) %>%
  as.data.frame() %>%
  rownames_to_column("model") %>%
  as_tibble() %T>%
  print()
# Warning: cannot trust stats because Pareto k diagnostic values are too high.
Vandendriessche_macro_samples$loo(cores = parallel::detectCores())
# 99.2% are good, so it's fine.

# 5.2.7 Prior-posterior comparison ####
# Sample priors
Vandendriessche_macro_prior <- prior_samples(
  model = Vandendriessche_macro_model,
  data = data %>%
    filter(reference == "Vandendriessche et al. 2007" &
             t != 0) %>%
    droplevels() %>%
    select(t, m, replicate) %>%
    compose_data(),
  samples = 1e3
)

Vandendriessche_conv_prior <- prior_samples(
  model = Vandendriessche_conv_model,
  data = data %>%
    filter(reference == "Vandendriessche et al. 2007" &
             t != 0) %>%
    droplevels() %>%
    select(t, m, replicate) %>%
    compose_data(),
  samples = 1e3
  )

# Plot comparison
Vandendriessche_macro_prior_posterior <- Vandendriessche_macro_prior %>% 
  prior_posterior_draws(
    posterior_samples = Vandendriessche_macro_samples,
    group = data %>%
      filter(reference == "Vandendriessche et al. 2007") %>%
      droplevels() %>%
      select(replicate),
    parameters = c("alpha[replicate]", "mu[replicate]", 
                   "tau[replicate]", "epsilon", "lambda", "theta"),
    format = "long"
  ) %>%
  prior_posterior_plot(group_name = "replicate", ridges = TRUE) +
  scale_x_continuous(
    labels = scales::label_number(style_negative = "minus")
  ) +
  labs(title = "Macroalgal model") +
  coord_cartesian(expand = FALSE) +
  mytheme +
  theme(axis.line.y = element_blank(),
        axis.ticks.y = element_blank(),
        axis.text.y = element_blank(),
        axis.title = element_blank())

Vandendriessche_conv_prior_posterior <- Vandendriessche_conv_prior %>% 
  prior_posterior_draws(
    posterior_samples = Vandendriessche_conv_samples,
    group = data %>%
      filter(reference == "Vandendriessche et al. 2007") %>%
      droplevels() %>%
      select(replicate),
    parameters = c("k[replicate]", "sigma"),
    format = "long"
  ) %>%
  prior_posterior_plot(group_name = "replicate", ridges = TRUE) +
  scale_x_continuous(
    labels = scales::label_number(style_negative = "minus")
  ) +
  labs(title = "Conventional model") +
  coord_cartesian(expand = FALSE) +
  mytheme +
  theme(axis.line.y = element_blank(),
        axis.ticks.y = element_blank(),
        axis.text.y = element_blank(),
        axis.title = element_blank())

Vandendriessche_conv_prior_posterior <- 
  ( Vandendriessche_macro_prior_posterior / Vandendriessche_conv_prior_posterior ) +
  plot_layout(heights = c(1, 1/3))

Vandendriessche_conv_prior_posterior %>%
  ggsave(filename = "Vandendriessche_conv_prior_posterior.pdf", path = "Plots",
         device = cairo_pdf, width = 20, height = 20, units = "cm")

# Clean up
rm(
  list = ls(
    pattern = "(?:_model|_prior|_posterior|_chains|macro_samples|conv_samples)$"
  )
)
gc()

# 5.3 Save diagnostic tables ####
conv_rhat_ess <- Frontier_conv_rhat_ess %>%
  mutate(reference = "Frontier et al. 2022") %>%
  bind_rows(
    Vandendriessche_conv_rhat_ess %>%
      mutate(reference = "Vandendriessche et al. 2007")
  ) %>%
  select(reference, model, 
         starts_with("rhat"),
         starts_with("ess")) %T>%
  print()

conv_rhat_ess %>%
  write_csv(here("Tables", "Diagnostic", "conv_rhat_ess.csv"))

conv_loo <- Frontier_conv_loo %>%
  mutate(reference = "Frontier et al. 2022") %>%
  bind_rows(
    Vandendriessche_conv_loo %>%
      mutate(reference = "Vandendriessche et al. 2007")
  ) %>%
  select(reference, model, everything()) %T>%
  print()

conv_loo %>%
  write_csv(here("Tables", "Diagnostic", "conv_loo.csv"))

# For simple averages, the conventional model may have similar
# predictive performance, but for prediction of the trajectory
# of individual replicates it fails miserably.

# Clean up
rm( list = ls( pattern = "Table|loo|ess" ) )
gc()

# 6. Examples ####
# In these examples I fit what I believe to be optimal case-specific
# version of the macroalgal model as well as the conventional one to
# a variety of data from the literature.

# 6.1 Brouwer 1996 ####
# 6.1.1 Data visualisation ####
data_mean_sd %>%
  filter(reference == "Brouwer 1996") %>%
  droplevels() %>%
  ggplot() +
  geom_pointrange(aes(t, m_mean, 
                      ymin = m_mean - m_sd,
                      ymax = m_mean + m_sd)) +
  facet_grid(~ treatment) +
  mytheme

# 6.1.2 Prior simulation ####
tibble(n = 1:1e3,
       alpha = rnorm( 1e3 , -0.01 , 0.005 ), 
       mu = rgamma( 1e3 , 150^2 / 100^2 , 150 / 100^2 ),
       tau = rgamma( 1e3 , 0.1^2 / 0.05^2 , 0.1 / 0.05^2 ),
       epsilon = rgamma( 1e3 , 4e4^2 / 2e4^2 , 4e4 / 2e4^2 ),
       lambda = rexp( 1e3 , 1 ),
       theta = rgamma( 1e3 , 500^2 / 250^2 , 500 / 250^2 )) %>%
  expand_grid(t = data_mean_sd %>%
                filter(reference == "Brouwer 1996") %$% 
                seq(min(t), max(t), length.out = 100)) %>%
  mutate(
    m_mu = exp(
      t * alpha - ( alpha + tau ) * mu / 5 * (
        log1p_exp( 5 / mu * ( t - mu ) ) - log1p_exp( -5 )
      )
    ),
    nu = theta + (epsilon - theta) * exp( -lambda * t ),
    m = rbetapr( n() , m_mu * ( 1 + nu ) , 2 + nu )
  ) %>%
  pivot_longer(cols = c(m_mu, m),
               names_to = "parameter") %>%
  ggplot(aes(t, value, group = n)) +
    geom_hline(yintercept = data_mean_sd %>%
                 filter(reference == "Brouwer 1996") %$%
                 range(m_mean)) +
    geom_line(alpha = 0.05) +
    coord_cartesian(expand = F, clip = "off") +
    facet_wrap(~parameter, scale = "free", nrow = 1) +
    theme_minimal() +
    theme(panel.grid = element_blank())

tibble(n = 1:1e3,
       k = rgamma( 1e3 , 0.1^2 / 0.05^2 , 0.1 / 0.05^2 ),
       sigma = rexp( 1e3 , 1 )) %>%
  expand_grid(t = data_mean_sd %>%
                filter(reference == "Brouwer 1996") %$% 
                seq(min(t), max(t), length.out = 100)) %>%
  mutate(
    m_mu = exp( -k * t ),
    m = rnorm( n() , m_mu , sigma )
  ) %>%
  pivot_longer(cols = c(m_mu, m),
               names_to = "parameter") %>%
  ggplot(aes(t, value, group = n)) +
    geom_hline(yintercept = data_mean_sd %>%
                 filter(reference == "Brouwer 1996") %$%
                 range(m_mean)) +
    geom_line(alpha = 0.05) +
    coord_cartesian(expand = F, clip = "off") +
    facet_wrap(~parameter, scale = "free", nrow = 1) +
    theme_minimal() +
    theme(panel.grid = element_blank())

# 6.1.3 Stan model ####
Brouwer_model <- here("Stan", "Brouwer.stan") %>% 
  read_file() %>%
  write_stan_file() %>%
  cmdstan_model()

Brouwer_k_model <- here("Stan", "Brouwer_k.stan") %>% 
  read_file() %>%
  write_stan_file() %>%
  cmdstan_model()

Brouwer_samples <- Brouwer_model$sample(
          data = data_mean_sd %>%
            filter(reference == "Brouwer 1996") %>% 
            droplevels() %>%
            select(t, m_mean, m_sd, treatment) %>%
            compose_data(),
          chains = 8,
          parallel_chains = parallel::detectCores(),
          iter_warmup = 1e4,
          iter_sampling = 1e4
        ) %T>%
  print()

Brouwer_k_samples <- Brouwer_k_model$sample(
          data = data_mean_sd %>% 
            # Not leaving out t0 because it was measured with error
            filter(reference == "Brouwer 1996") %>% 
            droplevels() %>%
            select(t, m_mean, m_sd, treatment) %>%
            compose_data(),
          chains = 8,
          parallel_chains = parallel::detectCores(),
          iter_warmup = 1e4,
          iter_sampling = 1e4
        ) %T>%
  print()

# Save draws
Brouwer_samples$draws() %>%
  write_rds(here("RDS", "Brouwer_samples.rds"))
Brouwer_samples$draws(format = "df") %>%
  write_rds(here("RDS", "Brouwer_samples_df.rds"))

Brouwer_k_samples$draws() %>%
  write_rds(here("RDS", "Brouwer_k_samples.rds"))
Brouwer_k_samples$draws(format = "df") %>%
  write_rds(here("RDS", "Brouwer_k_samples_df.rds"))

# 6.1.4 Model checks ####
# Rhat
Brouwer_samples$summary() %>%
  summarise(rhat_1.001 = mean( rhat > 1.001 ),
            rhat_mean = mean(rhat),
            rhat_sd = sd(rhat))
# 27% of rhat above 1.001. rhat = 1.00 ± 0.000639.

Brouwer_k_samples$summary() %>%
  summarise(rhat_1.001 = mean( rhat > 1.001 ),
            rhat_mean = mean(rhat),
            rhat_sd = sd(rhat))
# 96% of rhat above 1.001. rhat = 1.00 ± 0.00252.

# Chains
Brouwer_chains <- Brouwer_samples$draws(format = "df") %>%
  mcmc_rank_overlay(pars = c("lp__", "alpha[1]", "alpha[2]",
                             "mu[1]", "mu[2]", "tau", "epsilon", 
                             "lambda[1]", "lambda[2]", "theta")) +
  guides(colour = guide_legend(nrow = 1)) +
  labs(title = "Macroalgal model",
       y = "Frequency") +
  coord_cartesian(xlim = c(0, 8e4), ylim = c(0, 1e3),
                  expand = FALSE, clip = "off") +
  mytheme

Brouwer_k_chains <- Brouwer_k_samples$draws(format = "df") %>%
  mcmc_rank_overlay(pars = c("lp__", "k[1]", "k[2]", "sigma")) +
  guides(colour = guide_legend(nrow = 1)) +
  labs(title = "Conventional model",
       y = "Frequency") +
  coord_cartesian(xlim = c(0, 8e4), ylim = c(0, 1e3),
                  expand = FALSE, clip = "off") +
  mytheme

Brouwer_chains <- ( Brouwer_chains / Brouwer_k_chains ) +
  plot_layout(heights = c(1, 2/3))

Brouwer_chains %>%
  ggsave(filename = "Brouwer_chains.pdf", path = "Plots",
         device = cairo_pdf, width = 25, height = 25, units = "cm")

# Pairs
Brouwer_samples$draws(format = "df") %>%
  mcmc_pairs(
    pars = c("alpha[1]", "alpha[2]", "mu[1]", "mu[2]", "tau", 
             "epsilon", "lambda[1]", "lambda[2]", "theta"),
    grid_args = list(top = "Macroalgal model")
  ) %>%
  ggsave(filename = "Brouwer_pairs.png", path = "Plots",
         width = 40, height = 40, units = "cm", bg = "white")

Brouwer_k_samples$draws(format = "df") %>%
  mcmc_pairs(
    pars = c("k[1]", "k[2]", "sigma"),
    grid_args = list(top = "Conventional model")
  ) %>%
  ggsave(filename = "Brouwer_k_pairs.png", path = "Plots",
         width = 15, height = 15, units = "cm", bg = "white")

# 6.1.5 Prior-posterior comparison ####
Brouwer_prior <- prior_samples(
  model = Brouwer_model,
  data = data_mean_sd %>%
    filter(reference == "Brouwer 1996") %>% 
    droplevels() %>%
    select(t, m_mean, m_sd, treatment) %>%
    compose_data()
)

Brouwer_k_prior <- prior_samples(
  model = Brouwer_k_model,
  data = data_mean_sd %>%
    filter(reference == "Brouwer 1996") %>% 
    droplevels() %>%
    select(t, m_mean, m_sd, treatment) %>%
    compose_data()
)

Brouwer_prior_posterior <- Brouwer_prior %>% 
  prior_posterior_draws(
    posterior_samples = Brouwer_samples,
    group = data_mean_sd %>% 
      filter(reference == "Brouwer 1996") %>%
      droplevels() %>%
      select(treatment),
    parameters = c("alpha[treatment]", "mu[treatment]", "tau", 
                   "epsilon", "lambda[treatment]", "theta"),
    format = "long"
    ) %>%
  prior_posterior_plot(group_name = "treatment") +
  scale_x_continuous(
    labels = scales::label_number(style_negative = "minus")
  ) +
  labs(title = "Macroalgal model") +
  coord_cartesian(expand = FALSE) +
  mytheme +
  theme(axis.line.y = element_blank(),
        axis.ticks.y = element_blank(),
        axis.text.y = element_blank(),
        axis.title = element_blank())

Brouwer_k_prior_posterior <- Brouwer_k_prior %>% 
  prior_posterior_draws(
    posterior_samples = Brouwer_k_samples,
    group = data_mean_sd %>% 
      filter(reference == "Brouwer 1996") %>%
      droplevels() %>%
      select(treatment),
    parameters = c("k[treatment]", "sigma"),
    format = "long"
    ) %>%
  prior_posterior_plot(group_name = "treatment") +
  scale_x_continuous(
    labels = scales::label_number(style_negative = "minus")
  ) +
  labs(title = "Conventional model") +
  coord_cartesian(expand = FALSE) +
  mytheme +
  theme(axis.line.y = element_blank(),
        axis.ticks.y = element_blank(),
        axis.text.y = element_blank(),
        axis.title = element_blank())

Brouwer_prior_posterior <- 
  ( Brouwer_prior_posterior / Brouwer_k_prior_posterior ) +
  plot_layout(heights = c(1, 1/5))

Brouwer_prior_posterior %>%
  ggsave(filename = "Brouwer_prior_posterior.pdf", path = "Plots",
         device = cairo_pdf, width = 20, height = 20, units = "cm")

# 6.1.6 Prediction ####
# Parameter distributions
Brouwer_prior_posterior <- Brouwer_prior %>% 
  prior_posterior_draws(
    posterior_samples = Brouwer_samples,
    group = data_mean_sd %>% 
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

Brouwer_k_prior_posterior <- Brouwer_k_prior %>% 
  prior_posterior_draws(
    posterior_samples = Brouwer_k_samples,
    group = data_mean_sd %>% 
      filter(reference == "Brouwer 1996") %>%
      droplevels() %>%
      select(treatment),
    parameters = c("k[treatment]", "sigma"),
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

# Save parameter distributions
Brouwer_prior_posterior %>%
  write_rds(here("RDS", "Brouwer_prior_posterior.rds"))

Brouwer_k_prior_posterior %>%
  write_rds(here("RDS", "Brouwer_k_prior_posterior.rds"))

# Predict across predictor range
Brouwer_prediction <- Brouwer_prior_posterior %>%
  spread_continuous(data = data_mean_sd %>% 
                      filter(reference == "Brouwer 1996") %>%
                      droplevels(), 
                    predictor_name = "t",
                    group_name = "treatment",
                    length = 150) %>%
  mutate(
    m_mu = exp(
      t * alpha - ( alpha + tau ) * mu / 5 * (
        log1p_exp( 5 / mu * ( t - mu ) ) -
          log1p_exp( -5 )
      )
    ),
    k = ( alpha + tau ) / ( 1 + exp( 5 / mu * ( t - mu ) ) ) - tau,
    nu = ( epsilon - theta ) * exp( -lambda * t ) + theta,
    m = rbetapr( n() , m_mu * ( 1 + nu ) , 2 + nu )
  ) %>% # Summarise predictions
  group_by(t, treatment) %>%
  median_qi(m_mu, k, nu, m, .width = c(.5, .8, .9)) %T>%
  print()

Brouwer_k_prediction <- Brouwer_k_prior_posterior %>%
  spread_continuous(data = data_mean_sd %>% 
                      filter(reference == "Brouwer 1996") %>%
                      droplevels(), 
                    predictor_name = "t",
                    group_name = "treatment",
                    length = 150) %>%
  mutate(
    m_mu = exp( -k * t ),
    m = rnorm( n() , m_mu , sigma )
  ) %>% # Summarise predictions
  group_by(t, treatment) %>%
  median_qi(m_mu, m, .width = c(.5, .8, .9)) %T>%
  print()

# Save predictions
Brouwer_prediction %>%
  write_rds(here("RDS", "Brouwer_prediction.rds"))

Brouwer_k_prediction %>%
  write_rds(here("RDS", "Brouwer_k_prediction.rds"))

# 6.1.7 Visualisation of predictions ####
# Viusalise mean predictions
data_mean_sd %>%
  filter(reference == "Brouwer 1996") %>%
  droplevels() %>%
  ggplot() +
    geom_pointrange(aes(t, m_mean, colour = treatment,
                        ymin = m_mean - m_sd,
                        ymax = m_mean + m_sd)) +
    geom_line(data = Brouwer_prediction %>%
                filter(treatment != "Prior"),
              aes(t, m_mu, colour = treatment)) +
    geom_ribbon(data = Brouwer_prediction %>%
                  filter(treatment != "Prior"),
                aes(t, ymin = m_mu.lower, ymax = m_mu.upper, 
                    alpha = factor(.width), fill = treatment)) +
    scale_alpha_manual(values = c(0.5, 0.4, 0.3), guide = "none") +
    mytheme

data_mean_sd %>%
  filter(reference == "Brouwer 1996") %>%
  droplevels() %>%
  ggplot() +
    geom_pointrange(aes(t, m_mean, colour = treatment,
                        ymin = m_mean - m_sd,
                        ymax = m_mean + m_sd)) +
    geom_line(data = Brouwer_k_prediction %>%
                filter(treatment != "Prior"),
              aes(t, m_mu, colour = treatment)) +
    geom_ribbon(data = Brouwer_k_prediction %>%
                  filter(treatment != "Prior"),
                aes(t, ymin = m_mu.lower, ymax = m_mu.upper, 
                    alpha = factor(.width), fill = treatment)) +
    scale_alpha_manual(values = c(0.5, 0.4, 0.3), guide = "none") +
    mytheme

# Visualise predictions of new observations
data_mean_sd %>%
  filter(reference == "Brouwer 1996") %>%
  droplevels() %>%
  ggplot() +
    geom_pointrange(aes(t, m_mean, colour = treatment,
                        ymin = m_mean - m_sd,
                        ymax = m_mean + m_sd)) +
    geom_line(data = Brouwer_prediction %>%
                filter(treatment != "Prior"),
              aes(t, m, colour = treatment)) +
    geom_ribbon(data = Brouwer_prediction %>%
                  filter(treatment != "Prior"),
                aes(t, ymin = m.lower, ymax = m.upper, 
                    alpha = factor(.width), fill = treatment)) +
    scale_alpha_manual(values = c(0.5, 0.4, 0.3), guide = "none") +
    mytheme

data_mean_sd %>%
  filter(reference == "Brouwer 1996") %>%
  droplevels() %>%
  ggplot() +
    geom_pointrange(aes(t, m_mean, colour = treatment,
                        ymin = m_mean - m_sd,
                        ymax = m_mean + m_sd)) +
    geom_line(data = Brouwer_k_prediction %>%
                filter(treatment != "Prior"),
              aes(t, m, colour = treatment)) +
    geom_ribbon(data = Brouwer_k_prediction %>%
                  filter(treatment != "Prior"),
                aes(t, ymin = m.lower, ymax = m.upper, 
                    alpha = factor(.width), fill = treatment)) +
    scale_alpha_manual(values = c(0.5, 0.4, 0.3), guide = "none") +
    mytheme

# Visualise predictions of time-variant k
Brouwer_prediction %>%
  filter(treatment != "Prior") %>%
  ggplot() +
    geom_line(aes(t, k, colour = treatment)) +
    geom_ribbon(aes(t, ymin = k.lower, ymax = k.upper, 
                    alpha = factor(.width), fill = treatment)) +
    scale_alpha_manual(values = c(0.5, 0.4, 0.3), guide = "none") +
    mytheme

# Visualise predictions of time-variant nu
Brouwer_prediction %>%
  filter(treatment != "Prior") %>%
  ggplot() +
    geom_line(aes(t, nu, colour = treatment)) +
    geom_ribbon(aes(t, ymin = nu.lower, ymax = nu.upper, 
                    alpha = factor(.width), fill = treatment)) +
    scale_alpha_manual(values = c(0.5, 0.4, 0.3), guide = "none") +
    mytheme

# Clean up
rm( list = ls( pattern = "Brouwer" ) )
gc()

# 6.2 Hamersley et al. 2015 ####
# 6.2.1 Data visualisation ####
data %>%
  filter(reference == "Hamersley et al. 2015") %>%
  droplevels() %>%
  ggplot() +
    geom_point(aes(t, m), shape = 16, alpha = 0.5) +
    facet_grid(~ treatment) +
    mytheme

# This is the first case where I will use multilevel modelling
# to partially pool treatments. Fresh, senescent and detached
# are groups in a spectrum of tissue age, so predictions for
# new groups can be conceived of. Partial pooling also helps
# regularise predictions. Partial pooling is generally possible
# with most distributions in simple cases, but the normal is
# easiest and allows non-centred parameterisation. Therefore,
# positive parameters need to be modelled in log space.

# 6.2.2 Prior simulation ####
tibble(n = 1:1e3,
       alpha_mu = rnorm( 1e3 , 0 , 0.01 ), 
       log_mu_mu = rnorm( 1e3 , log(10) , 0.5 ),
       log_tau_mu = rnorm( 1e3 , log(0.2) , 0.5 ),
       alpha_sigma = rtnorm( 1e3 , 0 , 0.01 , 0 ), # half-normal prior
       log_mu_sigma = rtnorm( 1e3 , 0 , 0.5 , 0 ),
       log_tau_sigma = rtnorm( 1e3 , 0 , 0.5 , 0 ),
       alpha = rnorm( 1e3 , alpha_mu , alpha_sigma ),
       mu = rnorm( 1e3 , log_mu_mu , log_mu_sigma ) %>% exp(), # exponentiate
       tau = rnorm( 1e3 , log_tau_mu , log_tau_sigma ) %>% exp(),
       epsilon = rgamma( 1e3 , 4e4^2 / 2e4^2 , 4e4 / 2e4^2 ),
       lambda = rexp( 1e3 , 1 ),
       theta = rgamma( 1e3 , 500^2 / 250^2 , 500 / 250^2 )) %>%
  expand_grid(t = data %>%
                filter(reference == "Hamersley et al. 2015") %$% 
                seq(min(t), max(t), length.out = 100)) %>%
  mutate(
    m_mu = exp(
      t * alpha - ( alpha + tau ) * mu / 5 * (
        log1p_exp( 5 / mu * ( t - mu ) ) - log1p_exp( -5 )
      )
    ),
    nu = theta + (epsilon - theta) * exp( -lambda * t ),
    m = rbetapr( n() , m_mu * ( 1 + nu ) , 2 + nu )
  ) %>%
  pivot_longer(cols = c(m_mu, m),
               names_to = "parameter") %>%
  ggplot(aes(t, value, group = n)) +
    geom_hline(yintercept = data %>%
                 filter(reference == "Hamersley et al. 2015") %$%
                 range(m)) +
    geom_line(alpha = 0.05) +
    coord_cartesian(expand = F, clip = "off") +
    facet_wrap(~parameter, scale = "free", nrow = 1) +
    theme_minimal() +
    theme(panel.grid = element_blank())

tibble(n = 1:1e3,
       log_k_mu = rnorm( 1e3 , log(0.2) , 0.5 ),
       log_k_sigma = rtnorm( 1e3 , 0 , 0.5 , 0 ),
       k = rnorm( 1e3 , log_k_mu , log_k_sigma ) %>% exp(),
       sigma = rexp( 1e3 , 1 )) %>%
  expand_grid(t = data %>%
                filter(reference == "Hamersley et al. 2015") %$% 
                seq(min(t), max(t), length.out = 100)) %>%
  mutate(
    m_mu = exp( -k * t ),
    m = rnorm( n() , m_mu , sigma )
  ) %>%
  pivot_longer(cols = c(m_mu, m),
               names_to = "parameter") %>%
  ggplot(aes(t, value, group = n)) +
    geom_hline(yintercept = data %>%
                 filter(reference == "Hamersley et al. 2015") %$%
                 range(m)) +
    geom_line(alpha = 0.05) +
    coord_cartesian(expand = F, clip = "off") +
    facet_wrap(~parameter, scale = "free", nrow = 1) +
    theme_minimal() +
    theme(panel.grid = element_blank())

# 6.2.3 Stan model ####
Hamersley_model <- here("Stan", "Hamersley.stan") %>% 
  read_file() %>%
  write_stan_file() %>%
  cmdstan_model()

Hamersley_k_model <- here("Stan", "Hamersley_k.stan") %>% 
  read_file() %>%
  write_stan_file() %>%
  cmdstan_model()

Hamersley_samples <- Hamersley_model$sample(
          data = data %>%
            filter(reference == "Hamersley et al. 2015" & t != 0) %>%
            droplevels() %>%
            select(t, m, treatment) %>%
            compose_data(),
          chains = 8,
          parallel_chains = parallel::detectCores(),
          iter_warmup = 1e4,
          iter_sampling = 1e4
        ) %T>%
  print()

Hamersley_k_samples <- Hamersley_k_model$sample(
          data = data %>%
            filter(reference == "Hamersley et al. 2015" & t != 0) %>%
            droplevels() %>%
            select(t, m, treatment) %>%
            compose_data(),
          chains = 8,
          parallel_chains = parallel::detectCores(),
          iter_warmup = 1e4,
          iter_sampling = 1e4
        ) %T>%
  print()

# Save draws
Hamersley_samples$draws() %>%
  write_rds(here("RDS", "Hamersley_samples.rds"))
Hamersley_samples$draws(format = "df") %>%
  write_rds(here("RDS", "Hamersley_samples_df.rds"))

Hamersley_k_samples$draws() %>%
  write_rds(here("RDS", "Hamersley_k_samples.rds"))
Hamersley_k_samples$draws(format = "df") %>%
  write_rds(here("RDS", "Hamersley_k_samples_df.rds"))

# 6.2.4 Model checks ####
# Rhat
Hamersley_samples$summary() %>%
  summarise(rhat_1.001 = mean( rhat > 1.001 ),
            rhat_mean = mean(rhat),
            rhat_sd = sd(rhat))
# No of rhat above 1.001. rhat = 1.00 ± 0.000155.

Hamersley_k_samples$summary() %>%
  summarise(rhat_1.001 = mean( rhat > 1.001 ),
            rhat_mean = mean(rhat),
            rhat_sd = sd(rhat))
# No of rhat above 1.001. rhat = 1.00 ± 0.000224.

# Chains
Hamersley_chains <- Hamersley_samples$draws(format = "df") %>%
  mcmc_rank_overlay() +
  guides(colour = guide_legend(nrow = 1)) +
  labs(title = "Macroalgal model",
       y = "Frequency") +
  coord_cartesian(xlim = c(0, 8e4), ylim = c(0, 1e3),
                  expand = FALSE, clip = "off") +
  mytheme

Hamersley_k_chains <- Hamersley_k_samples$draws(format = "df") %>%
  mcmc_rank_overlay() +
  guides(colour = guide_legend(nrow = 1)) +
  labs(title = "Conventional model",
       y = "Frequency") +
  coord_cartesian(xlim = c(0, 8e4), ylim = c(0, 1e3),
                  expand = FALSE, clip = "off") +
  mytheme

Hamersley_chains <- ( Hamersley_chains / Hamersley_k_chains ) +
  plot_layout(heights = c(1, 3/5))

Hamersley_chains %>%
  ggsave(filename = "Hamersley_chains.pdf", path = "Plots",
         device = cairo_pdf, width = 40, height = 40, units = "cm")

# Pairs
Hamersley_samples$draws(format = "df") %>%
  mcmc_pairs(
    pars = c("alpha_mu", "alpha_sigma", "alpha[1]", "alpha[2]", "alpha[3]", 
             "log_mu_mu", "log_mu_sigma", "log_mu[1]", "log_mu[2]", "log_mu[3]",
             "log_tau_mu", "log_tau_sigma", "log_tau[1]", "log_tau[2]", "log_tau[3]",
             "epsilon", "lambda", "theta"),
    grid_args = list(top = "Macroalgal model")
  ) %>%
  ggsave(filename = "Hamersley_pairs.png", path = "Plots",
         width = 90, height = 90, units = "cm", bg = "white")

Hamersley_k_samples$draws(format = "df") %>%
  mcmc_pairs(
    pars = c("log_k_mu", "log_k_sigma", 
             "log_k[1]", "log_k[2]", "log_k[3]",
             "sigma"),
    grid_args = list(top = "Conventional model")
  ) %>%
  ggsave(filename = "Hamersley_k_pairs.png", path = "Plots",
         width = 30, height = 30, units = "cm", bg = "white")

# 6.2.5 Prior-posterior comparison ####
Hamersley_prior <- prior_samples(
  model = Hamersley_model,
  data = data %>%
    filter(reference == "Hamersley et al. 2015" & t != 0) %>%
    droplevels() %>%
    select(t, m, treatment) %>%
    compose_data()
)

Hamersley_k_prior <- prior_samples(
  model = Hamersley_k_model,
  data = data %>%
    filter(reference == "Hamersley et al. 2015" & t != 0) %>%
    droplevels() %>%
    select(t, m, treatment) %>%
    compose_data()
)

Hamersley_prior_posterior <- Hamersley_prior %>% 
  prior_posterior_draws(
    posterior_samples = Hamersley_samples,
    group = data %>% 
      filter(reference == "Hamersley et al. 2015") %>%
      droplevels() %>%
      select(treatment),
    parameters = c("alpha_mu", "alpha_sigma", "alpha[treatment]",
                   "log_mu_mu", "log_mu_sigma", "log_mu[treatment]",
                   "log_tau_mu", "log_tau_sigma", "log_tau[treatment]",
                   "epsilon", "lambda", "theta"),
    format = "long"
    ) %>%
  prior_posterior_plot(group_name = "treatment") +
  scale_x_continuous(
    labels = scales::label_number(style_negative = "minus")
  ) +
  labs(title = "Macroalgal model") +
  coord_cartesian(expand = FALSE) +
  mytheme +
  theme(axis.line.y = element_blank(),
        axis.ticks.y = element_blank(),
        axis.text.y = element_blank(),
        axis.title = element_blank())

Hamersley_k_prior_posterior <- Hamersley_k_prior %>% 
  prior_posterior_draws(
    posterior_samples = Hamersley_k_samples,
    group = data %>% 
      filter(reference == "Hamersley et al. 2015") %>%
      droplevels() %>%
      select(treatment),
    parameters = c("log_k_mu", "log_k_sigma", "log_k[treatment]",
                   "sigma"),
    format = "long"
    ) %>%
  prior_posterior_plot(group_name = "treatment") +
  scale_x_continuous(
    labels = scales::label_number(style_negative = "minus")
  ) +
  labs(title = "Conventional model") +
  coord_cartesian(expand = FALSE) +
  mytheme +
  theme(axis.line.y = element_blank(),
        axis.ticks.y = element_blank(),
        axis.text.y = element_blank(),
        axis.title = element_blank())

Hamersley_prior_posterior <- 
  ( Hamersley_prior_posterior / Hamersley_k_prior_posterior ) +
  plot_layout(heights = c(1, 2/5))

Hamersley_prior_posterior %>%
  ggsave(filename = "Hamersley_prior_posterior.pdf", path = "Plots",
         device = cairo_pdf, width = 30, height = 30, units = "cm")

# 6.2.6 Prediction ####
# Parameter distributions
Hamersley_prior_posterior_global <- Hamersley_prior %>% 
  prior_posterior_draws(
    posterior_samples = Hamersley_samples,
    parameters = c("alpha_mu", "alpha_sigma",
                   "log_mu_mu", "log_mu_sigma",
                   "log_tau_mu", "log_tau_sigma",
                   "epsilon", "lambda", "theta"),
    format = "short"
  ) %>% 
  mutate(
    alpha = rnorm( n() , alpha_mu , alpha_sigma ),
    mu = rnorm( n() , log_mu_mu , log_mu_sigma ) %>% exp(),
    tau = rnorm( n() , log_tau_mu , log_tau_sigma ) %>% exp()
  ) %>%
  select(!ends_with("_mu") & !ends_with("_sigma")) %T>%
  print()

Hamersley_prior_posterior_treatment <- Hamersley_prior %>% 
  prior_posterior_draws(
    posterior_samples = Hamersley_samples,
    group = data %>% 
      filter(reference == "Hamersley et al. 2015") %>%
      droplevels() %>%
      select(treatment),
    parameters = c("alpha[treatment]", "log_mu[treatment]", 
                   "log_tau[treatment]", "epsilon", "lambda", "theta"),
    format = "short"
  ) %>% 
  mutate(
    mu = exp( log_mu ),
    tau = exp( log_tau )
  ) %>%
  filter(!(treatment %in% c("Fresh", "Senescent") & 
             distribution == "prior")) %>%
  mutate(
    treatment = if_else(
      distribution == "prior", "Prior", treatment
    ) %>% fct()
  ) %>%
  select(-c(distribution, log_mu, log_tau)) %T>%
  print()

Hamersley_prior_posterior <- Hamersley_prior_posterior_treatment %>%
  bind_rows(
    Hamersley_prior_posterior_global %>%
      filter(distribution == "posterior") %>%
      select(-distribution) %>%
      mutate(treatment = "Global" %>% fct())
  ) %T>%
  print()
  
Hamersley_k_prior_posterior_global <- Hamersley_k_prior %>% 
  prior_posterior_draws(
    posterior_samples = Hamersley_k_samples,
    parameters = c("log_k_mu", "log_k_sigma", "sigma"),
    format = "short"
  ) %>% 
  mutate(
    k = rnorm( n() , log_k_mu , log_k_sigma ) %>% exp()
  ) %>%
  select(!ends_with("_mu") & !ends_with("_sigma")) %T>%
  print()

Hamersley_k_prior_posterior_treatment <- Hamersley_k_prior %>% 
  prior_posterior_draws(
    posterior_samples = Hamersley_k_samples,
    group = data %>% 
      filter(reference == "Hamersley et al. 2015") %>%
      droplevels() %>%
      select(treatment),
    parameters = c("log_k[treatment]", "sigma"),
    format = "short"
  ) %>% 
  mutate(
    k = exp( log_k )
  ) %>%
  filter(!(treatment %in% c("Fresh", "Senescent") & 
             distribution == "prior")) %>%
  mutate(
    treatment = if_else(
      distribution == "prior", "Prior", treatment
    ) %>% fct()
  ) %>%
  select(-c(distribution, log_k)) %T>%
  print()

Hamersley_k_prior_posterior <- Hamersley_k_prior_posterior_treatment %>%
  bind_rows(
    Hamersley_k_prior_posterior_global %>%
      filter(distribution == "posterior") %>%
      select(-distribution) %>%
      mutate(treatment = "Global" %>% fct())
  ) %T>%
  print()

# Save parameter distributions
Hamersley_prior_posterior %>%
  write_rds(here("RDS", "Hamersley_prior_posterior.rds"))

Hamersley_k_prior_posterior %>%
  write_rds(here("RDS", "Hamersley_k_prior_posterior.rds"))
  
# Predict across predictor range
Hamersley_prediction <- Hamersley_prior_posterior %>%
  spread_continuous(data = data %>% 
                      filter(reference == "Hamersley et al. 2015") %>%
                      droplevels(), 
                    predictor_name = "t",
                    group_name = "treatment",
                    length = 150) %>%
  mutate(
    m_mu = exp(
      t * alpha - ( alpha + tau ) * mu / 5 * (
        log1p_exp( 5 / mu * ( t - mu ) ) -
          log1p_exp( -5 )
      )
    ),
    k = ( alpha + tau ) / ( 1 + exp( 5 / mu * ( t - mu ) ) ) - tau,
    nu = ( epsilon - theta ) * exp( -lambda * t ) + theta,
    m = rbetapr( n() , m_mu * ( 1 + nu ) , 2 + nu )
  ) %>%
  group_by(t, treatment) %>%
  median_qi(m_mu, k, nu, m, .width = c(.5, .8, .9)) %T>%
  print()

Hamersley_k_prediction <- Hamersley_k_prior_posterior %>%
  spread_continuous(data = data %>% 
                      filter(reference == "Hamersley et al. 2015") %>%
                      droplevels(), 
                    predictor_name = "t",
                    group_name = "treatment",
                    length = 150) %>%
  mutate(
    m_mu = exp( -k * t ),
    m = rnorm( n() , m_mu , sigma )
  ) %>%
  group_by(t, treatment) %>%
  median_qi(m_mu, m, .width = c(.5, .8, .9)) %T>%
  print()

# Save predictions
Hamersley_prediction %>%
  write_rds(here("RDS", "Hamersley_prediction.rds"))

Hamersley_k_prediction %>%
  write_rds(here("RDS", "Hamersley_k_prediction.rds"))

# 6.2.7 Visualisation of predictions ####
# Viusalise mean predictions
data %>%
  filter(reference == "Hamersley et al. 2015" & t != 0) %>%
  droplevels() %>%
  ggplot() +
    geom_point(aes(t, m, colour = treatment), shape = 16, alpha = 0.5) +
    geom_line(data = Hamersley_prediction %>%
                filter(!treatment %in% c("Prior", "Global")),
              aes(t, m_mu, colour = treatment)) +
    geom_ribbon(data = Hamersley_prediction %>%
                  filter(!treatment %in% c("Prior", "Global")),
                aes(t, ymin = m_mu.lower, ymax = m_mu.upper, 
                    alpha = factor(.width), fill = treatment)) +
    scale_alpha_manual(values = c(0.5, 0.4, 0.3), guide = "none") +
    mytheme

data %>%
  filter(reference == "Hamersley et al. 2015" & t != 0) %>%
  droplevels() %>%
  ggplot() +
    geom_point(aes(t, m, colour = treatment), shape = 16, alpha = 0.5) +
    geom_line(data = Hamersley_k_prediction %>%
                filter(!treatment %in% c("Prior", "Global")),
              aes(t, m_mu, colour = treatment)) +
    geom_ribbon(data = Hamersley_k_prediction %>%
                  filter(!treatment %in% c("Prior", "Global")),
                aes(t, ymin = m_mu.lower, ymax = m_mu.upper, 
                    alpha = factor(.width), fill = treatment)) +
    scale_alpha_manual(values = c(0.5, 0.4, 0.3), guide = "none") +
    mytheme

# Visualise predictions of new observations
data %>%
  filter(reference == "Hamersley et al. 2015" & t != 0) %>%
  droplevels() %>%
  ggplot() +
    geom_point(aes(t, m, colour = treatment), shape = 16, alpha = 0.5) +
    geom_line(data = Hamersley_prediction %>%
                filter(!treatment %in% c("Prior", "Global")),
              aes(t, m, colour = treatment)) +
    geom_ribbon(data = Hamersley_prediction %>%
                  filter(!treatment %in% c("Prior", "Global")),
                aes(t, ymin = m.lower, ymax = m.upper, 
                    alpha = factor(.width), fill = treatment)) +
    scale_alpha_manual(values = c(0.5, 0.4, 0.3), guide = "none") +
    mytheme

data %>%
  filter(reference == "Hamersley et al. 2015" & t != 0) %>%
  droplevels() %>%
  ggplot() +
    geom_point(aes(t, m, colour = treatment), shape = 16, alpha = 0.5) +
    geom_line(data = Hamersley_k_prediction %>%
                filter(!treatment %in% c("Prior", "Global")),
              aes(t, m, colour = treatment)) +
    geom_ribbon(data = Hamersley_k_prediction %>%
                  filter(!treatment %in% c("Prior", "Global")),
                aes(t, ymin = m.lower, ymax = m.upper, 
                    alpha = factor(.width), fill = treatment)) +
    scale_alpha_manual(values = c(0.5, 0.4, 0.3), guide = "none") +
    mytheme

# Visualise predictions of time-variant k
Hamersley_prediction %>%
  filter(!treatment %in% c("Prior", "Global")) %>%
  ggplot() +
    geom_line(aes(t, k, colour = treatment)) +
    geom_ribbon(aes(t, ymin = k.lower, ymax = k.upper, 
                    alpha = factor(.width), fill = treatment)) +
    scale_alpha_manual(values = c(0.5, 0.4, 0.3), guide = "none") +
    mytheme

# Visualise predictions of time-variant nu (global)
Hamersley_prediction %>%
  filter(treatment == "Global") %>%
  ggplot() +
    geom_line(aes(t, nu)) +
    geom_ribbon(aes(t, ymin = nu.lower, ymax = nu.upper, 
                    alpha = factor(.width))) +
    scale_alpha_manual(values = c(0.5, 0.4, 0.3), guide = "none") +
    mytheme

# Clean up
rm( list = ls( pattern = "Hamersley" ) )
gc()

# 6.3 de Bettignies et al. 2020 ####
# 6.3.1 Data visualisation ####
data %>%
  filter(reference == "de Bettignies et al. 2020") %>%
  droplevels() %>%
  ggplot() +
    geom_point(aes(t, m), shape = 16, alpha = 0.5) +
    facet_grid(~ treatment) +
    mytheme

# I'll use partial pooling as in 6.2.

# 6.3.2 Prior simulation ####
# de Bettignies et al. 2020 provide k values (0.0366 and 0.0107).
# the mean of which (0.02365) can be used as a prior for tau and k.
# To avoid initial exponential decay from exceeding final exponential
# decay I will reparameterise alpha in terms of tau and delta.
tibble(n = 1:1e3,
       log_delta_mu = rnorm( 1e3 , log(0.01) , 0.2 ), # delta = alpha + tau
       log_mu_mu = rnorm( 1e3 , log(30) , 0.3 ),
       log_tau_mu = rnorm( 1e3 , log(0.02365) , 0.5 ),
       log_delta_sigma = rtnorm( 1e3 , 0 , 0.2 , 0 ), # half-normal prior
       log_mu_sigma = rtnorm( 1e3 , 0 , 0.3 , 0 ),
       log_tau_sigma = rtnorm( 1e3 , 0 , 0.5 , 0 ),
       delta = rnorm( 1e3 , log_delta_mu , log_delta_sigma ) %>% exp(), # exponentiate
       mu = rnorm( 1e3 , log_mu_mu , log_mu_sigma ) %>% exp(), 
       tau = rnorm( 1e3 , log_tau_mu , log_tau_sigma ) %>% exp(),
       alpha = delta - tau,
       epsilon = rgamma( 1e3 , 4e4^2 / 2e4^2 , 4e4 / 2e4^2 ),
       lambda = rexp( 1e3 , 1 ),
       theta = rgamma( 1e3 , 500^2 / 250^2 , 500 / 250^2 )) %>%
  expand_grid(t = data %>%
                filter(reference == "de Bettignies et al. 2020") %$% 
                seq(min(t), max(t), length.out = 100)) %>%
  mutate(
    m_mu = exp(
      t * alpha - ( alpha + tau ) * mu / 5 * (
        log1p_exp( 5 / mu * ( t - mu ) ) - log1p_exp( -5 )
      )
    ),
    nu = theta + (epsilon - theta) * exp( -lambda * t ),
    m = rbetapr( n() , m_mu * ( 1 + nu ) , 2 + nu )
  ) %>%
  pivot_longer(cols = c(m_mu, m),
               names_to = "parameter") %>%
  ggplot(aes(t, value, group = n)) +
    geom_hline(yintercept = data %>%
                 filter(reference == "de Bettignies et al. 2020") %$%
                 range(m)) +
    geom_line(alpha = 0.05) +
    coord_cartesian(expand = F, clip = "off") +
    facet_wrap(~parameter, scale = "free", nrow = 1) +
    theme_minimal() +
    theme(panel.grid = element_blank())

tibble(n = 1:1e3,
       log_k_mu = rnorm( 1e3 , log(0.02365) , 0.5 ),
       log_k_sigma = rtnorm( 1e3 , 0 , 0.5 , 0 ),
       k = rnorm( 1e3 , log_k_mu , log_k_sigma ) %>% exp(),
       sigma = rexp( 1e3 , 1 )) %>%
  expand_grid(t = data %>%
                filter(reference == "de Bettignies et al. 2020") %$% 
                seq(min(t), max(t), length.out = 100)) %>%
  mutate(
    m_mu = exp( -k * t ),
    m = rnorm( n() , m_mu , sigma )
  ) %>%
  pivot_longer(cols = c(m_mu, m),
               names_to = "parameter") %>%
  ggplot(aes(t, value, group = n)) +
    geom_hline(yintercept = data %>%
                 filter(reference == "de Bettignies et al. 2020") %$%
                 range(m)) +
    geom_line(alpha = 0.05) +
    coord_cartesian(expand = F, clip = "off") +
    facet_wrap(~parameter, scale = "free", nrow = 1) +
    theme_minimal() +
    theme(panel.grid = element_blank())

# 6.3.3 Stan model ####
Bettignies_model <- here("Stan", "Bettignies.stan") %>% 
  read_file() %>%
  write_stan_file() %>%
  cmdstan_model()

Bettignies_k_model <- here("Stan", "Bettignies_k.stan") %>% 
  read_file() %>%
  write_stan_file() %>%
  cmdstan_model()

Bettignies_samples <- Bettignies_model$sample(
          data = data %>%
            filter(reference == "de Bettignies et al. 2020" & t != 0) %>%
            droplevels() %>%
            select(t, m, treatment) %>%
            compose_data(),
          chains = 8,
          parallel_chains = parallel::detectCores(),
          iter_warmup = 1e4,
          iter_sampling = 1e4
        ) %T>%
  print()

Bettignies_k_samples <- Bettignies_k_model$sample(
          data = data %>%
            filter(reference == "de Bettignies et al. 2020" & t != 0) %>%
            droplevels() %>%
            select(t, m, treatment) %>%
            compose_data(),
          chains = 8,
          parallel_chains = parallel::detectCores(),
          iter_warmup = 1e4,
          iter_sampling = 1e4
        ) %T>%
  print()

# Save draws
Bettignies_samples$draws() %>%
  write_rds(here("RDS", "Bettignies_samples.rds"))
Bettignies_samples$draws(format = "df") %>%
  write_rds(here("RDS", "Bettignies_samples_df.rds"))

Bettignies_k_samples$draws() %>%
  write_rds(here("RDS", "Bettignies_k_samples.rds"))
Bettignies_k_samples$draws(format = "df") %>%
  write_rds(here("RDS", "Bettignies_k_samples_df.rds"))

# 6.3.4 Model checks ####
# Rhat
Bettignies_samples$summary() %>%
  summarise(rhat_1.001 = mean( rhat > 1.001 ),
            rhat_mean = mean(rhat),
            rhat_sd = sd(rhat))
# No rhat above 1.001. rhat = 1.00 ± 0.000211.

Bettignies_k_samples$summary() %>%
  summarise(rhat_1.001 = mean( rhat > 1.001 ),
            rhat_mean = mean(rhat),
            rhat_sd = sd(rhat))
# No of rhat above 1.001. rhat = 1.00 ± 0.0000981.

# Chains
Bettignies_chains <- Bettignies_samples$draws(format = "df") %>%
  mcmc_rank_overlay() +
  guides(colour = guide_legend(nrow = 1)) +
  labs(title = "Macroalgal model",
       y = "Frequency") +
  coord_cartesian(xlim = c(0, 8e4), ylim = c(0, 1e3),
                  expand = FALSE, clip = "off") +
  mytheme

Bettignies_k_chains <- Bettignies_k_samples$draws(format = "df") %>%
  mcmc_rank_overlay() +
  guides(colour = guide_legend(nrow = 1)) +
  labs(title = "Conventional model",
       y = "Frequency") +
  coord_cartesian(xlim = c(0, 8e4), ylim = c(0, 1e3),
                  expand = FALSE, clip = "off") +
  mytheme

Bettignies_chains <- ( Bettignies_chains / Bettignies_k_chains ) +
  plot_layout(heights = c(1, 3/5))

Bettignies_chains %>%
  ggsave(filename = "Bettignies_chains.pdf", path = "Plots",
         device = cairo_pdf, width = 40, height = 40, units = "cm")

# Pairs
Bettignies_samples$draws(format = "df") %>%
  mcmc_pairs(
    pars = c("log_delta_mu", "log_delta_sigma", 
             "log_delta[1]", "log_delta[2]", "log_delta[3]", 
             "log_mu_mu", "log_mu_sigma", 
             "log_mu[1]", "log_mu[2]", "log_mu[3]",
             "log_tau_mu", "log_tau_sigma", 
             "log_tau[1]", "log_tau[2]", "log_tau[3]",
             "epsilon", "lambda", "theta"),
    grid_args = list(top = "Macroalgal model")
  ) %>%
  ggsave(filename = "Bettignies_pairs.png", path = "Plots",
         width = 90, height = 90, units = "cm", bg = "white")

Bettignies_k_samples$draws(format = "df") %>%
  mcmc_pairs(
    pars = c("log_k_mu", "log_k_sigma", 
             "log_k[1]", "log_k[2]", "log_k[3]",
             "sigma"),
    grid_args = list(top = "Conventional model")
  ) %>%
  ggsave(filename = "Bettignies_k_pairs.png", path = "Plots",
         width = 30, height = 30, units = "cm", bg = "white")

# 6.3.5 Prior-posterior comparison ####
Bettignies_prior <- prior_samples(
  model = Bettignies_model,
  data = data %>%
    filter(reference == "de Bettignies et al. 2020" & t != 0) %>%
    droplevels() %>%
    select(t, m, treatment) %>%
    compose_data()
)

Bettignies_k_prior <- prior_samples(
  model = Bettignies_k_model,
  data = data %>%
    filter(reference == "de Bettignies et al. 2020" & t != 0) %>%
    droplevels() %>%
    select(t, m, treatment) %>%
    compose_data()
)

Bettignies_prior_posterior <- Bettignies_prior %>% 
  prior_posterior_draws(
    posterior_samples = Bettignies_samples,
    group = data %>% 
      filter(reference == "de Bettignies et al. 2020") %>%
      droplevels() %>%
      select(treatment),
    parameters = c("log_delta_mu", "log_delta_sigma", "log_delta[treatment]",
                   "log_mu_mu", "log_mu_sigma", "log_mu[treatment]",
                   "log_tau_mu", "log_tau_sigma", "log_tau[treatment]",
                   "epsilon", "lambda", "theta"),
    format = "long"
    ) %>%
  prior_posterior_plot(group_name = "treatment") +
  scale_x_continuous(
    labels = scales::label_number(style_negative = "minus")
  ) +
  labs(title = "Macroalgal model") +
  coord_cartesian(expand = FALSE) +
  mytheme +
  theme(axis.line.y = element_blank(),
        axis.ticks.y = element_blank(),
        axis.text.y = element_blank(),
        axis.title = element_blank())

Bettignies_k_prior_posterior <- Bettignies_k_prior %>% 
  prior_posterior_draws(
    posterior_samples = Bettignies_k_samples,
    group = data %>% 
      filter(reference == "de Bettignies et al. 2020") %>%
      droplevels() %>%
      select(treatment),
    parameters = c("log_k_mu", "log_k_sigma", "log_k[treatment]",
                   "sigma"),
    format = "long"
    ) %>%
  prior_posterior_plot(group_name = "treatment") +
  scale_x_continuous(
    labels = scales::label_number(style_negative = "minus")
  ) +
  labs(title = "Conventional model") +
  coord_cartesian(expand = FALSE) +
  mytheme +
  theme(axis.line.y = element_blank(),
        axis.ticks.y = element_blank(),
        axis.text.y = element_blank(),
        axis.title = element_blank())

Bettignies_prior_posterior <- 
  ( Bettignies_prior_posterior / Bettignies_k_prior_posterior ) +
  plot_layout(heights = c(1, 2/5))

Bettignies_prior_posterior %>%
  ggsave(filename = "Bettignies_prior_posterior.pdf", path = "Plots",
         device = cairo_pdf, width = 30, height = 30, units = "cm")

# 6.3.6 Prediction ####
# Parameter distributions
Bettignies_prior_posterior_global <- Bettignies_prior %>% 
  prior_posterior_draws(
    posterior_samples = Bettignies_samples,
    parameters = c("log_delta_mu", "log_delta_sigma",
                   "log_mu_mu", "log_mu_sigma",
                   "log_tau_mu", "log_tau_sigma",
                   "epsilon", "lambda", "theta"),
    format = "short"
  ) %>% 
  mutate(
    delta = rnorm( n() , log_delta_mu , log_delta_sigma ) %>% exp(),
    mu = rnorm( n() , log_mu_mu , log_mu_sigma ) %>% exp(),
    tau = rnorm( n() , log_tau_mu , log_tau_sigma ) %>% exp(),
    alpha = delta - tau
  ) %>%
  select(!ends_with("_mu") & !ends_with("_sigma")) %T>%
  print()

Bettignies_prior_posterior_treatment <- Bettignies_prior %>% 
  prior_posterior_draws(
    posterior_samples = Bettignies_samples,
    group = data %>% 
      filter(reference == "de Bettignies et al. 2020") %>%
      droplevels() %>%
      select(treatment),
    parameters = c("log_delta[treatment]", "log_mu[treatment]", 
                   "log_tau[treatment]", "epsilon", "lambda", "theta"),
    format = "short"
  ) %>%
  mutate(
    across(
      starts_with("log"), ~ exp(.x), .names = "{sub('^log_', '', .col)}"
    ),
    alpha = delta - tau
  ) %>%
  filter(treatment == "Fresh" & distribution == "prior" |
           distribution == "posterior") %>%
  mutate(
    treatment = if_else(
      distribution == "prior", "Prior", treatment
    ) %>% fct()
  ) %>%
  select(-c(distribution, starts_with("log"))) %T>%
  print()

Bettignies_prior_posterior <- Bettignies_prior_posterior_treatment %>%
  bind_rows(
    Bettignies_prior_posterior_global %>%
      filter(distribution == "posterior") %>%
      select(-distribution) %>%
      mutate(treatment = "Global" %>% fct())
  ) %T>%
  print()
  
Bettignies_k_prior_posterior_global <- Bettignies_k_prior %>% 
  prior_posterior_draws(
    posterior_samples = Bettignies_k_samples,
    parameters = c("log_k_mu", "log_k_sigma", "sigma"),
    format = "short"
  ) %>% 
  mutate(
    k = rnorm( n() , log_k_mu , log_k_sigma ) %>% exp()
  ) %>%
  select(!ends_with("_mu") & !ends_with("_sigma")) %T>%
  print()

Bettignies_k_prior_posterior_treatment <- Bettignies_k_prior %>% 
  prior_posterior_draws(
    posterior_samples = Bettignies_k_samples,
    group = data %>% 
      filter(reference == "de Bettignies et al. 2020") %>%
      droplevels() %>%
      select(treatment),
    parameters = c("log_k[treatment]", "sigma"),
    format = "short"
  ) %>% 
  mutate(
    k = exp( log_k )
  ) %>%
  filter(treatment == "Fresh" & distribution == "prior" |
           distribution == "posterior") %>%
  mutate(
    treatment = if_else(
      distribution == "prior", "Prior", treatment
    ) %>% fct()
  ) %>%
  select(-c(distribution, log_k)) %T>%
  print()

Bettignies_k_prior_posterior <- Bettignies_k_prior_posterior_treatment %>%
  bind_rows(
    Bettignies_k_prior_posterior_global %>%
      filter(distribution == "posterior") %>%
      select(-distribution) %>%
      mutate(treatment = "Global" %>% fct())
  ) %T>%
  print()

# Save parameter distributions
Bettignies_prior_posterior %>%
  write_rds(here("RDS", "Bettignies_prior_posterior.rds"))

Bettignies_k_prior_posterior %>%
  write_rds(here("RDS", "Bettignies_k_prior_posterior.rds"))
  
# Predict across predictor range
Bettignies_prediction <- Bettignies_prior_posterior %>%
  spread_continuous(data = data %>% 
                      filter(reference == "de Bettignies et al. 2020") %>%
                      droplevels(), 
                    predictor_name = "t",
                    group_name = "treatment",
                    length = 150) %>%
  mutate(
    m_mu = exp(
      t * alpha - ( alpha + tau ) * mu / 5 * (
        log1p_exp( 5 / mu * ( t - mu ) ) -
          log1p_exp( -5 )
      )
    ),
    k = ( alpha + tau ) / ( 1 + exp( 5 / mu * ( t - mu ) ) ) - tau,
    nu = ( epsilon - theta ) * exp( -lambda * t ) + theta,
    m = rbetapr( n() , m_mu * ( 1 + nu ) , 2 + nu )
  ) %>%
  # Underflow of m_mu to 0 causes m to be NA (only 49 cases in prior) and
  # median_qi() fails because it is memory-inefficient, so I use summarise()
  # with na.rm = T in all functions.
  group_by(t, treatment) %>%
  summarise(
    across(
      c(m_mu, k, nu, m),
      list(
        median = ~ median(.x, na.rm = T), 
        lower_0.5 = ~ qi(.x, .width = .5, na.rm = T)[1],
        upper_0.5 = ~ qi(.x, .width = .5, na.rm = T)[2],
        lower_0.8 = ~ qi(.x, .width = .8, na.rm = T)[1],
        upper_0.8 = ~ qi(.x, .width = .8, na.rm = T)[2],
        lower_0.9 = ~ qi(.x, .width = .9, na.rm = T)[1],
        upper_0.9 = ~ qi(.x, .width = .9, na.rm = T)[2]
      ),
      .names = "{.col}.{.fn}"
    )
  ) %>%
  ungroup() %>%
  rename(m_mu = m_mu.median, k = k.median, nu = nu.median, m = m.median) %>%
  pivot_longer(cols = contains("lower") | contains("upper")) %>%
  separate(col = name, into = c("name", ".width"), sep = "_(?=[^_]*$)") %>%
  pivot_wider(names_from = name, values_from = value) %T>%
  print()
# Warning can be ignored because I took care of NAs before summary:
Bettignies_prediction %>% filter(if_any(everything(), is.na))

Bettignies_k_prediction <- Bettignies_k_prior_posterior %>%
  spread_continuous(data = data %>% 
                      filter(reference == "de Bettignies et al. 2020") %>%
                      droplevels(), 
                    predictor_name = "t",
                    group_name = "treatment",
                    length = 150) %>%
  mutate(
    m_mu = exp( -k * t ),
    m = rnorm( n() , m_mu , sigma )
  ) %>%
  group_by(t, treatment) %>%
  median_qi(m_mu, m, .width = c(.5, .8, .9)) %T>%
  print()

# Save predictions
Bettignies_prediction %>%
  write_rds(here("RDS", "Bettignies_prediction.rds"))

Bettignies_k_prediction %>%
  write_rds(here("RDS", "Bettignies_k_prediction.rds"))

# 6.3.7 Visualisation of predictions ####
# Viusalise mean predictions
data %>%
  filter(reference == "de Bettignies et al. 2020" & t != 0) %>%
  droplevels() %>%
  ggplot() +
    geom_point(aes(t, m, colour = treatment), shape = 16, alpha = 0.5) +
    geom_line(data = Bettignies_prediction %>%
                filter(!treatment %in% c("Prior", "Global")),
              aes(t, m_mu, colour = treatment)) +
    geom_ribbon(data = Bettignies_prediction %>%
                  filter(!treatment %in% c("Prior", "Global")),
                aes(t, ymin = m_mu.lower, ymax = m_mu.upper, 
                    alpha = factor(.width), fill = treatment)) +
    scale_alpha_manual(values = c(0.5, 0.4, 0.3), guide = "none") +
    mytheme

data %>%
  filter(reference == "de Bettignies et al. 2020" & t != 0) %>%
  droplevels() %>%
  ggplot() +
    geom_point(aes(t, m, colour = treatment), shape = 16, alpha = 0.5) +
    geom_line(data = Bettignies_k_prediction %>%
                filter(!treatment %in% c("Prior", "Global")),
              aes(t, m_mu, colour = treatment)) +
    geom_ribbon(data = Bettignies_k_prediction %>%
                  filter(!treatment %in% c("Prior", "Global")),
                aes(t, ymin = m_mu.lower, ymax = m_mu.upper, 
                    alpha = factor(.width), fill = treatment)) +
    scale_alpha_manual(values = c(0.5, 0.4, 0.3), guide = "none") +
    mytheme

# Visualise predictions of new observations
data %>%
  filter(reference == "de Bettignies et al. 2020" & t != 0) %>%
  droplevels() %>%
  ggplot() +
    geom_point(aes(t, m, colour = treatment), shape = 16, alpha = 0.5) +
    geom_line(data = Bettignies_prediction %>%
                filter(!treatment %in% c("Prior", "Global")),
              aes(t, m, colour = treatment)) +
    geom_ribbon(data = Bettignies_prediction %>%
                  filter(!treatment %in% c("Prior", "Global")),
                aes(t, ymin = m.lower, ymax = m.upper, 
                    alpha = factor(.width), fill = treatment)) +
    scale_alpha_manual(values = c(0.5, 0.4, 0.3), guide = "none") +
    theme_minimal()

data %>%
  filter(reference == "de Bettignies et al. 2020" & t != 0) %>%
  droplevels() %>%
  ggplot() +
    geom_point(aes(t, m, colour = treatment), shape = 16, alpha = 0.5) +
    geom_line(data = Bettignies_k_prediction %>%
                filter(!treatment %in% c("Prior", "Global")),
              aes(t, m, colour = treatment)) +
    geom_ribbon(data = Bettignies_k_prediction %>%
                  filter(!treatment %in% c("Prior", "Global")),
                aes(t, ymin = m.lower, ymax = m.upper, 
                    alpha = factor(.width), fill = treatment)) +
    scale_alpha_manual(values = c(0.5, 0.4, 0.3), guide = "none") +
    theme_minimal()

# Visualise predictions of time-variant k
Bettignies_prediction %>%
  filter(!treatment %in% c("Prior", "Global")) %>%
  ggplot() +
    geom_line(aes(t, k, colour = treatment)) +
    geom_ribbon(aes(t, ymin = k.lower, ymax = k.upper, 
                    alpha = factor(.width), fill = treatment)) +
    scale_alpha_manual(values = c(0.5, 0.4, 0.3), guide = "none") +
    theme_minimal()

# Visualise predictions of time-variant nu
Bettignies_prediction %>%
  filter(treatment == "Fresh") %>%
  ggplot() +
    geom_line(aes(t, nu)) +
    geom_ribbon(aes(t, ymin = nu.lower, ymax = nu.upper, 
                    alpha = factor(.width))) +
    scale_alpha_manual(values = c(0.5, 0.4, 0.3), guide = "none") +
    theme_minimal()

# Clean up
rm( list = ls( pattern = "Bettignies" ) )
gc()

# 6.4 Bourguès et al. 1996 ####
# 6.4.1 Data visualisation ####
data %>%
  filter(reference == "Bourguès et al. 1996") %>%
  droplevels() %>%
  ggplot() +
    geom_point(aes(t, m), shape = 16, alpha = 0.5) +
    facet_grid(~ treatment) +
    theme_minimal()

# I'll use partial pooling as in 6.2 and 6.3.

# 6.4.2 Prior simulation ####
tibble(n = 1:1e3,
       log_alpha_mu = rnorm( 1e3 , log(0.005) , 0.2 ), 
       log_mu_mu = rnorm( 1e3 , log(15) , 0.5 ),
       log_tau_mu = rnorm( 1e3 , log(0.1) , 0.5 ),
       log_alpha_sigma = rtnorm( 1e3 , 0 , 0.2 , 0 ),
       log_mu_sigma = rtnorm( 1e3 , 0 , 0.5 , 0 ),
       log_tau_sigma = rtnorm( 1e3 , 0 , 0.5 , 0 ),
       alpha = rnorm( 1e3 , log_alpha_mu , log_alpha_sigma ) %>% exp(),
       mu = rnorm( 1e3 , log_mu_mu , log_mu_sigma ) %>% exp(),
       tau = rnorm( 1e3 , log_tau_mu , log_tau_sigma ) %>% exp(),
       epsilon = rgamma( 1e3 , 4e4^2 / 2e4^2 , 4e4 / 2e4^2 ),
       lambda = rexp( 1e3 , 1 ),
       theta = rgamma( 1e3 , 500^2 / 250^2 , 500 / 250^2 )) %>%
  expand_grid(t = data %>%
                filter(reference == "Bourguès et al. 1996") %$% 
                seq(min(t), max(t), length.out = 100)) %>%
  mutate(
    m_mu = exp(
      t * alpha - ( alpha + tau ) * mu / 5 * (
        log1p_exp( 5 / mu * ( t - mu ) ) - log1p_exp( -5 )
      )
    ),
    nu = theta + (epsilon - theta) * exp( -lambda * t ),
    m = rbetapr( n() , m_mu * ( 1 + nu ) , 2 + nu )
  ) %>%
  pivot_longer(cols = c(m_mu, m),
               names_to = "parameter") %>%
  ggplot(aes(t, value, group = n)) +
    geom_hline(yintercept = data %>%
                 filter(reference == "Bourguès et al. 1996") %$%
                 range(m)) +
    geom_line(alpha = 0.05) +
    coord_cartesian(expand = F, clip = "off") +
    facet_wrap(~parameter, scale = "free", nrow = 1) +
    theme_minimal() +
    theme(panel.grid = element_blank())

tibble(n = 1:1e3,
       log_k_mu = rnorm( 1e3 , log(0.1) , 0.5 ),
       log_k_sigma = rtnorm( 1e3 , 0 , 0.5 , 0 ),
       k = rnorm( 1e3 , log_k_mu , log_k_sigma ) %>% exp(),
       sigma = rexp( 1e3 , 1 )) %>%
  expand_grid(t = data %>%
                filter(reference == "Bourguès et al. 1996") %$% 
                seq(min(t), max(t), length.out = 100)) %>%
  mutate(
    m_mu = exp( -k * t ),
    m = rnorm( n() , m_mu , sigma )
  ) %>%
  pivot_longer(cols = c(m_mu, m),
               names_to = "parameter") %>%
  ggplot(aes(t, value, group = n)) +
    geom_hline(yintercept = data %>%
                 filter(reference == "Bourguès et al. 1996") %$%
                 range(m)) +
    geom_line(alpha = 0.05) +
    coord_cartesian(expand = F, clip = "off") +
    facet_wrap(~parameter, scale = "free", nrow = 1) +
    theme_minimal() +
    theme(panel.grid = element_blank())

# 6.4.3 Stan model ####
Bourguès_model <- here("Stan", "Bourguès.stan") %>% 
  read_file() %>%
  write_stan_file() %>%
  cmdstan_model()

Bourguès_k_model <- here("Stan", "Bourguès_k.stan") %>% 
  read_file() %>%
  write_stan_file() %>%
  cmdstan_model()

Bourguès_samples <- Bourguès_model$sample(
          data = data %>%
            filter(reference == "Bourguès et al. 1996" & t != 0) %>%
            droplevels() %>%
            select(t, m, treatment) %>%
            compose_data(),
          chains = 8,
          parallel_chains = parallel::detectCores(),
          iter_warmup = 1e4,
          iter_sampling = 1e4
        ) %T>%
  print()

Bourguès_k_samples <- Bourguès_k_model$sample(
          data = data %>%
            filter(reference == "Bourguès et al. 1996" & t != 0) %>%
            droplevels() %>%
            select(t, m, treatment) %>%
            compose_data(),
          chains = 8,
          parallel_chains = parallel::detectCores(),
          iter_warmup = 1e4,
          iter_sampling = 1e4
        ) %T>%
  print()

# Save draws
Bourguès_samples$draws() %>%
  write_rds(here("RDS", "Bourguès_samples.rds"))
Bourguès_samples$draws(format = "df") %>%
  write_rds(here("RDS", "Bourguès_samples_df.rds"))

Bourguès_k_samples$draws() %>%
  write_rds(here("RDS", "Bourguès_k_samples.rds"))
Bourguès_k_samples$draws(format = "df") %>%
  write_rds(here("RDS", "Bourguès_k_samples_df.rds"))

# 6.4.4 Model checks ####
# Rhat
Bourguès_samples$summary() %>%
  summarise(rhat_1.001 = mean( rhat > 1.001 ),
            rhat_mean = mean(rhat),
            rhat_sd = sd(rhat))
# No of rhat above 1.001. rhat = 1.00 ± 0.000122.

Bourguès_k_samples$summary() %>%
  summarise(rhat_1.001 = mean( rhat > 1.001 ),
            rhat_mean = mean(rhat),
            rhat_sd = sd(rhat))
# No of rhat above 1.001. rhat = 1.00 ± 0.000156.

# Chains
Bourguès_chains <- Bourguès_samples$draws(format = "df") %>%
  mcmc_rank_overlay() +
  guides(colour = guide_legend(nrow = 1)) +
  labs(title = "Macroalgal model",
       y = "Frequency") +
  coord_cartesian(xlim = c(0, 8e4), ylim = c(0, 1e3),
                  expand = FALSE, clip = "off") +
  mytheme

Bourguès_k_chains <- Bourguès_k_samples$draws(format = "df") %>%
  mcmc_rank_overlay() +
  guides(colour = guide_legend(nrow = 1)) +
  labs(title = "Conventional model",
       y = "Frequency") +
  coord_cartesian(xlim = c(0, 8e4), ylim = c(0, 1e3),
                  expand = FALSE, clip = "off") +
  mytheme

Bourguès_chains <- ( Bourguès_chains / Bourguès_k_chains ) +
  plot_layout(heights = c(1, 1/2))

Bourguès_chains %>%
  ggsave(filename = "Bourguès_chains.pdf", path = "Plots",
         device = cairo_pdf, width = 50, height = 40, units = "cm")

# Pairs
Bourguès_samples$draws(format = "df") %>%
  mcmc_pairs(
    pars = c("log_alpha_mu", "log_alpha_sigma", "log_alpha[1]", 
             "log_alpha[4]", "log_mu_mu", "log_mu_sigma", "log_mu[1]", 
             "log_mu[4]", "log_tau_mu", "log_tau_sigma", "log_tau[1]", 
             "log_tau[4]", "epsilon", "log_lambda_mu", "log_lambda_sigma", 
             "log_lambda[1]", "log_lambda[4]", "log_theta_mu", 
             "log_theta_sigma", "log_theta[1]", "log_theta[4]"),
    grid_args = list(top = "Macroalgal model")
  ) %>%
  ggsave(filename = "Bourguès_pairs.png", path = "Plots",
         width = 100, height = 100, units = "cm", bg = "white")

Bourguès_k_samples$draws(format = "df") %>%
  mcmc_pairs(
    pars = c("log_k_mu", "log_k_sigma", "log_k[1]", "log_k[2]", 
             "log_k[3]", "log_k[4]", "log_sigma_mu", "log_sigma_sigma",
             "log_sigma[1]", "log_sigma[2]", "log_sigma[3]", "log_sigma[4]"),
    grid_args = list(top = "Conventional model")
  ) %>%
  ggsave(filename = "Bourguès_k_pairs.png", path = "Plots",
         width = 60, height = 60, units = "cm", bg = "white")

# 6.4.5 Prior-posterior comparison ####
Bourguès_prior <- prior_samples(
  model = Bourguès_model,
  data = data %>%
    filter(reference == "Bourguès et al. 1996" & t != 0) %>%
    droplevels() %>%
    select(t, m, treatment) %>%
    compose_data()
)

Bourguès_k_prior <- prior_samples(
  model = Bourguès_k_model,
  data = data %>%
    filter(reference == "Bourguès et al. 1996" & t != 0) %>%
    droplevels() %>%
    select(t, m, treatment) %>%
    compose_data()
)

Bourguès_prior_posterior <- Bourguès_prior %>% 
  prior_posterior_draws(
    posterior_samples = Bourguès_samples,
    group = data %>% 
      filter(reference == "Bourguès et al. 1996") %>%
      droplevels() %>%
      select(treatment),
    parameters = c("log_alpha_mu", "log_alpha_sigma", "log_alpha[treatment]",
                   "log_mu_mu", "log_mu_sigma", "log_mu[treatment]",
                   "log_tau_mu", "log_tau_sigma", "log_tau[treatment]",
                   "epsilon", "log_lambda_mu", "log_lambda_sigma",
                   "log_lambda[treatment]", "log_theta_mu", "log_theta_sigma",
                   "log_theta[treatment]"),
    format = "long"
    ) %>%
  prior_posterior_plot(group_name = "treatment") +
  scale_x_continuous(
    labels = scales::label_number(style_negative = "minus")
  ) +
  labs(title = "Macroalgal model") +
  coord_cartesian(expand = FALSE) +
  mytheme +
  theme(axis.line.y = element_blank(),
        axis.ticks.y = element_blank(),
        axis.text.y = element_blank(),
        axis.title = element_blank())

Bourguès_k_prior_posterior <- Bourguès_k_prior %>% 
  prior_posterior_draws(
    posterior_samples = Bourguès_k_samples,
    group = data %>% 
      filter(reference == "Bourguès et al. 1996") %>%
      droplevels() %>%
      select(treatment),
    parameters = c("log_k_mu", "log_k_sigma", "log_k[treatment]",
                   "log_sigma_mu", "log_sigma_sigma", "log_sigma[treatment]"),
    format = "long"
    ) %>%
  prior_posterior_plot(group_name = "treatment") +
  scale_x_continuous(
    labels = scales::label_number(style_negative = "minus")
  ) +
  labs(title = "Conventional model") +
  coord_cartesian(expand = FALSE) +
  mytheme +
  theme(axis.line.y = element_blank(),
        axis.ticks.y = element_blank(),
        axis.text.y = element_blank(),
        axis.title = element_blank())

Bourguès_prior_posterior <- 
  ( Bourguès_prior_posterior / Bourguès_k_prior_posterior ) +
  plot_layout(heights = c(1, 2/5))

Bourguès_prior_posterior %>%
  ggsave(filename = "Bourguès_prior_posterior.pdf", path = "Plots",
         device = cairo_pdf, width = 30, height = 40, units = "cm")

# 6.4.6 Prediction ####
# Parameter distributions
Bourguès_prior_posterior_global <- Bourguès_prior %>% 
  prior_posterior_draws(
    posterior_samples = Bourguès_samples,
    parameters = c("log_alpha_mu", "log_alpha_sigma",
                   "log_mu_mu", "log_mu_sigma",
                   "log_tau_mu", "log_tau_sigma",
                   "epsilon", "log_lambda_mu", "log_lambda_sigma",
                   "log_theta_mu", "log_theta_sigma"),
    format = "short"
  ) %>% 
  mutate(
    alpha = rnorm( n() , log_alpha_mu , log_alpha_sigma ) %>% exp(),
    mu = rnorm( n() , log_mu_mu , log_mu_sigma ) %>% exp(),
    tau = rnorm( n() , log_tau_mu , log_tau_sigma ) %>% exp(),
    lambda = rnorm( n() , log_lambda_mu , log_lambda_sigma ) %>% exp(),
    theta = rnorm( n() , log_theta_mu , log_theta_sigma ) %>% exp()
  ) %>%
  select(!ends_with("_mu") & !ends_with("_sigma")) %T>%
  print()

Bourguès_prior_posterior_treatment <- Bourguès_prior %>% 
  prior_posterior_draws(
    posterior_samples = Bourguès_samples,
    group = data %>% 
      filter(reference == "Bourguès et al. 1996") %>%
      droplevels() %>%
      select(treatment),
    parameters = c("log_alpha[treatment]", "log_mu[treatment]", 
                   "log_tau[treatment]", "epsilon", 
                   "log_lambda[treatment]", "log_theta[treatment]"),
    format = "short"
  ) %>% 
  mutate(
    across(
      starts_with("log"), ~ exp(.x), 
      .names = "{sub('^log_', '', .col)}"
    )
  ) %>%
  filter(treatment == "Spring" & distribution == "prior" |
           distribution == "posterior") %>%
  mutate(
    treatment = if_else(
      distribution == "prior", "Prior", treatment
    ) %>% fct()
  ) %>%
  select(-c(distribution, starts_with("log"))) %T>%
  print()

Bourguès_prior_posterior <- Bourguès_prior_posterior_treatment %>%
  bind_rows(
    Bourguès_prior_posterior_global %>%
      filter(distribution == "posterior") %>%
      select(-distribution) %>%
      mutate(treatment = "Global" %>% fct())
  ) %T>%
  print()
  
Bourguès_k_prior_posterior_global <- Bourguès_k_prior %>% 
  prior_posterior_draws(
    posterior_samples = Bourguès_k_samples,
    parameters = c("log_k_mu", "log_k_sigma", 
                   "log_sigma_mu", "log_sigma_sigma"),
    format = "short"
  ) %>% 
  mutate(
    k = rnorm( n() , log_k_mu , log_k_sigma ) %>% exp(),
    sigma = rnorm( n() , log_sigma_mu , log_sigma_sigma ) %>% exp()
  ) %>%
  select(!ends_with("_mu") & !ends_with("_sigma")) %T>%
  print()

Bourguès_k_prior_posterior_treatment <- Bourguès_k_prior %>% 
  prior_posterior_draws(
    posterior_samples = Bourguès_k_samples,
    group = data %>% 
      filter(reference == "Bourguès et al. 1996") %>%
      droplevels() %>%
      select(treatment),
    parameters = c("log_k[treatment]", "log_sigma[treatment]"),
    format = "short"
  ) %>% 
  mutate(
    across(
      starts_with("log"), ~ exp(.x), 
      .names = "{sub('^log_', '', .col)}"
    )
  ) %>%
  filter(treatment == "Spring" & distribution == "prior" |
           distribution == "posterior") %>%
  mutate(
    treatment = if_else(
      distribution == "prior", "Prior", treatment
    ) %>% fct()
  ) %>%
  select(-c(distribution, starts_with("log"))) %T>%
  print()

Bourguès_k_prior_posterior <- Bourguès_k_prior_posterior_treatment %>%
  bind_rows(
    Bourguès_k_prior_posterior_global %>%
      filter(distribution == "posterior") %>%
      select(-distribution) %>%
      mutate(treatment = "Global" %>% fct())
  ) %T>%
  print()

# Save parameter distributions
Bourguès_prior_posterior %>%
  write_rds(here("RDS", "Bourguès_prior_posterior.rds"))

Bourguès_k_prior_posterior %>%
  write_rds(here("RDS", "Bourguès_k_prior_posterior.rds"))
  
# Predict across predictor range
Bourguès_prediction <- Bourguès_prior_posterior %>%
  spread_continuous(data = data %>% 
                      filter(reference == "Bourguès et al. 1996") %>%
                      droplevels(), 
                    predictor_name = "t",
                    group_name = "treatment",
                    length = 150) %>%
  mutate(
    m_mu = exp(
      t * alpha - ( alpha + tau ) * mu / 5 * (
        log1p_exp( 5 / mu * ( t - mu ) ) -
          log1p_exp( -5 )
      )
    ),
    k = ( alpha + tau ) / ( 1 + exp( 5 / mu * ( t - mu ) ) ) - tau,
    nu = ( epsilon - theta ) * exp( -lambda * t ) + theta,
    m = rbetapr( n() , m_mu * ( 1 + nu ) , 2 + nu )
  ) %>% # median_qi() is memory-inefficient
  group_by(t, treatment) %>%
  summarise(
    across(
      c(m_mu, k, nu, m),
      list(
        median = ~ median(.x, na.rm = T), 
        lower_0.5 = ~ qi(.x, .width = .5, na.rm = T)[1],
        upper_0.5 = ~ qi(.x, .width = .5, na.rm = T)[2],
        lower_0.8 = ~ qi(.x, .width = .8, na.rm = T)[1],
        upper_0.8 = ~ qi(.x, .width = .8, na.rm = T)[2],
        lower_0.9 = ~ qi(.x, .width = .9, na.rm = T)[1],
        upper_0.9 = ~ qi(.x, .width = .9, na.rm = T)[2]
      ),
      .names = "{.col}.{.fn}"
    )
  ) %>%
  ungroup() %>%
  rename(m_mu = m_mu.median, k = k.median, nu = nu.median, m = m.median) %>%
  pivot_longer(cols = contains("lower") | contains("upper")) %>%
  separate(col = name, into = c("name", ".width"), sep = "_(?=[^_]*$)") %>%
  pivot_wider(names_from = name, values_from = value) %T>%
  print()

Bourguès_k_prediction <- Bourguès_k_prior_posterior %>%
  spread_continuous(data = data %>% 
                      filter(reference == "Bourguès et al. 1996") %>%
                      droplevels(), 
                    predictor_name = "t",
                    group_name = "treatment",
                    length = 150) %>%
  mutate(
    m_mu = exp( -k * t ),
    m = rnorm( n() , m_mu , sigma )
  ) %>%
  group_by(t, treatment) %>%
  median_qi(m_mu, m, .width = c(.5, .8, .9)) %T>%
  print()

# Save predictions
Bourguès_prediction %>%
  write_rds(here("RDS", "Bourguès_prediction.rds"))

Bourguès_k_prediction %>%
  write_rds(here("RDS", "Bourguès_k_prediction.rds"))

# 6.4.7 Visualisation of predictions ####
# Viusalise mean predictions
data %>%
  filter(reference == "Bourguès et al. 1996" & t != 0) %>%
  droplevels() %>%
  ggplot() +
    geom_point(aes(t, m, colour = treatment), shape = 16, alpha = 0.5) +
    geom_line(data = Bourguès_prediction %>%
                filter(!treatment %in% c("Prior", "Global")),
              aes(t, m_mu, colour = treatment)) +
    geom_ribbon(data = Bourguès_prediction %>%
                  filter(!treatment %in% c("Prior", "Global")),
                aes(t, ymin = m_mu.lower, ymax = m_mu.upper, 
                    alpha = factor(.width), fill = treatment)) +
    scale_alpha_manual(values = c(0.5, 0.4, 0.3), guide = "none") +
    facet_grid(~ treatment) +
    mytheme

data %>%
  filter(reference == "Bourguès et al. 1996" & t != 0) %>%
  droplevels() %>%
  ggplot() +
    geom_point(aes(t, m, colour = treatment), shape = 16, alpha = 0.5) +
    geom_line(data = Bourguès_k_prediction %>%
                filter(!treatment %in% c("Prior", "Global")),
              aes(t, m_mu, colour = treatment)) +
    geom_ribbon(data = Bourguès_k_prediction %>%
                  filter(!treatment %in% c("Prior", "Global")),
                aes(t, ymin = m_mu.lower, ymax = m_mu.upper, 
                    alpha = factor(.width), fill = treatment)) +
    scale_alpha_manual(values = c(0.5, 0.4, 0.3), guide = "none") +
    facet_grid(~ treatment) +
    mytheme

# Visualise predictions of new observations
data %>%
  filter(reference == "Bourguès et al. 1996" & t != 0) %>%
  droplevels() %>%
  ggplot() +
    geom_point(aes(t, m, colour = treatment), shape = 16, alpha = 0.5) +
    geom_line(data = Bourguès_prediction %>%
                filter(!treatment %in% c("Prior", "Global")),
              aes(t, m, colour = treatment)) +
    geom_ribbon(data = Bourguès_prediction %>%
                  filter(!treatment %in% c("Prior", "Global")),
                aes(t, ymin = m.lower, ymax = m.upper, 
                    alpha = factor(.width), fill = treatment)) +
    scale_alpha_manual(values = c(0.5, 0.4, 0.3), guide = "none") +
    facet_grid(~ treatment) +
    mytheme

data %>%
  filter(reference == "Bourguès et al. 1996" & t != 0) %>%
  droplevels() %>%
  ggplot() +
    geom_point(aes(t, m, colour = treatment), shape = 16, alpha = 0.5) +
    geom_line(data = Bourguès_k_prediction %>%
                filter(!treatment %in% c("Prior", "Global")),
              aes(t, m, colour = treatment)) +
    geom_ribbon(data = Bourguès_k_prediction %>%
                  filter(!treatment %in% c("Prior", "Global")),
                aes(t, ymin = m.lower, ymax = m.upper, 
                    alpha = factor(.width), fill = treatment)) +
    scale_alpha_manual(values = c(0.5, 0.4, 0.3), guide = "none") +
    facet_grid(~ treatment) +
    mytheme

# Visualise predictions of time-variant k
Bourguès_prediction %>%
  filter(!treatment %in% c("Prior", "Global")) %>%
  ggplot() +
    geom_line(aes(t, k, colour = treatment)) +
    geom_ribbon(aes(t, ymin = k.lower, ymax = k.upper, 
                    alpha = factor(.width), fill = treatment)) +
    scale_alpha_manual(values = c(0.5, 0.4, 0.3), guide = "none") +
    facet_grid(~ treatment) +
    theme_minimal()

# Visualise predictions of time-variant nu (global)
Bourguès_prediction %>%
  filter(treatment == "Spring") %>%
  ggplot() +
    geom_line(aes(t, nu, colour = treatment)) +
    geom_ribbon(aes(t, ymin = nu.lower, ymax = nu.upper, 
                    alpha = factor(.width), fill = treatment)) +
    scale_alpha_manual(values = c(0.5, 0.4, 0.3), guide = "none") +
    facet_grid(~ treatment) +
    theme_minimal()

# Clean up
rm( list = ls( pattern = "Bourguès" ) )
gc()

# 6.5 Frontier et al. 2021 ####
# 6.5.1 Data visualisation ####
data %>%
  filter(reference == "Frontier et al. 2021") %>%
  droplevels() %>%
  ggplot() +
    geom_point(aes(t, m), shape = 16, alpha = 0.5) +
    geom_line(aes(t, m, group = replicate), alpha = 0.5) +
    facet_grid(treatment ~ species) +
    mytheme

# I can use partial pooling across species and treat depth as
# the continuous variable it is:
data %>%
  filter(reference == "Frontier et al. 2021" & t != 0) %>%
  droplevels() %>%
  mutate(depth = treatment %>% 
           str_extract("\\d+") %>% 
           as.numeric()) %>%
  distinct(treatment, depth)

# 6.5.2 Prior simulation ####
tibble(n = 1:1e3,
       log_alpha_mu = rnorm( 1e3 , log(0.004) , 0.2 ),
       log_mu_mu = rnorm( 1e3 , log(100) , 0.15 ), # this is log mu at depth = 0
       log_tau_mu = rnorm( 1e3 , log(0.12) , 0.2 ),
       log_beta_mu = rnorm( 1e3 , log(0.05) , 0.25 ), # depth effect on log mu
       log_alpha_sigma_s = rtnorm( 1e3 , 0 , 0.2 , 0 ), # species standard deviations
       log_mu_sigma_s = rtnorm( 1e3 , 0 , 0.15 , 0 ),
       log_tau_sigma_s = rtnorm( 1e3 , 0 , 0.2 , 0 ),
       log_alpha_sigma_r = rtnorm( 1e3 , 0 , 0.2 , 0 ), # reference standard deviations
       log_mu_sigma_r = rtnorm( 1e3 , 0 , 0.15 , 0 ), 
       log_tau_sigma_r = rtnorm( 1e3 , 0 , 0.2 , 0 ),
       log_beta_sigma = rtnorm( 1e3 , 0 , 0.25 , 0 ),
       beta = rnorm( 1e3 , log_beta_mu , log_beta_sigma ) %>% exp(),
       alpha = exp(
         rnorm( 1e3 , log_alpha_mu , log_alpha_sigma_s ) +
           rnorm( 1e3 , 0 , log_alpha_sigma_r )
       ),
       mu = exp(
         rnorm( 1e3 , log_mu_mu , log_mu_sigma_s ) +
           rnorm( 1e3 , 0 , log_mu_sigma_r ) - beta * 15 # change number for change in depth
       ),
       tau = exp(
         rnorm( 1e3 , log_tau_mu , log_tau_sigma_s ) +
           rnorm( 1e3 , 0 , log_tau_sigma_r )
       ),
       epsilon = rgamma( 1e3 , 4e4^2 / 2e4^2 , 4e4 / 2e4^2 ),
       lambda = rexp( 1e3 , 1 ),
       theta = rgamma( 1e3 , 500^2 / 250^2 , 500 / 250^2 )) %>%
  expand_grid(t = data %>%
                filter(reference == "Frontier et al. 2021") %$% 
                seq(min(t), max(t), length.out = 100)) %>%
  mutate(
    m_mu = exp(
      t * alpha - ( alpha + tau ) * mu / 5 * (
        log1p_exp( 5 / mu * ( t - mu ) ) - log1p_exp( -5 )
      )
    ),
    nu = theta + (epsilon - theta) * exp( -lambda * t ),
    m = rbetapr( n() , m_mu * ( 1 + nu ) , 2 + nu )
  ) %>%
  pivot_longer(cols = c(m_mu, m),
               names_to = "parameter") %>%
  ggplot(aes(t, value, group = n)) +
    geom_hline(yintercept = data %>%
                 filter(reference == "Frontier et al. 2021") %$%
                 range(m)) +
    geom_line(alpha = 0.05) +
    coord_cartesian(expand = F, clip = "off") +
    facet_wrap(~parameter, scale = "free", nrow = 1) +
    theme_minimal() +
    theme(panel.grid = element_blank())

tibble(n = 1:1e3,
       log_k_mu = rnorm( 1e3 , log(0.12) , 1 ), 
       log_k_sigma_s = rtnorm( 1e3 , 0 , 1 , 0 ),
       log_k_sigma_r = rtnorm( 1e3 , 0 , 1 , 0 ),
       beta_mu = rnorm( 1e3 , 0 , 0.1 ),
       beta_sigma = rtnorm( 1e3 , 0 , 0.1 , 0 ),
       beta = rnorm( 1e3 , beta_mu , beta_sigma ),
       k = exp(
         rnorm( 1e3 , log_k_mu , log_k_sigma_s ) + 
           rnorm( 1e3 , 0 , log_k_sigma_r ) + beta * 15
       ),
       sigma = rexp( 1e3 , 1 )) %>%
  expand_grid(t = data %>%
                filter(reference == "Frontier et al. 2021") %$% 
                seq(min(t), max(t), length.out = 100)) %>%
  mutate(
    m_mu = exp( -k * t ),
    m = rnorm( n() , m_mu , sigma )
  ) %>%
  pivot_longer(cols = c(m_mu, m),
               names_to = "parameter") %>%
  ggplot(aes(t, value, group = n)) +
    geom_hline(yintercept = data %>%
                 filter(reference == "Frontier et al. 2021") %$%
                 range(m)) +
    geom_line(alpha = 0.05) +
    coord_cartesian(expand = F, clip = "off") +
    facet_wrap(~parameter, scale = "free", nrow = 1) +
    theme_minimal() +
    theme(panel.grid = element_blank())
# This is not a reasonable prior given prior simulation
# and knowledge of detrital photosynthesis and since this
# is log_k at depth = 0 m. However, I want the prior
# difference between k and tau to be zero so I can assess 
# the posterior difference as an unbiased indicator of 
# k overestimating tau.

# 6.5.3 Stan model ####
Frontier2021_model <- here("Stan", "Frontier2021.stan") %>% 
  read_file() %>%
  write_stan_file() %>%
  cmdstan_model()

Frontier2021_k_model <- here("Stan", "Frontier2021_k.stan") %>% 
  read_file() %>%
  write_stan_file() %>%
  cmdstan_model()

Frontier2021_samples <- Frontier2021_model$sample(
          data = data %>%
            filter(reference == "Frontier et al. 2021" & t != 0) %>%
            droplevels() %>%
            mutate(depth = treatment %>% 
                     str_extract("\\d+") %>% 
                     as.numeric()) %>%
            select(t, m, species, 
                   replicate, depth) %>%
            compose_data(),
          chains = 8,
          parallel_chains = parallel::detectCores(),
          iter_warmup = 1e4,
          iter_sampling = 1e4
        ) %T>%
  print()

Frontier2021_k_samples <- Frontier2021_k_model$sample(
          data = data %>%
            filter(reference == "Frontier et al. 2021" & t != 0) %>%
            droplevels() %>%
            mutate(depth = treatment %>% 
                     str_extract("\\d+") %>% 
                     as.numeric()) %>%
            select(t, m, species, 
                   replicate, depth) %>%
            compose_data(),
          chains = 8,
          parallel_chains = parallel::detectCores(),
          iter_warmup = 1e4,
          iter_sampling = 1e4
        ) %T>%
  print()

# Save draws
Frontier2021_samples$draws() %>%
  write_rds(here("RDS", "Frontier2021_samples.rds"))
Frontier2021_samples$draws(format = "df") %>%
  write_rds(here("RDS", "Frontier2021_samples_df.rds"))

Frontier2021_k_samples$draws() %>%
  write_rds(here("RDS", "Frontier2021_k_samples.rds"))
Frontier2021_k_samples$draws(format = "df") %>%
  write_rds(here("RDS", "Frontier2021_k_samples_df.rds"))

# 6.5.4 Model checks ####
# Rhat
Frontier2021_samples$summary() %>%
  summarise(rhat_1.001 = mean( rhat > 1.001 ),
            rhat_mean = mean(rhat),
            rhat_sd = sd(rhat))
# 7% of rhat above 1.001. rhat = 1.00 ± 0.000356.

Frontier2021_k_samples$summary() %>%
  summarise(rhat_1.001 = mean( rhat > 1.001 ),
            rhat_mean = mean(rhat),
            rhat_sd = sd(rhat))
# No of rhat above 1.001. rhat = 1.00 ± 0.000123.

# Chains
Frontier2021_chains <- Frontier2021_samples$draws(format = "df") %>%
  mcmc_rank_overlay() +
  guides(colour = guide_legend(nrow = 1)) +
  labs(title = "Macroalgal model",
       y = "Frequency") +
  coord_cartesian(xlim = c(0, 8e4), ylim = c(0, 1e3),
                  expand = FALSE, clip = "off") +
  mytheme

Frontier2021_chains %>%
  ggsave(filename = "Frontier2021_chains.pdf", path = "Plots",
         device = cairo_pdf, width = 100, height = 60, units = "cm")

Frontier2021_k_chains <- Frontier2021_k_samples$draws(format = "df") %>%
  mcmc_rank_overlay() +
  guides(colour = guide_legend(nrow = 1)) +
  labs(title = "Conventional model",
       y = "Frequency") +
  coord_cartesian(xlim = c(0, 8e4), ylim = c(0, 1e3),
                  expand = FALSE, clip = "off") +
  mytheme

Frontier2021_k_chains %>%
  ggsave(filename = "Frontier2021_k_chains.pdf", path = "Plots",
         device = cairo_pdf, width = 60, height = 40, units = "cm")

# Pairs
Frontier2021_samples$draws(format = "df") %>%
  mcmc_pairs(
    pars = c("log_alpha_mu", "log_alpha_sigma_s", "log_alpha_s[1]", "log_alpha_s[2]",
             "log_alpha_sigma_r", "log_alpha_r[10]", "log_alpha_r[20]",
             "log_mu_mu", "log_mu_sigma_s", "log_mu_s[1]", "log_mu_s[2]",
             "log_mu_sigma_r", "log_mu_r[10]", "log_mu_r[20]",
             "log_beta_mu", "log_beta_sigma", "log_beta[1]", "log_beta[2]",
             "log_tau_mu", "log_tau_sigma_s", "log_tau_s[1]", "log_tau_s[2]",
             "log_tau_sigma_r", "log_tau_r[10]", "log_tau_r[20]",
             "epsilon", "lambda", "theta"),
    grid_args = list(top = "Macroalgal model")
  ) %>%
  ggsave(filename = "Frontier2021_pairs.png", path = "Plots",
         width = 100, height = 100, units = "cm", bg = "white")

Frontier2021_k_samples$draws(format = "df") %>%
  mcmc_pairs(
    pars = c("log_k_mu", "log_k_sigma_s", "log_k_s[1]", "log_k_s[2]",
             "log_k_sigma_r", "log_k_r[10]", "log_k_r[20]",
             "beta_mu", "beta_sigma", "beta[1]", "beta[2]",
             "sigma"),
    grid_args = list(top = "Conventional model")
  ) %>%
  ggsave(filename = "Frontier2021_k_pairs.png", path = "Plots",
         width = 55, height = 55, units = "cm", bg = "white")

# 6.5.5 Prior-posterior comparison ####
Frontier2021_prior <- prior_samples(
  model = Frontier2021_model,
  data = data %>%
    filter(reference == "Frontier et al. 2021" & t != 0) %>%
    droplevels() %>%
    mutate(depth = treatment %>% 
             str_extract("\\d+") %>% 
             as.numeric()) %>%
    select(t, m, species, 
           replicate, depth) %>%
    compose_data()
  )

Frontier2021_k_prior <- prior_samples(
  model = Frontier2021_k_model,
  data = data %>%
    filter(reference == "Frontier et al. 2021" & t != 0) %>%
    droplevels() %>%
    mutate(depth = treatment %>% 
             str_extract("\\d+") %>% 
             as.numeric()) %>%
    select(t, m, species, 
           replicate, depth) %>%
    compose_data()
)

Frontier2021_prior_posterior_species <- Frontier2021_prior %>% 
  prior_posterior_draws(
    posterior_samples = Frontier2021_samples,
    group = data %>% 
      filter(reference == "Frontier et al. 2021") %>%
      droplevels() %>%
      select(species),
    parameters = c("log_alpha_mu", "log_alpha_sigma_s", "log_alpha_s[species]",
                   "log_mu_mu", "log_mu_sigma_s", "log_mu_s[species]",
                   "log_beta_mu", "log_beta_sigma", "log_beta[species]",
                   "log_tau_mu", "log_tau_sigma_s", "log_tau_s[species]",
                   "epsilon", "lambda", "theta"),
    format = "long"
    ) %>%
  prior_posterior_plot(group_name = "species") +
  scale_x_continuous(
    labels = scales::label_number(style_negative = "minus")
  ) +
  labs(title = "Macroalgal model") +
  coord_cartesian(expand = FALSE) +
  mytheme +
  theme(axis.line.y = element_blank(),
        axis.ticks.y = element_blank(),
        axis.text.y = element_blank(),
        axis.title = element_blank())

Frontier2021_prior_posterior_replicate <- Frontier2021_prior %>% 
  prior_posterior_draws(
    posterior_samples = Frontier2021_samples,
    group = data %>% 
      filter(reference == "Frontier et al. 2021") %>%
      droplevels() %>%
      select(replicate),
    parameters = c("log_alpha_sigma_r", "log_alpha_r[replicate]",
                   "log_mu_sigma_r", "log_mu_r[replicate]",
                   "log_tau_sigma_r", "log_tau_r[replicate]"),
    format = "long"
    ) %>%
  prior_posterior_plot(group_name = "replicate", ridges = TRUE) +
  scale_x_continuous(
    labels = scales::label_number(style_negative = "minus")
  ) +
  coord_cartesian(expand = FALSE) +
  mytheme +
  theme(axis.line.y = element_blank(),
        axis.ticks.y = element_blank(),
        axis.text.y = element_blank(),
        axis.title = element_blank())

Frontier2021_k_prior_posterior_species <- Frontier2021_k_prior %>% 
  prior_posterior_draws(
    posterior_samples = Frontier2021_k_samples,
    group = data %>% 
      filter(reference == "Frontier et al. 2021") %>%
      droplevels() %>%
      select(species),
    parameters = c("log_k_mu", "log_k_sigma_s", "log_k_s[species]",
                   "beta_mu", "beta_sigma", "beta[species]", "sigma"),
    format = "long"
    ) %>%
  prior_posterior_plot(group_name = "species") +
  scale_x_continuous(
    labels = scales::label_number(style_negative = "minus")
  ) +
  labs(title = "Conventional model") +
  coord_cartesian(expand = FALSE) +
  mytheme +
  theme(axis.line.y = element_blank(),
        axis.ticks.y = element_blank(),
        axis.text.y = element_blank(),
        axis.title = element_blank())

Frontier2021_k_prior_posterior_replicate <- Frontier2021_k_prior %>% 
  prior_posterior_draws(
    posterior_samples = Frontier2021_k_samples,
    group = data %>% 
      filter(reference == "Frontier et al. 2021") %>%
      droplevels() %>%
      select(replicate),
    parameters = c("log_k_sigma_r", "log_k_r[replicate]"),
    format = "long"
    ) %>%
  prior_posterior_plot(group_name = "replicate", ridges = TRUE) +
  scale_x_continuous(
    labels = scales::label_number(style_negative = "minus")
  ) +
  coord_cartesian(expand = FALSE) +
  mytheme +
  theme(axis.line.y = element_blank(),
        axis.ticks.y = element_blank(),
        axis.text.y = element_blank(),
        axis.title = element_blank())

Frontier2021_prior_posterior <- 
  ( Frontier2021_prior_posterior_species / 
      Frontier2021_prior_posterior_replicate / 
      Frontier2021_k_prior_posterior_species / 
      Frontier2021_k_prior_posterior_replicate ) +
  plot_layout(heights = c(1, 2/5, 3/5, 1/5))

Frontier2021_prior_posterior %>%
  ggsave(filename = "Frontier2021_prior_posterior.pdf", path = "Plots",
         device = cairo_pdf, width = 40, height = 60, units = "cm")

# 6.5.6 Parameter distributions ####
# Macroalgal model treatment parameters
Frontier2021_prior_posterior_global <- Frontier2021_prior %>% 
  prior_posterior_draws(
    posterior_samples = Frontier2021_samples,
    parameters = c("log_alpha_mu", "log_alpha_sigma_s", "log_alpha_sigma_r",
                   "log_mu_mu", "log_mu_sigma_s", "log_mu_sigma_r",
                   "log_tau_mu", "log_tau_sigma_s", "log_tau_sigma_r",
                   "log_beta_mu", "log_beta_sigma", "epsilon", "lambda", "theta"),
    format = "short"
  ) %>% 
  mutate( # Calculate for new replicates from new species
    alpha = exp(
      rnorm( n() , log_alpha_mu , log_alpha_sigma_s ) +
        rnorm( n() , 0 , log_alpha_sigma_r )
    ),
    beta = exp( rnorm( n() , log_beta_mu , log_beta_sigma ) ),
    mu_0m = exp(
      rnorm( n() , log_mu_mu , log_mu_sigma_s ) +
        rnorm( n() , 0 , log_mu_sigma_r ) - beta * 0
    ),
    mu_15m = exp(
      rnorm( n() , log_mu_mu , log_mu_sigma_s ) +
        rnorm( n() , 0 , log_mu_sigma_r ) - beta * 15
    ),
    mu_30m = exp(
      rnorm( n() , log_mu_mu , log_mu_sigma_s ) +
        rnorm( n() , 0 , log_mu_sigma_r ) - beta * 30
    ),
    tau = exp(
      rnorm( n() , log_tau_mu , log_tau_sigma_s ) +
        rnorm( n() , 0 , log_tau_sigma_r )
    )
  ) %>%
  pivot_longer(cols = starts_with("mu"),
               names_to = "treatment",
               values_to = "mu",
               names_prefix = "mu_") %>%
  select(-starts_with("log")) %T>%
  print()

Frontier2021_prior_posterior_species <- Frontier2021_prior %>% 
  prior_posterior_draws(
    posterior_samples = Frontier2021_samples,
    group = data %>% 
      filter(reference == "Frontier et al. 2021") %>%
      droplevels() %>%
      select(species),
    parameters = c("log_alpha_s[species]", "log_mu_s[species]", "log_tau_s[species]", 
                   "log_beta[species]", "log_alpha_sigma_r", "log_mu_sigma_r", 
                   "log_tau_sigma_r", "epsilon", "lambda", "theta"),
    format = "short"
  ) %>% 
  mutate( # Calculate for new replicates
    alpha = exp( rnorm( n() , log_alpha_s , log_alpha_sigma_r ) ),
    beta = exp( log_beta ),
    mu_0m = exp(
      rnorm( n() , log_mu_s , log_mu_sigma_r ) - beta * 0
    ),
    mu_15m = exp(
      rnorm( n() , log_mu_s , log_mu_sigma_r ) - beta * 15
    ),
    mu_30m = exp(
      rnorm( n() , log_mu_s , log_mu_sigma_r ) - beta * 30
    ),
    tau = exp( rnorm( n() , log_tau_s , log_tau_sigma_r ) )
  ) %>%
  pivot_longer(cols = starts_with("mu"),
               names_to = "treatment",
               values_to = "mu",
               names_prefix = "mu_") %>%
  filter(species == "Laminaria hyperborea" & distribution == "prior" |
           distribution == "posterior") %>%
  mutate(
    species = if_else(
      distribution == "prior", "Prior", species
    ) %>% fct()
  ) %>%
  select(-c(distribution, starts_with("log"))) %T>%
  print()

Frontier2021_prior_posterior <- Frontier2021_prior_posterior_species %>%
  bind_rows(
    Frontier2021_prior_posterior_global %>%
      filter(distribution == "posterior") %>%
      select(-distribution) %>%
      mutate(species = "Global" %>% fct())
  ) %T>%
  print()

# Macroalgal model depth effect parameters
Frontier2021_prior_posterior_beta_global <- Frontier2021_prior %>% 
  prior_posterior_draws(
    posterior_samples = Frontier2021_samples,
    parameters = c("log_mu_mu", "log_mu_sigma_s", "log_mu_sigma_r",
                   "log_beta_mu", "log_beta_sigma"),
    format = "short"
  ) %>% 
  mutate( # Calculate for new replicates from new species
    beta = exp( rnorm( n() , log_beta_mu , log_beta_sigma ) ),
    log_mu = rnorm( n() , log_mu_mu , log_mu_sigma_s ) +
      rnorm( n() , 0 , log_mu_sigma_r )
  ) %>%
  select(starts_with("."), distribution, beta, log_mu) %T>%
  print()

Frontier2021_prior_posterior_beta_species <- Frontier2021_prior %>% 
  prior_posterior_draws(
    posterior_samples = Frontier2021_samples,
    group = data %>% 
      filter(reference == "Frontier et al. 2021") %>%
      droplevels() %>%
      select(species),
    parameters = c("log_mu_s[species]", "log_mu_sigma_r", "log_beta[species]"),
    format = "short"
  ) %>% 
  mutate( # Calculate for new replicates
    beta = exp( log_beta ),
    log_mu = rnorm( n() , log_mu_s , log_mu_sigma_r )
  ) %>%
  filter(species == "Laminaria hyperborea" & distribution == "prior" |
           distribution == "posterior") %>%
  mutate(
    species = if_else(
      distribution == "prior", "Prior", species
    ) %>% fct()
  ) %>%
  select(starts_with("."), species, beta, log_mu) %T>%
  print()

Frontier2021_prior_posterior_beta <- Frontier2021_prior_posterior_beta_species %>%
  bind_rows(
    Frontier2021_prior_posterior_beta_global %>%
      filter(distribution == "posterior") %>%
      select(-distribution) %>%
      mutate(species = "Global" %>% fct())
  ) %T>%
  print()

# Macroalgal model replicate parameters
Frontier2021_prior_posterior_replicate <- data %>% # Get pairs from data
  filter(reference == "Frontier et al. 2021") %>%
  droplevels() %>%
  mutate(depth = treatment %>% 
           str_extract("\\d+") %>% 
           as.numeric()) %>%
  distinct(species, replicate, treatment, depth) %>%
  left_join( # Join species distributions
    Frontier2021_prior %>% 
      prior_posterior_draws(
        posterior_samples = Frontier2021_samples,
        group = data %>% 
          filter(reference == "Frontier et al. 2021") %>%
          droplevels() %>%
          select(species),
        parameters = c("log_alpha_s[species]", "log_mu_s[species]", 
                       "log_tau_s[species]", "log_beta[species]"),
        format = "short"
      ),
    by = "species",
    relationship = "many-to-many"
  ) %>%
  left_join( # Join replicate distributions
    Frontier2021_prior %>% 
      prior_posterior_draws(
        posterior_samples = Frontier2021_samples,
        group = data %>% 
          filter(reference == "Frontier et al. 2021") %>%
          droplevels() %>%
          select(replicate),
        parameters = c("log_alpha_r[replicate]", 
                       "log_mu_r[replicate]", 
                       "log_tau_r[replicate]"),
        format = "short"
      ),
    by = c("replicate", ".chain", ".iteration", ".draw", "distribution"),
    relationship = "many-to-many"
  ) %>% 
  mutate( # Calculate for existing replicates
    alpha = exp( log_alpha_s + log_alpha_r ),
    beta = exp( log_beta ),
    mu = exp( log_mu_s + log_mu_r - beta * depth ),
    tau = exp( log_tau_s + log_tau_r )
  ) %>% # Pick one replicate from each depth for priors to keep
  filter(replicate %in% c("1", "6", "11") & distribution == "prior" |
           distribution == "posterior") %>%
  mutate(
    species = if_else(
      distribution == "prior", "Prior", species
    ) %>% fct(),
    replicate = if_else(
      distribution == "prior", "Prior", replicate
    ) %>% fct(),
    treatment = if_else(
      distribution == "prior", "Prior", treatment
    ) %>% fct()
  ) %>%
  select(-c(distribution, starts_with("log"))) %T>%
  print()

# Conventional model treatment parameters
Frontier2021_k_prior_posterior_global <- Frontier2021_k_prior %>% 
  prior_posterior_draws(
    posterior_samples = Frontier2021_k_samples,
    parameters = c("log_k_mu", "log_k_sigma_s", "log_k_sigma_r",
                   "beta_mu", "beta_sigma", "sigma"),
    format = "short"
  ) %>% 
  mutate( # Calculate for new replicates from new species
    beta = rnorm( n() , beta_mu , beta_sigma ),
    k_0m = exp(
      rnorm( n() , log_k_mu , log_k_sigma_s ) +
        rnorm( n() , 0 , log_k_sigma_r ) + beta * 0
    ),
    k_15m = exp(
      rnorm( n() , log_k_mu , log_k_sigma_s ) +
        rnorm( n() , 0 , log_k_sigma_r ) + beta * 15
    ),
    k_30m = exp(
      rnorm( n() , log_k_mu , log_k_sigma_s ) +
        rnorm( n() , 0 , log_k_sigma_r ) + beta * 30
    )
  ) %>%
  pivot_longer(cols = starts_with("k"),
               names_to = "treatment",
               values_to = "k",
               names_prefix = "k_") %>%
  select(-c(starts_with("log"), beta_mu, beta_sigma)) %T>%
  print()

Frontier2021_k_prior_posterior_species <- Frontier2021_k_prior %>% 
  prior_posterior_draws(
    posterior_samples = Frontier2021_k_samples,
    group = data %>% 
      filter(reference == "Frontier et al. 2021") %>%
      droplevels() %>%
      select(species),
    parameters = c("log_k_s[species]", "beta[species]", 
                   "log_k_sigma_r", "sigma"),
    format = "short"
  ) %>% 
  mutate( # Calculate for new replicates
    k_0m = exp(
      rnorm( n() , log_k_s , log_k_sigma_r ) + beta * 0
    ),
    k_15m = exp(
      rnorm( n() , log_k_s , log_k_sigma_r ) + beta * 15
    ),
    k_30m = exp(
      rnorm( n() , log_k_s , log_k_sigma_r ) + beta * 30
    )
  ) %>%
  pivot_longer(cols = starts_with("k"),
               names_to = "treatment",
               values_to = "k",
               names_prefix = "k_") %>%
  filter(species == "Laminaria hyperborea" & distribution == "prior" |
           distribution == "posterior") %>%
  mutate(
    species = if_else(
      distribution == "prior", "Prior", species
    ) %>% fct()
  ) %>%
  select(-c(distribution, starts_with("log"))) %T>%
  print()

Frontier2021_k_prior_posterior <- Frontier2021_k_prior_posterior_species %>%
  bind_rows(
    Frontier2021_k_prior_posterior_global %>%
      filter(distribution == "posterior") %>%
      select(-distribution) %>%
      mutate(species = "Global" %>% fct())
  ) %T>%
  print()

# Conventional model depth effect parameters
Frontier2021_k_prior_posterior_beta_global <- Frontier2021_k_prior %>% 
  prior_posterior_draws(
    posterior_samples = Frontier2021_k_samples,
    parameters = c("log_k_mu", "log_k_sigma_s", "log_k_sigma_r",
                   "beta_mu", "beta_sigma"),
    format = "short"
  ) %>% 
  mutate( # Calculate for new replicates from new species
    beta = rnorm( n() , beta_mu , beta_sigma ),
    log_k = rnorm( n() , log_k_mu , log_k_sigma_s ) +
      rnorm( n() , 0 , log_k_sigma_r )
  ) %>%
  select(starts_with("."), distribution, beta, log_k) %T>%
  print()

Frontier2021_k_prior_posterior_beta_species <- Frontier2021_k_prior %>% 
  prior_posterior_draws(
    posterior_samples = Frontier2021_k_samples,
    group = data %>% 
      filter(reference == "Frontier et al. 2021") %>%
      droplevels() %>%
      select(species),
    parameters = c("log_k_s[species]", "log_k_sigma_r", "beta[species]"),
    format = "short"
  ) %>% 
  mutate( # Calculate for new replicates
    log_k = rnorm( n() , log_k_s , log_k_sigma_r )
  ) %>%
  filter(species == "Laminaria hyperborea" & distribution == "prior" |
           distribution == "posterior") %>%
  mutate(
    species = if_else(
      distribution == "prior", "Prior", species
    ) %>% fct()
  ) %>%
  select(starts_with("."), species, beta, log_k) %T>%
  print()

Frontier2021_k_prior_posterior_beta <- Frontier2021_k_prior_posterior_beta_species %>%
  bind_rows(
    Frontier2021_k_prior_posterior_beta_global %>%
      filter(distribution == "posterior") %>%
      select(-distribution) %>%
      mutate(species = "Global" %>% fct())
  ) %T>%
  print()

# Conventional model replicate parameters
Frontier2021_k_prior_posterior_replicate <- data %>% # Get pairs from data
  filter(reference == "Frontier et al. 2021") %>%
  droplevels() %>%
  mutate(depth = treatment %>% 
           str_extract("\\d+") %>% 
           as.numeric()) %>%
  distinct(species, replicate, treatment, depth) %>%
  left_join( # Join species distributions
    Frontier2021_k_prior %>% 
      prior_posterior_draws(
        posterior_samples = Frontier2021_k_samples,
        group = data %>% 
          filter(reference == "Frontier et al. 2021") %>%
          droplevels() %>%
          select(species),
        parameters = c("log_k_s[species]", "beta[species]"),
        format = "short"
      ),
    by = "species",
    relationship = "many-to-many"
  ) %>%
  left_join( # Join replicate distributions
    Frontier2021_k_prior %>% 
      prior_posterior_draws(
        posterior_samples = Frontier2021_k_samples,
        group = data %>% 
          filter(reference == "Frontier et al. 2021") %>%
          droplevels() %>%
          select(replicate),
        parameters = c("log_k_r[replicate]"),
        format = "short"
      ),
    by = c("replicate", ".chain", ".iteration", ".draw", "distribution"),
    relationship = "many-to-many"
  ) %>% 
  mutate( # Calculate for existing replicates
    k = exp( log_k_s + log_k_r + beta * depth )
  ) %>% # Pick one replicate from each depth for priors to keep
  filter(replicate %in% c("1", "6", "11") & distribution == "prior" |
           distribution == "posterior") %>%
  mutate(
    species = if_else(
      distribution == "prior", "Prior", species
    ) %>% fct(),
    replicate = if_else(
      distribution == "prior", "Prior", replicate
    ) %>% fct(),
    treatment = if_else(
      distribution == "prior", "Prior", treatment
    ) %>% fct()
  ) %>%
  select(-c(distribution, starts_with("log"))) %T>%
  print()

# Save parameter distributions
Frontier2021_prior_posterior %>%
  write_rds(here("RDS", "Frontier2021_prior_posterior.rds"))
Frontier2021_prior_posterior_beta %>%
  write_rds(here("RDS", "Frontier2021_prior_posterior_beta.rds"))
Frontier2021_prior_posterior_replicate %>%
  write_rds(here("RDS", "Frontier2021_prior_posterior_replicate.rds"))

Frontier2021_k_prior_posterior %>%
  write_rds(here("RDS", "Frontier2021_k_prior_posterior.rds"))
Frontier2021_k_prior_posterior_beta %>%
  write_rds(here("RDS", "Frontier2021_k_prior_posterior_beta.rds"))
Frontier2021_k_prior_posterior_replicate %>%
  write_rds(here("RDS", "Frontier2021_k_prior_posterior_replicate.rds"))

# 6.5.7 Continuous prediction ####
# Treatment predictions
Frontier2021_prediction <- Frontier2021_prior_posterior %>%
  nest(.by = c(species, treatment), .key = "prior_posterior") %>%
  left_join(
    data %>% 
      filter(reference == "Frontier et al. 2021") %>%
      droplevels() %>%
      select(species, treatment, t) %>%
      nest(.by = c(species, treatment), .key = "t")
  ) %>%
  mutate(
    predictor = t %>% 
      map(
        ~if(is.null(.x)){
          data %>% 
            filter(reference == "Frontier et al. 2021") %$%
            seq(min(t), max(t), length.out = 150)
        } else {
          .x %$% 
            seq(min(t), max(t), length.out = 150)
        }
      ),
    prediction = map2(
      prior_posterior, predictor,
      ~.x %>% 
        slice( rep( 1:n() , each = length(.y) ) ) %>%
        mutate(
          t = rep( .y , times = nrow(.x) ),
          m_mu = exp(
            t * alpha - ( alpha + tau ) * mu / 5 * (
              log1p_exp( 5 / mu * ( t - mu ) ) -
                log1p_exp( -5 )
            )
          ),
          k = ( alpha + tau ) / ( 1 + exp( 5 / mu * ( t - mu ) ) ) - tau,
          nu = ( epsilon - theta ) * exp( -lambda * t ) + theta,
          m = rbetapr( n() , m_mu * ( 1 + nu ) , 2 + nu )
        ) %>%
        group_by(t) %>%
        median_qi(m_mu, k, nu, m, .width = c(.5, .8, .9)) %T>%
        print() # Printing helps keep track of progress
    )
  ) %>% 
  select(-c(prior_posterior, t, predictor)) %>%
  unnest(prediction) %T>%
  print()

Frontier2021_k_prediction <- Frontier2021_k_prior_posterior %>%
  nest(.by = c(species, treatment), .key = "prior_posterior") %>%
  left_join(
    data %>% 
      filter(reference == "Frontier et al. 2021") %>%
      droplevels() %>%
      select(species, treatment, t) %>%
      nest(.by = c(species, treatment), .key = "t")
  ) %>%
  mutate(
    predictor = t %>% 
      map(
        ~if(is.null(.x)){
          data %>% 
            filter(reference == "Frontier et al. 2021") %$%
            seq(min(t), max(t), length.out = 150)
        } else {
          .x %$% 
            seq(min(t), max(t), length.out = 150)
        }
      ),
    prediction = map2(
      prior_posterior, predictor,
      ~.x %>% 
        slice( rep( 1:n() , each = length(.y) ) ) %>%
        mutate(
          t = rep( .y , times = nrow(.x) ),
          m_mu = exp( -k * t ),
          m = rnorm( n() , m_mu , sigma )
        ) %>%
        group_by(t) %>%
        median_qi(m_mu, m, .width = c(.5, .8, .9)) %T>%
        print()
    )
  ) %>% 
  select(-c(prior_posterior, t, predictor)) %>%
  unnest(prediction) %T>%
  print()

# Save predictions
Frontier2021_prediction %>%
  write_rds(here("RDS", "Frontier2021_prediction.rds"))

Frontier2021_k_prediction %>%
  write_rds(here("RDS", "Frontier2021_k_prediction.rds"))

# Continuous depth predictions
Frontier2021_prediction_beta <- Frontier2021_prior_posterior_beta %>%
  spread_continuous(data = data %>%
                      filter(reference == "Frontier et al. 2021") %>%
                      droplevels() %>%
                      mutate(depth = treatment %>% # Be sure to extract depth
                               str_extract("\\d+") %>% 
                               as.numeric()),
                    predictor_name = "depth") %>%
  mutate( mu = exp( log_mu - beta * depth ) ) %>%
  group_by(depth, species) %>%
  median_qi(mu, .width = c(.5, .8, .9)) %T>%
  print()

Frontier2021_k_prediction_beta <- Frontier2021_k_prior_posterior_beta %>%
  spread_continuous(data = data %>%
                      filter(reference == "Frontier et al. 2021") %>%
                      droplevels() %>%
                      mutate(depth = treatment %>%
                               str_extract("\\d+") %>% 
                               as.numeric()),
                    predictor_name = "depth") %>%
  mutate( k = exp( log_k + beta * depth ) ) %>%
  group_by(depth, species) %>%
  median_qi(k, .width = c(.5, .8, .9)) %T>%
  print()

# Save predictions
Frontier2021_prediction_beta %>%
  write_rds(here("RDS", "Frontier2021_prediction_beta.rds"))

Frontier2021_k_prediction_beta %>%
  write_rds(here("RDS", "Frontier2021_k_prediction_beta.rds"))

# Replicate predictions
Frontier2021_prediction_replicate <- Frontier2021_prior_posterior_replicate %>%
  nest(.by = c(species, replicate, treatment, depth), .key = "prior_posterior") %>%
  left_join(
    data %>% 
      filter(reference == "Frontier et al. 2021") %>%
      droplevels() %>%
      select(species, replicate, treatment, t) %>%
      nest(.by = c(species, replicate, treatment), .key = "t")
  ) %>%
  mutate(
    predictor = t %>% 
      map(
        ~if(is.null(.x)){
          data %>% 
            filter(reference == "Frontier et al. 2021") %$%
            seq(min(t), max(t), length.out = 150)
        } else {
          .x %$% 
            seq(min(t), max(t), length.out = 150)
        }
      ),
    prediction = map2(
      prior_posterior, predictor,
      ~.x %>% 
        slice( rep( 1:n() , each = length(.y) ) ) %>%
        mutate(
          t = rep( .y , times = nrow(.x) ),
          m_mu = exp(
            t * alpha - ( alpha + tau ) * mu / 5 * (
              log1p_exp( 5 / mu * ( t - mu ) ) -
                log1p_exp( -5 )
            )
          ),
          k = ( alpha + tau ) / ( 1 + exp( 5 / mu * ( t - mu ) ) ) - tau
        ) %>%
        group_by(t) %>%
        median_qi(m_mu, k, .width = c(.5, .8, .9)) %T>%
        print() 
    )
  ) %>% 
  select(-c(prior_posterior, t, predictor)) %>%
  unnest(prediction) %T>%
  print()

Frontier2021_k_prediction_replicate <- Frontier2021_k_prior_posterior_replicate %>%
  nest(.by = c(species, replicate, treatment, depth), .key = "prior_posterior") %>%
  left_join(
    data %>% 
      filter(reference == "Frontier et al. 2021") %>%
      droplevels() %>%
      select(species, replicate, treatment, t) %>%
      nest(.by = c(species, replicate, treatment), .key = "t")
  ) %>%
  mutate(
    predictor = t %>% 
      map(
        ~if(is.null(.x)){
          data %>% 
            filter(reference == "Frontier et al. 2021") %$%
            seq(min(t), max(t), length.out = 150)
        } else {
          .x %$% 
            seq(min(t), max(t), length.out = 150)
        }
      ),
    prediction = map2(
      prior_posterior, predictor,
      ~.x %>% 
        slice( rep( 1:n() , each = length(.y) ) ) %>%
        mutate(
          t = rep( .y , times = nrow(.x) ),
          m_mu = exp( -k * t )
        ) %>%
        group_by(t) %>%
        median_qi(m_mu, .width = c(.5, .8, .9)) %T>%
        print()
    )
  ) %>% 
  select(-c(prior_posterior, t, predictor)) %>%
  unnest(prediction) %T>%
  print()

# Save predictions
Frontier2021_prediction_replicate %>%
  write_rds(here("RDS", "Frontier2021_prediction_replicate.rds"))

Frontier2021_k_prediction_replicate %>%
  write_rds(here("RDS", "Frontier2021_k_prediction_replicate.rds"))

# 6.5.8 Visualisation of predictions ####
# Viusalise mean predictions
data %>%
  filter(reference == "Frontier et al. 2021" & t != 0) %>%
  droplevels() %>%
  ggplot() +
    geom_point(aes(t, m, colour = treatment), shape = 16, alpha = 0.5) +
    geom_line(data = Frontier2021_prediction %>%
                filter(!species %in% c("Prior", "Global")),
              aes(t, m_mu, colour = treatment)) +
    geom_ribbon(data = Frontier2021_prediction %>%
                  filter(!species %in% c("Prior", "Global")),
                aes(t, ymin = m_mu.lower, ymax = m_mu.upper, 
                    alpha = factor(.width), fill = treatment)) +
    scale_alpha_manual(values = c(0.5, 0.4, 0.3), guide = "none") +
    facet_grid(treatment ~ species) +
    mytheme

data %>%
  filter(reference == "Frontier et al. 2021" & t != 0) %>%
  droplevels() %>%
  ggplot() +
    geom_point(aes(t, m, colour = treatment), shape = 16, alpha = 0.5) +
    geom_line(data = Frontier2021_k_prediction %>%
                filter(!species %in% c("Prior", "Global")),
              aes(t, m_mu, colour = treatment)) +
    geom_ribbon(data = Frontier2021_k_prediction %>%
                  filter(!species %in% c("Prior", "Global")),
                aes(t, ymin = m_mu.lower, ymax = m_mu.upper, 
                    alpha = factor(.width), fill = treatment)) +
    scale_alpha_manual(values = c(0.5, 0.4, 0.3), guide = "none") +
    facet_grid(treatment ~ species) +
    mytheme

# Visualise predictions of new observations
data %>%
  filter(reference == "Frontier et al. 2021" & t != 0) %>%
  droplevels() %>%
  ggplot() +
    geom_point(aes(t, m, colour = treatment), shape = 16, alpha = 0.5) +
    geom_line(data = Frontier2021_prediction %>%
                filter(!species %in% c("Prior", "Global")),
              aes(t, m, colour = treatment)) +
    geom_ribbon(data = Frontier2021_prediction %>%
                  filter(!species %in% c("Prior", "Global")),
                aes(t, ymin = m.lower, ymax = m.upper, 
                    alpha = factor(.width), fill = treatment)) +
    scale_alpha_manual(values = c(0.5, 0.4, 0.3), guide = "none") +
    facet_grid(treatment ~ species) +
    mytheme

data %>%
  filter(reference == "Frontier et al. 2021" & t != 0) %>%
  droplevels() %>%
  ggplot() +
    geom_point(aes(t, m, colour = treatment), shape = 16, alpha = 0.5) +
    geom_line(data = Frontier2021_k_prediction %>%
                filter(!species %in% c("Prior", "Global")),
              aes(t, m, colour = treatment)) +
    geom_ribbon(data = Frontier2021_k_prediction %>%
                  filter(!species %in% c("Prior", "Global")),
                aes(t, ymin = m.lower, ymax = m.upper, 
                    alpha = factor(.width), fill = treatment)) +
    scale_alpha_manual(values = c(0.5, 0.4, 0.3), guide = "none") +
    facet_grid(treatment ~ species) +
    mytheme

# Visualise predictions of replicates
data %>%
  filter(reference == "Frontier et al. 2021" & t != 0) %>%
  droplevels() %>%
  ggplot() +
    geom_point(aes(t, m, colour = treatment), shape = 16, alpha = 0.5) +
    geom_line(data = Frontier2021_prediction_replicate %>%
                filter(!species %in% c("Prior", "Global")),
              aes(t, m_mu, colour = treatment, group = replicate),
              alpha = 0.5) +
    # geom_ribbon(data = Frontier2021_prediction_replicate %>%
    #               filter(!species %in% c("Prior", "Global")),
    #             aes(t, ymin = m_mu.lower, ymax = m_mu.upper, 
    #                 alpha = factor(.width), fill = treatment, 
    #                 group = interaction(.width, replicate))) +
    # scale_alpha_manual(values = c(0.5, 0.4, 0.3), guide = "none") +
    facet_grid(treatment ~ species) +
    mytheme

data %>%
  filter(reference == "Frontier et al. 2021" & t != 0) %>%
  droplevels() %>%
  ggplot() +
    geom_point(aes(t, m, colour = treatment), shape = 16, alpha = 0.5) +
    geom_line(data = Frontier2021_k_prediction_replicate %>%
                filter(!species %in% c("Prior", "Global")),
              aes(t, m_mu, colour = treatment, group = replicate),
              alpha = 0.5) +
    facet_grid(treatment ~ species) +
    mytheme

# Visualise predictions by depth
Frontier2021_prediction_beta %>%
  filter(!species %in% c("Prior", "Global")) %>%
  ggplot() +
    geom_line(aes(depth, mu),
              alpha = 0.5) +
    geom_ribbon(aes(depth, ymin = .lower, ymax = .upper,
                    alpha = factor(.width))) +
    scale_alpha_manual(values = c(0.5, 0.4, 0.3), guide = "none") +
    facet_grid(~ species) +
    mytheme

Frontier2021_k_prediction_beta %>%
  filter(!species %in% c("Prior", "Global")) %>%
  ggplot() +
    geom_line(aes(depth, k),
              alpha = 0.5) +
    geom_ribbon(aes(depth, ymin = .lower, ymax = .upper,
                    alpha = factor(.width))) +
    scale_alpha_manual(values = c(0.5, 0.4, 0.3), guide = "none") +
    facet_grid(~ species) +
    mytheme

# Visualise predictions of time-variant k
Frontier2021_prediction %>%
  filter(!species %in% c("Prior", "Global")) %>%
  ggplot() +
    geom_line(aes(t, k, colour = treatment)) +
    geom_ribbon(aes(t, ymin = k.lower, ymax = k.upper, 
                    alpha = factor(.width), fill = treatment)) +
    scale_alpha_manual(values = c(0.5, 0.4, 0.3), guide = "none") +
    facet_grid(treatment ~ species) +
    mytheme

Frontier2021_prediction_replicate %>%
  filter(!species %in% c("Prior", "Global")) %>%
  ggplot() +
    geom_line(aes(t, k, colour = treatment, group = replicate)) +
    facet_grid(treatment ~ species) +
    mytheme

# Visualise predictions of time-variant nu (global)
Frontier2021_prediction %>%
  filter(species == "Global") %>%
  ggplot() +
    geom_line(aes(t, nu)) +
    geom_ribbon(aes(t, ymin = nu.lower, ymax = nu.upper, 
                    alpha = factor(.width))) +
    scale_alpha_manual(values = c(0.5, 0.4, 0.3), guide = "none") +
    mytheme

# Clean up
rm( list = ls( pattern = "Frontier" ) )
gc()

# 6.6 Frontier et al. 2022 ####
# 6.6.1 Data visualisation ####
data %>%
  filter(reference == "Frontier et al. 2022") %>%
  droplevels() %>%
  ggplot() +
    geom_point(aes(t, m), shape = 16, alpha = 0.5) +
    geom_line(aes(t, m, group = replicate), alpha = 0.5) +
    facet_grid(treatment ~ species) +
    mytheme

# Same as with 6.5

# 6.6.2 Prior simulation ####
tibble(n = 1:1e3,
       log_alpha_mu = rnorm( 1e3 , log(0.004) , 0.2 ),
       log_mu_mu = rnorm( 1e3 , log(100) , 0.05 ), # this is log mu at depth = 0
       log_tau_mu = rnorm( 1e3 , log(0.12) , 0.2 ),
       log_beta_mu = rnorm( 1e3 , log(0.25) , 0.2 ), # depth effect on log mu
       log_alpha_sigma_s = rtnorm( 1e3 , 0 , 0.2 , 0 ), # species standard deviations
       log_mu_sigma_s = rtnorm( 1e3 , 0 , 0.05 , 0 ),
       log_tau_sigma_s = rtnorm( 1e3 , 0 , 0.2 , 0 ),
       log_alpha_sigma_r = rtnorm( 1e3 , 0 , 0.2 , 0 ), # reference standard deviations
       log_mu_sigma_r = rtnorm( 1e3 , 0 , 0.05 , 0 ), 
       log_tau_sigma_r = rtnorm( 1e3 , 0 , 0.2 , 0 ),
       log_beta_sigma = rtnorm( 1e3 , 0 , 0.2 , 0 ),
       beta = rnorm( 1e3 , log_beta_mu , log_beta_sigma ) %>% exp(),
       alpha = exp(
         rnorm( 1e3 , log_alpha_mu , log_alpha_sigma_s ) +
           rnorm( 1e3 , 0 , log_alpha_sigma_r )
       ),
       mu = exp(
         rnorm( 1e3 , log_mu_mu , log_mu_sigma_s ) +
           rnorm( 1e3 , 0 , log_mu_sigma_r ) - beta * 3 # change number for change in depth
       ),
       tau = exp(
         rnorm( 1e3 , log_tau_mu , log_tau_sigma_s ) +
           rnorm( 1e3 , 0 , log_tau_sigma_r )
       ),
       epsilon = rgamma( 1e3 , 4e4^2 / 2e4^2 , 4e4 / 2e4^2 ),
       lambda = rexp( 1e3 , 1 ),
       theta = rgamma( 1e3 , 500^2 / 250^2 , 500 / 250^2 )) %>%
  expand_grid(t = data %>%
                filter(reference == "Frontier et al. 2022") %$% 
                seq(min(t), max(t), length.out = 100)) %>%
  mutate(
    m_mu = exp(
      t * alpha - ( alpha + tau ) * mu / 5 * (
        log1p_exp( 5 / mu * ( t - mu ) ) - log1p_exp( -5 )
      )
    ),
    nu = theta + (epsilon - theta) * exp( -lambda * t ),
    m = rbetapr( n() , m_mu * ( 1 + nu ) , 2 + nu )
  ) %>%
  pivot_longer(cols = c(m_mu, m),
               names_to = "parameter") %>%
  ggplot(aes(t, value, group = n)) +
    geom_hline(yintercept = data %>%
                 filter(reference == "Frontier et al. 2022") %$%
                 range(m)) +
    geom_line(alpha = 0.05) +
    coord_cartesian(expand = F, clip = "off") +
    facet_wrap(~parameter, scale = "free", nrow = 1) +
    theme_minimal() +
    theme(panel.grid = element_blank())

tibble(n = 1:1e3,
       log_k_mu = rnorm( 1e3 , log(0.12) , 1 ), # same rationale as before
       log_k_sigma_s = rtnorm( 1e3 , 0 , 1 , 0 ),
       log_k_sigma_r = rtnorm( 1e3 , 0 , 1 , 0 ),
       beta_mu = rnorm( 1e3 , 0 , 0.3 ),
       beta_sigma = rtnorm( 1e3 , 0 , 0.3 , 0 ),
       beta = rnorm( 1e3 , beta_mu , beta_sigma ),
       k = exp(
         rnorm( 1e3 , log_k_mu , log_k_sigma_s ) + 
           rnorm( 1e3 , 0 , log_k_sigma_r ) + beta * 3
       ),
       sigma = rexp( 1e3 , 1 )) %>%
  expand_grid(t = data %>%
                filter(reference == "Frontier et al. 2022") %$% 
                seq(min(t), max(t), length.out = 100)) %>%
  mutate(
    m_mu = exp( -k * t ),
    m = rnorm( n() , m_mu , sigma )
  ) %>%
  pivot_longer(cols = c(m_mu, m),
               names_to = "parameter") %>%
  ggplot(aes(t, value, group = n)) +
    geom_hline(yintercept = data %>%
                 filter(reference == "Frontier et al. 2022") %$%
                 range(m)) +
    geom_line(alpha = 0.05) +
    coord_cartesian(expand = F, clip = "off") +
    facet_wrap(~parameter, scale = "free", nrow = 1) +
    theme_minimal() +
    theme(panel.grid = element_blank())

# 6.6.3 Stan model ####
Frontier2022_model <- here("Stan", "Frontier2022.stan") %>% 
  read_file() %>%
  write_stan_file() %>%
  cmdstan_model()

Frontier2022_k_model <- here("Stan", "Frontier2022_k.stan") %>% 
  read_file() %>%
  write_stan_file() %>%
  cmdstan_model()

Frontier2022_samples <- Frontier2022_model$sample(
          data = data %>%
            filter(reference == "Frontier et al. 2022" & t != 0) %>%
            droplevels() %>%
            mutate(depth = treatment %>% 
                     str_extract("[\\d.]+") %>% 
                     as.numeric()) %>%
            select(t, m, species, 
                   replicate, depth) %>%
            compose_data(),
          chains = 8,
          parallel_chains = parallel::detectCores(),
          iter_warmup = 1e4,
          iter_sampling = 1e4
        ) %T>%
  print()

Frontier2022_k_samples <- Frontier2022_k_model$sample(
          data = data %>%
            filter(reference == "Frontier et al. 2022" & t != 0) %>%
            droplevels() %>%
            mutate(depth = treatment %>% 
                     str_extract("[\\d.]+") %>% 
                     as.numeric()) %>%
            select(t, m, species, 
                   replicate, depth) %>%
            compose_data(),
          chains = 8,
          parallel_chains = parallel::detectCores(),
          iter_warmup = 1e4,
          iter_sampling = 1e4
        ) %T>%
  print()

# Save draws
Frontier2022_samples$draws() %>%
  write_rds(here("RDS", "Frontier2022_samples.rds"))
Frontier2022_samples$draws(format = "df") %>%
  write_rds(here("RDS", "Frontier2022_samples_df.rds"))

Frontier2022_k_samples$draws() %>%
  write_rds(here("RDS", "Frontier2022_k_samples.rds"))
Frontier2022_k_samples$draws(format = "df") %>%
  write_rds(here("RDS", "Frontier2022_k_samples_df.rds"))

# 6.6.4 Model checks ####
# Rhat
Frontier2022_samples$summary() %>%
  summarise(rhat_1.001 = mean( rhat > 1.001 ),
            rhat_mean = mean(rhat),
            rhat_sd = sd(rhat))
# 0.4% of rhat above 1.001. rhat = 1.00 ± 0.000186.

Frontier2022_k_samples$summary() %>%
  summarise(rhat_1.001 = mean( rhat > 1.001 ),
            rhat_mean = mean(rhat),
            rhat_sd = sd(rhat))
# No of rhat above 1.001. rhat = 1.00 ± 0.0000692.

# Chains
Frontier2022_chains <- Frontier2022_samples$draws(format = "df") %>%
  mcmc_rank_overlay() +
  guides(colour = guide_legend(nrow = 1)) +
  labs(title = "Macroalgal model",
       y = "Frequency") +
  coord_cartesian(xlim = c(0, 8e4), ylim = c(0, 1e3),
                  expand = FALSE, clip = "off") +
  mytheme

Frontier2022_chains %>%
  ggsave(filename = "Frontier2022_chains.pdf", path = "Plots",
         device = cairo_pdf, width = 100, height = 60, units = "cm")

Frontier2022_k_chains <- Frontier2022_k_samples$draws(format = "df") %>%
  mcmc_rank_overlay() +
  guides(colour = guide_legend(nrow = 1)) +
  labs(title = "Conventional model",
       y = "Frequency") +
  coord_cartesian(xlim = c(0, 8e4), ylim = c(0, 1e3),
                  expand = FALSE, clip = "off") +
  mytheme

Frontier2022_k_chains %>%
  ggsave(filename = "Frontier2022_k_chains.pdf", path = "Plots",
         device = cairo_pdf, width = 60, height = 40, units = "cm")

# Pairs
Frontier2022_samples$draws(format = "df") %>%
  mcmc_pairs(
    pars = c("log_alpha_mu", "log_alpha_sigma_s", "log_alpha_s[1]", "log_alpha_s[2]",
             "log_alpha_sigma_r", "log_alpha_r[10]", "log_alpha_r[20]",
             "log_mu_mu", "log_mu_sigma_s", "log_mu_s[1]", "log_mu_s[2]",
             "log_mu_sigma_r", "log_mu_r[10]", "log_mu_r[20]",
             "log_beta_mu", "log_beta_sigma", "log_beta[1]", "log_beta[2]",
             "log_tau_mu", "log_tau_sigma_s", "log_tau_s[1]", "log_tau_s[2]",
             "log_tau_sigma_r", "log_tau_r[10]", "log_tau_r[20]",
             "epsilon", "lambda", "theta"),
    grid_args = list(top = "Macroalgal model")
  ) %>%
  ggsave(filename = "Frontier2022_pairs.png", path = "Plots",
         width = 100, height = 100, units = "cm", bg = "white")

Frontier2022_k_samples$draws(format = "df") %>%
  mcmc_pairs(
    pars = c("log_k_mu", "log_k_sigma_s", "log_k_s[1]", "log_k_s[2]",
             "log_k_sigma_r", "log_k_r[10]", "log_k_r[20]",
             "beta_mu", "beta_sigma", "beta[1]", "beta[2]",
             "sigma"),
    grid_args = list(top = "Conventional model")
  ) %>%
  ggsave(filename = "Frontier2022_k_pairs.png", path = "Plots",
         width = 55, height = 55, units = "cm", bg = "white")

# 6.6.5 Prior-posterior comparison ####
Frontier2022_prior <- prior_samples(
  model = Frontier2022_model,
  data = data %>%
    filter(reference == "Frontier et al. 2022" & t != 0) %>%
    droplevels() %>%
    mutate(depth = treatment %>% 
             str_extract("[\\d.]+") %>% 
             as.numeric()) %>%
    select(t, m, species, 
           replicate, depth) %>%
    compose_data()
)

Frontier2022_k_prior <- prior_samples(
  model = Frontier2022_k_model,
  data = data %>%
    filter(reference == "Frontier et al. 2022" & t != 0) %>%
    droplevels() %>%
    mutate(depth = treatment %>% 
             str_extract("[\\d.]+") %>% 
             as.numeric()) %>%
    select(t, m, species, 
           replicate, depth) %>%
    compose_data()
)

Frontier2022_prior_posterior_species <- Frontier2022_prior %>% 
  prior_posterior_draws(
    posterior_samples = Frontier2022_samples,
    group = data %>% 
      filter(reference == "Frontier et al. 2022") %>%
      droplevels() %>%
      select(species),
    parameters = c("log_alpha_mu", "log_alpha_sigma_s", "log_alpha_s[species]",
                   "log_mu_mu", "log_mu_sigma_s", "log_mu_s[species]",
                   "log_beta_mu", "log_beta_sigma", "log_beta[species]",
                   "log_tau_mu", "log_tau_sigma_s", "log_tau_s[species]",
                   "epsilon", "lambda", "theta"),
    format = "long"
    ) %>%
  prior_posterior_plot(group_name = "species") +
  scale_x_continuous(
    labels = scales::label_number(style_negative = "minus")
  ) +
  labs(title = "Macroalgal model") +
  coord_cartesian(expand = FALSE) +
  mytheme +
  theme(axis.line.y = element_blank(),
        axis.ticks.y = element_blank(),
        axis.text.y = element_blank(),
        axis.title = element_blank())

Frontier2022_prior_posterior_replicate <- Frontier2022_prior %>% 
  prior_posterior_draws(
    posterior_samples = Frontier2022_samples,
    group = data %>% 
      filter(reference == "Frontier et al. 2022") %>%
      droplevels() %>%
      select(replicate),
    parameters = c("log_alpha_sigma_r", "log_alpha_r[replicate]",
                   "log_mu_sigma_r", "log_mu_r[replicate]",
                   "log_tau_sigma_r", "log_tau_r[replicate]"),
    format = "long"
    ) %>%
  prior_posterior_plot(group_name = "replicate", ridges = TRUE) +
  scale_x_continuous(
    labels = scales::label_number(style_negative = "minus")
  ) +
  coord_cartesian(expand = FALSE) +
  mytheme +
  theme(axis.line.y = element_blank(),
        axis.ticks.y = element_blank(),
        axis.text.y = element_blank(),
        axis.title = element_blank())

Frontier2022_k_prior_posterior_species <- Frontier2022_k_prior %>% 
  prior_posterior_draws(
    posterior_samples = Frontier2022_k_samples,
    group = data %>% 
      filter(reference == "Frontier et al. 2022") %>%
      droplevels() %>%
      select(species),
    parameters = c("log_k_mu", "log_k_sigma_s", "log_k_s[species]",
                   "beta_mu", "beta_sigma", "beta[species]", "sigma"),
    format = "long"
    ) %>%
  prior_posterior_plot(group_name = "species") +
  scale_x_continuous(
    labels = scales::label_number(style_negative = "minus")
  ) +
  labs(title = "Conventional model") +
  coord_cartesian(expand = FALSE) +
  mytheme +
  theme(axis.line.y = element_blank(),
        axis.ticks.y = element_blank(),
        axis.text.y = element_blank(),
        axis.title = element_blank())

Frontier2022_k_prior_posterior_replicate <- Frontier2022_k_prior %>% 
  prior_posterior_draws(
    posterior_samples = Frontier2022_k_samples,
    group = data %>% 
      filter(reference == "Frontier et al. 2022") %>%
      droplevels() %>%
      select(replicate),
    parameters = c("log_k_sigma_r", "log_k_r[replicate]"),
    format = "long"
    ) %>%
  prior_posterior_plot(group_name = "replicate", ridges = TRUE) +
  scale_x_continuous(
    labels = scales::label_number(style_negative = "minus")
  ) +
  coord_cartesian(expand = FALSE) +
  mytheme +
  theme(axis.line.y = element_blank(),
        axis.ticks.y = element_blank(),
        axis.text.y = element_blank(),
        axis.title = element_blank())

Frontier2022_prior_posterior <- 
  ( Frontier2022_prior_posterior_species / 
      Frontier2022_prior_posterior_replicate / 
      Frontier2022_k_prior_posterior_species / 
      Frontier2022_k_prior_posterior_replicate ) +
  plot_layout(heights = c(1, 2/5, 3/5, 1/5))

Frontier2022_prior_posterior %>%
  ggsave(filename = "Frontier2022_prior_posterior.pdf", path = "Plots",
         device = cairo_pdf, width = 40, height = 60, units = "cm")

# 6.6.6 Parameter distributions ####
# Macroalgal model treatment parameters
Frontier2022_prior_posterior_global <- Frontier2022_prior %>% 
  prior_posterior_draws(
    posterior_samples = Frontier2022_samples,
    parameters = c("log_alpha_mu", "log_alpha_sigma_s", "log_alpha_sigma_r",
                   "log_mu_mu", "log_mu_sigma_s", "log_mu_sigma_r",
                   "log_tau_mu", "log_tau_sigma_s", "log_tau_sigma_r",
                   "log_beta_mu", "log_beta_sigma", "epsilon", "lambda", "theta"),
    format = "short"
  ) %>% 
  mutate( # Calculate for new replicates from new species
    alpha = exp(
      rnorm( n() , log_alpha_mu , log_alpha_sigma_s ) +
        rnorm( n() , 0 , log_alpha_sigma_r )
    ),
    beta = exp( rnorm( n() , log_beta_mu , log_beta_sigma ) ),
    mu_0.5m = exp(
      rnorm( n() , log_mu_mu , log_mu_sigma_s ) +
        rnorm( n() , 0 , log_mu_sigma_r ) - beta * 0.5
    ),
    mu_1.5m = exp(
      rnorm( n() , log_mu_mu , log_mu_sigma_s ) +
        rnorm( n() , 0 , log_mu_sigma_r ) - beta * 1.5
    ),
    mu_3m = exp(
      rnorm( n() , log_mu_mu , log_mu_sigma_s ) +
        rnorm( n() , 0 , log_mu_sigma_r ) - beta * 3
    ),
    tau = exp(
      rnorm( n() , log_tau_mu , log_tau_sigma_s ) +
        rnorm( n() , 0 , log_tau_sigma_r )
    )
  ) %>%
  pivot_longer(cols = starts_with("mu"),
               names_to = "treatment",
               values_to = "mu",
               names_prefix = "mu_") %>%
  select(-starts_with("log")) %T>%
  print()

Frontier2022_prior_posterior_species <- Frontier2022_prior %>% 
  prior_posterior_draws(
    posterior_samples = Frontier2022_samples,
    group = data %>% 
      filter(reference == "Frontier et al. 2022") %>%
      droplevels() %>%
      select(species),
    parameters = c("log_alpha_s[species]", "log_mu_s[species]", "log_tau_s[species]", 
                   "log_beta[species]", "log_alpha_sigma_r", "log_mu_sigma_r", 
                   "log_tau_sigma_r", "epsilon", "lambda", "theta"),
    format = "short"
  ) %>% 
  mutate( # Calculate for new replicates
    alpha = exp( rnorm( n() , log_alpha_s , log_alpha_sigma_r ) ),
    beta = exp( log_beta ),
    mu_0.5m = exp(
      rnorm( n() , log_mu_s , log_mu_sigma_r ) - beta * 0.5
    ),
    mu_1.5m = exp(
      rnorm( n() , log_mu_s , log_mu_sigma_r ) - beta * 1.5
    ),
    mu_3m = exp(
      rnorm( n() , log_mu_s , log_mu_sigma_r ) - beta * 3
    ),
    tau = exp( rnorm( n() , log_tau_s , log_tau_sigma_r ) )
  ) %>%
  pivot_longer(cols = starts_with("mu"),
               names_to = "treatment",
               values_to = "mu",
               names_prefix = "mu_") %>%
  filter(species == "Laminaria hyperborea" & distribution == "prior" |
           distribution == "posterior") %>%
  mutate(
    species = if_else(
      distribution == "prior", "Prior", species
    ) %>% fct()
  ) %>%
  select(-c(distribution, starts_with("log"))) %T>%
  print()

Frontier2022_prior_posterior <- Frontier2022_prior_posterior_species %>%
  bind_rows(
    Frontier2022_prior_posterior_global %>%
      filter(distribution == "posterior") %>%
      select(-distribution) %>%
      mutate(species = "Global" %>% fct())
  ) %T>%
  print()

# Macroalgal model depth effect parameters
Frontier2022_prior_posterior_beta_global <- Frontier2022_prior %>% 
  prior_posterior_draws(
    posterior_samples = Frontier2022_samples,
    parameters = c("log_mu_mu", "log_mu_sigma_s", "log_mu_sigma_r",
                   "log_beta_mu", "log_beta_sigma"),
    format = "short"
  ) %>% 
  mutate( # Calculate for new replicates from new species
    beta = exp( rnorm( n() , log_beta_mu , log_beta_sigma ) ),
    log_mu = rnorm( n() , log_mu_mu , log_mu_sigma_s ) +
      rnorm( n() , 0 , log_mu_sigma_r )
  ) %>%
  select(starts_with("."), distribution, beta, log_mu) %T>%
  print()

Frontier2022_prior_posterior_beta_species <- Frontier2022_prior %>% 
  prior_posterior_draws(
    posterior_samples = Frontier2022_samples,
    group = data %>% 
      filter(reference == "Frontier et al. 2022") %>%
      droplevels() %>%
      select(species),
    parameters = c("log_mu_s[species]", "log_mu_sigma_r", "log_beta[species]"),
    format = "short"
  ) %>% 
  mutate( # Calculate for new replicates
    beta = exp( log_beta ),
    log_mu = rnorm( n() , log_mu_s , log_mu_sigma_r )
  ) %>%
  filter(species == "Laminaria hyperborea" & distribution == "prior" |
           distribution == "posterior") %>%
  mutate(
    species = if_else(
      distribution == "prior", "Prior", species
    ) %>% fct()
  ) %>%
  select(starts_with("."), species, beta, log_mu) %T>%
  print()

Frontier2022_prior_posterior_beta <- Frontier2022_prior_posterior_beta_species %>%
  bind_rows(
    Frontier2022_prior_posterior_beta_global %>%
      filter(distribution == "posterior") %>%
      select(-distribution) %>%
      mutate(species = "Global" %>% fct())
  ) %T>%
  print()

# Macroalgal model replicate parameters
Frontier2022_prior_posterior_replicate <- data %>% # Get pairs from data
  filter(reference == "Frontier et al. 2022") %>%
  droplevels() %>%
  mutate(depth = treatment %>% 
           str_extract("[\\d.]+") %>% 
           as.numeric()) %>%
  distinct(species, replicate, treatment, depth) %>%
  left_join( # Join species distributions
    Frontier2022_prior %>% 
      prior_posterior_draws(
        posterior_samples = Frontier2022_samples,
        group = data %>% 
          filter(reference == "Frontier et al. 2022") %>%
          droplevels() %>%
          select(species),
        parameters = c("log_alpha_s[species]", "log_mu_s[species]", 
                       "log_tau_s[species]", "log_beta[species]"),
        format = "short"
      ),
    by = "species",
    relationship = "many-to-many"
  ) %>%
  left_join( # Join replicate distributions
    Frontier2022_prior %>% 
      prior_posterior_draws(
        posterior_samples = Frontier2022_samples,
        group = data %>% 
          filter(reference == "Frontier et al. 2022") %>%
          droplevels() %>%
          select(replicate),
        parameters = c("log_alpha_r[replicate]", 
                       "log_mu_r[replicate]", 
                       "log_tau_r[replicate]"),
        format = "short"
      ),
    by = c("replicate", ".chain", ".iteration", ".draw", "distribution"),
    relationship = "many-to-many"
  ) %>% 
  mutate( # Calculate for existing replicates
    alpha = exp( log_alpha_s + log_alpha_r ),
    beta = exp( log_beta ),
    mu = exp( log_mu_s + log_mu_r - beta * depth ),
    tau = exp( log_tau_s + log_tau_r )
  ) %>% # Pick one replicate from each depth for priors to keep
  filter(replicate %in% c("1", "5", "9") & distribution == "prior" |
           distribution == "posterior") %>%
  mutate(
    species = if_else(
      distribution == "prior", "Prior", species
    ) %>% fct(),
    replicate = if_else(
      distribution == "prior", "Prior", replicate
    ) %>% fct(),
    treatment = if_else(
      distribution == "prior", "Prior", treatment
    ) %>% fct()
  ) %>%
  select(-c(distribution, starts_with("log"))) %T>%
  print()

# Conventional model treatment parameters
Frontier2022_k_prior_posterior_global <- Frontier2022_k_prior %>% 
  prior_posterior_draws(
    posterior_samples = Frontier2022_k_samples,
    parameters = c("log_k_mu", "log_k_sigma_s", "log_k_sigma_r",
                   "beta_mu", "beta_sigma", "sigma"),
    format = "short"
  ) %>% 
  mutate( # Calculate for new replicates from new species
    beta = rnorm( n() , beta_mu , beta_sigma ),
    k_0.5m = exp(
      rnorm( n() , log_k_mu , log_k_sigma_s ) +
        rnorm( n() , 0 , log_k_sigma_r ) + beta * 0.5
    ),
    k_1.5m = exp(
      rnorm( n() , log_k_mu , log_k_sigma_s ) +
        rnorm( n() , 0 , log_k_sigma_r ) + beta * 1.5
    ),
    k_3m = exp(
      rnorm( n() , log_k_mu , log_k_sigma_s ) +
        rnorm( n() , 0 , log_k_sigma_r ) + beta * 3
    )
  ) %>%
  pivot_longer(cols = starts_with("k"),
               names_to = "treatment",
               values_to = "k",
               names_prefix = "k_") %>%
  select(-c(starts_with("log"), beta_mu, beta_sigma)) %T>%
  print()

Frontier2022_k_prior_posterior_species <- Frontier2022_k_prior %>% 
  prior_posterior_draws(
    posterior_samples = Frontier2022_k_samples,
    group = data %>% 
      filter(reference == "Frontier et al. 2022") %>%
      droplevels() %>%
      select(species),
    parameters = c("log_k_s[species]", "beta[species]", 
                   "log_k_sigma_r", "sigma"),
    format = "short"
  ) %>% 
  mutate( # Calculate for new replicates
    k_0.5m = exp(
      rnorm( n() , log_k_s , log_k_sigma_r ) + beta * 0.5
    ),
    k_1.5m = exp(
      rnorm( n() , log_k_s , log_k_sigma_r ) + beta * 1.5
    ),
    k_3m = exp(
      rnorm( n() , log_k_s , log_k_sigma_r ) + beta * 3
    )
  ) %>%
  pivot_longer(cols = starts_with("k"),
               names_to = "treatment",
               values_to = "k",
               names_prefix = "k_") %>%
  filter(species == "Laminaria hyperborea" & distribution == "prior" |
           distribution == "posterior") %>%
  mutate(
    species = if_else(
      distribution == "prior", "Prior", species
    ) %>% fct()
  ) %>%
  select(-c(distribution, starts_with("log"))) %T>%
  print()

Frontier2022_k_prior_posterior <- Frontier2022_k_prior_posterior_species %>%
  bind_rows(
    Frontier2022_k_prior_posterior_global %>%
      filter(distribution == "posterior") %>%
      select(-distribution) %>%
      mutate(species = "Global" %>% fct())
  ) %T>%
  print()

# Conventional model depth effect parameters
Frontier2022_k_prior_posterior_beta_global <- Frontier2022_k_prior %>% 
  prior_posterior_draws(
    posterior_samples = Frontier2022_k_samples,
    parameters = c("log_k_mu", "log_k_sigma_s", "log_k_sigma_r",
                   "beta_mu", "beta_sigma"),
    format = "short"
  ) %>% 
  mutate( # Calculate for new replicates from new species
    beta = rnorm( n() , beta_mu , beta_sigma ),
    log_k = rnorm( n() , log_k_mu , log_k_sigma_s ) +
      rnorm( n() , 0 , log_k_sigma_r )
  ) %>%
  select(starts_with("."), distribution, beta, log_k) %T>%
  print()

Frontier2022_k_prior_posterior_beta_species <- Frontier2022_k_prior %>% 
  prior_posterior_draws(
    posterior_samples = Frontier2022_k_samples,
    group = data %>% 
      filter(reference == "Frontier et al. 2022") %>%
      droplevels() %>%
      select(species),
    parameters = c("log_k_s[species]", "log_k_sigma_r", "beta[species]"),
    format = "short"
  ) %>% 
  mutate( # Calculate for new replicates
    log_k = rnorm( n() , log_k_s , log_k_sigma_r )
  ) %>%
  filter(species == "Laminaria hyperborea" & distribution == "prior" |
           distribution == "posterior") %>%
  mutate(
    species = if_else(
      distribution == "prior", "Prior", species
    ) %>% fct()
  ) %>%
  select(starts_with("."), species, beta, log_k) %T>%
  print()

Frontier2022_k_prior_posterior_beta <- Frontier2022_k_prior_posterior_beta_species %>%
  bind_rows(
    Frontier2022_k_prior_posterior_beta_global %>%
      filter(distribution == "posterior") %>%
      select(-distribution) %>%
      mutate(species = "Global" %>% fct())
  ) %T>%
  print()

# Conventional model replicate parameters
Frontier2022_k_prior_posterior_replicate <- data %>% # Get pairs from data
  filter(reference == "Frontier et al. 2022") %>%
  droplevels() %>%
  mutate(depth = treatment %>% 
           str_extract("[\\d.]+") %>% 
           as.numeric()) %>%
  distinct(species, replicate, treatment, depth) %>%
  left_join( # Join species distributions
    Frontier2022_k_prior %>% 
      prior_posterior_draws(
        posterior_samples = Frontier2022_k_samples,
        group = data %>% 
          filter(reference == "Frontier et al. 2022") %>%
          droplevels() %>%
          select(species),
        parameters = c("log_k_s[species]", "beta[species]"),
        format = "short"
      ),
    by = "species",
    relationship = "many-to-many"
  ) %>%
  left_join( # Join replicate distributions
    Frontier2022_k_prior %>% 
      prior_posterior_draws(
        posterior_samples = Frontier2022_k_samples,
        group = data %>% 
          filter(reference == "Frontier et al. 2022") %>%
          droplevels() %>%
          select(replicate),
        parameters = c("log_k_r[replicate]"),
        format = "short"
      ),
    by = c("replicate", ".chain", ".iteration", ".draw", "distribution"),
    relationship = "many-to-many"
  ) %>% 
  mutate( # Calculate for existing replicates
    k = exp( log_k_s + log_k_r + beta * depth )
  ) %>% # Pick one replicate from each depth for priors to keep
  filter(replicate %in% c("1", "5", "9") & distribution == "prior" |
           distribution == "posterior") %>%
  mutate(
    species = if_else(
      distribution == "prior", "Prior", species
    ) %>% fct(),
    replicate = if_else(
      distribution == "prior", "Prior", replicate
    ) %>% fct(),
    treatment = if_else(
      distribution == "prior", "Prior", treatment
    ) %>% fct()
  ) %>%
  select(-c(distribution, starts_with("log"))) %T>%
  print()

# Save parameter distributions
Frontier2022_prior_posterior %>%
  write_rds(here("RDS", "Frontier2022_prior_posterior.rds"))
Frontier2022_prior_posterior_beta %>%
  write_rds(here("RDS", "Frontier2022_prior_posterior_beta.rds"))
Frontier2022_prior_posterior_replicate %>%
  write_rds(here("RDS", "Frontier2022_prior_posterior_replicate.rds"))

Frontier2022_k_prior_posterior %>%
  write_rds(here("RDS", "Frontier2022_k_prior_posterior.rds"))
Frontier2022_k_prior_posterior_beta %>%
  write_rds(here("RDS", "Frontier2022_k_prior_posterior_beta.rds"))
Frontier2022_k_prior_posterior_replicate %>%
  write_rds(here("RDS", "Frontier2022_k_prior_posterior_replicate.rds"))

# 6.6.7 Continuous prediction ####
# Treatment predictions
Frontier2022_prediction <- Frontier2022_prior_posterior %>%
  nest(.by = c(species, treatment), .key = "prior_posterior") %>%
  left_join(
    data %>% 
      filter(reference == "Frontier et al. 2022") %>%
      droplevels() %>%
      select(species, treatment, t) %>%
      nest(.by = c(species, treatment), .key = "t")
  ) %>%
  mutate(
    predictor = t %>% 
      map(
        ~if(is.null(.x)){
          data %>% 
            filter(reference == "Frontier et al. 2022") %$%
            seq(min(t), max(t), length.out = 150)
        } else {
          .x %$% 
            seq(min(t), max(t), length.out = 150)
        }
      ),
    prediction = map2(
      prior_posterior, predictor,
      ~.x %>% 
        slice( rep( 1:n() , each = length(.y) ) ) %>%
        mutate(
          t = rep( .y , times = nrow(.x) ),
          m_mu = exp(
            t * alpha - ( alpha + tau ) * mu / 5 * (
              log1p_exp( 5 / mu * ( t - mu ) ) -
                log1p_exp( -5 )
            )
          ),
          k = ( alpha + tau ) / ( 1 + exp( 5 / mu * ( t - mu ) ) ) - tau,
          nu = ( epsilon - theta ) * exp( -lambda * t ) + theta,
          m = rbetapr( n() , m_mu * ( 1 + nu ) , 2 + nu )
        ) %>%
        group_by(t) %>%
        median_qi(m_mu, k, nu, m, .width = c(.5, .8, .9)) %T>%
        print()
    )
  ) %>% 
  select(-c(prior_posterior, t, predictor)) %>%
  unnest(prediction) %T>%
  print()

Frontier2022_k_prediction <- Frontier2022_k_prior_posterior %>%
  nest(.by = c(species, treatment), .key = "prior_posterior") %>%
  left_join(
    data %>% 
      filter(reference == "Frontier et al. 2022") %>%
      droplevels() %>%
      select(species, treatment, t) %>%
      nest(.by = c(species, treatment), .key = "t")
  ) %>%
  mutate(
    predictor = t %>% 
      map(
        ~if(is.null(.x)){
          data %>% 
            filter(reference == "Frontier et al. 2022") %$%
            seq(min(t), max(t), length.out = 150)
        } else {
          .x %$% 
            seq(min(t), max(t), length.out = 150)
        }
      ),
    prediction = map2(
      prior_posterior, predictor,
      ~.x %>% 
        slice( rep( 1:n() , each = length(.y) ) ) %>%
        mutate(
          t = rep( .y , times = nrow(.x) ),
          m_mu = exp( -k * t ),
          m = rnorm( n() , m_mu , sigma )
        ) %>%
        group_by(t) %>%
        median_qi(m_mu, m, .width = c(.5, .8, .9)) %T>%
        print()
    )
  ) %>% 
  select(-c(prior_posterior, t, predictor)) %>%
  unnest(prediction) %T>%
  print()

# Save predictions
Frontier2022_prediction %>%
  write_rds(here("RDS", "Frontier2022_prediction.rds"))

Frontier2022_k_prediction %>%
  write_rds(here("RDS", "Frontier2022_k_prediction.rds"))

# Continuous depth predictions
Frontier2022_prediction_beta <- Frontier2022_prior_posterior_beta %>%
  spread_continuous(data = data %>%
                      filter(reference == "Frontier et al. 2022") %>%
                      droplevels() %>%
                      mutate(depth = treatment %>% # Be sure to extract depth
                               str_extract("[\\d.]+") %>% 
                               as.numeric()),
                    predictor_name = "depth") %>%
  mutate( mu = exp( log_mu - beta * depth ) ) %>%
  group_by(depth, species) %>%
  median_qi(mu, .width = c(.5, .8, .9)) %T>%
  print()

Frontier2022_k_prediction_beta <- Frontier2022_k_prior_posterior_beta %>%
  spread_continuous(data = data %>%
                      filter(reference == "Frontier et al. 2022") %>%
                      droplevels() %>%
                      mutate(depth = treatment %>%
                               str_extract("[\\d.]+") %>% 
                               as.numeric()),
                    predictor_name = "depth") %>%
  mutate( k = exp( log_k + beta * depth ) ) %>%
  group_by(depth, species) %>%
  median_qi(k, .width = c(.5, .8, .9)) %T>%
  print()

# Save predictions
Frontier2022_prediction_beta %>%
  write_rds(here("RDS", "Frontier2022_prediction_beta.rds"))

Frontier2022_k_prediction_beta %>%
  write_rds(here("RDS", "Frontier2022_k_prediction_beta.rds"))

# Replicate predictions
Frontier2022_prediction_replicate <- Frontier2022_prior_posterior_replicate %>%
  nest(.by = c(species, replicate, treatment, depth), .key = "prior_posterior") %>%
  left_join(
    data %>% 
      filter(reference == "Frontier et al. 2022") %>%
      droplevels() %>%
      select(species, replicate, treatment, t) %>%
      nest(.by = c(species, replicate, treatment), .key = "t")
  ) %>%
  mutate(
    predictor = t %>% 
      map(
        ~if(is.null(.x)){
          data %>% 
            filter(reference == "Frontier et al. 2022") %$%
            seq(min(t), max(t), length.out = 150)
        } else {
          .x %$% 
            seq(min(t), max(t), length.out = 150)
        }
      ),
    prediction = map2(
      prior_posterior, predictor,
      ~.x %>% 
        slice( rep( 1:n() , each = length(.y) ) ) %>%
        mutate(
          t = rep( .y , times = nrow(.x) ),
          m_mu = exp(
            t * alpha - ( alpha + tau ) * mu / 5 * (
              log1p_exp( 5 / mu * ( t - mu ) ) -
                log1p_exp( -5 )
            )
          ),
          k = ( alpha + tau ) / ( 1 + exp( 5 / mu * ( t - mu ) ) ) - tau
        ) %>%
        group_by(t) %>%
        median_qi(m_mu, k, .width = c(.5, .8, .9)) %T>%
        print() 
    )
  ) %>% 
  select(-c(prior_posterior, t, predictor)) %>%
  unnest(prediction) %T>%
  print()

Frontier2022_k_prediction_replicate <- Frontier2022_k_prior_posterior_replicate %>%
  nest(.by = c(species, replicate, treatment, depth), .key = "prior_posterior") %>%
  left_join(
    data %>% 
      filter(reference == "Frontier et al. 2022") %>%
      droplevels() %>%
      select(species, replicate, treatment, t) %>%
      nest(.by = c(species, replicate, treatment), .key = "t")
  ) %>%
  mutate(
    predictor = t %>% 
      map(
        ~if(is.null(.x)){
          data %>% 
            filter(reference == "Frontier et al. 2022") %$%
            seq(min(t), max(t), length.out = 150)
        } else {
          .x %$% 
            seq(min(t), max(t), length.out = 150)
        }
      ),
    prediction = map2(
      prior_posterior, predictor,
      ~.x %>% 
        slice( rep( 1:n() , each = length(.y) ) ) %>%
        mutate(
          t = rep( .y , times = nrow(.x) ),
          m_mu = exp( -k * t )
        ) %>%
        group_by(t) %>%
        median_qi(m_mu, .width = c(.5, .8, .9)) %T>%
        print()
    )
  ) %>% 
  select(-c(prior_posterior, t, predictor)) %>%
  unnest(prediction) %T>%
  print()

# Save predictions
Frontier2022_prediction_replicate %>%
  write_rds(here("RDS", "Frontier2022_prediction_replicate.rds"))

Frontier2022_k_prediction_replicate %>%
  write_rds(here("RDS", "Frontier2022_k_prediction_replicate.rds"))

# 6.6.8 Visualisation of predictions ####
# Viusalise mean predictions
data %>%
  filter(reference == "Frontier et al. 2022" & t != 0) %>%
  droplevels() %>%
  ggplot() +
    geom_point(aes(t, m, colour = treatment), shape = 16, alpha = 0.5) +
    geom_line(data = Frontier2022_prediction %>%
                filter(!species %in% c("Prior", "Global")),
              aes(t, m_mu, colour = treatment)) +
    geom_ribbon(data = Frontier2022_prediction %>%
                  filter(!species %in% c("Prior", "Global")),
                aes(t, ymin = m_mu.lower, ymax = m_mu.upper, 
                    alpha = factor(.width), fill = treatment)) +
    scale_alpha_manual(values = c(0.5, 0.4, 0.3), guide = "none") +
    facet_grid(treatment ~ species) +
    mytheme

data %>%
  filter(reference == "Frontier et al. 2022" & t != 0) %>%
  droplevels() %>%
  ggplot() +
    geom_point(aes(t, m, colour = treatment), shape = 16, alpha = 0.5) +
    geom_line(data = Frontier2022_k_prediction %>%
                filter(!species %in% c("Prior", "Global")),
              aes(t, m_mu, colour = treatment)) +
    geom_ribbon(data = Frontier2022_k_prediction %>%
                  filter(!species %in% c("Prior", "Global")),
                aes(t, ymin = m_mu.lower, ymax = m_mu.upper, 
                    alpha = factor(.width), fill = treatment)) +
    scale_alpha_manual(values = c(0.5, 0.4, 0.3), guide = "none") +
    facet_grid(treatment ~ species) +
    mytheme

# Visualise predictions of new observations
data %>%
  filter(reference == "Frontier et al. 2022" & t != 0) %>%
  droplevels() %>%
  ggplot() +
    geom_point(aes(t, m, colour = treatment), shape = 16, alpha = 0.5) +
    geom_line(data = Frontier2022_prediction %>%
                filter(!species %in% c("Prior", "Global")),
              aes(t, m, colour = treatment)) +
    geom_ribbon(data = Frontier2022_prediction %>%
                  filter(!species %in% c("Prior", "Global")),
                aes(t, ymin = m.lower, ymax = m.upper, 
                    alpha = factor(.width), fill = treatment)) +
    scale_alpha_manual(values = c(0.5, 0.4, 0.3), guide = "none") +
    facet_grid(treatment ~ species) +
    mytheme

data %>%
  filter(reference == "Frontier et al. 2022" & t != 0) %>%
  droplevels() %>%
  ggplot() +
    geom_point(aes(t, m, colour = treatment), shape = 16, alpha = 0.5) +
    geom_line(data = Frontier2022_k_prediction %>%
                filter(!species %in% c("Prior", "Global")),
              aes(t, m, colour = treatment)) +
    geom_ribbon(data = Frontier2022_k_prediction %>%
                  filter(!species %in% c("Prior", "Global")),
                aes(t, ymin = m.lower, ymax = m.upper, 
                    alpha = factor(.width), fill = treatment)) +
    scale_alpha_manual(values = c(0.5, 0.4, 0.3), guide = "none") +
    facet_grid(treatment ~ species) +
    mytheme

# Visualise predictions of replicates
data %>%
  filter(reference == "Frontier et al. 2022" & t != 0) %>%
  droplevels() %>%
  ggplot() +
    geom_point(aes(t, m, colour = treatment), shape = 16, alpha = 0.5) +
    geom_line(data = Frontier2022_prediction_replicate %>%
                filter(!species %in% c("Prior", "Global")),
              aes(t, m_mu, colour = treatment, group = replicate),
              alpha = 0.5) +
    facet_grid(treatment ~ species) +
    mytheme

data %>%
  filter(reference == "Frontier et al. 2022" & t != 0) %>%
  droplevels() %>%
  ggplot() +
    geom_point(aes(t, m, colour = treatment), shape = 16, alpha = 0.5) +
    geom_line(data = Frontier2022_k_prediction_replicate %>%
                filter(!species %in% c("Prior", "Global")),
              aes(t, m_mu, colour = treatment, group = replicate),
              alpha = 0.5) +
    facet_grid(treatment ~ species) +
    mytheme

# Visualise predictions by depth
Frontier2022_prediction_beta %>%
  filter(!species %in% c("Prior", "Global")) %>%
  ggplot() +
    geom_line(aes(depth, mu),
              alpha = 0.5) +
    geom_ribbon(aes(depth, ymin = .lower, ymax = .upper,
                    alpha = factor(.width))) +
    scale_alpha_manual(values = c(0.5, 0.4, 0.3), guide = "none") +
    facet_grid(~ species) +
    mytheme

Frontier2022_k_prediction_beta %>%
  filter(!species %in% c("Prior", "Global")) %>%
  ggplot() +
    geom_line(aes(depth, k),
              alpha = 0.5) +
    geom_ribbon(aes(depth, ymin = .lower, ymax = .upper,
                    alpha = factor(.width))) +
    scale_alpha_manual(values = c(0.5, 0.4, 0.3), guide = "none") +
    facet_grid(~ species) +
    mytheme

# Visualise predictions of time-variant k
Frontier2022_prediction %>%
  filter(!species %in% c("Prior", "Global")) %>%
  ggplot() +
    geom_line(aes(t, k, colour = treatment)) +
    geom_ribbon(aes(t, ymin = k.lower, ymax = k.upper, 
                    alpha = factor(.width), fill = treatment)) +
    scale_alpha_manual(values = c(0.5, 0.4, 0.3), guide = "none") +
    facet_grid(treatment ~ species) +
    mytheme

Frontier2022_prediction_replicate %>%
  filter(!species %in% c("Prior", "Global")) %>%
  ggplot() +
    geom_line(aes(t, k, colour = treatment, group = replicate)) +
    facet_grid(treatment ~ species) +
    mytheme

# Visualise predictions of time-variant nu (global)
Frontier2022_prediction %>%
  filter(species == "Global") %>%
  ggplot() +
    geom_line(aes(t, nu)) +
    geom_ribbon(aes(t, ymin = nu.lower, ymax = nu.upper, 
                    alpha = factor(.width))) +
    scale_alpha_manual(values = c(0.5, 0.4, 0.3), guide = "none") +
    mytheme

# Clean up
rm( list = ls( pattern = "Frontier" ) )
gc()

# 6.7 Vandendriessche et al. 2007 ####
# 6.7.1 Data visualisation ####
data %>%
  filter(reference == "Vandendriessche et al. 2007") %>%
  droplevels() %>%
  ggplot() +
    geom_point(aes(t, m), shape = 16, alpha = 0.5) +
    geom_line(aes(t, m, group = replicate), alpha = 0.5) +
    facet_grid(treatment ~ species) +
    mytheme

# Same as with 6.5 and 6.6 but in addition to the continuous predictor
# (here temperature), there is a control-grazer contrast to model, which 
# is best effect-coded (-0.5, +0.5). It is reasonable to also centre
# temperature since a prediction at 0°C is not meaningful.

data %>%
  filter(reference == "Vandendriessche et al. 2007" & t != 0) %>%
  droplevels() %>%
  mutate(
    temperature = treatment %>% 
      str_extract("\\d+") %>% 
      as.numeric(),
    temperature_c = temperature - 12,
    grazing = if_else(
      treatment %>% str_detect("Grazed"),
      0.5, -0.5
    )
  ) %>%
  distinct(treatment, temperature, temperature_c, grazing)

# I tested this and it did not work, the model always lost chains, probably 
# because the temperature effect is nonlinear. Therefore I am unfortunately 
# forced to treat temperature as categorical. Partial pooling also failed.
# Hence I am treating species, temperature, grazing and replicate as fixed.

data %>%
  filter(reference == "Vandendriessche et al. 2007" & t != 0) %>%
  droplevels() %>%
  mutate(
    temperature = treatment %>% 
      str_extract("\\d+°C") %>% fct(),
    grazing = if_else(
      treatment %>% str_detect("Grazed"),
      0.5, -0.5
    )
  ) %>%
  distinct(treatment, temperature, grazing)

# 6.7.2 Prior simulation ####
tibble(n = 1:1e3,
       log_alpha_s = rnorm( 1e3 , log(0.004) , 0.2 ), # species intercepts
       log_mu_s = rnorm( 1e3 , log(30) , 0.2 ),
       log_tau_s = rnorm( 1e3 , log(0.1) , 0.2 ),
       log_mu_t = rnorm( 1e3 , 0 , 0.2 ), # temperature deviations (cannot affect alpha)
       log_tau_t = rnorm( 1e3 , 0 , 0.2 ),
       beta_mu = rnorm( 1e3 , 0 , 0.2 ), # grazing effects (cannot affect alpha)
       beta_tau = rnorm( 1e3 , 0 , 0.2 ),
       log_alpha_r = rnorm( 1e3 , 0 , 0.2 ), # replicate deviations
       log_mu_r = rnorm( 1e3 , 0 , 0.2 ),
       log_tau_r = rnorm( 1e3 , 0 , 0.2 ),
       alpha = exp( log_alpha_s + log_alpha_r ),
       mu = exp( log_mu_s + log_mu_t + beta_mu + log_mu_r ),
       tau = exp( log_tau_s + log_tau_t + beta_tau + log_tau_r ),
       epsilon = rgamma( 1e3 , 4e4^2 / 2e4^2 , 4e4 / 2e4^2 ),
       lambda = rexp( 1e3 , 1 ),
       theta = rgamma( 1e3 , 500^2 / 250^2 , 500 / 250^2 )) %>%
  expand_grid(t = data %>%
                filter(reference == "Vandendriessche et al. 2007") %$% 
                seq(min(t), max(t), length.out = 100)) %>%
  mutate(
    m_mu = exp(
      t * alpha - ( alpha + tau ) * mu / 5 * (
        log1p_exp( 5 / mu * ( t - mu ) ) - log1p_exp( -5 )
      )
    ),
    nu = theta + (epsilon - theta) * exp( -lambda * t ),
    m = rbetapr( n() , m_mu * ( 1 + nu ) , 2 + nu )
  ) %>%
  pivot_longer(cols = c(m_mu, m),
               names_to = "parameter") %>%
  ggplot(aes(t, value, group = n)) +
    geom_hline(yintercept = data %>%
                 filter(reference == "Vandendriessche et al. 2007") %$%
                 range(m)) +
    geom_line(alpha = 0.05) +
    coord_cartesian(expand = F, clip = "off") +
    facet_wrap(~parameter, scale = "free", nrow = 1) +
    theme_minimal() +
    theme(panel.grid = element_blank())
# This seems like a tight prior, but I tested looser priors and they cause
# failed convergence. The data are strong with this one, so it's fine.

tibble(n = 1:1e3,
       log_k_s = rnorm( 1e3 , log(0.1) , 1 ), # species intercept
       log_k_t = rnorm( 1e3 , 0 , 1 ), # temperature deviation
       beta = rnorm( 1e3 , 0 , 1 ), # grazing effects
       log_k_r = rnorm( 1e3 , 0 , 1 ), # replicate deviations
       k = exp( log_k_s + log_k_t + beta + log_k_r ),
       sigma = rexp( 1e3 , 1 )) %>%
  expand_grid(t = data %>%
                filter(reference == "Vandendriessche et al. 2007") %$% 
                seq(min(t), max(t), length.out = 100)) %>%
  mutate(
    m_mu = exp( -k * t ),
    m = rnorm( n() , m_mu , sigma )
  ) %>%
  pivot_longer(cols = c(m_mu, m),
               names_to = "parameter") %>%
  ggplot(aes(t, value, group = n)) +
    geom_hline(yintercept = data %>%
                 filter(reference == "Vandendriessche et al. 2007") %$%
                 range(m)) +
    geom_line(alpha = 0.05) +
    coord_cartesian(expand = F, clip = "off") +
    facet_wrap(~parameter, scale = "free", nrow = 1) +
    theme_minimal() +
    theme(panel.grid = element_blank())

# 6.7.3 Stan model ####
Vandendriessche_model <- here("Stan", "Vandendriessche.stan") %>% 
  read_file() %>%
  write_stan_file() %>%
  cmdstan_model()

Vandendriessche_k_model <- here("Stan", "Vandendriessche_k.stan") %>% 
  read_file() %>%
  write_stan_file() %>%
  cmdstan_model()

Vandendriessche_samples <- Vandendriessche_model$sample(
          data = data %>%
            filter(reference == "Vandendriessche et al. 2007" & t != 0) %>%
            droplevels() %>%
            mutate(
              temperature = treatment %>% 
                str_extract("\\d+°C") %>% fct(),
              grazing = if_else(
                treatment %>% str_detect("Grazed"),
                0.5, -0.5
              )
            ) %>%
            select(t, m, species, temperature, 
                   grazing, replicate) %>%
            compose_data(),
          chains = 8,
          parallel_chains = parallel::detectCores(),
          iter_warmup = 1e4,
          iter_sampling = 1e4
        ) %T>%
  print()


Vandendriessche_k_samples <- Vandendriessche_k_model$sample(
          data = data %>%
            filter(reference == "Vandendriessche et al. 2007" & t != 0) %>%
            droplevels() %>%
            mutate(
              temperature = treatment %>% 
                str_extract("\\d+°C") %>% fct(),
              grazing = if_else(
                treatment %>% str_detect("Grazed"),
                0.5, -0.5
              )
            ) %>%
            select(t, m, species, temperature, 
                   grazing, replicate) %>%
            compose_data(),
          chains = 8,
          parallel_chains = parallel::detectCores(),
          iter_warmup = 1e4,
          iter_sampling = 1e4
        ) %T>%
  print()

# Save draws
Vandendriessche_samples$draws() %>%
  write_rds(here("RDS", "Vandendriessche_samples.rds"))
Vandendriessche_samples$draws(format = "df") %>%
  write_rds(here("RDS", "Vandendriessche_samples_df.rds"))

Vandendriessche_k_samples$draws() %>%
  write_rds(here("RDS", "Vandendriessche_k_samples.rds"))
Vandendriessche_k_samples$draws(format = "df") %>%
  write_rds(here("RDS", "Vandendriessche_k_samples_df.rds"))

# 6.6.4 Model checks ####
# Rhat
Vandendriessche_samples$summary() %>%
  summarise(rhat_1.001 = mean( rhat > 1.001 ),
            rhat_mean = mean(rhat),
            rhat_sd = sd(rhat))
# No rhat above 1.001. rhat = 1.00 ± 0.0000731.

Vandendriessche_k_samples$summary() %>%
  summarise(rhat_1.001 = mean( rhat > 1.001 ),
            rhat_mean = mean(rhat),
            rhat_sd = sd(rhat))
# No of rhat above 1.001. rhat = 1.00 ± 0.000108.

# Chains
Vandendriessche_chains <- Vandendriessche_samples$draws(format = "df") %>%
  mcmc_rank_overlay() +
  guides(colour = guide_legend(nrow = 1)) +
  labs(title = "Macroalgal model",
       y = "Frequency") +
  coord_cartesian(xlim = c(0, 8e4), ylim = c(0, 1e3),
                  expand = FALSE, clip = "off") +
  mytheme

Vandendriessche_chains %>%
  ggsave(filename = "Vandendriessche_chains.pdf", path = "Plots",
         device = cairo_pdf, width = 100, height = 60, units = "cm")

Vandendriessche_k_chains <- Vandendriessche_k_samples$draws(format = "df") %>%
  mcmc_rank_overlay() +
  guides(colour = guide_legend(nrow = 1)) +
  labs(title = "Conventional model",
       y = "Frequency") +
  coord_cartesian(xlim = c(0, 8e4), ylim = c(0, 1e3),
                  expand = FALSE, clip = "off") +
  mytheme

Vandendriessche_k_chains %>%
  ggsave(filename = "Vandendriessche_k_chains.pdf", path = "Plots",
         device = cairo_pdf, width = 60, height = 40, units = "cm")

# Pairs
Vandendriessche_samples$draws(format = "df") %>%
  mcmc_pairs(
    pars = c("log_alpha_s[1]", "log_alpha_s[2]", "log_alpha_r[10]", "log_alpha_r[20]",
             "log_mu_s[1]", "log_mu_s[2]", "log_mu_t[1]", "log_mu_t[2]", 
             "beta_mu[1]", "beta_mu[2]", "log_mu_r[10]", "log_mu_r[20]",
             "log_tau_s[1]", "log_tau_s[2]", "log_tau_t[1]", "log_tau_t[2]", 
             "beta_tau[1]", "beta_tau[2]", "log_tau_r[10]", "log_tau_r[20]",
             "epsilon", "lambda", "theta"),
    grid_args = list(top = "Macroalgal model")
  ) %>%
  ggsave(filename = "Vandendriessche_pairs.png", path = "Plots",
         width = 100, height = 100, units = "cm", bg = "white")

Vandendriessche_k_samples$draws(format = "df") %>%
  mcmc_pairs(
    pars = c("log_k_s[1]", "log_k_s[2]", "log_k_t[1]", "log_k_t[2]",
             "beta[1]", "beta[2]", "log_k_r[10]", "log_k_r[20]", "sigma"),
    grid_args = list(top = "Conventional model")
  ) %>%
  ggsave(filename = "Vandendriessche_k_pairs.png", path = "Plots",
         width = 45, height = 45, units = "cm", bg = "white")

# 6.6.5 Prior-posterior comparison ####
Vandendriessche_prior <- prior_samples(
  model = Vandendriessche_model,
  data = data %>%
    filter(reference == "Vandendriessche et al. 2007" & t != 0) %>%
    droplevels() %>%
    mutate(
      temperature = treatment %>% 
        str_extract("\\d+°C") %>% fct(),
      grazing = if_else(
        treatment %>% str_detect("Grazed"),
        0.5, -0.5
      )
    ) %>%
    select(t, m, species, temperature, 
           grazing, replicate) %>%
    compose_data()
)

Vandendriessche_k_prior <- prior_samples(
  model = Vandendriessche_k_model,
  data = data %>%
    filter(reference == "Vandendriessche et al. 2007" & t != 0) %>%
    droplevels() %>%
    mutate(
      temperature = treatment %>% 
        str_extract("\\d+°C") %>% fct(),
      grazing = if_else(
        treatment %>% str_detect("Grazed"),
        0.5, -0.5
      )
    ) %>%
    select(t, m, species, temperature, 
           grazing, replicate) %>%
    compose_data()
)

Vandendriessche_prior_posterior_species <- Vandendriessche_prior %>% 
  prior_posterior_draws(
    posterior_samples = Vandendriessche_samples,
    group = data %>%
      filter(reference == "Vandendriessche et al. 2007" & t != 0) %>%
      droplevels() %>%
      select(species),
    parameters = c("log_alpha_s[species]",
                   "log_mu_s[species]",
                   "log_tau_s[species]",
                   "epsilon", "lambda", "theta"),
    format = "long"
    ) %>%
  prior_posterior_plot(group_name = "species") +
  scale_x_continuous(
    labels = scales::label_number(style_negative = "minus")
  ) +
  labs(title = "Macroalgal model") +
  coord_cartesian(expand = FALSE) +
  mytheme +
  theme(axis.line.y = element_blank(),
        axis.ticks.y = element_blank(),
        axis.text.y = element_blank(),
        axis.title = element_blank())

Vandendriessche_prior_posterior_temperature <- Vandendriessche_prior %>% 
  prior_posterior_draws(
    posterior_samples = Vandendriessche_samples,
    group = data %>%
      filter(reference == "Vandendriessche et al. 2007" & t != 0) %>%
      droplevels() %>%
      mutate(
        temperature = treatment %>% 
          str_extract("\\d+°C") %>% fct(),
        grazing = if_else(
          treatment %>% str_detect("Grazed"),
          0.5, -0.5
        )
      ) %>%
      select(temperature),
    parameters = c("log_mu_t[temperature]", "beta_mu[temperature]",
                   "log_tau_t[temperature]", "beta_tau[temperature]"),
    format = "long"
    ) %>%
  prior_posterior_plot(group_name = "temperature") +
  scale_x_continuous(
    labels = scales::label_number(style_negative = "minus")
  ) +
  coord_cartesian(expand = FALSE) +
  mytheme +
  theme(axis.line.y = element_blank(),
        axis.ticks.y = element_blank(),
        axis.text.y = element_blank(),
        axis.title = element_blank())

Vandendriessche_prior_posterior_replicate <- Vandendriessche_prior %>% 
  prior_posterior_draws(
    posterior_samples = Vandendriessche_samples,
    group = data %>%
      filter(reference == "Vandendriessche et al. 2007" & t != 0) %>%
      droplevels() %>%
      select(replicate),
    parameters = c("log_alpha_r[replicate]",
                   "log_mu_r[replicate]",
                   "log_tau_r[replicate]"),
    format = "long"
    ) %>%
  prior_posterior_plot(group_name = "replicate", ridges = TRUE) +
  scale_x_continuous(
    labels = scales::label_number(style_negative = "minus")
  ) +
  coord_cartesian(expand = FALSE) +
  mytheme +
  theme(axis.line.y = element_blank(),
        axis.ticks.y = element_blank(),
        axis.text.y = element_blank(),
        axis.title = element_blank())

Vandendriessche_k_prior_posterior_species <- Vandendriessche_k_prior %>% 
  prior_posterior_draws(
    posterior_samples = Vandendriessche_k_samples,
    group = data %>%
      filter(reference == "Vandendriessche et al. 2007" & t != 0) %>%
      droplevels() %>%
      select(species),
    parameters = c("log_k_s[species]", "sigma"),
    format = "long"
    ) %>%
  prior_posterior_plot(group_name = "species") +
  scale_x_continuous(
    labels = scales::label_number(style_negative = "minus")
  ) +
  labs(title = "Conventional model") +
  coord_cartesian(expand = FALSE) +
  mytheme +
  theme(axis.line.y = element_blank(),
        axis.ticks.y = element_blank(),
        axis.text.y = element_blank(),
        axis.title = element_blank())

Vandendriessche_k_prior_posterior_temperature <- Vandendriessche_k_prior %>% 
  prior_posterior_draws(
    posterior_samples = Vandendriessche_k_samples,
    group = data %>%
      filter(reference == "Vandendriessche et al. 2007" & t != 0) %>%
      droplevels() %>%
      mutate(
        temperature = treatment %>% 
          str_extract("\\d+°C") %>% fct(),
        grazing = if_else(
          treatment %>% str_detect("Grazed"),
          0.5, -0.5
        )
      ) %>%
      select(temperature),
    parameters = c("log_k_t[temperature]", "beta[temperature]"),
    format = "long"
    ) %>%
  prior_posterior_plot(group_name = "temperature") +
  scale_x_continuous(
    labels = scales::label_number(style_negative = "minus")
  ) +
  coord_cartesian(expand = FALSE) +
  mytheme +
  theme(axis.line.y = element_blank(),
        axis.ticks.y = element_blank(),
        axis.text.y = element_blank(),
        axis.title = element_blank())

Vandendriessche_k_prior_posterior_replicate <- Vandendriessche_k_prior %>% 
  prior_posterior_draws(
    posterior_samples = Vandendriessche_k_samples,
    group = data %>%
      filter(reference == "Vandendriessche et al. 2007" & t != 0) %>%
      droplevels() %>%
      select(replicate),
    parameters = c("log_k_r[replicate]"),
    format = "long"
    ) %>%
  prior_posterior_plot(group_name = "replicate", ridges = TRUE) +
  scale_x_continuous(
    labels = scales::label_number(style_negative = "minus")
  ) +
  coord_cartesian(expand = FALSE) +
  mytheme +
  theme(axis.line.y = element_blank(),
        axis.ticks.y = element_blank(),
        axis.text.y = element_blank(),
        axis.title = element_blank())

Vandendriessche_prior_posterior <- 
  ( Vandendriessche_prior_posterior_species / 
      Vandendriessche_prior_posterior_temperature / 
      Vandendriessche_prior_posterior_replicate / 
      Vandendriessche_k_prior_posterior_species /
      ( Vandendriessche_k_prior_posterior_temperature | 
          Vandendriessche_k_prior_posterior_replicate ) ) +
  plot_layout(heights = c(3/4, 1, 3/4, 1/6, 3/4))

Vandendriessche_prior_posterior %>%
  ggsave(filename = "Vandendriessche_prior_posterior.pdf", path = "Plots",
         device = cairo_pdf, width = 40, height = 60, units = "cm")

# 6.6.6 Parameter distributions ####
# Macroalgal model species and treatment parameters
Vandendriessche_prior_posterior <- Vandendriessche_prior %>% 
  prior_posterior_draws(
    posterior_samples = Vandendriessche_samples,
    group = data %>%
      filter(reference == "Vandendriessche et al. 2007") %>%
      droplevels() %>%
      mutate(
        temperature = treatment %>% 
          str_extract("\\d+°C") %>% fct(),
        grazing = if_else(
          treatment %>% str_detect("Grazed"),
          0.5, -0.5
        )
      ) %>%
      select(species, temperature),
    parameters = c("log_alpha_s[species]", "log_mu_s[species]", 
                   "log_mu_t[temperature]", "beta_mu[temperature]",
                   "log_tau_s[species]", "log_tau_t[temperature]",
                   "beta_tau[temperature]", "epsilon", "lambda", "theta"),
    format = "short"
  ) %>%
  mutate( # Predict for species and treatments averaged over replicates
    alpha = exp( log_alpha_s ),
    mu_Grazed = exp( log_mu_s + log_mu_t + beta_mu * 0.5 ),
    mu_Control = exp( log_mu_s + log_mu_t + beta_mu * -0.5 ),
    tau_Grazed = exp( log_tau_s + log_tau_t + beta_tau * 0.5 ),
    tau_Control = exp( log_tau_s + log_tau_t + beta_tau * -0.5 )
  ) %>%
  pivot_longer(cols = c(starts_with("mu"), starts_with("tau")),
               names_to = c("parameter", "grazing"),
               names_sep = "_") %>%
  pivot_wider(names_from = "parameter") %>%
  filter(species == "Fucus vesiculosus" & distribution == "prior" |
           distribution == "posterior") %>%
  mutate(
    species = if_else(
      distribution == "prior", "Prior", species
    ) %>% fct(),
    grazing = grazing %>% fct()
  ) %>%
  select(-c(distribution, starts_with("log"), starts_with("beta"))) %T>%
  print()

# Macroalgal model species parameters
Vandendriessche_prior_posterior_species <- Vandendriessche_prior %>% 
  prior_posterior_draws(
    posterior_samples = Vandendriessche_samples,
    group = data %>%
      filter(reference == "Vandendriessche et al. 2007") %>%
      droplevels() %>%
      select(species),
    parameters = c("log_alpha_s[species]", "log_mu_s[species]", 
                   "log_tau_s[species]", "epsilon", "lambda", "theta"),
    format = "short"
  ) %>%
  mutate( # Predict for species averaged over treatments
    across(
      starts_with("log"), 
      ~ exp(.x), .names = "{gsub('^log_|_s$', '', .col)}"
    ),
  ) %>%
  filter(species == "Fucus vesiculosus" & distribution == "prior" |
           distribution == "posterior") %>%
  mutate(
    species = if_else(
      distribution == "prior", "Prior", species
    ) %>% fct()
  ) %>%
  select(-c(distribution, starts_with("log"))) %T>%
  print()

# Macroalgal model temperature and grazing effect parameters
Vandendriessche_prior_posterior_beta <- Vandendriessche_prior %>% 
  prior_posterior_draws(
    posterior_samples = Vandendriessche_samples,
    group = data %>%
      filter(reference == "Vandendriessche et al. 2007") %>%
      droplevels() %>%
      mutate(
        temperature = treatment %>% 
          str_extract("\\d+°C") %>% fct(),
        grazing = if_else(
          treatment %>% str_detect("Grazed"),
          0.5, -0.5
        )
      ) %>%
      select(temperature),
    parameters = c("log_mu_t[temperature]", "beta_mu[temperature]",
                   "log_tau_t[temperature]", "beta_tau[temperature]"),
    format = "short"
  ) %>%
  filter(temperature == "10°C" & distribution == "prior" |
           distribution == "posterior") %>%
  mutate(
    temperature = if_else(
      distribution == "prior", "Prior", temperature
    ) %>% fct()
  ) %>%
  select(-distribution) %T>%
  print()

# Macroalgal model replicate parameters
# Replicates blow up R if added in prior_posterior_draws(parameters) alongside
# species and temperatures because prior_posterior_draws tries to cross all
# factors. Replicates are in fact unique per species-treatment combination so
# as with any nested factors I need to left_join as before for Frontier et al.
Vandendriessche_prior_posterior_replicate <- data %>% # Get nesting from data
  filter(reference == "Vandendriessche et al. 2007") %>%
  droplevels() %>%
  mutate(
    temperature = treatment %>% 
      str_extract("\\d+°C") %>% fct(),
    grazing = if_else(
      treatment %>% str_detect("Grazed"),
      0.5, -0.5
    )
  ) %>%
  distinct(species, temperature, grazing, replicate) %>% # 52 reps = 52 combinations
  left_join(
    Vandendriessche_prior %>% 
      prior_posterior_draws(
        posterior_samples = Vandendriessche_samples,
        group = data %>%
          filter(reference == "Vandendriessche et al. 2007" & t != 0) %>%
          droplevels() %>%
          mutate(
            temperature = treatment %>% 
              str_extract("\\d+°C") %>% fct(),
            grazing = if_else(
              treatment %>% str_detect("Grazed"),
              0.5, -0.5
            )
          ) %>%
          select(species, temperature), # species and temperature are crossed in the experiment
        parameters = c("log_alpha_s[species]", "log_mu_s[species]", 
                       "log_mu_t[temperature]", "beta_mu[temperature]",
                       "log_tau_s[species]", "log_tau_t[temperature]",
                       "beta_tau[temperature]", "epsilon", "lambda", "theta"),
        format = "short"
      ),
    by = c("species", "temperature"),
    relationship = "many-to-many"
  ) %>%
  left_join(
    Vandendriessche_prior %>% 
      prior_posterior_draws(
        posterior_samples = Vandendriessche_samples,
        group = data %>%
          filter(reference == "Vandendriessche et al. 2007" & t != 0) %>%
          droplevels() %>%
          select(replicate),
        parameters = c("log_alpha_r[replicate]", 
                       "log_mu_r[replicate]",
                       "log_tau_r[replicate]"),
        format = "short"
      ),
    by = c("replicate", "distribution", ".chain", ".iteration", ".draw"),
    relationship = "many-to-many"
  ) %>%
  mutate(
    alpha = exp( log_alpha_s + log_alpha_r ),
    mu = exp( log_mu_s + log_mu_t + beta_mu * grazing + log_mu_r ),
    tau = exp( log_tau_s + log_tau_t + beta_tau * grazing + log_tau_r )
  ) %>% # pick one replicate for grazing and one for control for prior
  filter(replicate %in% c("1", "2") & distribution == "prior" |
           distribution == "posterior") %>%
  mutate(
    species = if_else(
      distribution == "prior", "Prior", species
    ) %>% fct(),
    temperature = if_else(
      distribution == "prior", "Prior", temperature
    ) %>% fct(),
    replicate = if_else(
      distribution == "prior", "Prior", replicate
    ) %>% fct()
  ) %>%
  select(-c(distribution, starts_with("log"), starts_with("beta"))) %T>%
  print()
  
# Conventional model species and treatment parameters
Vandendriessche_k_prior_posterior <- Vandendriessche_k_prior %>% 
  prior_posterior_draws(
    posterior_samples = Vandendriessche_k_samples,
    group = data %>%
      filter(reference == "Vandendriessche et al. 2007") %>%
      droplevels() %>%
      mutate(
        temperature = treatment %>% 
          str_extract("\\d+°C") %>% fct(),
        grazing = if_else(
          treatment %>% str_detect("Grazed"),
          0.5, -0.5
        )
      ) %>%
      select(species, temperature),
    parameters = c("log_k_s[species]", "log_k_t[temperature]", 
                   "beta[temperature]", "sigma"),
    format = "short"
  ) %>%
  mutate( # Predict for species and treatments averaged over replicates
    k_Grazed = exp( log_k_s + log_k_t + beta * 0.5 ),
    k_Control = exp( log_k_s + log_k_t + beta * -0.5 )
  ) %>%
  pivot_longer(cols = starts_with("k"),
               names_to = "grazing",
               values_to = "k",
               names_prefix = "k_") %>%
  filter(species == "Fucus vesiculosus" & distribution == "prior" |
           distribution == "posterior") %>%
  mutate(
    species = if_else(
      distribution == "prior", "Prior", species
    ) %>% fct(),
    grazing = grazing %>% fct()
  ) %>%
  select(-c(distribution, starts_with("log"), starts_with("beta"))) %T>%
  print()

# Conventional model species parameters
Vandendriessche_k_prior_posterior_species <- Vandendriessche_k_prior %>% 
  prior_posterior_draws(
    posterior_samples = Vandendriessche_k_samples,
    group = data %>%
      filter(reference == "Vandendriessche et al. 2007") %>%
      droplevels() %>%
      select(species),
    parameters = c("log_k_s[species]", "sigma"),
    format = "short"
  ) %>%
  mutate(k = exp( log_k_s )) %>%
  filter(species == "Fucus vesiculosus" & distribution == "prior" |
           distribution == "posterior") %>%
  mutate(
    species = if_else(
      distribution == "prior", "Prior", species
    ) %>% fct()
  ) %>%
  select(-c(distribution, starts_with("log"))) %T>%
  print()

# Conventional model temperature and grazing effect parameters
Vandendriessche_k_prior_posterior_beta <- Vandendriessche_k_prior %>% 
  prior_posterior_draws(
    posterior_samples = Vandendriessche_k_samples,
    group = data %>%
      filter(reference == "Vandendriessche et al. 2007") %>%
      droplevels() %>%
      mutate(
        temperature = treatment %>% 
          str_extract("\\d+°C") %>% fct(),
        grazing = if_else(
          treatment %>% str_detect("Grazed"),
          0.5, -0.5
        )
      ) %>%
      select(temperature),
    parameters = c("log_k_t[temperature]", "beta[temperature]"),
    format = "short"
  ) %>%
  filter(temperature == "10°C" & distribution == "prior" |
           distribution == "posterior") %>%
  mutate(
    temperature = if_else(
      distribution == "prior", "Prior", temperature
    ) %>% fct()
  ) %>%
  select(-distribution) %T>%
  print()

# Conventional model replicate parameters
Vandendriessche_k_prior_posterior_replicate <- data %>% # Get nesting from data
  filter(reference == "Vandendriessche et al. 2007") %>%
  droplevels() %>%
  mutate(
    temperature = treatment %>% 
      str_extract("\\d+°C") %>% fct(),
    grazing = if_else(
      treatment %>% str_detect("Grazed"),
      0.5, -0.5
    )
  ) %>%
  distinct(species, temperature, grazing, replicate) %>% # 52 reps = 52 combinations
  left_join(
    Vandendriessche_k_prior %>% 
      prior_posterior_draws(
        posterior_samples = Vandendriessche_k_samples,
        group = data %>%
          filter(reference == "Vandendriessche et al. 2007" & t != 0) %>%
          droplevels() %>%
          mutate(
            temperature = treatment %>% 
              str_extract("\\d+°C") %>% fct(),
            grazing = if_else(
              treatment %>% str_detect("Grazed"),
              0.5, -0.5
            )
          ) %>%
          select(species, temperature), # species and temperature are crossed in the experiment
        parameters = c("log_k_s[species]", "log_k_t[temperature]", 
                       "beta[temperature]", "sigma"),
        format = "short"
      ),
    by = c("species", "temperature"),
    relationship = "many-to-many"
  ) %>%
  left_join(
    Vandendriessche_k_prior %>% 
      prior_posterior_draws(
        posterior_samples = Vandendriessche_k_samples,
        group = data %>%
          filter(reference == "Vandendriessche et al. 2007" & t != 0) %>%
          droplevels() %>%
          select(replicate),
        parameters = c("log_k_r[replicate]"),
        format = "short"
      ),
    by = c("replicate", "distribution", ".chain", ".iteration", ".draw"),
    relationship = "many-to-many"
  ) %>%
  mutate(
    k = exp( log_k_s + log_k_t + beta * grazing + log_k_r )
  ) %>%
  filter(replicate %in% c("1", "2") & distribution == "prior" |
           distribution == "posterior") %>%
  mutate(
    species = if_else(
      distribution == "prior", "Prior", species
    ) %>% fct(),
    temperature = if_else(
      distribution == "prior", "Prior", temperature
    ) %>% fct(),
    replicate = if_else(
      distribution == "prior", "Prior", replicate
    ) %>% fct()
  ) %>%
  select(-c(distribution, starts_with("log"), starts_with("beta"))) %T>%
  print()

# Save parameter distributions
Vandendriessche_prior_posterior %>%
  write_rds(here("RDS", "Vandendriessche_prior_posterior.rds"))
Vandendriessche_prior_posterior_species %>%
  write_rds(here("RDS", "Vandendriessche_prior_posterior_species.rds"))
Vandendriessche_prior_posterior_beta %>%
  write_rds(here("RDS", "Vandendriessche_prior_posterior_beta.rds"))
Vandendriessche_prior_posterior_replicate %>%
  write_rds(here("RDS", "Vandendriessche_prior_posterior_replicate.rds"))

Vandendriessche_k_prior_posterior %>%
  write_rds(here("RDS", "Vandendriessche_k_prior_posterior.rds"))
Vandendriessche_k_prior_posterior_species %>%
  write_rds(here("RDS", "Vandendriessche_k_prior_posterior_species.rds"))
Vandendriessche_k_prior_posterior_beta %>%
  write_rds(here("RDS", "Vandendriessche_k_prior_posterior_beta.rds"))
Vandendriessche_k_prior_posterior_replicate %>%
  write_rds(here("RDS", "Vandendriessche_k_prior_posterior_replicate.rds"))

# 6.6.7 Continuous prediction ####
# Macroalgal model species and treatment predictions
Vandendriessche_prediction <- Vandendriessche_prior_posterior %>%
  nest(.by = c(species, temperature, grazing), .key = "prior_posterior") %>%
  left_join(
    data %>%
      filter(reference == "Vandendriessche et al. 2007") %>%
      droplevels() %>%
      separate(treatment, into = c("temperature", "grazing"), sep = " ") %>%
      mutate(temperature = temperature %>% fct(),
             grazing = grazing %>% fct()) %>%
      select(species, temperature, grazing, t) %>%
      nest(.by = c(species, temperature, grazing), .key = "t")
  ) %>%
  mutate(
    predictor = t %>% 
      map(
        ~if(is.null(.x)){
          data %>%
            filter(reference == "Vandendriessche et al. 2007") %$%
            seq(min(t), max(t), length.out = 150)
        } else {
          .x %$% 
            seq(min(t), max(t), length.out = 150)
        }
      ),
    prediction = map2(
      prior_posterior, predictor,
      ~.x %>% 
        slice( rep( 1:n() , each = length(.y) ) ) %>%
        mutate(
          t = rep( .y , times = nrow(.x) ),
          m_mu = exp(
            t * alpha - ( alpha + tau ) * mu / 5 * (
              log1p_exp( 5 / mu * ( t - mu ) ) -
                log1p_exp( -5 )
            )
          ),
          k = ( alpha + tau ) / ( 1 + exp( 5 / mu * ( t - mu ) ) ) - tau,
          nu = ( epsilon - theta ) * exp( -lambda * t ) + theta,
          m = rbetapr( n() , m_mu * ( 1 + nu ) , 2 + nu )
        ) %>%
        group_by(t) %>%
        median_qi(m_mu, k, nu, m, .width = c(.5, .8, .9)) %T>%
        print()
    )
  ) %>% 
  select(-c(prior_posterior, t, predictor)) %>%
  unnest(prediction) %T>%
  print()

# Macroalgal model replicate predictions
Vandendriessche_prediction_replicate <- Vandendriessche_prior_posterior_replicate %>%
  mutate(grazing = if_else(grazing == 0.5, "Grazed", "Control") %>% fct()) %>%
  nest(.by = c(species, temperature, grazing, replicate), .key = "prior_posterior") %>%
  left_join(
    data %>%
      filter(reference == "Vandendriessche et al. 2007") %>%
      droplevels() %>%
      separate(treatment, into = c("temperature", "grazing"), sep = " ") %>%
      mutate(temperature = temperature %>% fct(),
             grazing = grazing %>% fct()) %>%
      select(species, temperature, grazing, replicate, t) %>%
      nest(.by = c(species, temperature, grazing, replicate), .key = "t")
  ) %>%
  mutate(
    predictor = t %>% 
      map(
        ~if(is.null(.x)){
          data %>%
            filter(reference == "Vandendriessche et al. 2007") %$%
            seq(min(t), max(t), length.out = 150)
        } else {
          .x %$% 
            seq(min(t), max(t), length.out = 150)
        }
      ),
    prediction = map2(
      prior_posterior, predictor,
      ~.x %>% 
        slice( rep( 1:n() , each = length(.y) ) ) %>%
        mutate(
          t = rep( .y , times = nrow(.x) ),
          m_mu = exp(
            t * alpha - ( alpha + tau ) * mu / 5 * (
              log1p_exp( 5 / mu * ( t - mu ) ) -
                log1p_exp( -5 )
            )
          ),
          k = ( alpha + tau ) / ( 1 + exp( 5 / mu * ( t - mu ) ) ) - tau
        ) %>%
        group_by(t) %>%
        median_qi(m_mu, k, .width = c(.5, .8, .9)) %T>%
        print()
    )
  ) %>% 
  select(-c(prior_posterior, t, predictor)) %>%
  unnest(prediction) %T>%
  print()

# Conventional model species and treatment predictions
Vandendriessche_k_prediction <- Vandendriessche_k_prior_posterior %>%
  nest(.by = c(species, temperature, grazing), .key = "prior_posterior") %>%
  left_join(
    data %>%
      filter(reference == "Vandendriessche et al. 2007") %>%
      droplevels() %>%
      separate(treatment, into = c("temperature", "grazing"), sep = " ") %>%
      mutate(temperature = temperature %>% fct(),
             grazing = grazing %>% fct()) %>%
      select(species, temperature, grazing, t) %>%
      nest(.by = c(species, temperature, grazing), .key = "t")
  ) %>%
  mutate(
    predictor = t %>% 
      map(
        ~if(is.null(.x)){
          data %>%
            filter(reference == "Vandendriessche et al. 2007") %$%
            seq(min(t), max(t), length.out = 150)
        } else {
          .x %$% 
            seq(min(t), max(t), length.out = 150)
        }
      ),
    prediction = map2(
      prior_posterior, predictor,
      ~.x %>% 
        slice( rep( 1:n() , each = length(.y) ) ) %>%
        mutate(
          t = rep( .y , times = nrow(.x) ),
          m_mu = exp( -k * t ),
          m = rnorm( n() , m_mu , sigma )
        ) %>%
        group_by(t) %>%
        median_qi(m_mu, m, .width = c(.5, .8, .9)) %T>%
        print()
    )
  ) %>% 
  select(-c(prior_posterior, t, predictor)) %>%
  unnest(prediction) %T>%
  print()

# Conventional model replicate predictions
Vandendriessche_k_prediction_replicate <- Vandendriessche_k_prior_posterior_replicate %>%
  mutate(grazing = if_else(grazing == 0.5, "Grazed", "Control") %>% fct()) %>%
  nest(.by = c(species, temperature, grazing, replicate), .key = "prior_posterior") %>%
  left_join(
    data %>%
      filter(reference == "Vandendriessche et al. 2007") %>%
      droplevels() %>%
      separate(treatment, into = c("temperature", "grazing"), sep = " ") %>%
      mutate(temperature = temperature %>% fct(),
             grazing = grazing %>% fct()) %>%
      select(species, temperature, grazing, replicate, t) %>%
      nest(.by = c(species, temperature, grazing, replicate), .key = "t")
  ) %>%
  mutate(
    predictor = t %>% 
      map(
        ~if(is.null(.x)){
          data %>%
            filter(reference == "Vandendriessche et al. 2007") %$%
            seq(min(t), max(t), length.out = 150)
        } else {
          .x %$% 
            seq(min(t), max(t), length.out = 150)
        }
      ),
    prediction = map2(
      prior_posterior, predictor,
      ~.x %>% 
        slice( rep( 1:n() , each = length(.y) ) ) %>%
        mutate(
          t = rep( .y , times = nrow(.x) ),
          m_mu = exp( -k * t )
        ) %>%
        group_by(t) %>%
        median_qi(m_mu, .width = c(.5, .8, .9)) %T>%
        print()
    )
  ) %>% 
  select(-c(prior_posterior, t, predictor)) %>%
  unnest(prediction) %T>%
  print()

# Save predictions
Vandendriessche_prediction %>%
  write_rds(here("RDS", "Vandendriessche_prediction.rds"))
Vandendriessche_prediction_replicate %>%
  write_rds(here("RDS", "Vandendriessche_prediction_replicate.rds"))

Vandendriessche_k_prediction %>%
  write_rds(here("RDS", "Vandendriessche_k_prediction.rds"))
Vandendriessche_k_prediction_replicate %>%
  write_rds(here("RDS", "Vandendriessche_k_prediction_replicate.rds"))

# 6.6.8 Visualisation of predictions ####
# Viusalise mean predictions
data %>%
  filter(reference == "Vandendriessche et al. 2007") %>%
  droplevels() %>%
  separate(treatment, into = c("temperature", "grazing"), sep = " ") %>%
  mutate(temperature = temperature %>% fct(),
         grazing = grazing %>% fct()) %>%
  ggplot() +
    geom_point(aes(t, m, colour = temperature), shape = 16, alpha = 0.5) +
    geom_line(data = Vandendriessche_prediction %>%
                filter(species != "Prior"),
              aes(t, m_mu, colour = temperature)) +
    geom_ribbon(data = Vandendriessche_prediction %>%
                  filter(species != "Prior"),
                aes(t, ymin = m_mu.lower, ymax = m_mu.upper, 
                    alpha = factor(.width), fill = temperature)) +
    scale_alpha_manual(values = c(0.5, 0.4, 0.3), guide = "none") +
    facet_nested(temperature ~ species + grazing, nest_line = TRUE) +
    mytheme

data %>%
  filter(reference == "Vandendriessche et al. 2007") %>%
  droplevels() %>%
  separate(treatment, into = c("temperature", "grazing"), sep = " ") %>%
  mutate(temperature = temperature %>% fct(),
         grazing = grazing %>% fct()) %>%
  ggplot() +
    geom_point(aes(t, m, colour = temperature), shape = 16, alpha = 0.5) +
    geom_line(data = Vandendriessche_k_prediction %>%
                filter(species != "Prior"),
              aes(t, m_mu, colour = temperature)) +
    geom_ribbon(data = Vandendriessche_k_prediction %>%
                  filter(species != "Prior"),
                aes(t, ymin = m_mu.lower, ymax = m_mu.upper, 
                    alpha = factor(.width), fill = temperature)) +
    scale_alpha_manual(values = c(0.5, 0.4, 0.3), guide = "none") +
    facet_nested(temperature ~ species + grazing, nest_line = TRUE) +
    mytheme

# Visualise predictions of new observations
data %>%
  filter(reference == "Vandendriessche et al. 2007") %>%
  droplevels() %>%
  separate(treatment, into = c("temperature", "grazing"), sep = " ") %>%
  mutate(temperature = temperature %>% fct(),
         grazing = grazing %>% fct()) %>%
  ggplot() +
    geom_point(aes(t, m, colour = temperature), shape = 16, alpha = 0.5) +
    geom_line(data = Vandendriessche_prediction %>%
                filter(species != "Prior"),
              aes(t, m, colour = temperature)) +
    geom_ribbon(data = Vandendriessche_prediction %>%
                  filter(species != "Prior"),
                aes(t, ymin = m.lower, ymax = m.upper, 
                    alpha = factor(.width), fill = temperature)) +
    scale_alpha_manual(values = c(0.5, 0.4, 0.3), guide = "none") +
    facet_nested(temperature ~ species + grazing, nest_line = TRUE) +
    mytheme

data %>%
  filter(reference == "Vandendriessche et al. 2007") %>%
  droplevels() %>%
  separate(treatment, into = c("temperature", "grazing"), sep = " ") %>%
  mutate(temperature = temperature %>% fct(),
         grazing = grazing %>% fct()) %>%
  ggplot() +
    geom_point(aes(t, m, colour = temperature), shape = 16, alpha = 0.5) +
    geom_line(data = Vandendriessche_k_prediction %>%
                filter(species != "Prior"),
              aes(t, m, colour = temperature)) +
    geom_ribbon(data = Vandendriessche_k_prediction %>%
                  filter(species != "Prior"),
                aes(t, ymin = m.lower, ymax = m.upper, 
                    alpha = factor(.width), fill = temperature)) +
    scale_alpha_manual(values = c(0.5, 0.4, 0.3), guide = "none") +
    facet_nested(temperature ~ species + grazing, nest_line = TRUE) +
    mytheme

# Visualise predictions of replicates
data %>%
  filter(reference == "Vandendriessche et al. 2007") %>%
  droplevels() %>%
  separate(treatment, into = c("temperature", "grazing"), sep = " ") %>%
  mutate(temperature = temperature %>% fct(),
         grazing = grazing %>% fct()) %>%
  ggplot() +
    geom_point(aes(t, m, colour = temperature), shape = 16, alpha = 0.5) +
    geom_line(data = Vandendriessche_prediction_replicate %>%
                filter(species != "Prior"),
              aes(t, m_mu, colour = temperature, group = replicate)) +
    geom_ribbon(data = Vandendriessche_prediction_replicate %>%
                  filter(species != "Prior"),
                aes(t, ymin = m_mu.lower, ymax = m_mu.upper, 
                    alpha = factor(.width), fill = temperature,
                    group = interaction(replicate, .width))) +
    scale_alpha_manual(values = c(0.5, 0.4, 0.3), guide = "none") +
    facet_nested(temperature ~ species + grazing, nest_line = TRUE) +
    mytheme

data %>%
  filter(reference == "Vandendriessche et al. 2007") %>%
  droplevels() %>%
  separate(treatment, into = c("temperature", "grazing"), sep = " ") %>%
  mutate(temperature = temperature %>% fct(),
         grazing = grazing %>% fct()) %>%
  ggplot() +
    geom_point(aes(t, m, colour = temperature), shape = 16, alpha = 0.5) +
    geom_line(data = Vandendriessche_k_prediction_replicate %>%
                filter(species != "Prior"),
              aes(t, m_mu, colour = temperature, group = replicate)) +
    # geom_ribbon(data = Vandendriessche_k_prediction_replicate %>%
    #               filter(species != "Prior"),
    #             aes(t, ymin = .lower, ymax = .upper, 
    #                 alpha = factor(.width), fill = temperature,
    #                 group = interaction(replicate, .width))) +
    scale_alpha_manual(values = c(0.5, 0.4, 0.3), guide = "none") +
    facet_nested(temperature ~ species + grazing, nest_line = TRUE) +
    mytheme

# Visualise predictions of time-variant k
Vandendriessche_prediction %>%
  filter(species != "Prior") %>%
  ggplot() +
    geom_line(aes(t, k, colour = temperature)) +
    geom_ribbon(aes(t, ymin = k.lower, ymax = k.upper, 
                    alpha = factor(.width), fill = temperature)) +
    scale_alpha_manual(values = c(0.5, 0.4, 0.3), guide = "none") +
    facet_nested(temperature ~ species + grazing, nest_line = TRUE) +
    mytheme

Vandendriessche_prediction_replicate %>%
  filter(species != "Prior") %>%
  ggplot() +
    geom_line(aes(t, k, colour = temperature, group = replicate)) +
    facet_nested(temperature ~ species + grazing, nest_line = TRUE) +
    mytheme

# Visualise predictions of time-variant nu (global)
Vandendriessche_prediction %>%
  filter(species == "Fucus vesiculosus") %>%
  ggplot() +
    geom_line(aes(t, nu)) +
    geom_ribbon(aes(t, ymin = nu.lower, ymax = nu.upper, 
                    alpha = factor(.width))) +
    scale_alpha_manual(values = c(0.5, 0.4, 0.3), guide = "none") +
    mytheme

# Clean up
rm( list = ls( pattern = "Vandendriessche" ) )
gc()

# 7. Tables ####
# 7.1 Tables 1 and 2 ####
# 7.1.1 Load data ####
# Example 1
Brouwer_prior_posterior <- here("RDS", "Brouwer_prior_posterior.rds") %>%
  read_rds()
Brouwer_k_prior_posterior <- here("RDS", "Brouwer_k_prior_posterior.rds") %>%
  read_rds()

# Example 2
Hamersley_prior_posterior <- here("RDS", "Hamersley_prior_posterior.rds") %>%
  read_rds()
Hamersley_k_prior_posterior <- here("RDS", "Hamersley_k_prior_posterior.rds") %>%
  read_rds()

# Example 3
Bettignies_prior_posterior <- here("RDS", "Bettignies_prior_posterior.rds") %>%
  read_rds()
Bettignies_k_prior_posterior <- here("RDS", "Bettignies_k_prior_posterior.rds") %>%
  read_rds()

# Example 4
Bourguès_prior_posterior <- here("RDS", "Bourguès_prior_posterior.rds") %>%
  read_rds()
Bourguès_k_prior_posterior <- here("RDS", "Bourguès_k_prior_posterior.rds") %>%
  read_rds()

# Example 5
Frontier2021_prior_posterior <- here("RDS", "Frontier2021_prior_posterior.rds") %>%
  read_rds()
Frontier2021_k_prior_posterior <- here("RDS", "Frontier2021_k_prior_posterior.rds") %>%
  read_rds()

# Example 6
Frontier2022_prior_posterior <- here("RDS", "Frontier2022_prior_posterior.rds") %>%
  read_rds()
Frontier2022_k_prior_posterior <- here("RDS", "Frontier2022_k_prior_posterior.rds") %>%
  read_rds()

# Example 7
Vandendriessche_prior_posterior <- here("RDS", "Vandendriessche_prior_posterior.rds") %>%
  read_rds()
Vandendriessche_k_prior_posterior <- here("RDS", "Vandendriessche_k_prior_posterior.rds") %>%
  read_rds()

# 7.1.2 Merge and contrast ####
# Example 1
Brouwer_parameters <- Brouwer_prior_posterior %>%
  full_join(Brouwer_k_prior_posterior) %T>%
  print()

# Example 2
Hamersley_parameters <- Hamersley_prior_posterior %>%
  full_join(Hamersley_k_prior_posterior) %T>%
  print()

# Example 3
Bettignies_parameters <- Bettignies_prior_posterior %>%
  full_join(Bettignies_k_prior_posterior) %T>%
  print()

# Example 4
Bourguès_parameters <- Bourguès_prior_posterior %>%
  full_join(Bourguès_k_prior_posterior) %T>%
  print()

# Example 5
Frontier2021_parameters <- Frontier2021_prior_posterior %>% select(-beta) %>%
  full_join(Frontier2021_k_prior_posterior %>% select(-beta)) %T>%
  print()

# Example 6
Frontier2022_parameters <- Frontier2022_prior_posterior %>% select(-beta) %>%
  full_join(Frontier2022_k_prior_posterior %>% select(-beta)) %T>%
  print()

# Example 7
Vandendriessche_parameters <- Vandendriessche_prior_posterior %>%
  full_join(Vandendriessche_k_prior_posterior) %T>%
  print()

# Combined
data_mean_sd %>% distinct(reference, species)
data %>% distinct(reference, species)

parameters <- bind_rows( # Combine into one tibble
  Brouwer = Brouwer_parameters %>% 
    mutate(species = "Desmarestia anceps" %>% fct()),
  Hamersley = Hamersley_parameters %>% 
    mutate(species = "Macrocystis pyrifera" %>% fct()),
  Bettignies = Bettignies_parameters %>% 
    select(-delta) %>%
    mutate(species = "Laminaria hyperborea" %>% fct()),
  Bourguès = Bourguès_parameters %>%
    mutate(species = "Ulvaria obscura" %>% fct()),
  Frontier2021 = Frontier2021_parameters %>%
    mutate(treatment = treatment %>% fct()),
  Frontier2022 = Frontier2022_parameters %>%
    mutate(treatment = treatment %>% fct()),
  Vandendriessche = Vandendriessche_parameters %>%
    unite(col = "treatment", temperature, grazing, sep = " ") %>%
    mutate(treatment = treatment %>% fct()),
  .id = "reference"
) %>%
  mutate(
    reference = reference %>% 
      fct_relevel("Brouwer", "Hamersley", "Bettignies", "Bourguès",
                  "Frontier2021", "Frontier2022"),
    tau_k_diff = tau - k,
    tau_k_ratio = tau / k,
    t0.5 = log(2)/k,
    t0.5_mu_diff = t0.5 - mu,
    t0.5_mu_ratio = t0.5 / mu 
  ) %T>%
  print()

# 7.1.3 Summarise ####
require(glue)
parameters_summary <- parameters %>% 
  mutate(k = k * 100, # I am converting exponential rates to % for readability
         alpha = alpha * 100,
         tau = tau * 100,
         tau_k_diff = tau_k_diff * 100,
         log_tau_k_ratio = log10(tau_k_ratio), # log ratios are more symmetric
         log_t0.5_mu_ratio = log10(t0.5_mu_ratio)) %>% 
  group_by(reference, species, treatment) %>%
  summarise(
    across(
      everything(), 
      list(
        mean = mean, 
        sd = sd, 
        median = median
      )
    ), 
    P_tau_k = mean( tau_k_diff > 0 ), # Probability that tau > k
    P_t0.5_mu = mean( t0.5_mu_diff > 0 ), # Probability that t0.5 > mu
    n = n()
  ) %>%
  ungroup() %>%
  mutate(
    across(
      where(is.numeric),
      ~ case_when(
        .x < 100 ~ signif(.x, 2),
        .x < 1e3 ~ signif(.x, 3),
        T ~ signif(.x, 4)
      )
    ),
    k = glue("{k_mean} ± {k_sd} ({k_median})"),
    t0.5 = glue("{t0.5_mean} ± {t0.5_sd} ({t0.5_median})"),
    alpha = glue("{alpha_mean} ± {alpha_sd} ({alpha_median})"),
    mu = glue("{mu_mean} ± {mu_sd} ({mu_median})"),
    tau = glue("{tau_mean} ± {tau_sd} ({tau_median})"),
    tau_k_diff = glue("{tau_k_diff_mean} ± {tau_k_diff_sd} ({tau_k_diff_median})"),
    t0.5_mu_diff = glue("{t0.5_mu_diff_mean} ± {t0.5_mu_diff_sd} ({t0.5_mu_diff_median})"),
    log_tau_k_ratio = glue("{log_tau_k_ratio_mean} ± {log_tau_k_ratio_sd}"),
    log_t0.5_mu_ratio = glue("{log_t0.5_mu_ratio_mean} ± {log_t0.5_mu_ratio_sd}")
  ) %>%
  select(!(contains("mean") | contains("sd") | contains("median"))) %T>%
  print(n = 70)

# 7.1.4 Split into tables ####
Table_1 <- parameters_summary %>%
  select(reference, species, treatment, k, alpha, mu, tau) %>%
  arrange(reference) %T>%
  print()
  
Table_2 <- parameters_summary %>%
  select(reference, species, treatment, log_tau_k_ratio, P_tau_k, 
         log_t0.5_mu_ratio, P_t0.5_mu) %>%
  arrange(reference) %T>%
  print()

# 7.1.5 Save ####
# Save tables as csv
Table_1 %>%
  write_csv(here("Tables", "Table_1.csv"))

Table_2 %>%
  write_csv(here("Tables", "Table_2.csv"))

# Save tables without prior and global as docx
require(officer)
read_docx() %>%
  body_add_table(
    value = Table_1 %>%
      filter(!species %in% c("Prior", "Global") &
               !treatment %in% c("Prior", "Global"))
  ) %>%
  print(target = here("Tables", "Table_1.docx"))

read_docx() %>%
  body_add_table(
    value = Table_2 %>%
      filter(!species %in% c("Prior", "Global") &
               !treatment %in% c("Prior", "Global"))
  ) %>%
  print(target = here("Tables", "Table_2.docx"))

# 7.2 Table S1 ####
# 7.2.1 Load data ####
param_rhat_ess <- here("Tables", "Diagnostic", "param_rhat_ess.csv") %>%
  read_csv() %>%
  mutate(model = model %>% fct_relevel("relative", "constant")) %>%
  arrange(reference, model) %T>%
  print()

param_loo <- here("Tables", "Diagnostic", "param_loo.csv") %>%
  read_csv() %>%
  mutate(model = model %>% fct_relevel("relative", "constant")) %T>%
  print()

# 7.2.2 Combine, round, summarise ####
Table_S1 <- param_rhat_ess %>%
  full_join(param_loo) %>%
  select(-contains("looic")) %>% # LOOIC is simply -2 * ELPD
  mutate(
    rhat_mean = rhat_mean %>% signif(3),
    across(
      c(rhat_sd, starts_with("ess"), 
        ends_with("diff"), ends_with("loo")),
      ~case_when(
        .x < 100 ~ signif(.x, 2),
        .x < 1e3 ~ signif(.x, 3),
        .x < 1e4 ~ signif(.x, 4),
        T ~ signif(.x, 5)
      )
    ),
    rhat = glue("{rhat_mean} ± {rhat_sd}"),
    ess = glue("{ess_mean} ± {ess_sd}"),
    elpd = glue("{elpd_loo} ± {se_elpd_loo}"),
    diff = glue("{elpd_diff} ± {se_diff}"),
    p = glue("{p_loo} ± {se_p_loo}")
  ) %>%
  select(reference, model, rhat, ess, elpd, diff, p) %T>%
  print()
  
# 7.2.3 Save ####
Table_S1 %>%
  write_csv(here("Tables", "Table_S1.csv"))

read_docx() %>%
  body_add_table(value = Table_S1) %>%
  print(target = here("Tables", "Table_S1.docx"))

# 7.3 Table S2 ####
# 7.3.1 Load data ####
lik_rhat_ess <- here("Tables", "Diagnostic", "lik_rhat_ess.csv") %>%
  read_csv() %>% 
  mutate(model = model %>% fct_relevel("betaprime", "gamma", "lognormal")) %>%
  arrange(reference, model) %T>%
  print()

lik_loo <- here("Tables", "Diagnostic", "lik_loo.csv") %>%
  read_csv() %>%
  mutate(model = model %>% fct_relevel("betaprime", "gamma", "lognormal")) %T>%
  print()

# 7.3.2 Combine, round, summarise ####
Table_S2 <- lik_rhat_ess %>%
  full_join(lik_loo) %>%
  select(-contains("looic")) %>%
  mutate(
    rhat_mean = rhat_mean %>% signif(3),
    across(
      c(rhat_sd, starts_with("ess"), 
        ends_with("diff"), ends_with("loo")),
      ~case_when(
        .x < 100 ~ signif(.x, 2),
        .x < 1e3 ~ signif(.x, 3),
        .x < 1e4 ~ signif(.x, 4),
        T ~ signif(.x, 5)
      )
    ),
    rhat = glue("{rhat_mean} ± {rhat_sd}"),
    ess = glue("{ess_mean} ± {ess_sd}"),
    elpd = glue("{elpd_loo} ± {se_elpd_loo}"),
    diff = glue("{elpd_diff} ± {se_diff}"),
    p = glue("{p_loo} ± {se_p_loo}")
  ) %>%
  select(reference, model, rhat, ess, elpd, diff, p) %T>%
  print()

# 7.3.3 Save ####
Table_S2 %>%
  write_csv(here("Tables", "Table_S2.csv"))

read_docx() %>%
  body_add_table(value = Table_S2) %>%
  print(target = here("Tables", "Table_S2.docx"))

# 7.4 Table S3 ####
# 7.4.1 Load data ####
het_rhat_ess <- here("Tables", "Diagnostic", "het_rhat_ess.csv") %>%
  read_csv() %>% 
  separate(model, into = c("likelihood", "variance"), sep = " ") %>%
  mutate(likelihood = likelihood %>% fct_relevel("betaprime"),
         variance = variance %>% fct_relevel("heteroskedastic")) %>%
  arrange(reference, likelihood, variance) %T>%
  print()

het_loo <- here("Tables", "Diagnostic", "het_loo.csv") %>%
  read_csv() %>% 
  separate(model, into = c("likelihood", "variance"), sep = "_") %>%
  mutate(likelihood = likelihood %>% fct_relevel("betaprime"),
         variance = variance %>% fct_relevel("heteroskedastic")) %T>%
  print()

# 7.4.2 Combine, round, summarise ####
Table_S3 <- het_rhat_ess %>%
  full_join(het_loo) %>%
  select(-contains("looic")) %>%
  mutate(
    rhat_mean = rhat_mean %>% signif(3),
    across(
      c(rhat_sd, starts_with("ess"), 
        ends_with("diff"), ends_with("loo")),
      ~case_when(
        .x < 100 ~ signif(.x, 2),
        .x < 1e3 ~ signif(.x, 3),
        .x < 1e4 ~ signif(.x, 4),
        T ~ signif(.x, 5)
      )
    ),
    rhat = glue("{rhat_mean} ± {rhat_sd}"),
    ess = glue("{ess_mean} ± {ess_sd}"),
    elpd = glue("{elpd_loo} ± {se_elpd_loo}"),
    diff = glue("{elpd_diff} ± {se_diff}"),
    p = glue("{p_loo} ± {se_p_loo}")
  ) %>%
  select(reference, likelihood, variance, 
         rhat, ess, elpd, diff, p) %T>%
  print()

# 7.4.3 Save ####
Table_S3 %>%
  write_csv(here("Tables", "Table_S3.csv"))

read_docx() %>%
  body_add_table(value = Table_S3) %>%
  print(target = here("Tables", "Table_S3.docx"))

# 7.5 Table S4 ####
# 7.5.1 Load data ####
conv_rhat_ess <- here("Tables", "Diagnostic", "conv_rhat_ess.csv") %>%
  read_csv() %T>%
  print()

conv_loo <- here("Tables", "Diagnostic", "conv_loo.csv") %>%
  read_csv() %T>%
  print()

# 7.5.2 Combine, round, summarise ####
Table_S4 <- conv_rhat_ess %>%
  full_join(conv_loo) %>%
  select(-contains("looic")) %>%
  mutate(
    rhat_mean = rhat_mean %>% signif(3),
    across(
      c(rhat_sd, starts_with("ess"), 
        ends_with("diff"), ends_with("loo")),
      ~case_when(
        .x < 100 ~ signif(.x, 2),
        .x < 1e3 ~ signif(.x, 3),
        .x < 1e4 ~ signif(.x, 4),
        T ~ signif(.x, 5)
      )
    ),
    rhat = glue("{rhat_mean} ± {rhat_sd}"),
    ess = glue("{ess_mean} ± {ess_sd}"),
    elpd = glue("{elpd_loo} ± {se_elpd_loo}"),
    diff = glue("{elpd_diff} ± {se_diff}"),
    p = glue("{p_loo} ± {se_p_loo}")
  ) %>%
  select(reference, model, rhat, ess, elpd, diff, p) %T>%
  print()

# 7.5.3 Save ####
Table_S4 %>%
  write_csv(here("Tables", "Table_S4.csv"))

read_docx() %>%
  body_add_table(value = Table_S4) %>%
  print(target = here("Tables", "Table_S4.docx"))

# 7.6 Effects for text ####
# 7.6.1 P(alpha > 0) ####
# Alpha was allowed to take negative values in the first 
# three examples, so it is interesting to know the
# probability of initial detrital growth P(alpha > 0).

P_alpha <- bind_rows(
  Brouwer = Brouwer_prior_posterior,
  Hamersley = Hamersley_prior_posterior,
  Bettignies = Bettignies_prior_posterior %>% select(-delta),
  .id = "reference"
)  %>%
  group_by(reference, treatment) %>%
  summarise(
    P_more = mean( alpha > 0 ) %>% signif(2),
    P_less = mean( alpha < 0 ) %>% signif(2),
    n = n()
  ) %>%
  ungroup() %T>%
  print()
  
P_alpha %>%
  write_csv(here("Tables", "P_alpha.csv"))

read_docx() %>%
  body_add_table(value = P_alpha) %>%
  print(target = here("Tables", "P_alpha.docx"))

# 7.6.2 Effect parameters ####
# The last three examples were effect coded. In examples
# 5 and 6 mu is coded to change continuously with depth
# and in example 7 mu and tau are coded to change
# with temperature and grazing.

# Load data
Frontier2021_prior_posterior_beta <- 
  here("RDS", "Frontier2021_prior_posterior_beta.rds") %>%
  read_rds()

Frontier2022_prior_posterior_beta <- 
  here("RDS", "Frontier2022_prior_posterior_beta.rds") %>%
  read_rds()

Vandendriessche_prior_posterior_beta <- 
  here("RDS", "Vandendriessche_prior_posterior_beta.rds") %>%
  read_rds()

# Depth effect
depth_effect <- bind_rows(
  Frontier2021 = Frontier2021_prior_posterior_beta,
  Frontier2022 = Frontier2022_prior_posterior_beta,
  .id = "reference"
) %>%
  mutate(
    mu = exp(log_mu), # intercept (depth = 0)
    ratio = exp(-beta) # multiplicative effect for increase in depth
  ) %>%
  group_by(reference, species) %>%
  summarise(
    across(
      everything(), 
      list(
        mean = mean, 
        sd = sd, 
        median = median
      )
    ),
    P = mean( beta > 0 ) %>% signif(2),
    n = n()
  ) %>%
  ungroup() %>%
  mutate(
    across(
      (contains("mean") | contains("sd") | contains("median")),
      ~if_else(.x < 100, signif(.x, 2), signif(.x, 3))
    ),
    mu = glue("{mu_mean} ± {mu_sd} ({mu_median})"),
    log_mu = glue("{log_mu_mean} ± {log_mu_sd} ({log_mu_median})"),
    beta = glue("{beta_mean} ± {beta_sd} ({beta_median})"),
    ratio = glue("{ratio_mean} ± {ratio_sd} ({ratio_median})")
  ) %>%
  select(!(contains("mean") | contains("sd") | contains("median"))) %T>%
  print()

depth_effect %>%
  write_csv(here("Tables", "depth_effect.csv"))

read_docx() %>%
  body_add_table(value = depth_effect) %>%
  print(target = here("Tables", "depth_effect.docx"))

# Temperature and grazing effects
temp_graz_effect <- Vandendriessche_prior_posterior_beta %>%
  mutate(
    mu_t_ratio = exp(log_mu_t), # multiplicative temp effects relative to baseline
    tau_t_ratio = exp(log_tau_t),
    mu_g_ratio = exp(beta_mu), # multiplicative grazing effect (grazed/control)
    tau_g_ratio = exp(beta_tau)
  ) %>%
  group_by(temperature) %>% # grazing effect is temperature-dependent
  summarise(
    across(
      everything(), 
      list(
        mean = mean, 
        sd = sd, 
        median = median
      )
    ),
    P_mu = mean( beta_mu < 0 ) %>% signif(2),
    P_tau = mean( beta_mu < 0 ) %>% signif(2),
    n = n()
  ) %>%
  ungroup() %>%
  mutate(
    across(
      (contains("mean") | contains("sd") | contains("median")),
      ~if_else(.x < 100, signif(.x, 2), signif(.x, 3))
    ),
    mu_t_ratio = glue("{mu_t_ratio_mean} ± {mu_t_ratio_sd} ({mu_t_ratio_median})"),
    tau_t_ratio = glue("{tau_t_ratio_mean} ± {tau_t_ratio_sd} ({tau_t_ratio_median})"),
    mu_g_ratio = glue("{mu_g_ratio_mean} ± {mu_g_ratio_sd} ({mu_g_ratio_median})"),
    tau_g_ratio = glue("{tau_g_ratio_mean} ± {tau_g_ratio_sd} ({tau_g_ratio_median})")
  ) %>%
  select(!(contains("mean") | contains("sd") | contains("median"))) %T>%
  print()

temp_graz_effect %>%
  write_csv(here("Tables", "temp_graz_effect.csv"))

read_docx() %>%
  body_add_table(value = temp_graz_effect) %>%
  print(target = here("Tables", "temp_graz_effect.docx"))

# 7.6.3 Calculated contrasts ####
# All examples include simple categorical contrasts.
# Example 1
Brouwer_contrast <- Brouwer_prior_posterior %>%
  filter(treatment != "Prior") %>%
  select(-c(epsilon, lambda, theta)) %>%
  mutate( # Convert exponential rates to %
    alpha = alpha * 100,
    tau = tau * 100
  ) %>%
  pivot_longer(cols = c(alpha, mu, tau),
               names_to = "parameter") %>%
  pivot_wider(names_from = treatment,
              values_from = value) %>%
  mutate(contrast = "KilledvControl",
         diff = `Pre-killed` - Control,
         ratio = `Pre-killed` / Control) %>%
  select(-c(`Pre-killed`, Control)) %T>%
  print()

# Example 2
Hamersley_contrast <- Hamersley_prior_posterior %>%
  filter(!treatment %in% c("Prior", "Global")) %>%
  select(-c(epsilon, lambda, theta)) %>%
  mutate(alpha = alpha * 100,
         tau = tau * 100) %>%
  pivot_longer(cols = c(alpha, mu, tau),
               names_to = "parameter") %>%
  pivot_wider(names_from = treatment,
              values_from = value) %>%
  mutate(SvF_diff = Senescent - Fresh,
         SvF_ratio = Senescent / Fresh,
         DvF_diff = Detached - Fresh,
         DvF_ratio = Detached / Fresh,
         DvS_diff = Detached - Senescent,
         DvS_ratio = Detached / Senescent) %>%
  select(-c(Fresh, Senescent, Detached)) %>%
  pivot_longer(cols = c(ends_with("diff"), ends_with("ratio"))) %>%
  separate(name, into = c("contrast", "type"), sep = "_") %>%
  pivot_wider(values_from = value,
              names_from = type) %T>%
  print()

# Example 3
Bettignies_contrast <- Bettignies_prior_posterior %>%
  filter(!treatment %in% c("Prior", "Global")) %>%
  select(-c(epsilon, lambda, theta, delta)) %>%
  mutate(alpha = alpha * 100,
         tau = tau * 100) %>%
  pivot_longer(cols = c(alpha, mu, tau),
               names_to = "parameter") %>%
  pivot_wider(names_from = treatment,
              values_from = value) %>%
  mutate(S1vF_diff = `Senescent Experiment 1` - Fresh,
         S1vF_ratio = `Senescent Experiment 1` / Fresh,
         S2vF_diff = `Senescent Experiment 2` - Fresh,
         S2vF_ratio = `Senescent Experiment 2` / Fresh,
         S2vS1_diff = `Senescent Experiment 2` - `Senescent Experiment 1`,
         S2vS1_ratio = `Senescent Experiment 2` / `Senescent Experiment 1`) %>%
  select(-c(Fresh, `Senescent Experiment 1`, `Senescent Experiment 2`)) %>%
  pivot_longer(cols = c(ends_with("diff"), ends_with("ratio"))) %>%
  separate(name, into = c("contrast", "type"), sep = "_") %>%
  pivot_wider(values_from = value,
              names_from = type) %T>%
  print()

# Example 4
Bourguès_contrast <- Bourguès_prior_posterior %>%
  filter(!treatment %in% c("Prior", "Global")) %>%
  select(-c(epsilon, lambda, theta)) %>%
  mutate(alpha = alpha * 100,
         tau = tau * 100) %>%
  pivot_longer(cols = c(alpha, mu, tau),
               names_to = "parameter") %>%
  pivot_wider(names_from = treatment,
              values_from = value) %>%
  mutate(SuvSp_diff = Summer - Spring,
         SuvSp_ratio = Summer / Spring,
         AuvSp_diff = Autumn - Spring,
         AuvSp_ratio = Autumn / Spring,
         WivSp_diff = Winter - Spring,
         WivSp_ratio = Winter / Spring,
         AuvSu_diff = Autumn - Summer,
         AuvSu_ratio = Autumn / Summer,
         WivSu_diff = Winter - Summer,
         WivSu_ratio = Winter / Summer,
         WivAu_diff = Winter - Autumn,
         WivAu_ratio = Winter / Autumn) %>%
  select(-c(Spring, Summer, Autumn, Winter)) %>%
  pivot_longer(cols = c(ends_with("diff"), ends_with("ratio"))) %>%
  separate(name, into = c("contrast", "type"), sep = "_") %>%
  pivot_wider(values_from = value,
              names_from = type) %T>%
  print()

# Example 5
Frontier2021_contrast <- Frontier2021_prior_posterior %>%
  # Filter for a single treatment since treatment only affects mu via beta
  filter(!species %in% c("Prior", "Global") & treatment == "0m") %>%
  select(-c(epsilon, lambda, theta, treatment)) %>%
  mutate(alpha = alpha * 100,
         tau = tau * 100) %>%
  pivot_longer(cols = c(alpha, mu, tau, beta),
               names_to = "parameter") %>%
  pivot_wider(names_from = species,
              values_from = value) %>%
  mutate(contrast = "OvH",
         diff = `Laminaria ochroleuca` - `Laminaria hyperborea`,
         ratio = `Laminaria ochroleuca` / `Laminaria hyperborea`) %>%
  select(-c(`Laminaria ochroleuca`, `Laminaria hyperborea`)) %T>%
  print()

# Example 6
Frontier2022_contrast <- Frontier2022_prior_posterior %>%
  filter(!species %in% c("Prior", "Global") & treatment == "0.5m") %>%
  select(-c(epsilon, lambda, theta, treatment)) %>%
  mutate(alpha = alpha * 100,
         tau = tau * 100) %>%
  pivot_longer(cols = c(alpha, mu, tau, beta),
               names_to = "parameter") %>%
  pivot_wider(names_from = species,
              values_from = value) %>%
  mutate(contrast = "OvH",
         diff = `Laminaria ochroleuca` - `Laminaria hyperborea`,
         ratio = `Laminaria ochroleuca` / `Laminaria hyperborea`) %>%
  select(-c(`Laminaria ochroleuca`, `Laminaria hyperborea`)) %T>%
  print()

# Example 7
Vandendriessche_prior_posterior_species <- 
  here("RDS", "Vandendriessche_prior_posterior_species.rds") %>%
  read_rds()

Vandendriessche_contrast <- Vandendriessche_prior_posterior_species %>%
  filter(species != "Prior") %>%
  select(-c(epsilon, lambda, theta)) %>%
  mutate(alpha = alpha * 100,
         tau = tau * 100) %>%
  pivot_longer(cols = c(alpha, mu, tau),
               names_to = "parameter") %>%
  pivot_wider(names_from = species,
              values_from = value) %>%
  mutate(contrast = "AvF",
         diff = `Ascophyllum nodosum` - `Fucus vesiculosus`,
         ratio = `Ascophyllum nodosum` / `Fucus vesiculosus`) %>%
  select(-c(`Ascophyllum nodosum`, `Fucus vesiculosus`)) %T>%
  print()

# Combine and summarise
contrast <- bind_rows(
  Brouwer = Brouwer_contrast,
  Hamersley = Hamersley_contrast,
  Bettignies = Bettignies_contrast,
  Bourguès = Bourguès_contrast,
  Frontier2021 = Frontier2021_contrast,
  Frontier2022 = Frontier2022_contrast,
  Vandendriessche = Vandendriessche_contrast,
  .id = "reference"
) %>%
  group_by(reference, parameter, contrast) %>%
  summarise(
    across(
      c(diff, ratio), 
      list(
        mean = mean, 
        sd = sd, 
        median = median
      )
    ), # This is the probability of group 1 < group 2
    P = mean( diff < 0 ), # same as ratio < 1
    n = n()
  ) %>%
  ungroup() %>%
  mutate(
    across(
      c(starts_with("diff"), starts_with("ratio"), P),
      ~case_when(
        .x < 100 ~ signif(.x, 2),
        .x < 1e3 ~ signif(.x, 3),
        .x < 1e4 ~ signif(.x, 4),
        T ~ signif(.x, 5)
      )
    ),
    diff = glue("{diff_mean} ± {diff_sd} ({diff_median})"),
    ratio = glue("{ratio_mean} ± {ratio_sd} ({ratio_median})")
  ) %>%
  select(!(contains("mean") | contains("sd") | contains("median"))) %T>%
  print(n = 50)

contrast %>%
  write_csv(here("Tables", "contrast.csv"))

read_docx() %>%
  body_add_table(value = contrast) %>%
  print(target = here("Tables", "contrast.docx"))

# 8. Figures ####
# 8.1 Dead or alive ####
# 8.1.1 Figure 2 ####
# Load predictions
Brouwer_prediction <- here("RDS", "Brouwer_prediction.rds") %>%
  read_rds()

# Predictions may be pivoted longer using:
  # %>% 
  # pivot_longer(cols = -c(t, treatment, .width, .point, .interval),
  #              names_to = c("parameter", "name"),
  #              names_sep = "\\.") %>%
  # replace_na(list(name = "median")) %>%
  # pivot_wider(names_from = name,
  #             values_from = value)

# Update theme
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

# Plot components
Fig_2_k <- Brouwer_prediction %>%
  filter(treatment != "Prior") %>%
  ggplot() +
    geom_line(aes(t, k, colour = treatment)) +
    geom_ribbon(aes(t, ymin = k.lower, ymax = k.upper,
                    alpha = factor(.width), fill = treatment)) +
    geom_density(data = Brouwer_prior_posterior %>%
                   filter(treatment != "Prior"),
                 aes(x = mu, y = after_stat(density) * 0.4, fill = treatment),
                 alpha = 0.8, colour = NA, position = position_nudge(y = -0.09),
                 bw = 320*0.005) +
    scale_colour_manual(values = c("#81a512", "#5e5003")) +
    scale_fill_manual(values = c("#81a512", "#5e5003")) +
    scale_alpha_manual(values = c(0.5, 0.4, 0.3), guide = "none") +
    scale_x_continuous(breaks = seq(0, 320, 80),
                       limits = c(0, 320),
                       oob = scales::oob_keep) +
    scale_y_continuous(breaks = seq(-0.09, 0, 0.03),
                       labels = scales::label_number(accuracy = c(rep(0.01, 3), 1),
                                                     style_negative = "minus")) +
    labs(x = expression(italic("t")*" (days)"),
         y = expression(italic("k")*" (day"^-1*")")) +
    coord_cartesian(ylim = c(-0.09, 0),
                    expand = F, clip = "off") +
    mytheme +
    theme(legend.position = "none",
          axis.title.y = element_text(vjust = -1))

Fig_2_mu <- Brouwer_prediction %>%
  filter(treatment != "Prior") %>%
  ggplot() +
    geom_hline(yintercept = 1) +
    geom_line(aes(t, m_mu, colour = treatment)) +
    geom_ribbon(aes(t, ymin = m_mu.lower, ymax = m_mu.upper,
                    alpha = factor(.width), fill = treatment)) +
    geom_vline(xintercept = 0) + # fake y axis to make geom_pointrange visible at x = 0
    geom_pointrange(data = data_mean_sd %>%
                      filter(reference == "Brouwer 1996") %>%
                      droplevels() %>% 
                      distinct(t, m_mean, m_sd, treatment),
                    aes(t, m_mean, colour = treatment,
                        ymin = m_mean - m_sd,
                        ymax = m_mean + m_sd),
                    size = 0.2, shape = 16,
                    linewidth = 0.4) +
    scale_colour_manual(values = c("#81a512", "#5e5003")) +
    scale_fill_manual(values = c("#81a512", "#5e5003")) +
    scale_alpha_manual(values = c(0.5, 0.4, 0.3), guide = "none") +
    scale_x_continuous(breaks = seq(0, 320, 80)) +
    scale_y_continuous(breaks = seq(0, 1.2, 0.4),
                       labels = scales::label_number(accuracy = c(1, rep(0.1, 3)))) +
    labs(x = expression(italic("t")*" (days)"),
         y = expression(italic(bar("m")))) +
    coord_cartesian(xlim = c(0, 320), ylim = c(0, 1.2),
                    expand = F, clip = "off") +
    mytheme +
    theme(axis.title.x = element_blank(),
          axis.text.x = element_blank(),
          axis.title.y = element_text(vjust = 0),
          axis.line.y = element_blank(),
          legend.position = "none")

Fig_2_m <- Brouwer_prediction %>%
  filter(treatment != "Prior") %>%
  ggplot() +
    geom_hline(yintercept = 1) +
    geom_line(aes(t, m, colour = treatment)) +
    geom_ribbon(aes(t, ymin = m.lower, ymax = m.upper,
                    alpha = factor(.width), fill = treatment)) +
    geom_vline(xintercept = 0) +
    geom_pointrange(data = data_mean_sd %>%
                      filter(reference == "Brouwer 1996") %>%
                      droplevels() %>% 
                      distinct(t, m_mean, m_sd, treatment),
                    aes(t, m_mean, colour = treatment,
                        ymin = m_mean - m_sd,
                        ymax = m_mean + m_sd),
                    size = 0.2, shape = 16,
                    linewidth = 0.4) +
    scale_colour_manual(values = c("#81a512", "#5e5003")) +
    scale_fill_manual(values = c("#81a512", "#5e5003")) +
    scale_alpha_manual(values = c(0.5, 0.4, 0.3), guide = "none") +
    scale_x_continuous(breaks = seq(0, 320, 80)) +
    scale_y_continuous(breaks = seq(0, 1.2, 0.4),
                       labels = scales::label_number(accuracy = c(1, rep(0.1, 3)))) +
    labs(x = expression(italic("t")*" (days)"),
         y = expression(italic(tilde("m")))) +
    coord_cartesian(xlim = c(0, 320), ylim = c(0, 1.2),
                    expand = F, clip = "off") +
    mytheme +
    theme(legend.position.inside = c(0.9, 0.15),
          axis.title.x = element_blank(),
          axis.text.x = element_blank(),
          axis.title.y = element_text(vjust = 0),
          axis.line.y = element_blank())

# Combine components and save
Fig_2 <- Fig_2_m / Fig_2_mu / Fig_2_k
Fig_2 %>%
  ggsave(filename = "Figure_2.pdf", path = "Figures",
         device = cairo_pdf, width = 10, height = 15, units = "cm")

# 8.1.2 Figure S2 ####
# Load predictions
Brouwer_k_prediction <- here("RDS", "Brouwer_k_prediction.rds") %>%
  read_rds()

# Plot components
Fig_S2_k <- Brouwer_k_prior_posterior %>%
  group_by(treatment) %>%
  median_qi(k, .width = c(.5, .8, .9)) %>%
  expand_grid(t = c(0, 320)) %>%
  filter(treatment != "Prior") %>%
  ggplot() +
    geom_line(aes(t, -k, colour = treatment)) +
    geom_ribbon(aes(t, ymin = -.upper, ymax = -.lower,
                    alpha = factor(.width), fill = treatment)) +
    geom_density(data = Brouwer_k_prior_posterior %>%
                   filter(treatment != "Prior") %>%
                   mutate(t0.5 = log(2)/k), # Calculate half-life
                 aes(x = t0.5, y = after_stat(density) * 0.2, fill = treatment),
                 alpha = 0.8, colour = NA, position = position_nudge(y = -0.09), 
                 bw = 320*0.005) +
    scale_colour_manual(values = c("#81a512", "#5e5003")) +
    scale_fill_manual(values = c("#81a512", "#5e5003")) +
    scale_alpha_manual(values = c(0.5, 0.4, 0.3), guide = "none") +
    scale_x_continuous(breaks = seq(0, 320, 80),
                       limits = c(0, 320),
                       oob = scales::oob_keep) +
    scale_y_continuous(breaks = seq(-0.09, 0, 0.03),
                       labels = scales::label_number(accuracy = c(rep(0.01, 3), 1),
                                                     style_negative = "minus")) +
    labs(x = expression(italic("t")*" (days)"),
         y = expression(italic("k")*" (day"^-1*")")) +
    coord_cartesian(ylim = c(-0.09, 0),
                    expand = F, clip = "off") +
    mytheme +
    theme(legend.position = "none",
          axis.title.y = element_text(vjust = -1))

Fig_S2_mu <- Brouwer_k_prediction %>%
  filter(treatment != "Prior") %>%
  ggplot() +
    geom_hline(yintercept = 1) +
    geom_line(aes(t, m_mu, colour = treatment)) +
    geom_ribbon(aes(t, ymin = m_mu.lower, ymax = m_mu.upper,
                    alpha = factor(.width), fill = treatment)) +
    geom_vline(xintercept = 0) +
    geom_pointrange(data = data_mean_sd %>%
                      filter(reference == "Brouwer 1996") %>%
                      droplevels() %>% 
                      distinct(t, m_mean, m_sd, treatment),
                    aes(t, m_mean, colour = treatment,
                        ymin = m_mean - m_sd,
                        ymax = m_mean + m_sd),
                    size = 0.2, shape = 16,
                    linewidth = 0.4) +
    scale_colour_manual(values = c("#81a512", "#5e5003")) +
    scale_fill_manual(values = c("#81a512", "#5e5003")) +
    scale_alpha_manual(values = c(0.5, 0.4, 0.3), guide = "none") +
    scale_x_continuous(breaks = seq(0, 320, 80)) +
    scale_y_continuous(breaks = seq(0, 1.2, 0.4),
                       labels = scales::label_number(accuracy = c(1, rep(0.1, 3)))) +
    labs(x = expression(italic("t")*" (days)"),
         y = expression(italic(bar("m")))) +
    coord_cartesian(xlim = c(0, 320), ylim = c(0, 1.2),
                    expand = F, clip = "off") +
    mytheme +
    theme(axis.title.x = element_blank(),
          axis.text.x = element_blank(),
          axis.title.y = element_text(vjust = 0),
          axis.line.y = element_blank(),
          legend.position = "none")

Fig_S2_m <- Brouwer_k_prediction %>%
  filter(treatment != "Prior") %>%
  ggplot() +
    geom_hline(yintercept = 1) +
    geom_line(aes(t, m, colour = treatment)) +
    geom_ribbon(aes(t, ymin = m.lower, ymax = m.upper,
                    alpha = factor(.width), fill = treatment)) +
    geom_vline(xintercept = 0) +
    geom_pointrange(data = data_mean_sd %>%
                      filter(reference == "Brouwer 1996") %>%
                      droplevels() %>% 
                      distinct(t, m_mean, m_sd, treatment),
                    aes(t, m_mean, colour = treatment,
                        ymin = m_mean - m_sd,
                        ymax = m_mean + m_sd),
                    size = 0.2, shape = 16,
                    linewidth = 0.4) +
    scale_colour_manual(values = c("#81a512", "#5e5003")) +
    scale_fill_manual(values = c("#81a512", "#5e5003")) +
    scale_alpha_manual(values = c(0.5, 0.4, 0.3), guide = "none") +
    scale_x_continuous(breaks = seq(0, 320, 80)) +
    scale_y_continuous(breaks = seq(0, 1.2, 0.4),
                       labels = scales::label_number(accuracy = c(1, rep(0.1, 3)))) +
    labs(x = expression(italic("t")*" (days)"),
         y = expression(italic(tilde("m")))) +
    coord_cartesian(xlim = c(0, 320), ylim = c(0, 1.2),
                    expand = F, clip = "off") +
    mytheme +
    theme(legend.position.inside = c(0.9, 0.15),
          axis.title.x = element_blank(),
          axis.text.x = element_blank(),
          axis.title.y = element_text(vjust = 0),
          axis.line.y = element_blank())

# Combine components and save
Fig_S2 <- Fig_S2_m / Fig_S2_mu / Fig_S2_k
Fig_S2 %>%
  ggsave(filename = "Figure_S2.pdf", path = "Figures",
         device = cairo_pdf, width = 10, height = 15, units = "cm")

# 8.2 Senescence ####
# 8.2.1 Figure 3 ####
# Load predictions
Hamersley_prediction <- here("RDS", "Hamersley_prediction.rds") %>%
  read_rds()
Bettignies_prediction <- here("RDS", "Bettignies_prediction.rds") %>%
  read_rds()

# Plot components
Fig_3a_k <- Hamersley_prediction %>%
  filter(!treatment %in% c("Prior", "Global")) %>%
  ggplot() +
    geom_hline(yintercept = 0) +
    geom_line(aes(t, k, colour = treatment)) +
    geom_ribbon(aes(t, ymin = k.lower, ymax = k.upper,
                    alpha = factor(.width), fill = treatment)) +
    geom_density(data = Hamersley_prior_posterior %>%
                   filter(!treatment %in% c("Prior", "Global")),
                 aes(x = mu, y = after_stat(density) * 0.055, fill = treatment),
                 alpha = 0.8, colour = NA, bw = 25*0.005, 
                 position = position_nudge(y = -0.17)) +
    scale_colour_manual(values = c("#81a512", "#afab00", "#5e5003")) +
    scale_fill_manual(values = c("#81a512", "#afab00", "#5e5003")) +
    scale_alpha_manual(values = c(0.5, 0.4, 0.3), guide = "none") +
    scale_x_continuous(breaks = seq(0, 25, 5),
                       limits = c(0, 25),
                       oob = scales::oob_keep) +
    scale_y_continuous(breaks = seq(-0.17, 0.01, 0.06),
                       labels = scales::label_number(style_negative = "minus")) +
    labs(x = expression(italic("t")*" (days)"),
         y = expression(italic("k")*" (day"^-1*")")) +
    coord_cartesian(ylim = c(-0.17, 0.01),
                    expand = F, clip = "off") +
    mytheme +
    theme(axis.title.y = element_text(vjust = -1),
          legend.position = "none")

Fig_3a_mu <- Hamersley_prediction %>%
  filter(!treatment %in% c("Prior", "Global")) %>%
  ggplot() +
    geom_line(aes(t, m_mu, colour = treatment)) +
    geom_ribbon(aes(t, ymin = m_mu.lower, ymax = m_mu.upper,
                    alpha = factor(.width), fill = treatment)) +
    geom_point(data = data %>%
                 filter(reference == "Hamersley et al. 2015" & t != 0) %>%
                 droplevels(),
               aes(t, m, colour = treatment), 
               size = 1, shape = 16, alpha = 0.6) +
    scale_colour_manual(values = c("#81a512", "#afab00", "#5e5003")) +
    scale_fill_manual(values = c("#81a512", "#afab00", "#5e5003")) +
    scale_alpha_manual(values = c(0.5, 0.4, 0.3), guide = "none") +
    scale_x_continuous(breaks = seq(0, 25, 5)) +
    scale_y_continuous(breaks = seq(0, 1, 0.25),
                       labels = scales::label_number(accuracy = c(1, 0.01, 0.1, 0.01, 1))) +
    labs(x = expression(italic("t")*" (days)"),
         y = expression(italic(bar("m")))) +
    coord_cartesian(xlim = c(0, 25), ylim = c(0, 1),
                    expand = F, clip = "off") +
    mytheme +
    theme(plot.margin = margin(0.2, 0.25, 0, 0, unit = "cm"),
          legend.position.inside = c(0.89, 0.89),
          axis.title.x = element_blank(),
          axis.text.x = element_blank(),
          axis.title.y = element_text(vjust = 0))

Fig_3b_k <- Bettignies_prediction %>%
  filter(!treatment %in% c("Prior", "Global")) %>%
  mutate(treatment2 = treatment %>% str_extract("^\\S+")) %>%
  ggplot() +
    geom_hline(yintercept = 0) +
    geom_line(aes(t, k, group = treatment, colour = treatment2)) +
    geom_ribbon(aes(t, ymin = k.lower, ymax = k.upper,
                    group = interaction(treatment, .width),
                    alpha = factor(.width), fill = treatment2)) +
    geom_density(data = Bettignies_prior_posterior %>%
                   filter(!treatment %in% c("Prior", "Global")) %>%
                   mutate(treatment2 = treatment %>% str_extract("^\\S+")),
                 aes(x = mu, y = after_stat(density) * 0.35, 
                     group = treatment, fill = treatment2),
                 alpha = 0.8, colour = NA, bw = 180*0.005, 
                 position = position_nudge(y = -0.08)) +
    scale_colour_manual(values = c("#81a512", "#afab00")) +
    scale_fill_manual(values = c("#81a512", "#afab00")) +
    scale_alpha_manual(values = c(0.5, 0.4, 0.3), guide = "none") +
    scale_x_continuous(breaks = seq(0, 180, 30),
                       limits = c(0, 180),
                       oob = scales::oob_keep) +
    scale_y_continuous(breaks = seq(-0.08, 0.01, 0.03),
                       labels = scales::label_number(style_negative = "minus")) +
    labs(x = expression(italic("t")*" (days)"),
         y = expression(italic("k")*" (day"^-1*")")) +
    coord_cartesian(ylim = c(-0.08, 0.01),
                    expand = F, clip = "off") +
    mytheme +
    theme(axis.title.y = element_text(vjust = -1),
          legend.position = "none")

Fig_3b_mu <- Bettignies_prediction %>%
  filter(!treatment %in% c("Prior", "Global")) %>%
  mutate(treatment2 = treatment %>% str_extract("^\\S+")) %>%
  ggplot() +
    geom_hline(yintercept = 1) +
    geom_line(aes(t, m_mu, group = treatment, colour = treatment2)) +
    geom_ribbon(aes(t, ymin = m_mu.lower, ymax = m_mu.upper,
                    group = interaction(treatment, .width),
                    alpha = factor(.width), fill = treatment2)) +
    geom_point(data = data %>%
                 filter(reference == "de Bettignies et al. 2020" & t != 0) %>%
                 droplevels() %>%
                 mutate(treatment2 = treatment %>% str_extract("^\\S+")),
               aes(t, m, colour = treatment2), 
               size = 1, shape = 16, alpha = 0.6) +
    scale_colour_manual(values = c("#81a512", "#afab00")) +
    scale_fill_manual(values = c("#81a512", "#afab00")) +
    scale_alpha_manual(values = c(0.5, 0.4, 0.3), guide = "none") +
    scale_x_continuous(breaks = seq(0, 180, 30)) +
    scale_y_continuous(breaks = seq(0, 1.2, 0.4),
                       labels = scales::label_number(accuracy = c(1, rep(0.1, 3)))) +
    labs(x = expression(italic("t")*" (days)"),
         y = expression(italic(bar("m")))) +
    coord_cartesian(xlim = c(0, 180), ylim = c(0, 1.2),
                    expand = F, clip = "off") +
    mytheme +
    theme(plot.margin = margin(0.2, 0.25, 0, 0, unit = "cm"),
          legend.position = "none",
          axis.title.x = element_blank(),
          axis.text.x = element_blank(),
          axis.title.y = element_text(vjust = 0))

# Combine components and save
Fig_3 <- ( Fig_3a_mu / Fig_3a_k / Fig_3b_mu / Fig_3b_k ) +
  plot_annotation(tag_levels = list(c("a", "", "b", ""))) &
  theme(plot.tag = element_text(family = "Futura", size = 12, face = "bold"),
        plot.tag.position = c(-0.002, 1.04))

Fig_3 %>%
  ggsave(filename = "Figure_3.pdf", path = "Figures",
         device = cairo_pdf, width = 10, height = 20, units = "cm")

# 8.2.2 Figure S3 ####
# Load predictions
Hamersley_k_prediction <- here("RDS", "Hamersley_k_prediction.rds") %>%
  read_rds()
Bettignies_k_prediction <- here("RDS", "Bettignies_k_prediction.rds") %>%
  read_rds()

# Plot components
Fig_S3a_k <- Hamersley_k_prior_posterior %>%
  group_by(treatment) %>%
  median_qi(k, .width = c(.5, .8, .9)) %>%
  expand_grid(t = c(0, 25)) %>%
  filter(!treatment %in% c("Prior", "Global")) %>%
  ggplot() +
    geom_hline(yintercept = 0) +
    geom_line(aes(t, -k, colour = treatment)) +
    geom_ribbon(aes(t, ymin = -.upper, ymax = -.lower,
                    alpha = factor(.width), fill = treatment)) +
    geom_density(data = Hamersley_k_prior_posterior %>%
                   filter(!treatment %in% c("Prior", "Global")) %>%
                   mutate(t0.5 = log(2)/k), # Calculate half-life,
                 aes(x = t0.5, y = after_stat(density) * 0.07, fill = treatment),
                 alpha = 0.8, colour = NA, bw = 25*0.005, 
                 position = position_nudge(y = -0.17)) +
    scale_colour_manual(values = c("#81a512", "#afab00", "#5e5003")) +
    scale_fill_manual(values = c("#81a512", "#afab00", "#5e5003")) +
    scale_alpha_manual(values = c(0.5, 0.4, 0.3), guide = "none") +
    scale_x_continuous(breaks = seq(0, 25, 5),
                       limits = c(0, 25),
                       oob = scales::oob_keep) +
    scale_y_continuous(breaks = seq(-0.17, 0.01, 0.06),
                       labels = scales::label_number(style_negative = "minus")) +
    labs(x = expression(italic("t")*" (days)"),
         y = expression(italic("k")*" (day"^-1*")")) +
    coord_cartesian(ylim = c(-0.17, 0.01),
                    expand = F, clip = "off") +
    mytheme +
    theme(axis.title.y = element_text(vjust = -1),
          legend.position = "none")

Fig_S3a_mu <- Hamersley_k_prediction %>%
  filter(!treatment %in% c("Prior", "Global")) %>%
  ggplot() +
    geom_line(aes(t, m_mu, colour = treatment)) +
    geom_ribbon(aes(t, ymin = m_mu.lower, ymax = m_mu.upper,
                    alpha = factor(.width), fill = treatment)) +
    geom_point(data = data %>%
                 filter(reference == "Hamersley et al. 2015" & t != 0) %>%
                 droplevels(),
               aes(t, m, colour = treatment), 
               size = 1, shape = 16, alpha = 0.6) +
    scale_colour_manual(values = c("#81a512", "#afab00", "#5e5003")) +
    scale_fill_manual(values = c("#81a512", "#afab00", "#5e5003")) +
    scale_alpha_manual(values = c(0.5, 0.4, 0.3), guide = "none") +
    scale_x_continuous(breaks = seq(0, 25, 5)) +
    scale_y_continuous(breaks = seq(0, 1, 0.25),
                       labels = scales::label_number(accuracy = c(1, 0.01, 0.1, 0.01, 1))) +
    labs(x = expression(italic("t")*" (days)"),
         y = expression(italic(bar("m")))) +
    coord_cartesian(xlim = c(0, 25), ylim = c(0, 1),
                    expand = F, clip = "off") +
    mytheme +
    theme(plot.margin = margin(0.2, 0.25, 0, 0, unit = "cm"),
          legend.position.inside = c(0.89, 0.89),
          axis.title.x = element_blank(),
          axis.text.x = element_blank(),
          axis.title.y = element_text(vjust = 0))

Fig_S3b_k <- Bettignies_k_prior_posterior %>%
  group_by(treatment) %>%
  median_qi(k, .width = c(.5, .8, .9)) %>%
  expand_grid(t = c(0, 180)) %>%
  filter(!treatment %in% c("Prior", "Global")) %>%
  mutate(treatment2 = treatment %>% str_extract("^\\S+")) %>%
  ggplot() +
    geom_hline(yintercept = 0) +
    geom_line(aes(t, -k, group = treatment, colour = treatment2)) +
    geom_ribbon(aes(t, ymin = -.upper, ymax = -.lower,
                    group = interaction(treatment, .width),
                    alpha = factor(.width), fill = treatment2)) +
    geom_density(data = Bettignies_k_prior_posterior %>%
                   filter(!treatment %in% c("Prior", "Global")) %>%
                   mutate(treatment2 = treatment %>% str_extract("^\\S+"),
                          t0.5 = log(2)/k),
                 aes(x = t0.5, y = after_stat(density) * 0.25, 
                     group = treatment, fill = treatment2),
                 alpha = 0.8, colour = NA, bw = 180*0.005, 
                 position = position_nudge(y = -0.08)) +
    scale_colour_manual(values = c("#81a512", "#afab00")) +
    scale_fill_manual(values = c("#81a512", "#afab00")) +
    scale_alpha_manual(values = c(0.5, 0.4, 0.3), guide = "none") +
    scale_x_continuous(breaks = seq(0, 180, 30),
                       limits = c(0, 180),
                       oob = scales::oob_keep) +
    scale_y_continuous(breaks = seq(-0.08, 0.01, 0.03),
                       labels = scales::label_number(style_negative = "minus")) +
    labs(x = expression(italic("t")*" (days)"),
         y = expression(italic("k")*" (day"^-1*")")) +
    coord_cartesian(ylim = c(-0.08, 0.01),
                    expand = F, clip = "off") +
    mytheme +
    theme(axis.title.y = element_text(vjust = -1),
          legend.position = "none")

Fig_S3b_mu <- Bettignies_k_prediction %>%
  filter(!treatment %in% c("Prior", "Global")) %>%
  mutate(treatment2 = treatment %>% str_extract("^\\S+")) %>%
  ggplot() +
    geom_hline(yintercept = 1) +
    geom_line(aes(t, m_mu, group = treatment, colour = treatment2)) +
    geom_ribbon(aes(t, ymin = m_mu.lower, ymax = m_mu.upper,
                    group = interaction(treatment, .width),
                    alpha = factor(.width), fill = treatment2)) +
    geom_point(data = data %>%
                 filter(reference == "de Bettignies et al. 2020" & t != 0) %>%
                 droplevels() %>%
                 mutate(treatment2 = treatment %>% str_extract("^\\S+")),
               aes(t, m, colour = treatment2), 
               size = 1, shape = 16, alpha = 0.6) +
    scale_colour_manual(values = c("#81a512", "#afab00")) +
    scale_fill_manual(values = c("#81a512", "#afab00")) +
    scale_alpha_manual(values = c(0.5, 0.4, 0.3), guide = "none") +
    scale_x_continuous(breaks = seq(0, 180, 30)) +
    scale_y_continuous(breaks = seq(0, 1.2, 0.4),
                       labels = scales::label_number(accuracy = c(1, rep(0.1, 3)))) +
    labs(x = expression(italic("t")*" (days)"),
         y = expression(italic(bar("m")))) +
    coord_cartesian(xlim = c(0, 180), ylim = c(0, 1.2),
                    expand = F, clip = "off") +
    mytheme +
    theme(plot.margin = margin(0.2, 0.25, 0, 0, unit = "cm"),
          legend.position = "none",
          axis.title.x = element_blank(),
          axis.text.x = element_blank(),
          axis.title.y = element_text(vjust = 0))

# Combine components and save
Fig_S3 <- ( Fig_S3a_mu / Fig_S3a_k / Fig_S3b_mu / Fig_S3b_k ) +
  plot_annotation(tag_levels = list(c("a", "", "b", ""))) &
  theme(plot.tag = element_text(family = "Futura", size = 12, face = "bold"),
        plot.tag.position = c(-0.002, 1.04))

Fig_S3 %>%
  ggsave(filename = "Figure_S3.pdf", path = "Figures",
         device = cairo_pdf, width = 10, height = 20, units = "cm")

# 8.3 Season ####
# 8.3.1 Figure 4 ####
# Load predictions
Bourguès_prediction <- here("RDS", "Bourguès_prediction.rds") %>%
  read_rds()

# Plot
Fig_4 <- Bourguès_prediction %>%
  filter(!treatment %in% c("Prior", "Global")) %>%
  ggplot() +
    geom_hline(yintercept = 1) +
    geom_line(aes(t, m, colour = treatment)) +
    geom_ribbon(aes(t, ymin = m.lower, ymax = m.upper,
                    alpha = factor(.width), fill = treatment)) +
    geom_point(data = data %>%
                 filter(reference == "Bourguès et al. 1996" & t != 0) %>%
                 droplevels(),
               aes(t, m, colour = treatment), 
               size = 1, shape = 16, alpha = 0.6) +
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
    labs(x = expression(italic("t")*" (days)"),
         y = expression(italic(tilde("m")))) +
    coord_cartesian(xlim = c(0, 36), ylim = c(0, 1.2),
                    expand = F, clip = "off") +
    facet_grid(rows = vars(treatment)) +
    mytheme +
    theme(strip.text = element_blank(),
          plot.margin = margin(0.45, 0.45, 0, 0.45, unit = "cm"))

# Save
Fig_4 %>%
  ggsave(filename = "Figure_4.pdf", path = "Figures",
         device = cairo_pdf, width = 10, height = 10, units = "cm")

# 8.3.2 Figure S4 ####
# Load predictions
Bourguès_k_prediction <- here("RDS", "Bourguès_k_prediction.rds") %>%
  read_rds()

# Plot
Fig_S4 <- Bourguès_k_prediction %>%
  filter(!treatment %in% c("Prior", "Global")) %>%
  ggplot() +
    geom_hline(yintercept = 1) +
    geom_line(aes(t, m, colour = treatment)) +
    geom_ribbon(aes(t, ymin = m.lower, ymax = m.upper,
                    alpha = factor(.width), fill = treatment)) +
    geom_point(data = data %>%
                 filter(reference == "Bourguès et al. 1996" & t != 0) %>%
                 droplevels(),
               aes(t, m, colour = treatment), 
               size = 1, shape = 16, alpha = 0.6) +
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
    labs(x = expression(italic("t")*" (days)"),
         y = expression(italic(tilde("m")))) +
    coord_cartesian(xlim = c(0, 36), ylim = c(0, 1.2),
                    expand = F, clip = "off") +
    facet_grid(rows = vars(treatment)) +
    mytheme +
    theme(strip.text = element_blank(),
          plot.margin = margin(0.45, 0.45, 0, 0.45, unit = "cm"))

# Save
Fig_S4 %>%
  ggsave(filename = "Figure_S4.pdf", path = "Figures",
         device = cairo_pdf, width = 10, height = 10, units = "cm")

# 8.4 Light ####
# 8.4.1 Figure 5 ####
# Load predictions
Frontier2021_prediction <- here("RDS", "Frontier2021_prediction.rds") %>%
  read_rds()
Frontier2021_prediction_replicate <- here("RDS", "Frontier2021_prediction_replicate.rds") %>%
  read_rds()
Frontier2022_prediction <- here("RDS", "Frontier2022_prediction.rds") %>%
  read_rds()
Frontier2022_prediction_replicate <- here("RDS", "Frontier2022_prediction_replicate.rds") %>%
  read_rds()

# Plot panels
Fig_5a <- Frontier2021_prediction %>%
  filter(!species %in% c("Prior", "Global") & .width == 0.9) %>%
  ggplot() +
    geom_hline(yintercept = 1) +
    geom_ribbon(aes(t, ymin = m_mu.lower, ymax = m_mu.upper,
                    fill = treatment), alpha = 0.3) +
    geom_line(data = Frontier2021_prediction_replicate %>%
                filter(treatment != "Prior" & .width == 0.5),
              aes(t, m_mu, colour = treatment, group = replicate),
              alpha = 0.6) +
    geom_point(data = data %>%
                 filter(reference == "Frontier et al. 2021" & t != 0) %>%
                 droplevels(),
               aes(t, m, colour = treatment), 
               size = 1, shape = 16, alpha = 0.6) +
    scale_colour_manual(values = c("#f5a54a", "#6a98b4", "#2e4a5b"),
                        guide = "none") +
    scale_fill_manual(values = c("#f5a54a", "#6a98b4", "#2e4a5b"),
                      guide = "none") +
    scale_x_continuous(breaks = seq(0, 100, 20)) +
    scale_y_continuous(breaks = seq(0, 1.5, 0.5),
                       labels = scales::label_number(accuracy = c(1, 0.1, 1, 0.1))) +
    labs(x = expression(italic("t")*" (days)"),
         y = expression(italic(bar("m")))) +
    coord_cartesian(xlim = c(0, 100), ylim = c(0, 1.5),
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

Fig_5b <- Frontier2022_prediction %>%
  filter(!species %in% c("Prior", "Global") & .width == 0.9) %>%
  ggplot() +
    geom_hline(yintercept = 1) +
    geom_ribbon(aes(t, ymin = m_mu.lower, ymax = m_mu.upper,
                    fill = treatment), alpha = 0.3) +
    geom_line(data = Frontier2022_prediction_replicate %>%
                filter(treatment != "Prior" & .width == 0.5),
              aes(t, m_mu, colour = treatment, group = replicate),
              alpha = 0.6) +
    geom_point(data = data %>%
                 filter(reference == "Frontier et al. 2022" & t != 0) %>%
                 droplevels(),
               aes(t, m, colour = treatment), 
               size = 1, shape = 16, alpha = 0.6) +
    scale_colour_manual(values = c("#f5a54a", "#6a98b4", "#2e4a5b"),
                        guide = "none") +
    scale_fill_manual(values = c("#f5a54a", "#6a98b4", "#2e4a5b"),
                      guide = "none") +
    scale_x_continuous(breaks = seq(0, 40, 20)) +
    scale_y_continuous(breaks = seq(0, 1.5, 0.5),
                       labels = scales::label_number(accuracy = c(1, 0.1, 1, 0.1))) +
    labs(x = expression(italic("t")*" (days)"),
         y = expression(italic(bar("m")))) +
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

# Combine panels and save
Fig_5 <- ( Fig_5a | Fig_5b ) +
  plot_layout(widths = c(1, 0.428)) +
  plot_annotation(tag_levels = c("a", "b")) &
  theme(plot.tag = element_text(family = "Futura", size = 12, face = "bold"),
        plot.tag.position = c(-0.018, 0.996))

Fig_5 %>%
  ggsave(filename = "Figure_5.pdf", path = "Figures",
         device = cairo_pdf, width = 20, height = 10, units = "cm")

# 8.4.2 Figure S5 ####
# Load predictions
Frontier2021_k_prediction <- here("RDS", "Frontier2021_k_prediction.rds") %>%
  read_rds()
Frontier2021_k_prediction_replicate <- here("RDS", "Frontier2021_k_prediction_replicate.rds") %>%
  read_rds()
Frontier2022_k_prediction <- here("RDS", "Frontier2022_k_prediction.rds") %>%
  read_rds()
Frontier2022_k_prediction_replicate <- here("RDS", "Frontier2022_k_prediction_replicate.rds") %>%
  read_rds()

# Plot panels
Fig_S5a <- Frontier2021_k_prediction %>%
  filter(!species %in% c("Prior", "Global") & .width == 0.9) %>%
  ggplot() +
    geom_hline(yintercept = 1) +
    geom_ribbon(aes(t, ymin = m_mu.lower, ymax = m_mu.upper,
                    fill = treatment), alpha = 0.3) +
    geom_line(data = Frontier2021_k_prediction_replicate %>%
                filter(treatment != "Prior" & .width == 0.5),
              aes(t, m_mu, colour = treatment, group = replicate),
              alpha = 0.6) +
    geom_point(data = data %>%
                 filter(reference == "Frontier et al. 2021" & t != 0) %>%
                 droplevels(),
               aes(t, m, colour = treatment), 
               size = 1, shape = 16, alpha = 0.6) +
    scale_colour_manual(values = c("#f5a54a", "#6a98b4", "#2e4a5b"),
                        guide = "none") +
    scale_fill_manual(values = c("#f5a54a", "#6a98b4", "#2e4a5b"),
                      guide = "none") +
    scale_x_continuous(breaks = seq(0, 100, 20)) +
    scale_y_continuous(breaks = seq(0, 1.5, 0.5),
                       labels = scales::label_number(accuracy = c(1, 0.1, 1, 0.1))) +
    labs(x = expression(italic("t")*" (days)"),
         y = expression(italic(bar("m")))) +
    coord_cartesian(xlim = c(0, 100), ylim = c(0, 1.5),
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

Fig_S5b <- Frontier2022_k_prediction %>%
  filter(!species %in% c("Prior", "Global") & .width == 0.9) %>%
  ggplot() +
    geom_hline(yintercept = 1) +
    geom_ribbon(aes(t, ymin = m_mu.lower, ymax = m_mu.upper,
                    fill = treatment), alpha = 0.3) +
    geom_line(data = Frontier2022_k_prediction_replicate %>%
                filter(treatment != "Prior" & .width == 0.5),
              aes(t, m_mu, colour = treatment, group = replicate),
              alpha = 0.6) +
    geom_point(data = data %>%
                 filter(reference == "Frontier et al. 2022" & t != 0) %>%
                 droplevels(),
               aes(t, m, colour = treatment), 
               size = 1, shape = 16, alpha = 0.6) +
    scale_colour_manual(values = c("#f5a54a", "#6a98b4", "#2e4a5b"),
                        guide = "none") +
    scale_fill_manual(values = c("#f5a54a", "#6a98b4", "#2e4a5b"),
                      guide = "none") +
    scale_x_continuous(breaks = seq(0, 40, 20)) +
    scale_y_continuous(breaks = seq(0, 1.5, 0.5),
                       labels = scales::label_number(accuracy = c(1, 0.1, 1, 0.1))) +
    labs(x = expression(italic("t")*" (days)"),
         y = expression(italic(bar("m")))) +
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

# Combine panels and save
Fig_S5 <- ( Fig_S5a | Fig_S5b ) +
  plot_layout(widths = c(1, 0.428)) +
  plot_annotation(tag_levels = c("a", "b")) &
  theme(plot.tag = element_text(family = "Futura", size = 12, face = "bold"),
        plot.tag.position = c(-0.018, 0.996))

Fig_S5 %>%
  ggsave(filename = "Figure_S5.pdf", path = "Figures",
         device = cairo_pdf, width = 20, height = 10, units = "cm")

# 8.5 Temperature ####
# 8.5.1 Figure 6 ####
# Load predictions
Vandendriessche_prediction_replicate <- here("RDS", "Vandendriessche_prediction_replicate.rds") %>%
  read_rds()

# Plot
Fig_6 <- Vandendriessche_prediction_replicate %>%
  filter(species != "Prior") %>%
  mutate(temperature = temperature %>% fct_relevel("5°C"),
         species_grazing = interaction(species, grazing, sep = " ") %>%
           fct_relevel("Ascophyllum nodosum Control", "Ascophyllum nodosum Grazed",
                       "Fucus vesiculosus Control")) %>%
  ggplot() +
    geom_hline(yintercept = c(0, 1)) +
    geom_line(aes(t, m_mu, colour = temperature, group = replicate)) +
    geom_ribbon(aes(t, ymin = m_mu.lower, ymax = m_mu.upper,
                    fill = temperature, alpha = factor(.width),
                    group = interaction(replicate, .width))) +
    geom_point(data = data %>%
                 filter(reference == "Vandendriessche et al. 2007" & t != 0) %>%
                 droplevels() %>%
                 separate(treatment, into = c("temperature", "grazing"), sep = " ") %>%
                 mutate(temperature = temperature %>% fct_relevel("5°C"),
                        species_grazing = interaction(species, grazing, sep = " ") %>%
                          fct_relevel("Ascophyllum nodosum Control", "Ascophyllum nodosum Grazed",
                                      "Fucus vesiculosus Control")),
               aes(t, m, colour = temperature), 
               size = 1, shape = 16, alpha = 0.6) +
    geom_text(data = . %>% distinct(temperature, species_grazing) %>%
                mutate(
                  label_species = case_when(
                    temperature == "5°C" & 
                      species_grazing == "Ascophyllum nodosum Control" ~
                      "Ascophyllum nodosum",
                    temperature == "5°C" & 
                      species_grazing == "Fucus vesiculosus Control" ~
                      "Fucus vesiculosus"
                  ),
                  label_grazing = case_when(
                    temperature == "5°C" & 
                      species_grazing %>% str_detect("Control") ~
                      "Control",
                    temperature == "5°C" & 
                      species_grazing %>% str_detect("Grazed") ~
                      "Grazed"
                  )
                ) %>%
                pivot_longer(cols = contains("label"),
                             values_to = "label",
                             names_prefix = "label_") %>%
                mutate(
                  y = case_when(
                    species_grazing %>% str_detect("Asco") &
                      name == "species" ~ 4,
                    species_grazing %>% str_detect("Fucus") &
                      name == "species" ~ 3,
                    species_grazing == "Ascophyllum nodosum Control" &
                      name == "grazing" ~ 3.5,
                    species_grazing == "Ascophyllum nodosum Grazed" &
                      name == "grazing" ~ 2,
                    species_grazing == "Fucus vesiculosus Control" &
                      name == "grazing" ~ 2.5,
                    species_grazing == "Fucus vesiculosus Grazed" &
                      name == "grazing" ~ 3
                  ),
                  fontface = if_else(name == "species", "italic", "plain")
                ),
              aes(x = 3.3, y = y, label = label, fontface = fontface), 
              family = "Futura", size = 8, size.unit = "pt", hjust = 0, vjust = 1) +
    scale_colour_manual(values = c("#2e4a5b", "#6a98b4", "#f5a54a", "#d1750c"),
                        guide = "none") +
    scale_fill_manual(values = c("#2e4a5b", "#6a98b4", "#f5a54a", "#d1750c"),
                      guide = "none") +
    scale_alpha_manual(values = c(0.5, 0.4, 0.3), guide = "none") +
    labs(x = expression(italic("t")*" (days)"),
         y = expression(italic(bar("m")))) +
    facet_grid(species_grazing ~ temperature, scales = "free", space = "free") +
    facetted_pos_scales(
      x = list(
        temperature == "5°C" ~
          scale_x_continuous(limits = c(0, 240),
                             breaks = seq(0, 240, by = 30)),
        temperature == "10°C" ~
          scale_x_continuous(limits = c(0, 150),
                             breaks = seq(0, 150, by = 30)),
        temperature == "15°C" ~
          scale_x_continuous(limits = c(0, 90),
                             breaks = seq(0, 90, by = 30)),
        temperature == "18°C" ~
          scale_x_continuous(limits = c(0, 120),
                             breaks = seq(0, 120, by = 30))
      ),
      y = list(
        species_grazing == "Ascophyllum nodosum Control" ~
          scale_y_continuous(limits = c(0, 4),
                             breaks = 0:4),
        species_grazing == "Ascophyllum nodosum Grazed" ~
          scale_y_continuous(limits = c(0, 2),
                             breaks = 0:2),
        species_grazing == "Fucus vesiculosus Control" ~
          scale_y_continuous(limits = c(0, 3),
                             breaks = 0:3),
        species_grazing == "Fucus vesiculosus Grazed" ~
          scale_y_continuous(limits = c(0, 3),
                             breaks = 0:3)
      )
    ) +
    coord_cartesian(expand = F, clip = "off") +
    mytheme +
    theme(strip.text = element_text(hjust = 0),
          strip.text.y = element_blank(),
          plot.margin = margin(0.45, 0.45, 0, 0.45, unit = "cm"))

# Save
Fig_6 %>%
  ggsave(filename = "Figure_6.pdf", path = "Figures",
         device = cairo_pdf, width = 20, height = 12, units = "cm")
# Warnings can be ignored. They're just about the missing text in most panels.

# 8.5.2 Figure S6 ####
# Load predictions
Vandendriessche_k_prediction_replicate <- here("RDS", "Vandendriessche_k_prediction_replicate.rds") %>%
  read_rds()

# Plot
Fig_S6 <- Vandendriessche_k_prediction_replicate %>%
  filter(species != "Prior") %>%
  mutate(temperature = temperature %>% fct_relevel("5°C"),
         species_grazing = interaction(species, grazing, sep = " ") %>%
           fct_relevel("Ascophyllum nodosum Control", "Ascophyllum nodosum Grazed",
                       "Fucus vesiculosus Control")) %>%
  ggplot() +
    geom_hline(yintercept = c(0, 1)) +
    geom_line(aes(t, m_mu, colour = temperature, group = replicate)) +
    geom_ribbon(aes(t, ymin = .lower, ymax = .upper,
                    fill = temperature, alpha = factor(.width),
                    group = interaction(replicate, .width))) +
    geom_point(data = data %>%
                 filter(reference == "Vandendriessche et al. 2007" & t != 0) %>%
                 droplevels() %>%
                 separate(treatment, into = c("temperature", "grazing"), sep = " ") %>%
                 mutate(temperature = temperature %>% fct_relevel("5°C"),
                        species_grazing = interaction(species, grazing, sep = " ") %>%
                          fct_relevel("Ascophyllum nodosum Control", "Ascophyllum nodosum Grazed",
                                      "Fucus vesiculosus Control")),
               aes(t, m, colour = temperature), 
               size = 1, shape = 16, alpha = 0.6) +
    geom_text(data = . %>% distinct(temperature, species_grazing) %>%
                mutate(
                  label_species = case_when(
                    temperature == "5°C" & 
                      species_grazing == "Ascophyllum nodosum Control" ~
                      "Ascophyllum nodosum",
                    temperature == "5°C" & 
                      species_grazing == "Fucus vesiculosus Control" ~
                      "Fucus vesiculosus"
                  ),
                  label_grazing = case_when(
                    temperature == "5°C" & 
                      species_grazing %>% str_detect("Control") ~
                      "Control",
                    temperature == "5°C" & 
                      species_grazing %>% str_detect("Grazed") ~
                      "Grazed"
                  )
                ) %>%
                pivot_longer(cols = contains("label"),
                             values_to = "label",
                             names_prefix = "label_") %>%
                mutate(
                  y = case_when(
                    species_grazing %>% str_detect("Asco") &
                      name == "species" ~ 4,
                    species_grazing %>% str_detect("Fucus") &
                      name == "species" ~ 3,
                    species_grazing == "Ascophyllum nodosum Control" &
                      name == "grazing" ~ 3.5,
                    species_grazing == "Ascophyllum nodosum Grazed" &
                      name == "grazing" ~ 2,
                    species_grazing == "Fucus vesiculosus Control" &
                      name == "grazing" ~ 2.5,
                    species_grazing == "Fucus vesiculosus Grazed" &
                      name == "grazing" ~ 3
                  ),
                  fontface = if_else(name == "species", "italic", "plain")
                ),
              aes(x = 3.3, y = y, label = label, fontface = fontface), 
              family = "Futura", size = 8, size.unit = "pt", hjust = 0, vjust = 1) +
    scale_colour_manual(values = c("#2e4a5b", "#6a98b4", "#f5a54a", "#d1750c"),
                        guide = "none") +
    scale_fill_manual(values = c("#2e4a5b", "#6a98b4", "#f5a54a", "#d1750c"),
                      guide = "none") +
    scale_alpha_manual(values = c(0.5, 0.4, 0.3), guide = "none") +
    labs(x = expression(italic("t")*" (days)"),
         y = expression(italic(bar("m")))) +
    facet_grid(species_grazing ~ temperature, scales = "free", space = "free") +
    facetted_pos_scales(
      x = list(
        temperature == "5°C" ~
          scale_x_continuous(limits = c(0, 240),
                             breaks = seq(0, 240, by = 30)),
        temperature == "10°C" ~
          scale_x_continuous(limits = c(0, 150),
                             breaks = seq(0, 150, by = 30)),
        temperature == "15°C" ~
          scale_x_continuous(limits = c(0, 90),
                             breaks = seq(0, 90, by = 30)),
        temperature == "18°C" ~
          scale_x_continuous(limits = c(0, 120),
                             breaks = seq(0, 120, by = 30))
      ),
      y = list(
        species_grazing == "Ascophyllum nodosum Control" ~
          scale_y_continuous(limits = c(0, 4),
                             breaks = 0:4),
        species_grazing == "Ascophyllum nodosum Grazed" ~
          scale_y_continuous(limits = c(0, 2),
                             breaks = 0:2),
        species_grazing == "Fucus vesiculosus Control" ~
          scale_y_continuous(limits = c(0, 3),
                             breaks = 0:3),
        species_grazing == "Fucus vesiculosus Grazed" ~
          scale_y_continuous(limits = c(0, 3),
                             breaks = 0:3)
      )
    ) +
    coord_cartesian(expand = F, clip = "off") +
    mytheme +
    theme(strip.text = element_text(hjust = 0),
          strip.text.y = element_blank(),
          plot.margin = margin(0.45, 0.45, 0, 0.45, unit = "cm"))

# Save
Fig_S6 %>%
  ggsave(filename = "Figure_S6.pdf", path = "Figures",
         device = cairo_pdf, width = 20, height = 12, units = "cm")
# Warnings can be ignored. They're just about the missing text in most panels.

# Final clean up
rm(list = ls())
dev.off()