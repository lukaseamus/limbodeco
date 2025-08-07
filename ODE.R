
require(tidyverse)
set.seed(100)
df <- tibble(t = seq(2, 100, 10),
             m = exp(-0.05*seq(2, 100, 10)),
             sem = sort(rgamma(10, 0.04^2 / 0.04^2, 0.04 / 0.04^2),
                        decreasing = T))
df %>%
  ggplot(aes(t, m)) +
    geom_pointrange(aes(t, m, ymin = m - sem, ymax = m + sem)) +
    scale_y_continuous(limits = c(0, 1))

# Stan
ode_stan <- "
functions {
  vector mdd(real t,
             vector m,
             real k) {
     
     // Ordinary Differential Equation
     vector[1] dm_dt;
     dm_dt[1] = -k * m[1];
     return dm_dt;
     }
}

data {
  int n;
  vector[n] m;
  vector<lower=0>[n] sem;
  array[n] real t;
  real t_0;
  int n_pred;
  array[n_pred] real t_pred;
}

parameters {
  vector[1] m_0;
  vector[n] m_true;
  real<lower=0> k;
  real<lower=0> sigma;
}

model {
  // Priors
  m_0 ~ normal( 1, 0.01 );
  k ~ gamma( 0.05^2 / 0.02^2, 0.05 / 0.02^2 );
  sigma ~ exponential( 1 );

  // Model
  array[n] vector[1] mu_array;
  vector[n] mu;

  mu_array = ode_rk45(mdd, m_0, t_0, t, k);

  for (i in 1:n) {
    mu[i] = mu_array[i, 1];
  }

  // Likelihood
  m_true ~ normal(mu, sigma);
  m ~ normal(m_true, sem);
}

generated quantities {
  array[n_pred] vector[1] mu_pred = ode_rk45(mdd, m_0, t_0, t_pred, k);
}
"

require(cmdstanr)
ode_mod <- 
  cmdstan_model(stan_file = 
                  write_stan_file(code = ode_stan)
  )

require(tidybayes)
ode_samples <- 
ode_mod$sample(data = c(compose_data(df),
                        list(t_0 = 0, 
                             n_pred = length(seq(1e-10, 100, length.out = 500)),
                             t_pred = seq(1e-10, 100, length.out = 500))),
                             seed = 100,
                             chains = 8,
                             parallel_chains = parallel::detectCores(),
                             iter_warmup = 1e4,
                             iter_sampling = 1e4)

# check Rhat, n_eff and chains
options(cmdstanr_max_rows = 1e3)
ode_samples

ode_summary <- 
  ode_samples$summary()

ode_draws <- 
  ode_samples$draws(format = "df")

require(bayesplot)
mcmc_rank_overlay(ode_draws)

ode_prior_posterior <- 
  ode_draws %>%
  spread_draws(m_0[1], k, sigma) %>%
  ungroup() %>%
  mutate(
    m_0 = as.numeric(m_0),
    sigma__prior = rexp(length(.draw), 1), 
    k__prior = rgamma(length(.draw), 0.05^2 / 0.02^2, 0.05 / 0.02^2),
    m_0__prior = rnorm(length(.draw), 1, 0.01)
    )

ode_prior_posterior %>%
  pivot_longer(cols = c("k", "k__prior", "m_0", "m_0__prior"),
               names_to = c("parameter", "distribution"),
               names_sep = "__", # this will produce NAs and throw a warning message 
               values_to = "samples") %>%
  mutate(parameter = fct(parameter),
         distribution = fct(ifelse(is.na(distribution), # here the NAs are dealt with
                                   "posterior", distribution))) %>%
  ggplot(aes(samples, fill = distribution)) +
    facet_wrap(~parameter, scales = "free", nrow = 1) +
    geom_density(colour = NA, alpha = 0.5) +
    theme_minimal() +
    theme(legend.position = "top", legend.justification = 0)

ode_pred <- 
  ode_draws %>%
  spread_draws(mu_pred[timepoint, state]) %>%
  mean_qi(mu_pred, .width = c(.5, .8, .9)) %>%
  rename(mean = mu_pred) %>%
  mutate(time = rep(seq(1e-10, 100, length.out = 500), 3))

ode_pred %>%
  ggplot() +
    geom_line(aes(x = time, y = mean)) +
    geom_ribbon(aes(x = time, y = mean, ymin = .lower, ymax = .upper, 
                    alpha = factor(.width))) +
    geom_pointrange(data = df, aes(t, m, ymin = m - sem, ymax = m + sem)) +
    scale_alpha_manual(values = c(0.5, 0.4, 0.3), guide = "none") +
    lims(y = c(0, NA)) +
    theme_minimal()


require(rbi)