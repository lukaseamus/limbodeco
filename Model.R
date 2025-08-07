
# Model introduction

# k <- function(t, r, t0, kmin, kd) {
#   kd / ( 1 + exp( ( t - t0 ) / r ) ) + kmin
# }
# 
# m <- function(t, m0, r, t0, kmin, kd) {
#   m0 * exp(
#   -(
#     kmin * t + kd * ( 
#       t - r * log( 
#         ( 1 + exp( -(t0 - t) / r ) ) / 
#           ( 1 + exp( -t0 / r ) )
#         ) 
#       )
#     )
#   )
# }


# k <- function(t, r, t0, kmin, kd) {
#   kd / ( 1 + exp( -r * ( t - t0 ) ) ) + kmin
# }
# 
# m <- function(t, m0, r, t0, kmin, kd) {
#   m0 * exp(
#     -(
#       t * (kmin + kd) + kd/r * log( 
#           ( 1 + exp( -r * (t - t0) ) ) / 
#             ( 1 + exp( r * t0 ) )
#           ) 
#       )
#     )
# }

k <- function(t, r, t_d, k_g, k_d) {
  (k_g + k_d) / ( 1 + exp( -r * ( t - t_d ) ) ) - k_g
}

m <- function(t, m_0, r, t_d, k_g, k_d) {
  m_0 * exp(
    -(
      t * k_d + (k_g + k_d) / r * 
        log( 
        ( 1 + exp( -r * (t - t_d) ) ) / 
          ( 1 + exp( r * t_d ) )
        ) 
    )
  )
}



require(tidyverse)
r_flex <- expand_grid(
  t = seq(0, 10, 0.015),
  m_0 = 1,
  r = seq(0.01, 4, length.out = 10),
  t_d = 4,
  k_g = 0.05,
  k_d = 1
) %>%
  mutate(m = m(t, m_0, r, t_d, k_g, k_d),
         k = k(t, r, t_d, k_g, k_d))

t_d_flex <- expand_grid(
  t = seq(0, 10, 0.015),
  m_0 = 1,
  r = 2,
  t_d = seq(0, 6, length.out = 10),
  k_g = 0.05,
  k_d = 1
) %>%
  mutate(m = m(t, m_0, r, t_d, k_g, k_d),
         k = k(t, r, t_d, k_g, k_d))

k_g_flex <- expand_grid(
  t = seq(0, 10, 0.015),
  m_0 = 1,
  r = 2,
  t_d = 4,
  k_g = seq(0, 0.15, length.out = 10),
  k_d = 1
) %>%
  mutate(m = m(t, m_0, r, t_d, k_g, k_d),
         k = k(t, r, t_d, k_g, k_d))

k_d_flex <- expand_grid(
  t = seq(0, 10, 0.015),
  m_0 = 1,
  r = 2,
  t_d = 4,
  k_g = 0.05,
  k_d = seq(0.4, 1, length.out = 10)
) %>%
  mutate(m = m(t, m_0, r, t_d, k_g, k_d),
         k = k(t, r, t_d, k_g, k_d))

mytheme <- theme(
  panel.background = element_blank(),
  panel.grid = element_blank(),
  panel.border = element_rect(fill = NA),
  text = element_text(family = "Futura"),
  axis.title = element_text(face = "italic", size = 10),
  axis.text = element_text(colour = "black", size = 8),
  axis.ticks = element_line(colour = "black"),
  legend.title = element_text(face = "italic", size = 10),
  legend.text = element_text(size = 8),
  legend.justification = "left",
  legend.background = element_blank(),
  legend.margin = margin(0, 0, -8, 0)
)


Fig_1a <- 
  r_flex %>%
  ggplot(aes(x = t, y = k, colour = r, group = r)) +
    geom_hline(yintercept = 0) +
    geom_line() +
    scale_color_gradient(low = "#dbdddf", high = "black",
                         breaks = c(0.01, 4),
                         labels = scales::label_number(accuracy = c(0.01, 1)),
                         guide = guide_colourbar(ticks = F,
                                                 label.hjust = c(0, 1),
                                                 title.position = "top",
                                                 barheight = unit(0.1, "cm"),
                                                 barwidth = unit(4.15, "cm"))) +
    scale_x_continuous(breaks = seq(0, 10, 2),
                       limits = c(0, 10)) +
    scale_y_continuous(breaks = seq(-0.2, 1, 0.2),
                       limits = c(-0.2, 1),
                       labels = scales::label_number(accuracy = c(0.1, 1, rep(0.1, 4), 1),
                                                     style_negative = "minus")) +
    annotate("text", x = 6.3, y = c(0.4, 0.25, 0.1), 
             label = c("italic(t[d])*' = 4'", 
                       "italic(k[g])*' = 0.05'", 
                       "italic(k[d])*' = 1'"),
             hjust = 0, family = "Futura", size = 2.8, parse = T) +
    coord_cartesian(expand = F, clip = "off") +
    mytheme +
    theme(legend.position = "top",
          axis.title.x = element_blank(),
          axis.text.x = element_blank())

Fig_1b <- 
  r_flex %>%
  ggplot(aes(x = t, y = m, colour = r, group = r)) +
    geom_hline(yintercept = 1) +
    geom_line() +
    scale_color_gradient(low = "#dbdddf", high = "black") +
    scale_x_continuous(breaks = seq(0, 10, 2),
                       limits = c(0, 10)) +
    scale_y_continuous(breaks = seq(0, 1.5, 0.5),
                       limits = c(0, 1.5),
                       labels = scales::label_number(accuracy = c(1, 0.1, 1, 0.1),
                                                     style_negative = "minus")) +
    coord_cartesian(expand = F, clip = "off") +
    mytheme +
    theme(legend.position = "none")

Fig_1c <-
  t_d_flex %>%
  ggplot(aes(x = t, y = k, colour = t_d, group = t_d)) +
    geom_hline(yintercept = 0) +
    geom_line() +
    scale_color_gradient(low = "#dbdddf", high = "black",
                         breaks = c(0, 6),
                         guide = guide_colourbar(ticks = F,
                                                 label.hjust = c(0, 1),
                                                 title = expression(italic(t[d])),
                                                 title.position = "top",
                                                 barheight = unit(0.1, "cm"),
                                                 barwidth = unit(4.15, "cm"))) +
    scale_x_continuous(breaks = seq(0, 10, 2),
                       limits = c(0, 10)) +
    scale_y_continuous(breaks = seq(-0.2, 1, 0.2),
                       limits = c(-0.2, 1),
                       labels = scales::label_number(accuracy = c(0.1, 1, rep(0.1, 4), 1),
                                                     style_negative = "minus")) +
    annotate("text", x = 6.3, y = c(0.4, 0.25, 0.1), 
             label = c("italic(r)*' = 2'", 
                       "italic(k[g])*' = 0.05'", 
                       "italic(k[d])*' = 1'"),
             hjust = 0, family = "Futura", size = 2.8, parse = T) +
    coord_cartesian(expand = F, clip = "off") +
    mytheme +
    theme(legend.position = "top",
          axis.title = element_blank(),
          axis.text = element_blank())

Fig_1d <-
  t_d_flex %>%
  ggplot(aes(x = t, y = m, colour = t_d, group = t_d)) +
    geom_hline(yintercept = 1) +
    geom_line() +
    scale_color_gradient(low = "#dbdddf", high = "black") +
    scale_x_continuous(breaks = seq(0, 10, 2),
                       limits = c(0, 10)) +
    scale_y_continuous(breaks = seq(0, 1.5, 0.5),
                       limits = c(0, 1.5),
                       labels = scales::label_number(accuracy = c(1, 0.1, 1, 0.1),
                                                     style_negative = "minus")) +
    coord_cartesian(expand = F, clip = "off") +
    mytheme +
    theme(legend.position = "none",
          axis.title.y = element_blank(),
          axis.text.y = element_blank())

Fig_1e <-
  k_g_flex %>%
  ggplot(aes(x = t, y = k, colour = k_g, group = k_g)) +
    geom_hline(yintercept = 0) +
    geom_line() +
    scale_color_gradient(low = "#dbdddf", high = "black",
                         breaks = c(0, 0.15),
                         labels = scales::label_number(accuracy = c(1, 0.01)),
                         guide = guide_colourbar(ticks = F,
                                                 label.hjust = c(0, 1),
                                                 title = expression(italic(k[g])),
                                                 title.position = "top",
                                                 barheight = unit(0.1, "cm"),
                                                 barwidth = unit(4.15, "cm"))) +
    scale_x_continuous(breaks = seq(0, 10, 2),
                       limits = c(0, 10)) +
    scale_y_continuous(breaks = seq(-0.2, 1, 0.2),
                       limits = c(-0.2, 1),
                       labels = scales::label_number(accuracy = c(0.1, 1, rep(0.1, 4), 1),
                                                     style_negative = "minus")) +
    annotate("text", x = 6.3, y = c(0.4, 0.25, 0.1), 
             label = c("italic(r)*' = 2'",
                       "italic(t[d])*' = 4'",
                       "italic(k[d])*' = 1'"),
             hjust = 0, family = "Futura", size = 2.8, parse = T) +
    coord_cartesian(expand = F, clip = "off") +
    mytheme +
    theme(legend.position = "top",
          axis.title = element_blank(),
          axis.text = element_blank())

Fig_1f <-
  k_g_flex %>%
  ggplot(aes(x = t, y = m, colour = k_g, group = k_g)) +
    geom_hline(yintercept = 1) +
    geom_line() +
    scale_color_gradient(low = "#dbdddf", high = "black") +
    scale_x_continuous(breaks = seq(0, 10, 2),
                       limits = c(0, 10)) +
    scale_y_continuous(breaks = seq(0, 1.5, 0.5),
                       limits = c(0, 1.5),
                       labels = scales::label_number(accuracy = c(1, 0.1, 1, 0.1),
                                                     style_negative = "minus")) +
    coord_cartesian(expand = F, clip = "off") +
    mytheme +
    theme(legend.position = "none",
          axis.title.y = element_blank(),
          axis.text.y = element_blank())

Fig_1g <-
  k_d_flex %>%
  ggplot(aes(x = t, y = k, colour = k_d, group = k_d)) +
    geom_hline(yintercept = 0) +
    geom_line() +
    scale_color_gradient(low = "#dbdddf", high = "black",
                         breaks = c(0.4, 1),
                         labels = scales::label_number(accuracy = c(0.1, 1),
                                                       style_negative = "minus"),
                         guide = guide_colourbar(ticks = F,
                                                 label.hjust = c(0, 1),
                                                 title = expression(italic(k[d])),
                                                 title.position = "top",
                                                 barheight = unit(0.1, "cm"),
                                                 barwidth = unit(4.15, "cm"))) +
    scale_x_continuous(breaks = seq(0, 10, 2),
                       limits = c(0, 10)) +
    scale_y_continuous(breaks = seq(-0.2, 1, 0.2),
                       limits = c(-0.2, 1),
                       labels = scales::label_number(accuracy = c(0.1, 1, rep(0.1, 4), 1),
                                                     style_negative = "minus")) +
    annotate("text", x = 6.3, y = c(0.4, 0.25, 0.1), 
             label = c("italic(r)*' = 2'",
                       "italic(t[d])*' = 4'",
                       "italic(k[g])*' = 0.05'"),
             hjust = 0, family = "Futura", size = 2.8, parse = T) +
    coord_cartesian(expand = F, clip = "off") +
    mytheme +
    theme(legend.position = "top",
          axis.title = element_blank(),
          axis.text = element_blank())

Fig_1h <-
  k_d_flex %>%
  ggplot(aes(x = t, y = m, colour = k_d, group = k_d)) +
    geom_hline(yintercept = 1) +
    geom_line() +
    scale_color_gradient(low = "#dbdddf", high = "black") +
    scale_x_continuous(breaks = seq(0, 10, 2),
                       limits = c(0, 10)) +
    scale_y_continuous(breaks = seq(0, 1.5, 0.5),
                       limits = c(0, 1.5),
                       labels = scales::label_number(accuracy = c(1, 0.1, 1, 0.1),
                                                     style_negative = "minus")) +
    coord_cartesian(expand = F, clip = "off") +
    mytheme +
    theme(legend.position = "none",
          axis.title.y = element_blank(),
          axis.text.y = element_blank())

require(patchwork)
Fig_1 <- 
  ( Fig_1a | Fig_1c | Fig_1e | Fig_1g ) /
  ( Fig_1b | Fig_1d | Fig_1f | Fig_1h )

ggsave(Fig_1, filename = "Fig_1.pdf", path = "~/Desktop",
       device = cairo_pdf, width = 20, height = 12, units = "cm")


# Model implementation
# Load data from the two most striking examples of derital photosynthesis:

# Brouwer et  al. 1996 (doi: 10.1007/BF02390433), digitised from Figure 4 and Table 1
Brouwer <- read.csv("~/Desktop/Brouwer.csv") %>%
  mutate(sem = sd / sqrt( n ),
         t = as.numeric(t),
         group = as.factor(group)) %>%
  select(-c(sd, n))

Brouwer %>%
  ggplot(aes(x = t, y = m, group = group)) +
  geom_pointrange(aes(ymin = m - sem, ymax = m + sem)) +
  geom_line() +
  mytheme

# Frontier et al. 2021 (doi: 10.1016/j.marenvres.2021.105277), available at
# https://github.com/nadiafrontier/The-degradation-of-kelp-detritus/blob/master/Biomass%20lost.csv
Frontier <- read.csv("~/Desktop/Biomass lost.csv") %>%
  mutate(ID = as.factor(paste0(Code, "_", Depth)),
         Species = as.factor(Species),
         Depth = as.factor(Depth),
         t = as.numeric( Days - Days[ Period == "T1" ] )) %>%
  group_by(ID) %>%
  mutate(m = Biomass / Biomass[ Period == "T1" ]) %>%
  ungroup() %>%
  select(ID, Species, Depth, t, m)

Frontier %>%
  ggplot(aes(x = t, y = m, group = ID)) +
    geom_point() +
    geom_line() +
    facet_grid(Depth ~ Species) +
    mytheme

# Analysis
# Brouwer et al. 1996
# Frequentist (nonlinear least squares with maximum likelihood estimation)
r <- 1 # fix r at 1 since transition from one decay state to the next (i.e. detrital death)
# can be assumed to be instantaneous and 1 reflects this and is convenient

Brouwer_nls_mod <- nls(
  m ~ exp(
    -( # kmax does not logically vary across states (alive vs. dead) of the same species
      t * kmax + (kmax - kmin[group]) / r * 
        log( 
          ( 1 + exp( -r * (t - t0[group]) ) ) / 
            ( 1 + exp( r * t0[group] ) )
          ) 
      )
    ),
  start = list(t0 = c(320, 0), # note order of factor levels: alive, dead
               kmin = c(0.001, 0.07),
               kmax = 0.07), # no factoring for kmax, so only one starting value
  lower = c(t0 = c(300, 0), # t0 and kmin are unbounded
            kmin = c(-0.1, 0), 
            kmax = 0),
  upper = c(t0 = c(330, 20), # t0 and kmin are unbounded
            kmin = c(1, 1), 
            kmax = 1),
  weights = 1/sem, # weighted by inverse of standard error
  data = Brouwer,
  algorithm = "port")



summary(Brouwer_nls_mod)

Brouwer %>%
  group_by(group) %>%
  nls(
    m ~ exp(
      -(
        t * kmax + (kmax - kmin) / r * 
          log( 
            ( 1 + exp( -r * (t - t0) ) ) / 
              ( 1 + exp( r * t0 ) )
          ) 
      )
    ),
    start = list(t0 = c(320, 0), # note order of factor levels: alive, dead
                 kmin = c(0.001, 0.07),
                 kmax = 0.07), # no factoring for kmax, so only one starting value
    lower = c(t0 = c(300, 0), # t0 and kmin are unbounded
              kmin = c(-0.1, 0), 
              kmax = 0),
    upper = c(t0 = c(330, 20), # t0 and kmin are unbounded
              kmin = c(1, 1), 
              kmax = 1),
    weights = 1/sem, # weighted by inverse of standard error
    data = Brouwer,
    algorithm = "port")


require(broom)
Brouwer %>%
  nest_by(group) %>%
  mutate(mod = list(lm(m ~ t, data = data))) %>%
  reframe(tidy(mod))

Brouwer %>%
  nest_by(group) %>%
  mutate(mod = list(
    nlsLM(
      m ~ exp(
        -(
          t * 1 + (1 - k_min) / 1 * 
            log( 
              ( 1 + exp( -1 * (t - t_d) ) ) / 
                ( 1 + exp( 1 * t_d ) )
              ) 
          )
        ),
      start = list(t_d = 150, # note order of factor levels: alive, dead
                   k_min = 0), # no factoring for kmax, so only one starting value
      lower = c(t_d = 0,
                k_min = -0.1),
      upper = c(t_d = 350,
                k_min = 1),
      data = data)
    )) %>%
  reframe(tidy(mod))


nlsLM(
  m ~ exp(
    -(
      t * k_max + (k_max - k_min) / 1 * 
        log( 
          ( 1 + exp( -1 * (t - t_d) ) ) / 
            ( 1 + exp( 1 * t_d ) )
        ) 
    )
  ),
  start = list(t_d = 0, # note order of factor levels: alive, dead
               k_min = 0.01,
               k_max = 0.1), # no factoring for kmax, so only one starting value
  lower = c(t_d = 0,
            k_min = -0.1,
            k_max = 0),
  upper = c(t_d = 300,
            k_min = 1,
            k_max = 1),
  data = Brouwer %>% filter(group == "dead"))


require(lme4)
nlmer(formula = m ~ exp(
  -(
    t * k_max + (k_max - k_min) / 1 * 
      log( 
        ( 1 + exp( -1 * (t - t_d) ) ) / 
          ( 1 + exp( 1 * t_d ) )
      ) 
  )
) ~ group,
data = Brouwer,
start = list(nlpars = c(t_d = c(150, 0), 
             k_min = c(0, 0),
             k_max = c(0.1, 0.1))))


# Simple Bayesian (Hamiltonian Monte Carlo with integral)
# Prior simulation

require(magrittr)
Brouwer.prior.sim <- 
  tibble(n = 1:1e3,
         r = rgamma(n = 1e3, shape = 1^2 / 0.1^2, rate = 1 / 0.1^2),
         t_d = runif(n = 1e3, min = min(Brouwer$t), max = max(Brouwer$t)),
         k_g = rnorm(n = 1e3, mean = 0, sd = 5e-4),
         k_d = rgamma(n = 1e3, shape = 0.1^2 / 0.1^2, rate = 0.1 / 0.1^2)) %>% 
  expand_grid(t = Brouwer %$% seq(min(t), max(t), length.out = 1e3)) %>%
  mutate(k = k(t, r, t_d, k_g, k_d),
         m = m(t, m_0 = 1, r, t_d, k_g, k_d))

Fig_S1a <- 
  ggplot(Brouwer.prior.sim,
         aes(x = t, y = k, group = n)) +
    geom_hline(yintercept = 0) +
    geom_line(alpha = 0.05) +
    geom_line(data = Brouwer.prior.sim %>% filter(n == 1),
              aes(x = t, y = k(t = t, r = 1, t_d = max(Brouwer$t) / 2, 
                               k_g = 0, k_d = 0.1)),
              colour = "white") +
    scale_x_continuous(limits = c(0, 300)) +
    scale_y_continuous(limits = c(0, 1),
                       labels = scales::label_number(accuracy = c(1, 0.01, 0.1, 0.01, 1))) +
    coord_cartesian(expand = F, clip = "off") +
    mytheme +
    theme(axis.title.x = element_blank(),
          axis.text.x = element_blank())

Fig_S1b <- 
  ggplot(Brouwer.prior.sim,
         aes(x = t, y = m, group = n)) +
    geom_hline(yintercept = 1) +
    geom_line(alpha = 0.05) +
    geom_line(data = Brouwer.prior.sim %>% filter(n == 1),
              aes(x = t, y = m(t = t, m_0 = 1, r = 1, t_d = max(Brouwer$t) / 2, 
                               k_g = 0, k_d = 0.1)),
              colour = "white") +
    scale_x_continuous(limits = c(0, 300)) +
    scale_y_continuous(limits = c(0, 1.2),
                       breaks = seq(0, 1.2, 0.4),
                       labels = scales::label_number(accuracy = c(1, rep(0.1, 3)))) +
    coord_cartesian(expand = F, clip = "off") +
    mytheme

Fig_S1 <- Fig_S1a / Fig_S1b
ggsave(Fig_S1, filename = "Fig_S1.pdf", path = "~/Desktop",
       device = cairo_pdf, width = 7, height = 12, units = "cm")


# Model
Brouwer_hmc_int_stan <- "
data{
  int n;
  int n_group;
  vector[n] m;
  vector<lower=0>[n] sem;
  vector[n] t;
  array[n] int group;
}

transformed data{
  real t_min = min(t);
  real t_max = max(t);
}

parameters{
  vector[n] m_true;
  real<lower=0> r;
  vector<lower=t_min,
         upper=t_max>[n_group] t_d;
  vector[n_group] k_g;
  real<lower=0> k_d;
  real<lower=0> sigma;
}

model{
  // Priors
  r ~ gamma( 1^2 / 0.1^2, 1 / 0.1^2 ); // reparameterised with mean and sd
  t_d ~ uniform( t_min, t_max );
  k_g ~ normal( 0, 5e-4 );
  k_d ~ gamma( 0.1^2 / 0.1^2, 0.1 / 0.1^2 ); // reparameterised with mean and sd
  sigma ~ exponential( 1 );

  // Model using integral of dynamic exponential decay function
  vector[n] mu;
  for ( i in 1:n ) {
    mu[i] = exp(
    -(
      t[i] * k_d + (k_g[ group[i] ] + k_d) / r * 
        log( 
        ( 1 + exp( -r * ( t[i] - t_d[ group[i] ] ) ) ) / 
          ( 1 + exp( r * t_d[ group[i] ] ) )
        ) 
    )
  );
  }

  // Likelihood incorporating measurement error
  m_true ~ normal( mu , sigma );
  m ~ normal( m_true , sem );
}
"

# require(rstan)
# Brouwer_hmc_int_mod <- 
#   stan(model_code = Brouwer_hmc_int_stan,
#        data = compose_data(Brouwer), 
#        iter = 2e4, warmup = 1e4, # 1e4 samples per chain
#        chains = 8, cores = parallel::detectCores())


require(cmdstanr)
Brouwer_hmc_int_mod <- 
  cmdstan_model(stan_file = 
                  write_stan_file(code = Brouwer_hmc_int_stan)
                )

require(tidybayes)
Brouwer_hmc_int_samples <- 
  Brouwer_hmc_int_mod$sample(data = compose_data(Brouwer),
                             seed = 100,
                             chains = 8,
                             parallel_chains = parallel::detectCores(),
                             iter_warmup = 1e4,
                             iter_sampling = 1e4)


# check Rhat, n_eff and chains
options(cmdstanr_max_rows = 1e3)
Brouwer_hmc_int_samples

Brouwer_hmc_int_summary <- 
  Brouwer_hmc_int_samples$summary()

Brouwer_hmc_int_draws <- 
  Brouwer_hmc_int_samples$draws(format = "df")

require(bayesplot)
mcmc_rank_overlay(Brouwer_hmc_int_draws)
mcmc_pairs(Brouwer_hmc_int_draws, pars = c("r[1]", "t_d[1]", "k_g[1]", "k_d"))
mcmc_pairs(Brouwer_hmc_int_draws, pars = c("r[2]", "t_d[2]", "k_g[2]", "k_d"))

Brouwer_hmc_int_prior_posterior <- 
  Brouwer_hmc_int_draws %>%
  spread_draws(t0[group], kmin[group], kmax, sigma)





stancode(Brouwer.HMC.int.mod)
trankplot(Brouwer.HMC.int.mod)


# Advanced Bayesian (Hamiltonian Monte Carlo with ordinary differential equation solver)

Brouwer_hmc_ode_stan <- "
functions {
  vector mdd(real t,
             vector m,
             real r,
             real t_d,
             real k_min,
             real k_max) {
     
     // Logistic function describing change in k
     real k = (k_max - k_min) / ( 1 + exp( -r * ( t - t_d ) ) ) + k_min;
     
     // Ordinary Differential Equation
     vector[1] dm_dt;
     dm_dt[1] = -k * m[1];
     return dm_dt;
     }
}

data {
  int n;
  int n_group;
  vector[n] m;
  vector<lower=0>[n] sem;
  array[n] real t;
  array[n] int group;
  real t0;
  vector[1] m0;
  int n_pred;
  array[n_pred] real t_pred;
}

parameters {
  vector[n] m_true;
  real<lower=0> r;
  vector[n_group] t_d;
  vector[n_group] k_min;
  real<lower=0> k_max;
  real<lower=0> sigma;
}

model {
  // Priors
  r ~ gamma( 1^2 / 0.1^2, 1 / 0.1^2 );
  t_d ~ normal(150, 60);
  k_min ~ normal(1e-3, 5e-4);
  k_max ~ gamma( 0.1^2 / 5e-3^2, 0.1 / 5e-3^2 );
  sigma ~ exponential(1);

  // Model
  array[n] vector[1] mu_array;
  vector[n] mu;

  mu_array = ode_rk45(mdd, m0, t0, t, 
                      r, t_d[group[i]], k_min[group[i]], k_max);

  for (i in 1:n) {
    mu[i] = mu_array[i, 1];
  }

  // Likelihood incorporating standard error
  m_true ~ normal(mu, sigma);
  m ~ normal(m_true, sem);
}

generated quantities {
  array[n_pred] vector[1] mu_pred = ode_rk45(mdd, m0, t0, t_pred, 
                                             r, t_d, k_min, k_max);
}
"

require(cmdstanr)
Brouwer_hmc_ode_mod <- 
  cmdstan_model(stan_file = 
                  write_stan_file(code = Brouwer_hmc_ode_stan)
  )

require(tidybayes)
Brouwer_hmc_ode_samples <- 
  Brouwer_hmc_ode_mod$sample(data = c(compose_data(Brouwer),
                                      list(t0 = 0, m0 = 1, 
                                           n_pred = length(seq(0, max(Brouwer$t), 1)),
                                           t_pred = seq(0, max(Brouwer$t), 1))),
                             seed = 100,
                             chains = 8,
                             parallel_chains = parallel::detectCores(),
                             iter_warmup = 1e4,
                             iter_sampling = 1e4)


# functions {
#   // Define the ODE function
#   real f(real t, real m, real k) {
#     return -k * m;
#   }
#   
#   // Define the time-dependent decay rate function k(t)
#   real k(real t, real kmax, real kmin, real r, real t0) {
#     return kmax / (1 + exp(-r * (t - t0))) + kmin;
#   }
# }
# 
# data {
#   int<lower=0> N; // Number of time points
#   real t[N]; // Time points
#   real y[N]; // Observed values
#   real kmax; // Parameter kmax
#   real kmin; // Parameter kmin
#   real r; // Parameter r
#   real t0; // Parameter t0
# }
# 
# parameters {
#   real<lower=0> m0; // Initial value of m
# }
# 
# transformed parameters {
#   real<lower=0> m[N];
#   
#   // Solve the ODE using the integrate_ode_rk45 function
#   m = integrate_ode_rk45(f, m0, t, k(t, kmax, kmin, r, t0), 1e-6, 1e-6);
# }
# 
# model {
#   // Likelihood
#   y ~ normal(m, 0.1); // Assuming some noise in the observations
# }
# 


# Frontier et al. 2021

require(nlme)
Brouwer_nlme_mod <- nlme(
  model = m ~ exp(
    -(t * (kmin + kmax) + kmax / 1 * 
        log(
          (1 + exp(-1 * (t - t0))) /
            (1 + exp(1 * t0))
        )
    )
  ),
  fixed = list(t0 ~ group,
               kmin ~ group,
               kmax ~ 1),
  groups = ~ group,
  start = list(fixed = c(t0 = c(300, 0),
                         kmin = c(-1e-3, 0.1),
                         kmax = 0.1)),
  data = Brouwer)

summary(Brouwer_nlme_mod)

stan_code <- "
data {
  int n;         // Number of observations
  int n_ID;      // Number of levels in ID grouping variable
  int n_Species; // Number of levels in Species grouping variable
  int n_Depth;   // Number of levels in Depth grouping variable
  vector[n] m;   // Response variable
  vector[n] t;   // Predictor variable
  vector[n] ID;  // ID grouping variable
  vector[n] Species; // Species grouping variable
  vector[n] Depth;   // Depth grouping variable
}

parameters {
  real<lower=0> sigma; // Standard deviation
  real<lower=0> r;  // Inverse of logistic rate of change in k
  real<lower=0> t0; // Inflection point of change in k
  real kmin; // Minimum k
  real<lower=0> kmax; // Maximum increase in k
}

model {
  // Priors
  sigma ~ exponential( 1 );
  r ~ lognormal( 1, 1 );
  t0 ~ lognormal( 1, 1 );
  kmin ~ normal( 0 , 1 );
  kmax ~ lognormal( 0.5 , 1 );

  // Prediction function
  vector[n] mu; // Mean prediction for y
  for( i in 1:n ) {
    mu[i] = exp( -(
    kmin * t + kmax * ( 
      t + r * log( 
        ( 1 + exp( -(t0 - t) / -r ) ) / 
          ( 1 + exp( -t0 / -r ) ) 
      ) ) ) ); // Calculate mu
                   }

  // Likelihood
  m ~ normal( mu, sigma );
}
"

compose_data(Frontier)


require(rethinking)
Brouwer.HMC.int.mod <- ulam(
  alist(
    # response measurement error
    m ~ dnorm(mean = m_true, sd = sem), # here the observed measurement error is introduced
    vector[n]:m_true ~ dnorm(mean = mu, sd = sigma), # this describes the true, unobserved m
    
    # model using integral of dynamic exponential decay function
    mu <- exp(
      -(
        t * ( 1 + kmin[group] ) + 1 / r[group] * log( 
          ( 1 + exp( -r[group] * ( t - t0[group] ) ) ) / 
            ( 1 + exp( r[group] * t0[group] ) )
        ) 
      )
    ),
    
    # Priors
    r[group] ~ dlnorm(meanlog = log(0.4), sdlog = 0.04),
    t0[group] ~ dlnorm(meanlog = log(100), sdlog = 0.4),
    kmin[group] ~ dnorm(mean = 1e-3, sd = 8e-4),
    sigma ~ dexp(rate = 1) # standard deviation
  ),
  data = compose_data(Brouwer), chains = 12, 
  cores = parallel::detectCores(), iter = 1e4,
)


precis(Brouwer.HMC.int.mod, depth = 2)
stancode(Brouwer.HMC.int.mod)
