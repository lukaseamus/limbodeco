#### limbodeco: a model of macroalgal decomposition ####
#### Part 1: Mathematical model and simulation      ####
#### Luka Seamus Wright                             ####

# 1. Mathematical model ####
# The inspiration for this came from Rovira & Rovira 
# (2010, doi: 10.1016/j.geoderma.2009.11.033) and
# the model contributed by Bojana Manojlovic to 
# Trevathan-Tackett et al. (2020, doi: 
# 10.1016/j.scitotenv.2019.135806).

# Taking the logistic function from Rovira & Rovira
# a / ( 1 + exp( ( t - t0 ) / b ) ) + c
# and its integral
# c * t + a * (
# t - b * 
# log( 
# ( 1 + exp( -( t0 - t ) / b ) ) / 
# ( 1 + exp( -t0 / b )  )
# )
# )

# a is the change in y, c is the y minimum, t0 is
# the midpoint and b is the inverse logistic rate.
# Rather than the change in y, I want to parameterise
# the y maximum. My logistic function to descibe the
# decline in detrital photosynthesis is

# ( alpha + tau ) / ( 1 + exp( r * ( t - mu ) ) ) - tau

# where alpha is the initial net photosynthesis or 
# relative growth, tau is the final net photosynthesis
# or relative decomposition, r is the logistic decay  
# rate and mu is the half-life of photosynthesis.
# Since r and mu are not independent and are therefore
# difficult to estimate, it is best to remove one of
# these parameters. There are two possible approaches:

# 1. Treat r as a constant. This assumes that the
# transition  always happens at the same rate and
# doesn't commence at t = 0. The pattern of transition 
# is unaffected by when it happens.

# 2. Express r as a function of mu. This assumes
# that the transition from alive to dead commences 
# at t = 0 (excision or detachment) and proceeds 
# at a rate inversely proportional to mu. If limbo 
# is longer (i.e. mu is greater) r is smaller.

# Option 2 seems more suitable in this context 
# because t = 0 is a natural initialising event. 
# However, it does imply that for zero initial growth
# the decline is immediate, albeit slow. This is not 
# always what we see in nature. Seaweed can persist 
# detached without any noticeable decline and once 
# growth has ceased decomposition is usually rapid. 
# This would favour option 1.

# 1.1 Option 1 ####
# A reasonable timescale for transition is one week. 
# Taking the logistic intercept to be 5, so r = 5 / mu, 
# this gives r = 5/7 = 0.71 if we expect the half-life
# to be 7 days, or r = 1 if we expect the entire transition
# to last 7 days, which is more convenient. Inserted into my
# logistic function results in this simplified version:

# ( alpha + tau ) / ( 1 + exp( t - mu ) ) - tau

# The integral with r is

# - tau * t + ( alpha + tau ) * (
# t - (1 / r) * 
# log( 
# ( 1 + exp( -( mu - t ) / (1 / r) ) ) / 
# ( 1 + exp( -mu / (1 / r) )  )
# )
# )

# Expanded to

# - tau * t + alpha * t + tau * t - ( alpha + tau ) / r *
# log( 
# ( 1 + exp( r * -( mu - t ) ) ) / 
# ( 1 + exp( r * -mu )  )
# )

# Simplified to

# t * alpha - ( alpha + tau ) / r *
# log( 
# ( 1 + exp( r * ( t - mu ) ) ) / 
# ( 1 + exp( -r * mu )  )
# )

# For r = 1 this simply is

# t * alpha - ( alpha + tau ) *
# log( 
# ( 1 + exp( t - mu ) ) / 
# ( 1 + exp( -mu )  )
# )

# R doesn't have a built-in log1p_exp function
log1p_exp <- function(x) {
  ifelse(
    x > 0, 
    x + log1p(exp(-x)),
    log1p(exp(x))
  )
}

k1 <- function(t, alpha, mu, tau) {
  ( alpha + tau ) / ( 1 + exp( t - mu ) ) - tau
}

p1 <- function(t, alpha, mu, tau) {
  exp(
    t * alpha - ( alpha + tau ) * (
      log1p_exp( t - mu ) - log1p_exp( -mu )
    )
  )
}

# 1.1 Option 2 ####
# Since r * mu is the logistic intercept (log odds
# at t = 0) and I know y is reasonably close to 
# alpha at t = 0, r * mu = 5 is a good choice.
# Solving for r gives 5 / mu. Inserted into my
# logistic function results in this simplified version:

# ( alpha + tau ) / ( 1 + exp( 5 / mu * ( t - mu ) ) ) - tau

# The integral then becomes

# - tau * t + ( alpha + tau ) * (
# t - (mu / 5) * 
# log( 
# ( 1 + exp( -( mu - t ) / (mu / 5) ) ) / 
# ( 1 + exp( -mu / (mu / 5) )  )
# )
# )

# Expanded to

# - tau * t + alpha * t + tau * t - ( alpha + tau ) * (mu / 5) *
# log( 
# ( 1 + exp( 5 / mu * -( mu - t ) ) ) / 
# ( 1 + exp( 5 / mu * -mu )  )
# )

# Simplified to

# t * alpha - ( alpha + tau ) * mu / 5 *
# log(
# ( 1 + exp( 5 / mu * ( t - mu ) ) ) /
# ( 1 + exp( -5 )  )
# )

k2 <- function(t, alpha, mu, tau) {
  ( alpha + tau ) / ( 1 + exp( 5 / mu * ( t - mu ) ) ) - tau
}

p2 <- function(t, alpha, mu, tau) {
  exp(
      t * alpha - ( alpha + tau ) * mu / 5 * (
        log1p_exp( 5 / mu * ( t - mu ) ) - log1p_exp( -5 )
      )
  )
}

# 2. Simulation ####
# 2.1 Parameter values ####
require(tidyverse)
require(magrittr)

alpha_flex <- expand_grid(
  t = seq(0, 60, 0.01),
  mu = 40,
  alpha = seq(-0.1, 0.02, length.out = 10),
  tau = 0.1
)

mu_flex <- expand_grid(
  t = seq(0, 60, 0.01),
  mu = seq(1, 60, length.out = 10),
  alpha = 0.01,
  tau = 0.1
)

tau_flex <- expand_grid(
  t = seq(0, 60, 0.01),
  mu = 40,
  alpha = 0.01,
  tau = seq(0.02, 0.2, length.out = 10)
)

# 2.2 Response values ####
sim <- bind_rows(
  alpha_flex %>% mutate(flex = "alpha"),
  mu_flex %>% mutate(flex = "mu"),
  tau_flex %>% mutate(flex = "tau")
) %>%
  mutate(k1 = k1(t, alpha, mu, tau),
         p1 = p1(t, alpha, mu, tau),
         k2 = k2(t, alpha, mu, tau),
         p2 = p2(t, alpha, mu, tau)) %>%
  pivot_longer(cols = c(alpha, mu, tau),
               values_to = "parameter_value",
               names_to = "parameter") %>%
  filter(parameter == flex) %>%
  select(-flex) %>%
  pivot_longer(cols = c(k1, p1, k2, p2),
               values_to = "response_value",
               names_to = "variable") %T>%
  print()

# 3. Visualisation ####
# 3.1 Option 1 ####
mytheme <- theme(
  plot.margin = margin(0.5, 0.5, 0, 0, unit = "cm"),
  panel.background = element_blank(),
  panel.grid = element_blank(),
  panel.border = element_rect(fill = NA, linejoin = "mitre"),
  text = element_text(family = "Futura"),
  axis.title = element_text(face = "italic", size = 10),
  axis.title.y = element_text(margin = margin(l = -0.2, unit = "cm")),
  axis.text = element_text(colour = "black", size = 8),
  axis.ticks = element_line(colour = "black"),
  legend.position = "top",
  legend.title = element_text(face = "italic", size = 10),
  legend.text = element_text(size = 8),
  legend.ticks = element_blank(),
  legend.justification = "left",
  legend.background = element_blank(),
  legend.margin = margin(0, 0, -8, 0)
)

k1_alpha <- sim %>%
  filter(variable == "k1" & parameter == "alpha") %>%
  ggplot() +
    geom_hline(yintercept = 0) +
    geom_line(aes(x = t, y = response_value, 
                  colour = parameter_value, 
                  group = parameter_value)) +
    scale_colour_gradient(low = "#d4e19e", high = "#4a7518",
                          breaks = c(-0.1, 0.02),
                          labels = scales::label_number(accuracy = c(0.1, 0.01),
                                                        style_negative = "minus"),
                          guide = guide_colourbar(label.hjust = c(0, 1),
                                                  title = expression(italic("α")),
                                                  title.position = "top",
                                                  barheight = unit(0.1, "cm"),
                                                  barwidth = unit(5.67, "cm"))) +
   scale_x_continuous(breaks = seq(0, 60, 20),
                       limits = c(0, 60)) +
    scale_y_continuous(breaks = seq(-0.2, 0.04, 0.08),
                       limits = c(-0.2, 0.04),
                       labels = scales::label_number(accuracy = c(0.1, rep(0.01, 3)),
                                                     style_negative = "minus")) +
    annotate("text", x = 2, y = c(-0.14, -0.175),
             label = c("italic('μ')*' = 40'",
                       "italic('τ')*' = 0.1'"),
             family = "Futura", size = 8, size.unit = "pt",
             hjust = 0, parse = T) +
    labs(x = "t", y = "k") +
    coord_cartesian(expand = F, clip = "off") +
    mytheme +
    theme(plot.margin = margin(0, 0.5, 0, 0, unit = "cm"),
          axis.title.x = element_blank(),
          axis.text.x = element_blank(),
          axis.title.y = element_text(vjust = 0))

k1_mu <- sim %>%
  filter(variable == "k1" & parameter == "mu") %>%
  ggplot() +
    geom_hline(yintercept = 0) +
    geom_line(aes(x = t, y = response_value, 
                  colour = parameter_value, 
                  group = parameter_value)) +
    scale_colour_gradient(low = "#cae4f0", high = "#2e4a5b",
                          breaks = c(1, 60),
                          guide = guide_colourbar(label.hjust = c(0, 1),
                                                  title = expression(italic("μ")),
                                                  title.position = "top",
                                                  barheight = unit(0.1, "cm"),
                                                  barwidth = unit(5.67, "cm"))) +
    scale_x_continuous(breaks = seq(0, 60, 20),
                       limits = c(0, 60)) +
    scale_y_continuous(breaks = seq(-0.2, 0.04, 0.08),
                       limits = c(-0.2, 0.04),
                       labels = scales::label_number(accuracy = c(0.1, rep(0.01, 3)),
                                                     style_negative = "minus")) +
    annotate("text", x = 2, y = c(-0.14, -0.175), 
             label = c("italic('α')*' = 0.01'",
                       "italic('τ')*' = 0.1'"),
             family = "Futura", size = 8, size.unit = "pt",
             hjust = 0, parse = T) +
    labs(x = "t", y = "k") +
    coord_cartesian(expand = F, clip = "off") +
    mytheme +
    theme(plot.margin = margin(0, 0.5, 0, 0, unit = "cm"),
          axis.title.x = element_blank(),
          axis.title.y = element_blank(),
          axis.text = element_blank())

k1_tau <- sim %>%
  filter(variable == "k1" & parameter == "tau") %>%
  ggplot() +
    geom_hline(yintercept = 0) +
    geom_line(aes(x = t, y = response_value, 
                  colour = parameter_value, 
                  group = parameter_value)) +
    scale_colour_gradient(low = "#c7b300", high = "#5e5003",
                          breaks = c(0.02, 0.2),
                          labels = scales::label_number(accuracy = c(0.01, 0.1)),
                          guide = guide_colourbar(label.hjust = c(0, 1),
                                                  title = expression(italic("τ")),
                                                  title.position = "top",
                                                  barheight = unit(0.1, "cm"),
                                                  barwidth = unit(5.67, "cm"))) +
    scale_x_continuous(breaks = seq(0, 60, 20),
                       limits = c(0, 60)) +
    scale_y_continuous(breaks = seq(-0.2, 0.04, 0.08),
                       limits = c(-0.2, 0.04),
                       labels = scales::label_number(accuracy = c(0.1, rep(0.01, 3)),
                                                     style_negative = "minus")) +
    annotate("text", x = 2, y = c(-0.14, -0.175), 
             label = c("italic('α')*' = 0.01'",
                       "italic('μ')*' = 40'"),
             family = "Futura", size = 8, size.unit = "pt",
             hjust = 0, parse = T) +
    labs(x = "t", y = "k") +
    coord_cartesian(expand = F, clip = "off") +
    mytheme +
    theme(plot.margin = margin(0, 0.2, 0, 0, unit = "cm"),
          axis.title.x = element_blank(),
          axis.title.y = element_blank(),
          axis.text = element_blank())

p1_alpha <- sim %>%
  filter(variable == "p1" & parameter == "alpha") %>%
  ggplot() +
    geom_hline(yintercept = 1) +
    geom_line(aes(x = t, y = response_value, 
                  colour = parameter_value, 
                  group = parameter_value)) +
    scale_colour_gradient(low = "#d4e19e", high = "#4a7518",
                          breaks = c(-0.1, 0.02),
                          labels = scales::label_number(accuracy = c(0.1, 0.01),
                                                        style_negative = "minus"),
                          guide = "none") +
    scale_x_continuous(breaks = seq(0, 60, 20),
                       limits = c(0, 60)) +
    scale_y_continuous(breaks = seq(0, 1.5, 0.5),
                       limits = c(0, 1.5),
                       labels = scales::label_number(accuracy = c(1, 0.1, 1, 0.1))) +
    labs(x = "t", y = "m") +
    coord_cartesian(expand = F, clip = "off") +
    mytheme +
    theme(axis.title.x = element_blank(),
          axis.title.y = element_text(vjust = 0))

p1_mu <- sim %>%
  filter(variable == "p1" & parameter == "mu") %>%
  ggplot() +
    geom_hline(yintercept = 1) +
    geom_line(aes(x = t, y = response_value, 
                  colour = parameter_value, 
                  group = parameter_value)) +
    scale_colour_gradient(low = "#cae4f0", high = "#2e4a5b",
                          breaks = c(1, 60),
                          guide = "none") +
    scale_x_continuous(breaks = seq(0, 60, 20),
                       limits = c(0, 60)) +
    scale_y_continuous(breaks = seq(0, 1.5, 0.5),
                       limits = c(0, 1.5),
                       labels = scales::label_number(accuracy = c(1, 0.1, 1, 0.1))) +
    labs(x = "t", y = "m") +
    coord_cartesian(expand = F, clip = "off") +
    mytheme +
    theme(axis.title.y = element_blank(),
          axis.text.y = element_blank())

p1_tau <- sim %>%
  filter(variable == "p1" & parameter == "tau") %>%
  ggplot() +
    geom_hline(yintercept = 1) +
    geom_line(aes(x = t, y = response_value, 
                  colour = parameter_value, 
                  group = parameter_value)) +
    scale_colour_gradient(low = "#c7b300", high = "#5e5003",
                          breaks = c(0.02, 0.2),
                          labels = scales::label_number(accuracy = c(0.01, 0.1)),
                          guide = "none") +
    scale_x_continuous(breaks = seq(0, 60, 20),
                       limits = c(0, 60)) +
    scale_y_continuous(breaks = seq(0, 1.5, 0.5),
                       limits = c(0, 1.5),
                       labels = scales::label_number(accuracy = c(1, 0.1, 1, 0.1))) +
    labs(x = "t", y = "m") +
    coord_cartesian(expand = F, clip = "off") +
    mytheme +
    theme(plot.margin = margin(0.5, 0.2, 0, 0, unit = "cm"),
          axis.title.x = element_blank(),
          axis.title.y = element_blank(),
          axis.text.y = element_blank())

require(patchwork)
Fig_S1 <- 
  ( k1_alpha | k1_mu | k1_tau ) /
  ( p1_alpha | p1_mu | p1_tau )

Fig_S1 %>%
  ggsave(filename = "Figure_S1.pdf", path = "Figures",
         device = cairo_pdf, width = 20, height = 10, units = "cm")

# 3.2 Option 2 ####
k2_alpha <- sim %>%
  filter(variable == "k2" & parameter == "alpha") %>%
  ggplot() +
    geom_hline(yintercept = 0) +
    geom_line(aes(x = t, y = response_value, 
                  colour = parameter_value, 
                  group = parameter_value)) +
    scale_colour_gradient(low = "#d4e19e", high = "#4a7518",
                          breaks = c(-0.1, 0.02),
                          labels = scales::label_number(accuracy = c(0.1, 0.01),
                                                        style_negative = "minus"),
                          guide = guide_colourbar(label.hjust = c(0, 1),
                                                  title = expression(italic("α")),
                                                  title.position = "top",
                                                  barheight = unit(0.1, "cm"),
                                                  barwidth = unit(5.67, "cm"))) +
   scale_x_continuous(breaks = seq(0, 60, 20),
                       limits = c(0, 60)) +
    scale_y_continuous(breaks = seq(-0.2, 0.04, 0.08),
                       limits = c(-0.2, 0.04),
                       labels = scales::label_number(accuracy = c(0.1, rep(0.01, 3)),
                                                     style_negative = "minus")) +
    annotate("text", x = 2, y = c(-0.14, -0.175),
             label = c("italic('μ')*' = 40'",
                       "italic('τ')*' = 0.1'"),
             family = "Futura", size = 8, size.unit = "pt",
             hjust = 0, parse = T) +
    labs(x = "t", y = "k") +
    coord_cartesian(expand = F, clip = "off") +
    mytheme +
    theme(plot.margin = margin(0, 0.5, 0, 0, unit = "cm"),
          axis.title.x = element_blank(),
          axis.text.x = element_blank(),
          axis.title.y = element_text(vjust = 0))

k2_mu <- sim %>%
  filter(variable == "k2" & parameter == "mu") %>%
  ggplot() +
    geom_hline(yintercept = 0) +
    geom_line(aes(x = t, y = response_value, 
                  colour = parameter_value, 
                  group = parameter_value)) +
    scale_colour_gradient(low = "#cae4f0", high = "#2e4a5b",
                          breaks = c(1, 60),
                          guide = guide_colourbar(label.hjust = c(0, 1),
                                                  title = expression(italic("μ")),
                                                  title.position = "top",
                                                  barheight = unit(0.1, "cm"),
                                                  barwidth = unit(5.67, "cm"))) +
    scale_x_continuous(breaks = seq(0, 60, 20),
                       limits = c(0, 60)) +
    scale_y_continuous(breaks = seq(-0.2, 0.04, 0.08),
                       limits = c(-0.2, 0.04),
                       labels = scales::label_number(accuracy = c(0.1, rep(0.01, 3)),
                                                     style_negative = "minus")) +
    annotate("text", x = 2, y = c(-0.14, -0.175), 
             label = c("italic('α')*' = 0.01'",
                       "italic('τ')*' = 0.1'"),
             family = "Futura", size = 8, size.unit = "pt",
             hjust = 0, parse = T) +
    labs(x = "t", y = "k") +
    coord_cartesian(expand = F, clip = "off") +
    mytheme +
    theme(plot.margin = margin(0, 0.5, 0, 0, unit = "cm"),
          axis.title.x = element_blank(),
          axis.title.y = element_blank(),
          axis.text = element_blank())

k2_tau <- sim %>%
  filter(variable == "k2" & parameter == "tau") %>%
  ggplot() +
    geom_hline(yintercept = 0) +
    geom_line(aes(x = t, y = response_value, 
                  colour = parameter_value, 
                  group = parameter_value)) +
    scale_colour_gradient(low = "#c7b300", high = "#5e5003",
                          breaks = c(0.02, 0.2),
                          labels = scales::label_number(accuracy = c(0.01, 0.1)),
                          guide = guide_colourbar(label.hjust = c(0, 1),
                                                  title = expression(italic("τ")),
                                                  title.position = "top",
                                                  barheight = unit(0.1, "cm"),
                                                  barwidth = unit(5.67, "cm"))) +
    scale_x_continuous(breaks = seq(0, 60, 20),
                       limits = c(0, 60)) +
    scale_y_continuous(breaks = seq(-0.2, 0.04, 0.08),
                       limits = c(-0.2, 0.04),
                       labels = scales::label_number(accuracy = c(0.1, rep(0.01, 3)),
                                                     style_negative = "minus")) +
    annotate("text", x = 2, y = c(-0.14, -0.175), 
             label = c("italic('α')*' = 0.01'",
                       "italic('μ')*' = 40'"),
             family = "Futura", size = 8, size.unit = "pt",
             hjust = 0, parse = T) +
    labs(x = "t", y = "k") +
    coord_cartesian(expand = F, clip = "off") +
    mytheme +
    theme(plot.margin = margin(0, 0.2, 0, 0, unit = "cm"),
          axis.title.x = element_blank(),
          axis.title.y = element_blank(),
          axis.text = element_blank())

p2_alpha <- sim %>%
  filter(variable == "p2" & parameter == "alpha") %>%
  ggplot() +
    geom_hline(yintercept = 1) +
    geom_line(aes(x = t, y = response_value, 
                  colour = parameter_value, 
                  group = parameter_value)) +
    scale_colour_gradient(low = "#d4e19e", high = "#4a7518",
                          breaks = c(-0.1, 0.02),
                          labels = scales::label_number(accuracy = c(0.1, 0.01),
                                                        style_negative = "minus"),
                          guide = "none") +
    scale_x_continuous(breaks = seq(0, 60, 20),
                       limits = c(0, 60)) +
    scale_y_continuous(breaks = seq(0, 1.5, 0.5),
                       limits = c(0, 1.5),
                       labels = scales::label_number(accuracy = c(1, 0.1, 1, 0.1))) +
    labs(x = "t", y = "m") +
    coord_cartesian(expand = F, clip = "off") +
    mytheme +
    theme(axis.title.x = element_blank(),
          axis.title.y = element_text(vjust = 0))

p2_mu <- sim %>%
  filter(variable == "p2" & parameter == "mu") %>%
  ggplot() +
    geom_hline(yintercept = 1) +
    geom_line(aes(x = t, y = response_value, 
                  colour = parameter_value, 
                  group = parameter_value)) +
    scale_colour_gradient(low = "#cae4f0", high = "#2e4a5b",
                          breaks = c(1, 60),
                          guide = "none") +
    scale_x_continuous(breaks = seq(0, 60, 20),
                       limits = c(0, 60)) +
    scale_y_continuous(breaks = seq(0, 1.5, 0.5),
                       limits = c(0, 1.5),
                       labels = scales::label_number(accuracy = c(1, 0.1, 1, 0.1))) +
    labs(x = "t", y = "m") +
    coord_cartesian(expand = F, clip = "off") +
    mytheme +
    theme(axis.title.y = element_blank(),
          axis.text.y = element_blank())

p2_tau <- sim %>%
  filter(variable == "p2" & parameter == "tau") %>%
  ggplot() +
    geom_hline(yintercept = 1) +
    geom_line(aes(x = t, y = response_value, 
                  colour = parameter_value, 
                  group = parameter_value)) +
    scale_colour_gradient(low = "#c7b300", high = "#5e5003",
                          breaks = c(0.02, 0.2),
                          labels = scales::label_number(accuracy = c(0.01, 0.1)),
                          guide = "none") +
    scale_x_continuous(breaks = seq(0, 60, 20),
                       limits = c(0, 60)) +
    scale_y_continuous(breaks = seq(0, 1.5, 0.5),
                       limits = c(0, 1.5),
                       labels = scales::label_number(accuracy = c(1, 0.1, 1, 0.1))) +
    labs(x = "t", y = "m") +
    coord_cartesian(expand = F, clip = "off") +
    mytheme +
    theme(plot.margin = margin(0.5, 0.2, 0, 0, unit = "cm"),
          axis.title.x = element_blank(),
          axis.title.y = element_blank(),
          axis.text.y = element_blank())

Fig_1 <- 
  ( k2_alpha | k2_mu | k2_tau ) /
  ( p2_alpha | p2_mu | p2_tau )

Fig_1 %>%
  ggsave(filename = "Figure_1.pdf", path = "Figures",
         device = cairo_pdf, width = 20, height = 10, units = "cm")