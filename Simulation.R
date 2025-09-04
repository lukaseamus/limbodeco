
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

# ( alpha + tau ) / ( 1 + exp( k * ( t - mu ) ) ) - tau

# where alpha is the initial net photosynthesis or 
# relative growth, tau is the final net photosynthesis
# or relative decomposition, both in absolute units,
# k is the logistic decay rate and mu is the midpoint.
# Since k and mu are not independent and are therefore
# difficult to estimate, it is best to remove one of
# these parameters. There are two possible approaches:

# 1. Express k as a function of mu. This assumes
# that the transition from alive to dead commences 
# at t = 0 (excision or detachment) and proceeds 
# at a rate inversely proportional to mu. If limbo 
# is longer the curve is flatter.

# 2. Treat k as a constant. This assumes that the
# transition  always happens at the same rate and
# doesn't commence at t = 0. The pattern of transition 
# is unaffected by when it happens.

# Option 1 may seem more suitable in this context 
# because t = 0 is a natural initialising event. 
# However, it does imply that for zero growth
# the decline is immediate, albeit slow. This is 
# not what we see in nature. Seaweed can persist 
# detached without any noticeable decline and once 
# growth has ceased decomposition is usually rapid. 
# This would favour option 1.

# Option 1
# Since k * mu is the logistic intercept (log odds
# at t = 0) and I know y is reasonably close to 
# alpha at t = 0, k * mu = 5 is a good choice.
# Solving for k gives 5 / mu. Inserted into the
# above function results in this simplified version:

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

k1 <- function(t, alpha, mu, tau) {
  ( alpha + tau ) / ( 1 + exp( 5 / mu * ( t - mu ) ) ) - tau
}

p1 <- function(t, alpha, mu, tau) {
  exp(
      t * alpha - ( alpha + tau ) * mu / 5 * 
        log( 
          ( 1 + exp( 5 / mu * ( t - mu ) ) ) / 
            ( 1 + exp( -5 ) )
        )
  )
}

# Option 2
# A reasonable timescale for transition is one week. 
# Taking the logistic intercept to be 5, so k = 5 / mu, 
# this gives k = 0.71, or k = 1 for convenience.

# The integral with k is

# - tau * t + ( alpha + tau ) * (
# t - (1 / k) * 
# log( 
# ( 1 + exp( -( mu - t ) / (1 / k) ) ) / 
# ( 1 + exp( -mu / (1 / k) )  )
# )
# )

# Expanded to

# - tau * t + alpha * t + tau * t - ( alpha + tau ) / k *
# log( 
# ( 1 + exp( k * -( mu - t ) ) ) / 
# ( 1 + exp( k * -mu )  )
# )

# Simplified to

# t * alpha - ( alpha + tau ) / k *
# log( 
# ( 1 + exp( k * ( t - mu ) ) ) / 
# ( 1 + exp( -k * mu )  )
# )

# For k = 1 this simply is

# t * alpha - ( alpha + tau ) *
# log( 
# ( 1 + exp( t - mu ) ) / 
# ( 1 + exp( -mu )  )
# )


k2 <- function(t, alpha, mu, tau) {
  ( alpha + tau ) / ( 1 + exp( t - mu ) ) - tau
}

p2 <- function(t, alpha, mu, tau) {
  exp(
      t * alpha - ( alpha + tau ) * 
        log( 
          ( 1 + exp( t - mu ) ) / 
            ( 1 + exp( -mu ) )
        )
  )
}

require(tidyverse)
require(magrittr)

alpha_flex <- expand_grid(
  t = seq(0, 10, 0.01),
  mu = 5,
  alpha = seq(-0.8, 0.2, length.out = 10),
  tau = 1
)

mu_flex <- expand_grid(
  t = seq(0, 10, 0.01),
  mu = seq(0.1, 10, length.out = 10),
  alpha = 0.1,
  tau = 1
)

tau_flex <- expand_grid(
  t = seq(0, 10, 0.01),
  mu = 5,
  alpha = 0.1,
  tau = seq(0.1, 1, length.out = 10)
)

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

mytheme <- theme(
  panel.background = element_blank(),
  panel.grid = element_blank(),
  panel.border = element_rect(fill = NA),
  text = element_text(family = "Futura"),
  axis.title = element_text(face = "italic", size = 10),
  axis.text = element_text(colour = "black", size = 8),
  axis.ticks = element_line(colour = "black"),
  legend.position = "top",
  legend.title = element_text(face = "italic", size = 10),
  legend.text = element_text(size = 8),
  legend.justification = "left",
  legend.background = element_blank(),
  legend.margin = margin(0, 0, -8, 0)
)

k_alpha <- sim %>%
  filter(variable == "k2" & parameter == "alpha") %>%
  ggplot() +
    geom_hline(yintercept = 0) +
    geom_line(aes(x = t, y = response_value, 
                  colour = parameter_value, 
                  group = parameter_value)) +
    scale_colour_gradient(low = "#dbdddf", high = "black",
                          breaks = c(0, 0.2),
                          labels = scales::label_number(accuracy = c(1, 0.1)),
                          guide = guide_colourbar(ticks = F,
                                                  label.hjust = c(0, 1),
                                                  title = expression(italic("a")),
                                                  title.position = "top",
                                                  barheight = unit(0.1, "cm"),
                                                  barwidth = unit(4.15, "cm"))) +
    scale_x_continuous(breaks = seq(0, 10, 2),
                       limits = c(0, 10)) +
    scale_y_continuous(breaks = seq(-1, 0.6, 0.4),
                       limits = c(-1, 0.6),
                       labels = scales::label_number(accuracy = c(1, rep(0.1, 4)),
                                                     style_negative = "minus")) +
    annotate("text", x = 8, y = c(0.45, 0.3), 
             label = c("italic('m')*' = 5'",
                       "italic('t')*' = 1'"),
             hjust = 0, family = "Futura", size = 2.8, parse = T) +
    labs(x = expression(italic("t")),
         y = expression(italic("k"))) +
    coord_cartesian(expand = F, clip = "off") +
    mytheme +
    theme(axis.title.x = element_blank(),
          axis.text.x = element_blank())

k_mu <- sim %>%
  filter(variable == "k2" & parameter == "mu") %>%
  ggplot() +
    geom_hline(yintercept = 0) +
    geom_line(aes(x = t, y = response_value, 
                  colour = parameter_value, 
                  group = parameter_value)) +
    scale_colour_gradient(low = "#dbdddf", high = "black",
                          breaks = c(0.1, 10),
                          labels = scales::label_number(accuracy = c(0.1, 1)),
                          guide = guide_colourbar(ticks = F,
                                                  label.hjust = c(0, 1),
                                                  title = expression(italic("m")),
                                                  title.position = "top",
                                                  barheight = unit(0.1, "cm"),
                                                  barwidth = unit(4.15, "cm"))) +
    scale_x_continuous(breaks = seq(0, 10, 2),
                       limits = c(0, 10)) +
    scale_y_continuous(breaks = seq(-1, 0.6, 0.4),
                       limits = c(-1, 0.6),
                       labels = scales::label_number(accuracy = c(1, rep(0.1, 4)),
                                                     style_negative = "minus")) +
    annotate("text", x = 8, y = c(0.45, 0.3), 
             label = c("italic('a')*' = 0.1'",
                       "italic('t')*' = 1'"),
             hjust = 0, family = "Futura", size = 2.8, parse = T) +
    labs(x = expression(italic("t")),
         y = expression(italic("k"))) +
    coord_cartesian(expand = F, clip = "off") +
    mytheme +
    theme(axis.title = element_blank(),
          axis.text = element_blank())

k_tau <- sim %>%
  filter(variable == "k2" & parameter == "tau") %>%
  ggplot() +
    geom_hline(yintercept = 0) +
    geom_line(aes(x = t, y = response_value, 
                  colour = parameter_value, 
                  group = parameter_value)) +
    scale_colour_gradient(low = "#dbdddf", high = "black",
                          breaks = c(0.05, 1),
                          labels = scales::label_number(accuracy = c(0.01, 1)),
                          guide = guide_colourbar(ticks = F,
                                                  label.hjust = c(0, 1),
                                                  title = expression(italic("t")),
                                                  title.position = "top",
                                                  barheight = unit(0.1, "cm"),
                                                  barwidth = unit(4.15, "cm"))) +
    scale_x_continuous(breaks = seq(0, 10, 2),
                       limits = c(0, 10)) +
    scale_y_continuous(breaks = seq(-1, 0.6, 0.4),
                       limits = c(-1, 0.6),
                       labels = scales::label_number(accuracy = c(1, rep(0.1, 4)),
                                                     style_negative = "minus")) +
    annotate("text", x = 8, y = c(0.45, 0.3), 
             label = c("italic('a')*' = 0.1'",
                       "italic('m')*' = 5'"),
             hjust = 0, family = "Futura", size = 2.8, parse = T) +
    labs(x = expression(italic("t")),
         y = expression(italic("k"))) +
    coord_cartesian(expand = F, clip = "off") +
    mytheme +
    theme(axis.title = element_blank(),
          axis.text = element_blank())

p_alpha <- sim %>%
  filter(variable == "p2" & parameter == "alpha") %>%
  ggplot() +
    geom_hline(yintercept = 1) +
    geom_line(aes(x = t, y = response_value, 
                  colour = parameter_value, 
                  group = parameter_value)) +
    scale_colour_gradient(low = "#dbdddf", high = "black",
                          breaks = c(0, 0.2),
                          labels = scales::label_number(accuracy = c(1, 0.1)),
                          guide = "none") +
    scale_x_continuous(breaks = seq(0, 10, 2),
                       limits = c(0, 10)) +
    scale_y_continuous(breaks = seq(0, 1.6, 0.4),
                       limits = c(0, 1.6),
                       labels = scales::label_number(accuracy = c(1, rep(0.1, 4)))) +
    labs(x = expression(italic("t")),
         y = expression(italic("p"))) +
    coord_cartesian(expand = F, clip = "off") +
    mytheme +
    theme(axis.title.x = element_blank())

p_mu <- sim %>%
  filter(variable == "p2" & parameter == "mu") %>%
  ggplot() +
    geom_hline(yintercept = 1) +
    geom_line(aes(x = t, y = response_value, 
                  colour = parameter_value, 
                  group = parameter_value)) +
    scale_colour_gradient(low = "#dbdddf", high = "black",
                          breaks = c(0.1, 10),
                          labels = scales::label_number(accuracy = c(0.1, 1)),
                          guide = "none") +
    scale_x_continuous(breaks = seq(0, 10, 2),
                       limits = c(0, 10)) +
    scale_y_continuous(breaks = seq(0, 1.6, 0.4),
                       limits = c(0, 1.6),
                       labels = scales::label_number(accuracy = c(1, rep(0.1, 4)))) +
    labs(x = expression(italic("t")),
         y = expression(italic("p"))) +
    coord_cartesian(expand = F, clip = "off") +
    mytheme +
    theme(axis.title.y = element_blank(),
          axis.text.y = element_blank())

p_tau <- sim %>%
  filter(variable == "p2" & parameter == "tau") %>%
  ggplot() +
    geom_hline(yintercept = 1) +
    geom_line(aes(x = t, y = response_value, 
                  colour = parameter_value, 
                  group = parameter_value)) +
    scale_colour_gradient(low = "#dbdddf", high = "black",
                          breaks = c(0.05, 1),
                          labels = scales::label_number(accuracy = c(0.01, 1)),
                          guide = "none") +
    scale_x_continuous(breaks = seq(0, 10, 2),
                       limits = c(0, 10)) +
    scale_y_continuous(breaks = seq(0, 1.6, 0.4),
                       limits = c(0, 1.6),
                       labels = scales::label_number(accuracy = c(1, rep(0.1, 4)))) +
    labs(x = expression(italic("t")),
         y = expression(italic("p"))) +
    coord_cartesian(expand = F, clip = "off") +
    mytheme +
    theme(axis.title = element_blank(),
          axis.text.y = element_blank())

require(patchwork)
Fig_1 <- 
  ( k_alpha | k_mu | k_tau ) /
  ( p_alpha | p_mu | p_tau )

Fig_1 %>%
  ggsave(filename = "Fig_1.pdf", path = "Figures",
         device = cairo_pdf, width = 20, height = 12, units = "cm")
