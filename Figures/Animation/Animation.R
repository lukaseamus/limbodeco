# 1. Function ####
# 1.1 Simulate data ####
# R doesn't have a built-in log1p_exp function
log1p_exp <- function(x) {
  ifelse(
    x > 0, 
    x + log1p(exp(-x)),
    log1p(exp(x))
  )
}

k <- function(t, alpha, mu, tau) {
  ( alpha + tau ) / ( 1 + exp( 5 / mu * ( t - mu ) ) ) - tau
}

m <- function(t, alpha, mu, tau) {
  exp(
      t * alpha - ( alpha + tau ) * mu / 5 * (
        log1p_exp( 5 / mu * ( t - mu ) ) - log1p_exp( -5 )
      )
  )
}

require(tidyverse)
require(magrittr)

k_seq <- expand_grid(
  t = seq(0, 60, length.out = 200),
  k = -c(
    1, 0.5, 0.3, 0.2, 0.14, 0.1, 0.07, 0.05, 
    0.038, 0.028, 0.02, 0.014, 0.01, 0.007, 
    0.0042, 0.002, 0.0001
  )
) %T>%
  print()

old_sim <- k_seq %>%
  mutate(
    m = exp(k * t),
    t0.5 = log(2)/-k # calculate half-life
  ) %T>%
  print()

alpha_seq <- expand_grid(
  t = seq(0, 60, length.out = 200),
  alpha = c( # avoid seq() because numbers need to be exact
    -0.1, -0.09, -0.08, -0.07, -0.06, -0.05, -0.04,
    -0.03, -0.02, -0.01, 0, 0.01, 0.02
  ),
  mu = 35,
  tau = 0.1
) %T>%
  print()

mu_seq <- expand_grid(
  t = seq(0, 60, length.out = 200),
  alpha = 0.01,
  mu = c(
    1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 
    12, 14, 16, 18, 20, 
    25, 30, 35, 40, 45, 50, 55, 60
  ),
  tau = 0.1
) %T>%
  print()

tau_seq <- expand_grid(
  t = seq(0, 60, length.out = 200),
  alpha = 0.01,
  mu = 35,
  tau = c(
    0.02, 0.04, 0.06, 0.08, 0.10, 0.12, 0.14, 0.16, 0.18, 0.2
  )
) %T>%
  print()

new_sim <- bind_rows(
  alpha = alpha_seq,
  mu = mu_seq,
  tau = tau_seq,
  .id = "parameter"
) %>%
  mutate(k = k(t, alpha, mu, tau),
         m = m(t, alpha, mu, tau)) %T>%
  print()

# 1.2 Static figures ####
# Define custom theme
mytheme <- theme(
  panel.background = element_blank(),
  panel.grid.major = element_blank(),
  panel.grid.minor = element_blank(),
  panel.border = element_blank(),
  plot.margin = margin(0.2, 0.5, 0.2, 0.2, unit = "cm"),
  plot.title = element_text(size = 12, hjust = 0),
  axis.line = element_line(),
  axis.title = element_text(size = 12),
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
  panel.spacing = unit(1, "cm"),
  text = element_text(family = "Futura")
)

# Old model
ggplot() +
  geom_line(
    data = old_sim,
    aes(t, m, group = k)
  ) +
  geom_rug(
    data = old_sim,
    aes(t0.5, group = t0.5), outside = TRUE,
    length = unit(.25, "cm"), lineend = "square"
  ) +
  scale_x_continuous(
    breaks = seq(0, 60, 10),
    limits = c(0, 60)
  ) +
  scale_y_continuous(
    breaks = seq(0, 1, 0.2),
    limits = c(0, 1),
    labels = scales::label_number(accuracy = c(1, rep(0.1, 4), 1))
  ) +
  labs(
    x = expression(italic("t")), 
    y = expression(italic("m")*"/"*italic("m")[0]),
    title = expression(italic("k")*" =   "*italic("t")["½"]*" = ")
  ) +
  coord_cartesian(expand = F, clip = "off") +
  mytheme
# ignore warning: t0.5 is bound to be outside the plotting area for slow decay

# New model, top panel
ggplot() +
  geom_hline(yintercept = 0) +
  geom_line(
    data = new_sim,
    aes(t, k, group = interaction(alpha, mu, tau))
  ) +
  geom_rug(
    data = new_sim,
    aes(mu, group = interaction(alpha, mu, tau)), outside = TRUE,
    length = unit(.25, "cm"), lineend = "square"
  ) +
  geom_text(
    data = new_sim,
    aes(2, -0.175, label = "alpha = "),
    hjust = 0, vjust = 0, size.unit = "pt",
    size = 12, family = "Futura", check_overlap = T
  ) +
  geom_text(
    data = new_sim,
    aes(22, -0.175, label = "mu = "),
    hjust = 0, vjust = 0, size.unit = "pt",
    size = 12, family = "Futura", check_overlap = T
  ) +
  geom_text(
    data = new_sim,
    aes(42, -0.175, label = "tau = "),
    hjust = 0, vjust = 0, size.unit = "pt",
    size = 12, family = "Futura", check_overlap = T
  ) +
  scale_x_continuous(
    breaks = seq(0, 60, 10),
    limits = c(0, 60)
  ) +
  scale_y_continuous(
    breaks = seq(-0.2, 0.04, 0.08),
    limits = c(-0.2, 0.04),
    labels = scales::label_number(accuracy = c(0.1, rep(0.01, 3)),
                                  style_negative = "minus")
  ) +
  facet_grid(~ parameter, space = "free", scales = "free") +
  labs(x = expression(italic("t")), 
       y = expression(italic("k"))) +
  coord_cartesian(expand = F, clip = "off") +
  mytheme
# ignore warning: in the animated version, geom_text will be essential

# New model, bottom panel
ggplot() +
  geom_hline(yintercept = 1) +
  geom_line(
    data = new_sim,
    aes(t, m, group = interaction(alpha, mu, tau))
  ) +
  geom_rug(
    data = new_sim,
    aes(x = mu, group = interaction(alpha, mu, tau)), outside = TRUE,
    length = unit(.25, "cm"), lineend = "square"
  ) +
  scale_x_continuous(
    breaks = seq(0, 60, 10),
    limits = c(0, 60)
  ) +
  scale_y_continuous(
    breaks = seq(0, 1.5, 0.5),
    limits = c(0, 1.5),
    labels = scales::label_number(accuracy = c(1, 0.1, 1, 0.1))
  ) +
  facet_grid(~ parameter, space = "free", scales = "free") +
  labs(x = expression(italic("t")), 
       y = expression(italic("m")*"/"*italic("m")[0])) +
  coord_cartesian(expand = F, clip = "off") +
  mytheme
# ignore warning: in the animated version, geom_text will be essential

# 1.3 Animation ####
# Function to calculate frames based on area under the curve (integral) difference
k2frames <- function(k1, k2, tmax = 60, scale = 10) {
  auc1 <- ( exp(k1 * tmax) - 1 ) / k1
  auc2 <- ( exp(k2 * tmax) - 1 ) / k2
  frames <- floor( abs(auc1 - auc2) * scale + .5 ) # always round halves up
  return(frames)
}

# Tween old model
require(transformr)
old_sim_ani <- bind_rows(
  tween_path(
    old_sim %>% filter(k == -0.0001), 
    old_sim %>% filter(k == -0.002),
    ease = "linear", nframes = k2frames(-0.0001, -0.002)
  ),
  tween_path(
    old_sim %>% filter(k == -0.002), 
    old_sim %>% filter(k == -0.0042),
    ease = "linear", nframes = k2frames(-0.002, -0.0042)
  ) %>%
    mutate(
      .frame = .frame + k2frames(-0.0001, -0.002)
    ),
  tween_path(
    old_sim %>% filter(k == -0.0042), 
    old_sim %>% filter(k == -0.007),
    ease = "linear", nframes = k2frames(-0.0042, -0.007)
  ) %>%
    mutate(
      .frame = .frame + k2frames(-0.0001, -0.002) +
        k2frames(-0.002, -0.0042)
    ),
  tween_path(
    old_sim %>% filter(k == -0.007), 
    old_sim %>% filter(k == -0.01),
    ease = "linear", nframes = k2frames(-0.007, -0.01)
  ) %>%
    mutate(
      .frame = .frame + k2frames(-0.0001, -0.002) +
        k2frames(-0.002, -0.0042) + k2frames(-0.0042, -0.007)
    ),
  tween_path(
    old_sim %>% filter(k == -0.01), 
    old_sim %>% filter(k == -0.014),
    ease = "linear", nframes = k2frames(-0.01, -0.014)
  ) %>%
    mutate(
      .frame = .frame + k2frames(-0.0001, -0.002) +
        k2frames(-0.002, -0.0042) + k2frames(-0.0042, -0.007) +
        k2frames(-0.007, -0.01)
    ),
  tween_path(
    old_sim %>% filter(k == -0.014), 
    old_sim %>% filter(k == -0.02),
    ease = "linear", nframes = k2frames(-0.014, -0.02)
  ) %>%
    mutate(
      .frame = .frame + k2frames(-0.0001, -0.002) +
        k2frames(-0.002, -0.0042) + k2frames(-0.0042, -0.007) +
        k2frames(-0.007, -0.01) + k2frames(-0.01, -0.014)
    ),
  tween_path(
    old_sim %>% filter(k == -0.02), 
    old_sim %>% filter(k == -0.028),
    ease = "linear", nframes = k2frames(-0.02, -0.028)
  ) %>%
    mutate(
      .frame = .frame + k2frames(-0.0001, -0.002) +
        k2frames(-0.002, -0.0042) + k2frames(-0.0042, -0.007) +
        k2frames(-0.007, -0.01) + k2frames(-0.01, -0.014) +
        k2frames(-0.014, -0.02)
    ),
  tween_path(
    old_sim %>% filter(k == -0.028), 
    old_sim %>% filter(k == -0.038),
    ease = "linear", nframes = k2frames(-0.028, -0.038)
  ) %>%
    mutate(
      .frame = .frame + k2frames(-0.0001, -0.002) +
        k2frames(-0.002, -0.0042) + k2frames(-0.0042, -0.007) +
        k2frames(-0.007, -0.01) + k2frames(-0.01, -0.014) +
        k2frames(-0.014, -0.02) + k2frames(-0.02, -0.028)
    ),
  tween_path(
    old_sim %>% filter(k == -0.038), 
    old_sim %>% filter(k == -0.05),
    ease = "linear", nframes = k2frames(-0.038, -0.05)
  ) %>%
    mutate(
      .frame = .frame + k2frames(-0.0001, -0.002) +
        k2frames(-0.002, -0.0042) + k2frames(-0.0042, -0.007) +
        k2frames(-0.007, -0.01) + k2frames(-0.01, -0.014) +
        k2frames(-0.014, -0.02) + k2frames(-0.02, -0.028) +
        k2frames(-0.028, -0.038)
    ),
  tween_path(
    old_sim %>% filter(k == -0.05), 
    old_sim %>% filter(k == -0.07),
    ease = "linear", nframes = k2frames(-0.05, -0.07)
  ) %>%
    mutate(
      .frame = .frame + k2frames(-0.0001, -0.002) +
        k2frames(-0.002, -0.0042) + k2frames(-0.0042, -0.007) +
        k2frames(-0.007, -0.01) + k2frames(-0.01, -0.014) +
        k2frames(-0.014, -0.02) + k2frames(-0.02, -0.028) +
        k2frames(-0.028, -0.038) + k2frames(-0.038, -0.05)
    ),
  tween_path(
    old_sim %>% filter(k == -0.07), 
    old_sim %>% filter(k == -0.1),
    ease = "linear", nframes = k2frames(-0.07, -0.1)
  ) %>%
    mutate(
      .frame = .frame + k2frames(-0.0001, -0.002) +
        k2frames(-0.002, -0.0042) + k2frames(-0.0042, -0.007) +
        k2frames(-0.007, -0.01) + k2frames(-0.01, -0.014) +
        k2frames(-0.014, -0.02) + k2frames(-0.02, -0.028) +
        k2frames(-0.028, -0.038) + k2frames(-0.038, -0.05) +
        k2frames(-0.05, -0.07)
    ),
  tween_path(
    old_sim %>% filter(k == -0.1), 
    old_sim %>% filter(k == -0.14),
    ease = "linear", nframes = k2frames(-0.1, -0.14)
  ) %>%
    mutate(
      .frame = .frame + k2frames(-0.0001, -0.002) +
        k2frames(-0.002, -0.0042) + k2frames(-0.0042, -0.007) +
        k2frames(-0.007, -0.01) + k2frames(-0.01, -0.014) +
        k2frames(-0.014, -0.02) + k2frames(-0.02, -0.028) +
        k2frames(-0.028, -0.038) + k2frames(-0.038, -0.05) +
        k2frames(-0.05, -0.07) + k2frames(-0.07, -0.1)
    ),
  tween_path(
    old_sim %>% filter(k == -0.14), 
    old_sim %>% filter(k == -0.2),
    ease = "linear", nframes = k2frames(-0.14, -0.2)
  ) %>%
    mutate(
      .frame = .frame + k2frames(-0.0001, -0.002) +
        k2frames(-0.002, -0.0042) + k2frames(-0.0042, -0.007) +
        k2frames(-0.007, -0.01) + k2frames(-0.01, -0.014) +
        k2frames(-0.014, -0.02) + k2frames(-0.02, -0.028) +
        k2frames(-0.028, -0.038) + k2frames(-0.038, -0.05) +
        k2frames(-0.05, -0.07) + k2frames(-0.07, -0.1) +
        k2frames(-0.1, -0.14)
    ),
  tween_path(
    old_sim %>% filter(k == -0.2), 
    old_sim %>% filter(k == -0.3),
    ease = "linear", nframes = k2frames(-0.2, -0.3)
  ) %>%
    mutate(
      .frame = .frame + k2frames(-0.0001, -0.002) +
        k2frames(-0.002, -0.0042) + k2frames(-0.0042, -0.007) +
        k2frames(-0.007, -0.01) + k2frames(-0.01, -0.014) +
        k2frames(-0.014, -0.02) + k2frames(-0.02, -0.028) +
        k2frames(-0.028, -0.038) + k2frames(-0.038, -0.05) +
        k2frames(-0.05, -0.07) + k2frames(-0.07, -0.1) +
        k2frames(-0.1, -0.14) + k2frames(-0.14, -0.2)
    ),
  tween_path(
    old_sim %>% filter(k == -0.3), 
    old_sim %>% filter(k == -0.5),
    ease = "linear", nframes = k2frames(-0.3, -0.5)
  ) %>%
    mutate(
      .frame = .frame + k2frames(-0.0001, -0.002) +
        k2frames(-0.002, -0.0042) + k2frames(-0.0042, -0.007) +
        k2frames(-0.007, -0.01) + k2frames(-0.01, -0.014) +
        k2frames(-0.014, -0.02) + k2frames(-0.02, -0.028) +
        k2frames(-0.028, -0.038) + k2frames(-0.038, -0.05) +
        k2frames(-0.05, -0.07) + k2frames(-0.07, -0.1) +
        k2frames(-0.1, -0.14) + k2frames(-0.14, -0.2) +
        k2frames(-0.2, -0.3)
    ),
  tween_path(
    old_sim %>% filter(k == -0.5), 
    old_sim %>% filter(k == -1),
    ease = "linear", nframes = k2frames(-0.5, -1)
  ) %>%
    mutate(
      .frame = .frame + k2frames(-0.0001, -0.002) +
        k2frames(-0.002, -0.0042) + k2frames(-0.0042, -0.007) +
        k2frames(-0.007, -0.01) + k2frames(-0.01, -0.014) +
        k2frames(-0.014, -0.02) + k2frames(-0.02, -0.028) +
        k2frames(-0.028, -0.038) + k2frames(-0.038, -0.05) +
        k2frames(-0.05, -0.07) + k2frames(-0.07, -0.1) +
        k2frames(-0.1, -0.14) + k2frames(-0.14, -0.2) +
        k2frames(-0.2, -0.3) + k2frames(-0.3, -0.5)
    ),
  tween_path(
    old_sim %>% filter(k == -1), 
    old_sim %>% filter(k == -0.5),
    ease = "linear", nframes = k2frames(-0.5, -1)
  ) %>%
    mutate(
      .frame = .frame + k2frames(-0.0001, -0.002) +
        k2frames(-0.002, -0.0042) + k2frames(-0.0042, -0.007) +
        k2frames(-0.007, -0.01) + k2frames(-0.01, -0.014) +
        k2frames(-0.014, -0.02) + k2frames(-0.02, -0.028) +
        k2frames(-0.028, -0.038) + k2frames(-0.038, -0.05) +
        k2frames(-0.05, -0.07) + k2frames(-0.07, -0.1) +
        k2frames(-0.1, -0.14) + k2frames(-0.14, -0.2) +
        k2frames(-0.2, -0.3) + k2frames(-0.3, -0.5) +
        k2frames(-0.5, -1)
    ),
  tween_path(
    old_sim %>% filter(k == -0.5), 
    old_sim %>% filter(k == -0.3),
    ease = "linear", nframes = k2frames(-0.3, -0.5)
  ) %>%
    mutate(
      .frame = .frame + k2frames(-0.0001, -0.002) +
        k2frames(-0.002, -0.0042) + k2frames(-0.0042, -0.007) +
        k2frames(-0.007, -0.01) + k2frames(-0.01, -0.014) +
        k2frames(-0.014, -0.02) + k2frames(-0.02, -0.028) +
        k2frames(-0.028, -0.038) + k2frames(-0.038, -0.05) +
        k2frames(-0.05, -0.07) + k2frames(-0.07, -0.1) +
        k2frames(-0.1, -0.14) + k2frames(-0.14, -0.2) +
        k2frames(-0.2, -0.3) + k2frames(-0.3, -0.5) +
        2*k2frames(-0.5, -1)
    ),
  tween_path(
    old_sim %>% filter(k == -0.3), 
    old_sim %>% filter(k == -0.2),
    ease = "linear", nframes = k2frames(-0.2, -0.3)
  ) %>%
    mutate(
      .frame = .frame + k2frames(-0.0001, -0.002) +
        k2frames(-0.002, -0.0042) + k2frames(-0.0042, -0.007) +
        k2frames(-0.007, -0.01) + k2frames(-0.01, -0.014) +
        k2frames(-0.014, -0.02) + k2frames(-0.02, -0.028) +
        k2frames(-0.028, -0.038) + k2frames(-0.038, -0.05) +
        k2frames(-0.05, -0.07) + k2frames(-0.07, -0.1) +
        k2frames(-0.1, -0.14) + k2frames(-0.14, -0.2) +
        k2frames(-0.2, -0.3) + 2*k2frames(-0.3, -0.5) +
        2*k2frames(-0.5, -1)
    ),
  tween_path(
    old_sim %>% filter(k == -0.2), 
    old_sim %>% filter(k == -0.14),
    ease = "linear", nframes = k2frames(-0.14, -0.2)
  ) %>%
    mutate(
      .frame = .frame + k2frames(-0.0001, -0.002) +
        k2frames(-0.002, -0.0042) + k2frames(-0.0042, -0.007) +
        k2frames(-0.007, -0.01) + k2frames(-0.01, -0.014) +
        k2frames(-0.014, -0.02) + k2frames(-0.02, -0.028) +
        k2frames(-0.028, -0.038) + k2frames(-0.038, -0.05) +
        k2frames(-0.05, -0.07) + k2frames(-0.07, -0.1) +
        k2frames(-0.1, -0.14) + k2frames(-0.14, -0.2) +
        2*k2frames(-0.2, -0.3) + 2*k2frames(-0.3, -0.5) +
        2*k2frames(-0.5, -1)
    ),
  tween_path(
    old_sim %>% filter(k == -0.14), 
    old_sim %>% filter(k == -0.1),
    ease = "linear", nframes = k2frames(-0.1, -0.14)
  ) %>%
    mutate(
      .frame = .frame + k2frames(-0.0001, -0.002) +
        k2frames(-0.002, -0.0042) + k2frames(-0.0042, -0.007) +
        k2frames(-0.007, -0.01) + k2frames(-0.01, -0.014) +
        k2frames(-0.014, -0.02) + k2frames(-0.02, -0.028) +
        k2frames(-0.028, -0.038) + k2frames(-0.038, -0.05) +
        k2frames(-0.05, -0.07) + k2frames(-0.07, -0.1) +
        k2frames(-0.1, -0.14) + 2*k2frames(-0.14, -0.2) +
        2*k2frames(-0.2, -0.3) + 2*k2frames(-0.3, -0.5) +
        2*k2frames(-0.5, -1)
    ),
  tween_path(
    old_sim %>% filter(k == -0.1), 
    old_sim %>% filter(k == -0.07),
    ease = "linear", nframes = k2frames(-0.07, -0.1)
  ) %>%
    mutate(
      .frame = .frame + k2frames(-0.0001, -0.002) +
        k2frames(-0.002, -0.0042) + k2frames(-0.0042, -0.007) +
        k2frames(-0.007, -0.01) + k2frames(-0.01, -0.014) +
        k2frames(-0.014, -0.02) + k2frames(-0.02, -0.028) +
        k2frames(-0.028, -0.038) + k2frames(-0.038, -0.05) +
        k2frames(-0.05, -0.07) + k2frames(-0.07, -0.1) +
        2*k2frames(-0.1, -0.14) + 2*k2frames(-0.14, -0.2) +
        2*k2frames(-0.2, -0.3) + 2*k2frames(-0.3, -0.5) +
        2*k2frames(-0.5, -1)
    ),
  tween_path(
    old_sim %>% filter(k == -0.07), 
    old_sim %>% filter(k == -0.05),
    ease = "linear", nframes = k2frames(-0.05, -0.07)
  ) %>%
    mutate(
      .frame = .frame + k2frames(-0.0001, -0.002) +
        k2frames(-0.002, -0.0042) + k2frames(-0.0042, -0.007) +
        k2frames(-0.007, -0.01) + k2frames(-0.01, -0.014) +
        k2frames(-0.014, -0.02) + k2frames(-0.02, -0.028) +
        k2frames(-0.028, -0.038) + k2frames(-0.038, -0.05) +
        k2frames(-0.05, -0.07) + 2*k2frames(-0.07, -0.1) +
        2*k2frames(-0.1, -0.14) + 2*k2frames(-0.14, -0.2) +
        2*k2frames(-0.2, -0.3) + 2*k2frames(-0.3, -0.5) +
        2*k2frames(-0.5, -1)
    ),
  tween_path(
    old_sim %>% filter(k == -0.05), 
    old_sim %>% filter(k == -0.038),
    ease = "linear", nframes = k2frames(-0.038, -0.05)
  ) %>%
    mutate(
      .frame = .frame + k2frames(-0.0001, -0.002) +
        k2frames(-0.002, -0.0042) + k2frames(-0.0042, -0.007) +
        k2frames(-0.007, -0.01) + k2frames(-0.01, -0.014) +
        k2frames(-0.014, -0.02) + k2frames(-0.02, -0.028) +
        k2frames(-0.028, -0.038) + k2frames(-0.038, -0.05) +
        2*k2frames(-0.05, -0.07) + 2*k2frames(-0.07, -0.1) +
        2*k2frames(-0.1, -0.14) + 2*k2frames(-0.14, -0.2) +
        2*k2frames(-0.2, -0.3) + 2*k2frames(-0.3, -0.5) +
        2*k2frames(-0.5, -1)
    ),
  tween_path(
    old_sim %>% filter(k == -0.038), 
    old_sim %>% filter(k == -0.028),
    ease = "linear", nframes = k2frames(-0.028, -0.038)
  ) %>%
    mutate(
      .frame = .frame + k2frames(-0.0001, -0.002) +
        k2frames(-0.002, -0.0042) + k2frames(-0.0042, -0.007) +
        k2frames(-0.007, -0.01) + k2frames(-0.01, -0.014) +
        k2frames(-0.014, -0.02) + k2frames(-0.02, -0.028) +
        k2frames(-0.028, -0.038) + 2*k2frames(-0.038, -0.05) +
        2*k2frames(-0.05, -0.07) + 2*k2frames(-0.07, -0.1) +
        2*k2frames(-0.1, -0.14) + 2*k2frames(-0.14, -0.2) +
        2*k2frames(-0.2, -0.3) + 2*k2frames(-0.3, -0.5) +
        2*k2frames(-0.5, -1)
    ),
  tween_path(
    old_sim %>% filter(k == -0.028), 
    old_sim %>% filter(k == -0.02),
    ease = "linear", nframes = k2frames(-0.02, -0.028)
  ) %>%
    mutate(
      .frame = .frame + k2frames(-0.0001, -0.002) +
        k2frames(-0.002, -0.0042) + k2frames(-0.0042, -0.007) +
        k2frames(-0.007, -0.01) + k2frames(-0.01, -0.014) +
        k2frames(-0.014, -0.02) + k2frames(-0.02, -0.028) +
        2*k2frames(-0.028, -0.038) + 2*k2frames(-0.038, -0.05) +
        2*k2frames(-0.05, -0.07) + 2*k2frames(-0.07, -0.1) +
        2*k2frames(-0.1, -0.14) + 2*k2frames(-0.14, -0.2) +
        2*k2frames(-0.2, -0.3) + 2*k2frames(-0.3, -0.5) +
        2*k2frames(-0.5, -1)
    ),
  tween_path(
    old_sim %>% filter(k == -0.02), 
    old_sim %>% filter(k == -0.014),
    ease = "linear", nframes = k2frames(-0.014, -0.02)
  ) %>%
    mutate(
      .frame = .frame + k2frames(-0.0001, -0.002) +
        k2frames(-0.002, -0.0042) + k2frames(-0.0042, -0.007) +
        k2frames(-0.007, -0.01) + k2frames(-0.01, -0.014) +
        k2frames(-0.014, -0.02) + 2*k2frames(-0.02, -0.028) +
        2*k2frames(-0.028, -0.038) + 2*k2frames(-0.038, -0.05) +
        2*k2frames(-0.05, -0.07) + 2*k2frames(-0.07, -0.1) +
        2*k2frames(-0.1, -0.14) + 2*k2frames(-0.14, -0.2) +
        2*k2frames(-0.2, -0.3) + 2*k2frames(-0.3, -0.5) +
        2*k2frames(-0.5, -1)
    ),
  tween_path(
    old_sim %>% filter(k == -0.014), 
    old_sim %>% filter(k == -0.01),
    ease = "linear", nframes = k2frames(-0.01, -0.014)
  ) %>%
    mutate(
      .frame = .frame + k2frames(-0.0001, -0.002) +
        k2frames(-0.002, -0.0042) + k2frames(-0.0042, -0.007) +
        k2frames(-0.007, -0.01) + k2frames(-0.01, -0.014) +
        2*k2frames(-0.014, -0.02) + 2*k2frames(-0.02, -0.028) +
        2*k2frames(-0.028, -0.038) + 2*k2frames(-0.038, -0.05) +
        2*k2frames(-0.05, -0.07) + 2*k2frames(-0.07, -0.1) +
        2*k2frames(-0.1, -0.14) + 2*k2frames(-0.14, -0.2) +
        2*k2frames(-0.2, -0.3) + 2*k2frames(-0.3, -0.5) +
        2*k2frames(-0.5, -1)
    ),
  tween_path(
    old_sim %>% filter(k == -0.01), 
    old_sim %>% filter(k == -0.007),
    ease = "linear", nframes = k2frames(-0.007, -0.01)
  ) %>%
    mutate(
      .frame = .frame + k2frames(-0.0001, -0.002) +
        k2frames(-0.002, -0.0042) + k2frames(-0.0042, -0.007) +
        k2frames(-0.007, -0.01) + 2*k2frames(-0.01, -0.014) +
        2*k2frames(-0.014, -0.02) + 2*k2frames(-0.02, -0.028) +
        2*k2frames(-0.028, -0.038) + 2*k2frames(-0.038, -0.05) +
        2*k2frames(-0.05, -0.07) + 2*k2frames(-0.07, -0.1) +
        2*k2frames(-0.1, -0.14) + 2*k2frames(-0.14, -0.2) +
        2*k2frames(-0.2, -0.3) + 2*k2frames(-0.3, -0.5) +
        2*k2frames(-0.5, -1)
    ),
  tween_path(
    old_sim %>% filter(k == -0.007), 
    old_sim %>% filter(k == -0.0042),
    ease = "linear", nframes = k2frames(-0.0042, -0.007)
  ) %>%
    mutate(
      .frame = .frame + k2frames(-0.0001, -0.002) +
        k2frames(-0.002, -0.0042) + k2frames(-0.0042, -0.007) +
        2*k2frames(-0.007, -0.01) + 2*k2frames(-0.01, -0.014) +
        2*k2frames(-0.014, -0.02) + 2*k2frames(-0.02, -0.028) +
        2*k2frames(-0.028, -0.038) + 2*k2frames(-0.038, -0.05) +
        2*k2frames(-0.05, -0.07) + 2*k2frames(-0.07, -0.1) +
        2*k2frames(-0.1, -0.14) + 2*k2frames(-0.14, -0.2) +
        2*k2frames(-0.2, -0.3) + 2*k2frames(-0.3, -0.5) +
        2*k2frames(-0.5, -1)
    ),
  tween_path(
    old_sim %>% filter(k == -0.0042), 
    old_sim %>% filter(k == -0.002),
    ease = "linear", nframes = k2frames(-0.002, -0.0042)
  ) %>%
    mutate(
      .frame = .frame + k2frames(-0.0001, -0.002) +
        k2frames(-0.002, -0.0042) + 2*k2frames(-0.0042, -0.007) +
        2*k2frames(-0.007, -0.01) + 2*k2frames(-0.01, -0.014) +
        2*k2frames(-0.014, -0.02) + 2*k2frames(-0.02, -0.028) +
        2*k2frames(-0.028, -0.038) + 2*k2frames(-0.038, -0.05) +
        2*k2frames(-0.05, -0.07) + 2*k2frames(-0.07, -0.1) +
        2*k2frames(-0.1, -0.14) + 2*k2frames(-0.14, -0.2) +
        2*k2frames(-0.2, -0.3) + 2*k2frames(-0.3, -0.5) +
        2*k2frames(-0.5, -1)
    ),
  tween_path(
    old_sim %>% filter(k == -0.002), 
    old_sim %>% filter(k == -0.0001),
    ease = "linear", nframes = k2frames(-0.0001, -0.002)
  ) %>%
    mutate(
      .frame = .frame + k2frames(-0.0001, -0.002) +
        2*k2frames(-0.002, -0.0042) + 2*k2frames(-0.0042, -0.007) +
        2*k2frames(-0.007, -0.01) + 2*k2frames(-0.01, -0.014) +
        2*k2frames(-0.014, -0.02) + 2*k2frames(-0.02, -0.028) +
        2*k2frames(-0.028, -0.038) + 2*k2frames(-0.038, -0.05) +
        2*k2frames(-0.05, -0.07) + 2*k2frames(-0.07, -0.1) +
        2*k2frames(-0.1, -0.14) + 2*k2frames(-0.14, -0.2) +
        2*k2frames(-0.2, -0.3) + 2*k2frames(-0.3, -0.5) +
        2*k2frames(-0.5, -1)
    )
) %T>%
  print()

old_sim_ani %$% n_distinct(.frame) # 1174 frames in total

# Tween new model
require(tweenr)
new_sim_ani <- bind_rows(
  tween_path(
    new_sim %>% filter(parameter == "alpha", alpha == -0.1), 
    new_sim %>% filter(parameter == "alpha", alpha == -0.09),
    ease = "linear", nframes = 10
  ),
  tween_path(
    new_sim %>% filter(parameter == "alpha", alpha == -0.09), 
    new_sim %>% filter(parameter == "alpha", alpha == -0.08),
    ease = "linear", nframes = 10
  ) %>%
    mutate(
      .frame = .frame + 1 * 10
    ),
  tween_path(
    new_sim %>% filter(parameter == "alpha", alpha == -0.08), 
    new_sim %>% filter(parameter == "alpha", alpha == -0.07),
    ease = "linear", nframes = 10
  ) %>%
    mutate(
      .frame = .frame + 2 * 10
    ),
  tween_path(
    new_sim %>% filter(parameter == "alpha", alpha == -0.07), 
    new_sim %>% filter(parameter == "alpha", alpha == -0.06),
    ease = "linear", nframes = 10
  ) %>%
    mutate(
      .frame = .frame + 3 * 10
    ),
  tween_path(
    new_sim %>% filter(parameter == "alpha", alpha == -0.06), 
    new_sim %>% filter(parameter == "alpha", alpha == -0.05),
    ease = "linear", nframes = 10
  ) %>%
    mutate(
      .frame = .frame + 4 * 10
    ),
  tween_path(
    new_sim %>% filter(parameter == "alpha", alpha == -0.05), 
    new_sim %>% filter(parameter == "alpha", alpha == -0.04),
    ease = "linear", nframes = 10
  ) %>%
    mutate(
      .frame = .frame + 5 * 10
    ),
  tween_path(
    new_sim %>% filter(parameter == "alpha", alpha == -0.04), 
    new_sim %>% filter(parameter == "alpha", alpha == -0.03),
    ease = "linear", nframes = 10
  ) %>%
    mutate(
      .frame = .frame + 6 * 10
    ),
  tween_path(
    new_sim %>% filter(parameter == "alpha", alpha == -0.03), 
    new_sim %>% filter(parameter == "alpha", alpha == -0.02),
    ease = "linear", nframes = 10
  ) %>%
    mutate(
      .frame = .frame + 7 * 10
    ),
  tween_path(
    new_sim %>% filter(parameter == "alpha", alpha == -0.02), 
    new_sim %>% filter(parameter == "alpha", alpha == -0.01),
    ease = "linear", nframes = 10
  ) %>%
    mutate(
      .frame = .frame + 8 * 10
    ),
  tween_path(
    new_sim %>% filter(parameter == "alpha", alpha == -0.01), 
    new_sim %>% filter(parameter == "alpha", alpha == 0),
    ease = "linear", nframes = 10
  ) %>%
    mutate(
      .frame = .frame + 9 * 10
    ),
  tween_path(
    new_sim %>% filter(parameter == "alpha", alpha == 0), 
    new_sim %>% filter(parameter == "alpha", alpha == 0.01),
    ease = "linear", nframes = 10
  ) %>%
    mutate(
      .frame = .frame + 10 * 10
    ),
  tween_path(
    new_sim %>% filter(parameter == "alpha", alpha == 0.01), 
    new_sim %>% filter(parameter == "alpha", alpha == 0.02),
    ease = "linear", nframes = 10
  ) %>%
    mutate(
      .frame = .frame + 11 * 10
    ),
  tween_path(
    new_sim %>% filter(parameter == "alpha", alpha == 0.02), 
    new_sim %>% filter(parameter == "alpha", alpha == 0.01),
    ease = "linear", nframes = 10
  ) %>%
    keep_state(nframes = 20) %>%
    mutate(
      .frame = .frame + 12 * 10
    ),
  tween_path(
    new_sim %>% filter(parameter == "mu", mu == 35), 
    new_sim %>% filter(parameter == "mu", mu == 30),
    ease = "linear", nframes = 20
  ) %>%
    mutate(
      .frame = .frame + 13 * 10 + 1 * 20
    ),
  tween_path(
    new_sim %>% filter(parameter == "mu", mu == 30), 
    new_sim %>% filter(parameter == "mu", mu == 25),
    ease = "linear", nframes = 20
  ) %>%
    mutate(
      .frame = .frame + 13 * 10 + 2 * 20
    ),
  tween_path(
    new_sim %>% filter(parameter == "mu", mu == 25), 
    new_sim %>% filter(parameter == "mu", mu == 20),
    ease = "linear", nframes = 20
  ) %>%
    mutate(
      .frame = .frame + 13 * 10 + 3 * 20
    ),
  tween_path(
    new_sim %>% filter(parameter == "mu", mu == 20), 
    new_sim %>% filter(parameter == "mu", mu == 18),
    ease = "linear", nframes = 8
  ) %>%
    mutate(
      .frame = .frame + 13 * 10 + 4 * 20
    ),
  tween_path(
    new_sim %>% filter(parameter == "mu", mu == 18), 
    new_sim %>% filter(parameter == "mu", mu == 16),
    ease = "linear", nframes = 8
  ) %>%
    mutate(
      .frame = .frame + 13 * 10 + 4 * 20 + 1 * 8
    ),
  tween_path(
    new_sim %>% filter(parameter == "mu", mu == 16), 
    new_sim %>% filter(parameter == "mu", mu == 14),
    ease = "linear", nframes = 8
  ) %>%
    mutate(
      .frame = .frame + 13 * 10 + 4 * 20 + 2 * 8
    ),
  tween_path(
    new_sim %>% filter(parameter == "mu", mu == 14), 
    new_sim %>% filter(parameter == "mu", mu == 12),
    ease = "linear", nframes = 8
  ) %>%
    mutate(
      .frame = .frame + 13 * 10 + 4 * 20 + 3 * 8
    ),
  tween_path(
    new_sim %>% filter(parameter == "mu", mu == 12), 
    new_sim %>% filter(parameter == "mu", mu == 10),
    ease = "linear", nframes = 8
  ) %>%
    mutate(
      .frame = .frame + 13 * 10 + 4 * 20 + 4 * 8
    ),
  tween_path(
    new_sim %>% filter(parameter == "mu", mu == 10), 
    new_sim %>% filter(parameter == "mu", mu == 9),
    ease = "linear", nframes = 4
  ) %>%
    mutate(
      .frame = .frame + 13 * 10 + 4 * 20 + 5 * 8
    ),
  tween_path(
    new_sim %>% filter(parameter == "mu", mu == 9), 
    new_sim %>% filter(parameter == "mu", mu == 8),
    ease = "linear", nframes = 4
  ) %>%
    mutate(
      .frame = .frame + 13 * 10 + 4 * 20 + 5 * 8 + 1 * 4
    ),
  tween_path(
    new_sim %>% filter(parameter == "mu", mu == 8), 
    new_sim %>% filter(parameter == "mu", mu == 7),
    ease = "linear", nframes = 4
  ) %>%
    mutate(
      .frame = .frame + 13 * 10 + 4 * 20 + 5 * 8 + 2 * 4
    ),
  tween_path(
    new_sim %>% filter(parameter == "mu", mu == 7), 
    new_sim %>% filter(parameter == "mu", mu == 6),
    ease = "linear", nframes = 4
  ) %>%
    mutate(
      .frame = .frame + 13 * 10 + 4 * 20 + 5 * 8 + 3 * 4
    ),
  tween_path(
    new_sim %>% filter(parameter == "mu", mu == 6), 
    new_sim %>% filter(parameter == "mu", mu == 5),
    ease = "linear", nframes = 4
  ) %>%
    mutate(
      .frame = .frame + 13 * 10 + 4 * 20 + 5 * 8 + 4 * 4
    ),
  tween_path(
    new_sim %>% filter(parameter == "mu", mu == 5), 
    new_sim %>% filter(parameter == "mu", mu == 4),
    ease = "linear", nframes = 4
  ) %>%
    mutate(
      .frame = .frame + 13 * 10 + 4 * 20 + 5 * 8 + 5 * 4
    ),
  tween_path(
    new_sim %>% filter(parameter == "mu", mu == 4), 
    new_sim %>% filter(parameter == "mu", mu == 3),
    ease = "linear", nframes = 4
  ) %>%
    mutate(
      .frame = .frame + 13 * 10 + 4 * 20 + 5 * 8 + 6 * 4
    ),
  tween_path(
    new_sim %>% filter(parameter == "mu", mu == 3), 
    new_sim %>% filter(parameter == "mu", mu == 2),
    ease = "linear", nframes = 4
  ) %>%
    mutate(
      .frame = .frame + 13 * 10 + 4 * 20 + 5 * 8 + 7 * 4
    ),
  tween_path(
    new_sim %>% filter(parameter == "mu", mu == 2), 
    new_sim %>% filter(parameter == "mu", mu == 1),
    ease = "linear", nframes = 4
  ) %>%
    mutate(
      .frame = .frame + 13 * 10 + 4 * 20 + 5 * 8 + 8 * 4
    ),
  tween_path(
    new_sim %>% filter(parameter == "mu", mu == 1), 
    new_sim %>% filter(parameter == "mu", mu == 2),
    ease = "linear", nframes = 4
  ) %>%
    mutate(
      .frame = .frame + 13 * 10 + 4 * 20 + 5 * 8 + 9 * 4
    ),
  tween_path(
    new_sim %>% filter(parameter == "mu", mu == 2), 
    new_sim %>% filter(parameter == "mu", mu == 3),
    ease = "linear", nframes = 4
  ) %>%
    mutate(
      .frame = .frame + 13 * 10 + 4 * 20 + 5 * 8 + 10 * 4
    ),
  tween_path(
    new_sim %>% filter(parameter == "mu", mu == 3), 
    new_sim %>% filter(parameter == "mu", mu == 4),
    ease = "linear", nframes = 4
  ) %>%
    mutate(
      .frame = .frame + 13 * 10 + 4 * 20 + 5 * 8 + 11 * 4
    ),
  tween_path(
    new_sim %>% filter(parameter == "mu", mu == 4), 
    new_sim %>% filter(parameter == "mu", mu == 5),
    ease = "linear", nframes = 4
  ) %>%
    mutate(
      .frame = .frame + 13 * 10 + 4 * 20 + 5 * 8 + 12 * 4
    ),
  tween_path(
    new_sim %>% filter(parameter == "mu", mu == 5), 
    new_sim %>% filter(parameter == "mu", mu == 6),
    ease = "linear", nframes = 4
  ) %>%
    mutate(
      .frame = .frame + 13 * 10 + 4 * 20 + 5 * 8 + 13 * 4
    ),
  tween_path(
    new_sim %>% filter(parameter == "mu", mu == 6), 
    new_sim %>% filter(parameter == "mu", mu == 7),
    ease = "linear", nframes = 4
  ) %>%
    mutate(
      .frame = .frame + 13 * 10 + 4 * 20 + 5 * 8 + 14 * 4
    ),
  tween_path(
    new_sim %>% filter(parameter == "mu", mu == 7), 
    new_sim %>% filter(parameter == "mu", mu == 8),
    ease = "linear", nframes = 4
  ) %>%
    mutate(
      .frame = .frame + 13 * 10 + 4 * 20 + 5 * 8 + 15 * 4
    ),
  tween_path(
    new_sim %>% filter(parameter == "mu", mu == 8), 
    new_sim %>% filter(parameter == "mu", mu == 9),
    ease = "linear", nframes = 4
  ) %>%
    mutate(
      .frame = .frame + 13 * 10 + 4 * 20 + 5 * 8 + 16 * 4
    ),
  tween_path(
    new_sim %>% filter(parameter == "mu", mu == 9), 
    new_sim %>% filter(parameter == "mu", mu == 10),
    ease = "linear", nframes = 4
  ) %>%
    mutate(
      .frame = .frame + 13 * 10 + 4 * 20 + 5 * 8 + 17 * 4
    ),
  tween_path(
    new_sim %>% filter(parameter == "mu", mu == 10), 
    new_sim %>% filter(parameter == "mu", mu == 12),
    ease = "linear", nframes = 8
  ) %>%
    mutate(
      .frame = .frame + 13 * 10 + 4 * 20 + 5 * 8 + 18 * 4
    ),
  tween_path(
    new_sim %>% filter(parameter == "mu", mu == 12), 
    new_sim %>% filter(parameter == "mu", mu == 14),
    ease = "linear", nframes = 8
  ) %>%
    mutate(
      .frame = .frame + 13 * 10 + 4 * 20 + 6 * 8 + 18 * 4
    ),
  tween_path(
    new_sim %>% filter(parameter == "mu", mu == 14), 
    new_sim %>% filter(parameter == "mu", mu == 16),
    ease = "linear", nframes = 8
  ) %>%
    mutate(
      .frame = .frame + 13 * 10 + 4 * 20 + 7 * 8 + 18 * 4
    ),
  tween_path(
    new_sim %>% filter(parameter == "mu", mu == 16), 
    new_sim %>% filter(parameter == "mu", mu == 18),
    ease = "linear", nframes = 8
  ) %>%
    mutate(
      .frame = .frame + 13 * 10 + 4 * 20 + 8 * 8 + 18 * 4
    ),
  tween_path(
    new_sim %>% filter(parameter == "mu", mu == 18), 
    new_sim %>% filter(parameter == "mu", mu == 20),
    ease = "linear", nframes = 8
  ) %>%
    mutate(
      .frame = .frame + 13 * 10 + 4 * 20 + 9 * 8 + 18 * 4
    ),
  tween_path(
    new_sim %>% filter(parameter == "mu", mu == 20), 
    new_sim %>% filter(parameter == "mu", mu == 25),
    ease = "linear", nframes = 20
  ) %>%
    mutate(
      .frame = .frame + 13 * 10 + 4 * 20 + 10 * 8 + 18 * 4
    ),
  tween_path(
    new_sim %>% filter(parameter == "mu", mu == 25), 
    new_sim %>% filter(parameter == "mu", mu == 30),
    ease = "linear", nframes = 20
  ) %>%
    mutate(
      .frame = .frame + 13 * 10 + 5 * 20 + 10 * 8 + 18 * 4
    ),
  tween_path(
    new_sim %>% filter(parameter == "mu", mu == 30), 
    new_sim %>% filter(parameter == "mu", mu == 35),
    ease = "linear", nframes = 20
  ) %>%
    mutate(
      .frame = .frame + 13 * 10 + 6 * 20 + 10 * 8 + 18 * 4
    ),
  tween_path(
    new_sim %>% filter(parameter == "mu", mu == 35), 
    new_sim %>% filter(parameter == "mu", mu == 40),
    ease = "linear", nframes = 20
  ) %>%
    mutate(
      .frame = .frame + 13 * 10 + 7 * 20 + 10 * 8 + 18 * 4
    ),
  tween_path(
    new_sim %>% filter(parameter == "mu", mu == 40), 
    new_sim %>% filter(parameter == "mu", mu == 45),
    ease = "linear", nframes = 20
  ) %>%
    mutate(
      .frame = .frame + 13 * 10 + 8 * 20 + 10 * 8 + 18 * 4
    ),
  tween_path(
    new_sim %>% filter(parameter == "mu", mu == 45), 
    new_sim %>% filter(parameter == "mu", mu == 50),
    ease = "linear", nframes = 20
  ) %>%
    mutate(
      .frame = .frame + 13 * 10 + 9 * 20 + 10 * 8 + 18 * 4
    ),
  tween_path(
    new_sim %>% filter(parameter == "mu", mu == 50), 
    new_sim %>% filter(parameter == "mu", mu == 55),
    ease = "linear", nframes = 20
  ) %>%
    mutate(
      .frame = .frame + 13 * 10 + 10 * 20 + 10 * 8 + 18 * 4
    ),
  tween_path(
    new_sim %>% filter(parameter == "mu", mu == 55), 
    new_sim %>% filter(parameter == "mu", mu == 60),
    ease = "linear", nframes = 20
  ) %>%
    mutate(
      .frame = .frame + 13 * 10 + 11 * 20 + 10 * 8 + 18 * 4
    ),
  tween_path(
    new_sim %>% filter(parameter == "mu", mu == 60), 
    new_sim %>% filter(parameter == "mu", mu == 55),
    ease = "linear", nframes = 20
  ) %>%
    mutate(
      .frame = .frame + 13 * 10 + 12 * 20 + 10 * 8 + 18 * 4
    ),
  tween_path(
    new_sim %>% filter(parameter == "mu", mu == 55), 
    new_sim %>% filter(parameter == "mu", mu == 50),
    ease = "linear", nframes = 20
  ) %>%
    mutate(
      .frame = .frame + 13 * 10 + 13 * 20 + 10 * 8 + 18 * 4
    ),
  tween_path(
    new_sim %>% filter(parameter == "mu", mu == 50), 
    new_sim %>% filter(parameter == "mu", mu == 45),
    ease = "linear", nframes = 20
  ) %>%
    mutate(
      .frame = .frame + 13 * 10 + 14 * 20 + 10 * 8 + 18 * 4
    ),
  tween_path(
    new_sim %>% filter(parameter == "mu", mu == 45), 
    new_sim %>% filter(parameter == "mu", mu == 40),
    ease = "linear", nframes = 20
  ) %>%
    mutate(
      .frame = .frame + 13 * 10 + 15 * 20 + 10 * 8 + 18 * 4
    ),
  tween_path(
    new_sim %>% filter(parameter == "mu", mu == 40), 
    new_sim %>% filter(parameter == "mu", mu == 35),
    ease = "linear", nframes = 20
  ) %>%
    keep_state(nframes = 20) %>%
    mutate(
      .frame = .frame + 13 * 10 + 16 * 20 + 10 * 8 + 18 * 4
    ),
  tween_path(
    new_sim %>% filter(parameter == "tau", tau == 0.1), 
    new_sim %>% filter(parameter == "tau", tau == 0.12),
    ease = "linear", nframes = 20
  ) %>%
    mutate(
      .frame = .frame + 13 * 10 + 18 * 20 + 10 * 8 + 18 * 4
    ),
  tween_path(
    new_sim %>% filter(parameter == "tau", tau == 0.12), 
    new_sim %>% filter(parameter == "tau", tau == 0.14),
    ease = "linear", nframes = 20
  ) %>%
    mutate(
      .frame = .frame + 13 * 10 + 19 * 20 + 10 * 8 + 18 * 4
    ),
  tween_path(
    new_sim %>% filter(parameter == "tau", tau == 0.14), 
    new_sim %>% filter(parameter == "tau", tau == 0.16),
    ease = "linear", nframes = 20
  ) %>%
    mutate(
      .frame = .frame + 13 * 10 + 20 * 20 + 10 * 8 + 18 * 4
    ),
  tween_path(
    new_sim %>% filter(parameter == "tau", tau == 0.16), 
    new_sim %>% filter(parameter == "tau", tau == 0.18),
    ease = "linear", nframes = 20
  ) %>%
    mutate(
      .frame = .frame + 13 * 10 + 21 * 20 + 10 * 8 + 18 * 4
    ),
  tween_path(
    new_sim %>% filter(parameter == "tau", tau == 0.18), 
    new_sim %>% filter(parameter == "tau", tau == 0.2),
    ease = "linear", nframes = 20
  ) %>%
    mutate(
      .frame = .frame + 13 * 10 + 22 * 20 + 10 * 8 + 18 * 4
    ),
  tween_path(
    new_sim %>% filter(parameter == "tau", tau == 0.2), 
    new_sim %>% filter(parameter == "tau", tau == 0.18),
    ease = "linear", nframes = 20
  ) %>%
    mutate(
      .frame = .frame + 13 * 10 + 23 * 20 + 10 * 8 + 18 * 4
    ),
  tween_path(
    new_sim %>% filter(parameter == "tau", tau == 0.18), 
    new_sim %>% filter(parameter == "tau", tau == 0.16),
    ease = "linear", nframes = 20
  ) %>%
    mutate(
      .frame = .frame + 13 * 10 + 24 * 20 + 10 * 8 + 18 * 4
    ),
  tween_path(
    new_sim %>% filter(parameter == "tau", tau == 0.16), 
    new_sim %>% filter(parameter == "tau", tau == 0.14),
    ease = "linear", nframes = 20
  ) %>%
    mutate(
      .frame = .frame + 13 * 10 + 25 * 20 + 10 * 8 + 18 * 4
    ),
  tween_path(
    new_sim %>% filter(parameter == "tau", tau == 0.14), 
    new_sim %>% filter(parameter == "tau", tau == 0.12),
    ease = "linear", nframes = 20
  ) %>%
    mutate(
      .frame = .frame + 13 * 10 + 26 * 20 + 10 * 8 + 18 * 4
    ),
  tween_path(
    new_sim %>% filter(parameter == "tau", tau == 0.12), 
    new_sim %>% filter(parameter == "tau", tau == 0.1),
    ease = "linear", nframes = 20
  ) %>%
    mutate(
      .frame = .frame + 13 * 10 + 27 * 20 + 10 * 8 + 18 * 4
    ),
  tween_path(
    new_sim %>% filter(parameter == "tau", tau == 0.1), 
    new_sim %>% filter(parameter == "tau", tau == 0.08),
    ease = "linear", nframes = 20
  ) %>%
    mutate(
      .frame = .frame + 13 * 10 + 28 * 20 + 10 * 8 + 18 * 4
    ),
  tween_path(
    new_sim %>% filter(parameter == "tau", tau == 0.08), 
    new_sim %>% filter(parameter == "tau", tau == 0.06),
    ease = "linear", nframes = 20
  ) %>%
    mutate(
      .frame = .frame + 13 * 10 + 29 * 20 + 10 * 8 + 18 * 4
    ),
  tween_path(
    new_sim %>% filter(parameter == "tau", tau == 0.06), 
    new_sim %>% filter(parameter == "tau", tau == 0.04),
    ease = "linear", nframes = 20
  ) %>%
    mutate(
      .frame = .frame + 13 * 10 + 30 * 20 + 10 * 8 + 18 * 4
    ),
  tween_path(
    new_sim %>% filter(parameter == "tau", tau == 0.04), 
    new_sim %>% filter(parameter == "tau", tau == 0.02),
    ease = "linear", nframes = 20
  ) %>%
    mutate(
      .frame = .frame + 13 * 10 + 31 * 20 + 10 * 8 + 18 * 4
    ),
  tween_path(
    new_sim %>% filter(parameter == "tau", tau == 0.02), 
    new_sim %>% filter(parameter == "tau", tau == 0.04),
    ease = "linear", nframes = 20
  ) %>%
    mutate(
      .frame = .frame + 13 * 10 + 32 * 20 + 10 * 8 + 18 * 4
    ),
  tween_path(
    new_sim %>% filter(parameter == "tau", tau == 0.04), 
    new_sim %>% filter(parameter == "tau", tau == 0.06),
    ease = "linear", nframes = 20
  ) %>%
    mutate(
      .frame = .frame + 13 * 10 + 33 * 20 + 10 * 8 + 18 * 4
    ),
  tween_path(
    new_sim %>% filter(parameter == "tau", tau == 0.06), 
    new_sim %>% filter(parameter == "tau", tau == 0.08),
    ease = "linear", nframes = 20
  ) %>%
    mutate(
      .frame = .frame + 13 * 10 + 34 * 20 + 10 * 8 + 18 * 4
    ),
  tween_path(
    new_sim %>% filter(parameter == "tau", tau == 0.08), 
    new_sim %>% filter(parameter == "tau", tau == 0.1),
    ease = "linear", nframes = 20
  ) %>%
    keep_state(nframes = 20) %>%
    mutate(
      .frame = .frame + 13 * 10 + 35 * 20 + 10 * 8 + 18 * 4
    ),
  tween_path(
    new_sim %>% filter(parameter == "alpha", alpha == 0.01), 
    new_sim %>% filter(parameter == "alpha", alpha == 0),
    ease = "linear", nframes = 10
  ) %>%
    mutate(
      .frame = .frame + 13 * 10 + 37 * 20 + 10 * 8 + 18 * 4
    ),
  tween_path(
    new_sim %>% filter(parameter == "alpha", alpha == 0), 
    new_sim %>% filter(parameter == "alpha", alpha == -0.01),
    ease = "linear", nframes = 10
  ) %>%
    mutate(
      .frame = .frame + 14 * 10 + 37 * 20 + 10 * 8 + 18 * 4
    ),
  tween_path(
    new_sim %>% filter(parameter == "alpha", alpha == -0.01), 
    new_sim %>% filter(parameter == "alpha", alpha == -0.02),
    ease = "linear", nframes = 10
  ) %>%
    mutate(
      .frame = .frame + 15 * 10 + 37 * 20 + 10 * 8 + 18 * 4
    ),
  tween_path(
    new_sim %>% filter(parameter == "alpha", alpha == -0.02), 
    new_sim %>% filter(parameter == "alpha", alpha == -0.03),
    ease = "linear", nframes = 10
  ) %>%
    mutate(
      .frame = .frame + 16 * 10 + 37 * 20 + 10 * 8 + 18 * 4
    ),
  tween_path(
    new_sim %>% filter(parameter == "alpha", alpha == -0.03), 
    new_sim %>% filter(parameter == "alpha", alpha == -0.04),
    ease = "linear", nframes = 10
  ) %>%
    mutate(
      .frame = .frame + 17 * 10 + 37 * 20 + 10 * 8 + 18 * 4
    ),
  tween_path(
    new_sim %>% filter(parameter == "alpha", alpha == -0.04), 
    new_sim %>% filter(parameter == "alpha", alpha == -0.05),
    ease = "linear", nframes = 10
  ) %>%
    mutate(
      .frame = .frame + 18 * 10 + 37 * 20 + 10 * 8 + 18 * 4
    ),
  tween_path(
    new_sim %>% filter(parameter == "alpha", alpha == -0.05), 
    new_sim %>% filter(parameter == "alpha", alpha == -0.06),
    ease = "linear", nframes = 10
  ) %>%
    mutate(
      .frame = .frame + 19 * 10 + 37 * 20 + 10 * 8 + 18 * 4
    ),
  tween_path(
    new_sim %>% filter(parameter == "alpha", alpha == -0.06), 
    new_sim %>% filter(parameter == "alpha", alpha == -0.07),
    ease = "linear", nframes = 10
  ) %>%
    mutate(
      .frame = .frame + 20 * 10 + 37 * 20 + 10 * 8 + 18 * 4
    ),
  tween_path(
    new_sim %>% filter(parameter == "alpha", alpha == -0.07), 
    new_sim %>% filter(parameter == "alpha", alpha == -0.08),
    ease = "linear", nframes = 10
  ) %>%
    mutate(
      .frame = .frame + 21 * 10 + 37 * 20 + 10 * 8 + 18 * 4
    ),
  tween_path(
    new_sim %>% filter(parameter == "alpha", alpha == -0.08), 
    new_sim %>% filter(parameter == "alpha", alpha == -0.09),
    ease = "linear", nframes = 10
  ) %>%
    mutate(
      .frame = .frame + 22 * 10 + 37 * 20 + 10 * 8 + 18 * 4
    ),
  tween_path(
    new_sim %>% filter(parameter == "alpha", alpha == -0.09), 
    new_sim %>% filter(parameter == "alpha", alpha == -0.1),
    ease = "linear", nframes = 10
  ) %>%
    mutate(
      .frame = .frame + 23 * 10 + 37 * 20 + 10 * 8 + 18 * 4
    )
) %T>%
  print()

new_sim_ani %$% n_distinct(.frame) # 1132 frames in total


# 1.4 Animated figures ####
require(here)
require(gganimate)
require(ggtext) # for html and markdown syntax
require(glue) # for dynamic text

# Old model
( ggplot() +
    geom_line(
      data = old_sim_ani,
      aes(t, m)
    ) +
    geom_rug(
      data = old_sim_ani,
      aes(t0.5), outside = TRUE,
      length = unit(.25, "cm"), lineend = "square"
    ) +
    # Using glue in title is not possible so I am using geom_richtext
    geom_richtext(
      data = old_sim_ani %>%
        distinct(k, t0.5, .frame) %>%
        mutate(
          # Format numbers
          k_form = k %>% map( # map ensures format is evaluated individually
            ~.x %>% signif(2) %>% 
              format(scientific = F) %>% # remove scientific notation
              str_replace('-', '−') # ensure proper minus symbol is used
          ),
          t0.5_form = floor(t0.5 + .5)
        ),
      aes(
        x = 0, y = 1, 
        label = glue(
          "*k* = {k_form}<br>
          *t*<sub><span style='font-size:8.4pt'>½</span></sub> = {t0.5_form}"
        )
      ),
      hjust = 0, vjust = 0, size = 12/.pt, # mm to pt conversion
      lineheight = 1.5, # increases spacing of <br>
      family = "Futura", fill = NA, label.size = 0 # remove label box
    ) +
    scale_x_continuous(
      breaks = seq(0, 60, 10),
      limits = c(0, 60)
    ) +
    scale_y_continuous(
      breaks = seq(0, 1, 0.2),
      labels = scales::label_number(accuracy = c(1, rep(0.1, 4), 1))
    ) +
    labs(
      x = "*t*", 
      y = "*m*/*m*<sub><span style='font-size:8.4pt'>0</span></sub>"
    ) +
    coord_cartesian(ylim = c(0, 1), expand = F, clip = "off") +
    transition_manual(.frame) +
    mytheme +
    theme(
      axis.title.x = element_markdown(), # explicitly set for each axis
      axis.title.y = element_markdown(),
      plot.margin = margin(1.5, 0.5, 0.2, 0.2, unit = "cm")
    ) ) %>%
  animate(
    fps = 25, duration = 9, detail = 5, # 25*9*5 is less than 1174 frames
    width = 20, height = 10,
    units = "cm", res = 300,
    renderer = gifski_renderer(),
    device = "ragg_png" # necessary for italic!
  ) %>%
  anim_save(
    filename = "old_sim.gif", 
    path = here("Figures", "Animation")
  )

# New model, top panel

# Register medium font variant for Greek letters
systemfonts::register_variant(
  name = "Helvetica Neue Medium",
  family = "Helvetica Neue",
  weight = "medium"
)

( ggplot() +
    geom_hline(yintercept = 0) +
    geom_line(
      data = new_sim_ani,
      aes(t, k)
    ) +
    geom_rug(
      data = new_sim_ani,
      aes(mu), outside = TRUE,
      length = unit(.25, "cm"), lineend = "square"
    ) +
    geom_richtext(
      data = new_sim_ani %>%
        distinct(alpha, mu, tau, .frame) %>%
        mutate(
          # Format numbers
          alpha = alpha %>% signif(2) %>% str_replace('-', '−'),
          mu = floor(mu + .5) %>% as.character(),
          tau = tau %>% signif(2) %>% as.character()
        ) %>%
        pivot_longer(
          cols = -.frame, names_to = "parameter"
        ) %>%
        mutate(
          letter = case_when(
            parameter == "alpha" ~ "α",
            parameter == "mu" ~ "μ",
            parameter == "tau" ~ "τ"
          ),
          x = case_when(
            parameter == "alpha" ~ 1,
            parameter == "mu" ~ 13,
            parameter == "tau" ~ 25
          ),
          y = -0.185
        ),
      aes(
        x, y, label = glue(
          # Futura does not support Greek letters, medium Helvetica Neue is closest
          "*<span style='font-family:\"Helvetica Neue Medium\"'>{letter}</span>* = {value}"
        )
      ),
      hjust = 0, vjust = 0, size = 12/.pt, # mm to pt conversion
      family = "Futura", fill = NA, label.size = 0 # remove label box
    ) +
    scale_x_continuous(
      breaks = seq(0, 60, 10),
      limits = c(0, 60)
    ) +
    scale_y_continuous(
      breaks = seq(-0.2, 0.04, 0.08),
      limits = c(-0.2, 0.04),
      labels = scales::label_number(accuracy = c(0.1, rep(0.01, 3)),
                                    style_negative = "minus")
    ) +
    labs(x = "*t*", y = "*k*") +
    coord_cartesian(expand = F, clip = "off") +
    transition_manual(.frame) +
    mytheme +
    theme(
      axis.title.x = element_markdown(),
      axis.title.y = element_markdown()
    ) ) %>%
  animate(
    fps = 25, duration = 30, width = 20, height = 5,
    units = "cm", res = 300, renderer = gifski_renderer(),
    device = "ragg_png"
  ) %>%
  anim_save(
    filename = "new_sim_top.gif", 
    path = here("Figures", "Animation")
  )

# Clear registered fonts
systemfonts::clear_registry()

# New model, bottom panel
( ggplot() +
    geom_hline(yintercept = 1) +
    geom_line(
      data = new_sim_ani,
      aes(t, m)
    ) +
    geom_rug(
      data = new_sim_ani,
      aes(mu), outside = TRUE,
      length = unit(.25, "cm"), lineend = "square"
    ) +
    scale_x_continuous(
      breaks = seq(0, 60, 10),
      limits = c(0, 60)
    ) +
    scale_y_continuous(
      breaks = seq(0, 1.5, 0.5),
      limits = c(0, 1.5),
      # match y axis text spacing of top panel
      labels = c("<span style='color: transparent'>−00</span>0", "0.5", "1", "1.5")
    ) +
    labs(
      x = "*t*", 
      y = "*m*/*m*<sub><span style='font-size:8.4pt'>0</span></sub>"
    ) +
    coord_cartesian(expand = F, clip = "off") +
    transition_manual(.frame) +
    mytheme +
    theme(
      axis.title.x = element_markdown(),
      axis.title.y = element_markdown(),
      axis.text.y = element_markdown()
    ) ) %>%
  animate(
    fps = 25, duration = 30, width = 20, height = 5,
    units = "cm", res = 300, renderer = gifski_renderer(),
    device = "ragg_png"
  ) %>%
  anim_save(
    filename = "new_sim_bottom.gif", 
    path = here("Figures", "Animation")
  )

# 2. Example ####
# 2.1 Load data ####
# I am only using the Vandendriessche et al. 2007 data for one replicate
# of ungrazed Ascophyllum nodosum per temperature to make my point.

data <- read_csv("Examples.csv", col_types = list( "f", "c", "f", "f", "f" )) %>%
  filter(
    reference == "Vandendriessche et al. 2007" &
      species == "Ascophyllum nodosum" &
      treatment %>% str_detect("Control") &
      t != 0
  ) %>%
  droplevels() %>%
  mutate(
    temperature = treatment %>% 
      str_extract("\\d+°C") %>% 
      fct_relevel("5°C", "10°C", "15°C"),
    m = if_else(m_mean == 0, 1e-5, m_mean)
  ) %>%
  select(t, m, temperature, replicate) %T>%
  print()

data %>%
  ggplot(aes(t, m, colour = temperature)) +
    geom_point() +
    facet_grid(cols = vars(replicate)) +
    theme_minimal() +
    theme(legend.position = "top")
# replicates 48, 8, 21, and 35 look most complete

data %<>%
  filter(replicate %in% c(48, 8, 21, 35)) %>%
  select(-replicate) %T>%
  print()

data %>%
  ggplot(aes(t, m, colour = temperature)) +
    geom_point() +
    theme_minimal() +
    theme(legend.position = "top")

Vandendriessche_prior_posterior <- 
  here("RDS", "Vandendriessche_prior_posterior_replicate.rds") %>%
  read_rds() %>%
  filter(
    replicate %in% c("Prior", 48, 8, 21, 35) &
      grazing == -0.5
  ) %>%
  droplevels() %>%
  mutate(
    temperature = temperature %>% 
      fct_relevel("Prior", "5°C", "10°C", "15°C")
  ) %>%
  select(-c(species, grazing, replicate)) %T>%
  print()

Vandendriessche_k_prior_posterior <- 
  here("RDS", "Vandendriessche_k_prior_posterior_replicate.rds") %>%
  read_rds() %>%
  filter(
    replicate %in% c("Prior", 48, 8, 21, 35) &
      grazing == -0.5
  ) %>%
  mutate(
    temperature = temperature %>% 
      fct_relevel("Prior", "5°C", "10°C", "15°C")
  ) %>%
  droplevels() %>%
  select(-c(species, grazing, replicate)) %T>%
  print()

# Predict across full predictor range
require(extraDistr) # needed for beta prime
require(tidybayes) # needed for qi()

# New model
Vandendriessche_prediction <- Vandendriessche_prior_posterior %>%
  expand_grid(t = seq(0, max(data$t), length.out = 200)) %>%
  mutate(
    k = ( alpha + tau ) / ( 1 + exp( 5 / mu * ( t - mu ) ) ) - tau,
    m_mu = exp(
      t * alpha - ( alpha + tau ) * mu / 5 * (
        log1p_exp( 5 / mu * ( t - mu ) ) -
          log1p_exp( -5 )
      )
    ),
    nu = ( epsilon - theta ) * exp( -lambda * t ) + theta,
    m = rbetapr( n() , m_mu * ( 1 + nu ) , 2 + nu )
  ) %>%
  summarise(
    across(
      c(k, m_mu, m),
      list(median = median, 
           lower_0.5 = ~ qi(.x, .width = .5)[1],
           upper_0.5 = ~ qi(.x, .width = .5)[2],
           lower_0.8 = ~ qi(.x, .width = .8)[1],
           upper_0.8 = ~ qi(.x, .width = .8)[2],
           lower_0.9 = ~ qi(.x, .width = .9)[1],
           upper_0.9 = ~ qi(.x, .width = .9)[2]),
      .names = "{.col}.{.fn}"
    ),
    .by = c(t, temperature)
  ) %>%
  rename(k = k.median, m_mu = m_mu.median, m = m.median) %>%
  pivot_longer(cols = contains("lower") | contains("upper")) %>%
  separate(col = name, into = c("name", ".width"), sep = "_(?=[^_]*$)") %>%
  pivot_wider(names_from = name, values_from = value) %T>%
  print()

# Old model
Vandendriessche_k_prediction <- Vandendriessche_k_prior_posterior %>%
  expand_grid(t = seq(0, max(data$t), length.out = 200)) %>%
  mutate(
    m_mu = exp( -k * t ),
    m = rnorm( n() , m_mu , sigma ),
    k = -k # reverse sign
  ) %>%
  summarise(
    across(
      c(k, m_mu, m),
      list(median = median, 
           lower_0.5 = ~ qi(.x, .width = .5)[1],
           upper_0.5 = ~ qi(.x, .width = .5)[2],
           lower_0.8 = ~ qi(.x, .width = .8)[1],
           upper_0.8 = ~ qi(.x, .width = .8)[2],
           lower_0.9 = ~ qi(.x, .width = .9)[1],
           upper_0.9 = ~ qi(.x, .width = .9)[2]),
      .names = "{.col}.{.fn}"
    ),
    .by = c(t, temperature)
  ) %>%
  rename(k = k.median, m_mu = m_mu.median, m = m.median) %>%
  pivot_longer(cols = contains("lower") | contains("upper")) %>%
  separate(col = name, into = c("name", ".width"), sep = "_(?=[^_]*$)") %>%
  pivot_wider(names_from = name, values_from = value) %T>%
  print()

# 2.2 Aesthetics ####
# Points
points <- data %>%
  mutate(
    alpha = 0.5, # Baseline transparency
    colour = case_when( # Colours
      temperature == "5°C" ~ "#2e4a5b",
      temperature == "10°C" ~ "#6a98b4",
      temperature == "15°C" ~ "#f5a54a",
      temperature == "18°C" ~ "#d1750c"
    )
  ) %>%
  rename(x = t, y = m) %>% # Rename to x and y for consistency
  select(temperature, x, y, alpha, colour) %T>% # Select relevant variables
  print()

# Text
text <- Vandendriessche_prediction %>%
  distinct(temperature) %>%
  mutate(
    colour = case_when(
      temperature == "Prior" ~ "#b5b8ba", # here I also have Prior
      temperature == "5°C" ~ "#2e4a5b",
      temperature == "10°C" ~ "#6a98b4",
      temperature == "15°C" ~ "#f5a54a",
      temperature == "18°C" ~ "#d1750c"
    ),
    # characters are sometimes interpreted as colours by tweenr
    # so I am defining label as a factor here
    label = temperature
  ) %T>%
  print()

# Lines
old_k_line <- Vandendriessche_k_prediction %>%
  select(temperature, t, k) %>%
  rename(x = t, y = k) %>%
  mutate(
    colour = case_when(
      temperature == "Prior" ~ "#b5b8ba",
      temperature == "5°C" ~ "#2e4a5b",
      temperature == "10°C" ~ "#6a98b4",
      temperature == "15°C" ~ "#f5a54a",
      temperature == "18°C" ~ "#d1750c"
    )
  ) %T>%
  print()

old_m_line <- Vandendriessche_k_prediction %>%
  select(temperature, t, m_mu) %>%
  rename(x = t, y = m_mu) %>%
  mutate(
    colour = case_when(
      temperature == "Prior" ~ "#b5b8ba",
      temperature == "5°C" ~ "#2e4a5b",
      temperature == "10°C" ~ "#6a98b4",
      temperature == "15°C" ~ "#f5a54a",
      temperature == "18°C" ~ "#d1750c"
    )
  ) %T>%
  print()

new_k_line <- Vandendriessche_prediction %>%
  select(temperature, t, k) %>%
  rename(x = t, y = k) %>%
  mutate(
    colour = case_when(
      temperature == "Prior" ~ "#b5b8ba",
      temperature == "5°C" ~ "#2e4a5b",
      temperature == "10°C" ~ "#6a98b4",
      temperature == "15°C" ~ "#f5a54a",
      temperature == "18°C" ~ "#d1750c"
    )
  ) %T>%
  print()

new_m_line <- Vandendriessche_prediction %>%
  select(temperature, t, m_mu) %>%
  rename(x = t, y = m_mu) %>%
  mutate(
    colour = case_when(
      temperature == "Prior" ~ "#b5b8ba",
      temperature == "5°C" ~ "#2e4a5b",
      temperature == "10°C" ~ "#6a98b4",
      temperature == "15°C" ~ "#f5a54a",
      temperature == "18°C" ~ "#d1750c"
    )
  ) %T>%
  print()

# Ribbons
old_k_ribbon <- Vandendriessche_k_prediction %>%
  group_by(temperature, .width) %>%
  reframe(
    x = c(t, rev(t)),
    y = c(k.upper, rev(k.lower))
  ) %>%
  mutate(
    alpha = case_when(
      .width == .9 ~ .5, 
      .width == .8 ~ .4, 
      .width == .5 ~ .3
    ),
    fill = case_when(
      temperature == "Prior" ~ "#b5b8ba",
      temperature == "5°C" ~ "#2e4a5b",
      temperature == "10°C" ~ "#6a98b4",
      temperature == "15°C" ~ "#f5a54a",
      temperature == "18°C" ~ "#d1750c"
    )
  ) %>%
  select(-.width) %T>%
  print()

old_m_ribbon <- Vandendriessche_k_prediction %>%
  group_by(temperature, .width) %>%
  reframe(
    x = c(t, rev(t)),
    y = c(m_mu.upper, rev(m_mu.lower))
  ) %>%
  mutate(
    alpha = case_when(
      .width == .9 ~ .5, 
      .width == .8 ~ .4, 
      .width == .5 ~ .3
    ),
    fill = case_when(
      temperature == "Prior" ~ "#b5b8ba",
      temperature == "5°C" ~ "#2e4a5b",
      temperature == "10°C" ~ "#6a98b4",
      temperature == "15°C" ~ "#f5a54a",
      temperature == "18°C" ~ "#d1750c"
    )
  ) %>%
  select(-.width) %T>%
  print()

new_k_ribbon <- Vandendriessche_prediction %>%
  group_by(temperature, .width) %>%
  reframe(
    x = c(t, rev(t)),
    y = c(k.upper, rev(k.lower))
  ) %>%
  mutate(
    alpha = case_when(
      .width == .9 ~ .5, 
      .width == .8 ~ .4, 
      .width == .5 ~ .3
    ),
    fill = case_when(
      temperature == "Prior" ~ "#b5b8ba",
      temperature == "5°C" ~ "#2e4a5b",
      temperature == "10°C" ~ "#6a98b4",
      temperature == "15°C" ~ "#f5a54a",
      temperature == "18°C" ~ "#d1750c"
    )
  ) %>%
  select(-.width) %T>%
  print()

new_m_ribbon <- Vandendriessche_prediction %>%
  group_by(temperature, .width) %>%
  reframe(
    x = c(t, rev(t)),
    y = c(m_mu.upper, rev(m_mu.lower))
  ) %>%
  mutate(
    alpha = case_when(
      .width == .9 ~ .5, 
      .width == .8 ~ .4, 
      .width == .5 ~ .3
    ),
    fill = case_when(
      temperature == "Prior" ~ "#b5b8ba",
      temperature == "5°C" ~ "#2e4a5b",
      temperature == "10°C" ~ "#6a98b4",
      temperature == "15°C" ~ "#f5a54a",
      temperature == "18°C" ~ "#d1750c"
    )
  ) %>%
  select(-.width) %T>%
  print()

# 2.3 Static figures ####
# Define custom theme
mytheme <- theme(
  panel.background = element_blank(),
  panel.grid.major = element_blank(),
  panel.grid.minor = element_blank(),
  panel.border = element_blank(),
  plot.margin = margin(0.2, 0.5, 0.2, 0.2, unit = "cm"),
  plot.title = element_text(size = 12, hjust = 0),
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
  panel.spacing = unit(1, "cm"),
  text = element_text(family = "Futura")
)

# Top panel, points only
ggplot() +
  geom_hline(yintercept = 1) +
  geom_point(
    data = points,
    aes(x, y, colour = colour, alpha = alpha),
    shape = 16, size = 2.35
  ) +
  geom_text(
    data = text,
    aes(x = 4, y = 4, label = label, colour = colour),
    hjust = 0, vjust = 1, size.unit = "pt", size = 12, family = "Futura"
  ) +
  scale_alpha_identity() +
  scale_colour_identity() +
  scale_x_continuous(breaks = seq(0, 240, by = 30)) +
  facet_grid(~ temperature, space = "free", scales = "free") +
  labs(x = "Detrital age (days)", 
       y = expression("Detrital mass ("*italic("m")*"/"*italic("m")[0]*")")) +
  coord_cartesian(xlim = c(0, 240), ylim = c(0, 4), expand = F, clip = "off") +
  mytheme

# Top panel, old model
ggplot() +
  geom_hline(yintercept = 1) +
  geom_point(
    data = points,
    aes(x, y, colour = colour, alpha = alpha),
    shape = 16, size = 2.35
  ) +
  geom_line(
    data = old_m_line,
    aes(x, y, colour = colour)
  ) +
  geom_polygon(
    data = old_m_ribbon,
    aes(x, y, fill = fill, alpha = alpha, group = alpha)
  ) +
  geom_text(
    data = text,
    aes(x = 4, y = 4, label = label, colour = colour),
    hjust = 0, vjust = 1, size.unit = "pt", size = 12, family = "Futura"
  ) +
  scale_alpha_identity() +
  scale_colour_identity() +
  scale_fill_identity() +
  scale_x_continuous(breaks = seq(0, 240, by = 30)) +
  facet_grid(~ temperature, space = "free", scales = "free") +
  labs(x = "Detrital age (days)", 
       y = expression("Detrital mass ("*italic("m")*"/"*italic("m")[0]*")")) +
  coord_cartesian(xlim = c(0, 240), ylim = c(0, 4), expand = F, clip = "off") +
  mytheme

# Top panel, new model
ggplot() +
  geom_hline(yintercept = 1) +
  geom_point(
    data = points,
    aes(x, y, colour = colour, alpha = alpha),
    shape = 16, size = 2.35
  ) +
  geom_line(
    data = new_m_line,
    aes(x, y, colour = colour)
  ) +
  geom_polygon(
    data = new_m_ribbon,
    aes(x, y, fill = fill, alpha = alpha, group = alpha)
  ) +
  geom_text(
    data = text,
    aes(x = 4, y = 4, label = label, colour = colour),
    hjust = 0, vjust = 1, size.unit = "pt", size = 12, family = "Futura"
  ) +
  scale_alpha_identity() +
  scale_colour_identity() +
  scale_fill_identity() +
  scale_x_continuous(breaks = seq(0, 240, by = 30)) +
  facet_grid(~ temperature, space = "free", scales = "free") +
  labs(x = "Detrital age (days)", 
       y = expression("Detrital mass ("*italic("m")*"/"*italic("m")[0]*")")) +
  coord_cartesian(xlim = c(0, 240), ylim = c(0, 4), expand = F, clip = "off") +
  mytheme

# Bottom panel, old model
ggplot() +
  geom_hline(yintercept = 0) +
  geom_line(
    data = old_k_line,
    aes(x, y, colour = colour)
  ) +
  geom_polygon(
    data = old_k_ribbon,
    aes(x, y, fill = fill, alpha = alpha, group = alpha)
  ) +
  scale_alpha_identity() +
  scale_colour_identity() +
  scale_fill_identity() +
  scale_x_continuous(breaks = seq(0, 240, by = 30)) +
  scale_y_continuous(
    breaks = seq(-0.05, 0.01, 0.02),
    labels = scales::label_number(style_negative = "minus")
  ) +
  facet_grid(~ temperature, space = "free", scales = "free") +
  labs(x = "Detrital age (days)", 
       y = expression(italic("k")*"(day"^-1*")")) +
  coord_cartesian(xlim = c(0, 240), ylim = c(-0.05, 0.01), expand = F) +
  mytheme

# Bottom panel, new model
ggplot() +
  geom_hline(yintercept = 0) +
  geom_line(
    data = new_k_line,
    aes(x, y, colour = colour)
  ) +
  geom_polygon(
    data = new_k_ribbon,
    aes(x, y, fill = fill, alpha = alpha, group = alpha)
  ) +
  scale_alpha_identity() +
  scale_colour_identity() +
  scale_fill_identity() +
  scale_x_continuous(breaks = seq(0, 240, by = 30)) +
  scale_y_continuous(
    breaks = seq(-0.05, 0.01, 0.02),
    labels = scales::label_number(style_negative = "minus")
  ) +
  facet_grid(~ temperature, space = "free", scales = "free") +
  labs(x = "Detrital age (days)", 
       y = expression(italic("k")*"(day"^-1*")")) +
  coord_cartesian(xlim = c(0, 240), ylim = c(-0.05, 0.01), expand = F) +
  mytheme

# 2.4 Animation ####
# Define enter/exit functions
enter <- function(data){
  data$alpha <- 0
  data
}

exit <- function(data){
  data$alpha <- 0
  data
}

# Tween points
points_short_ani <- bind_rows( 
  tween_state( # Points are arbitrarily paired
    points %>% filter(temperature == "18°C"),
    points %>% filter(temperature == "15°C"),
    ease = "cubic-in-out", nframes = 100,
    # enter and exit are needed because sample size is not consistent
    enter = enter, exit = exit
  ) %>%
    keep_state(nframes = 50),
  tween_state(
    points %>% filter(temperature == "15°C"),
    points %>% filter(temperature == "10°C"),
    ease = "cubic-in-out", nframes = 100,
    enter = enter, exit = exit
  ) %>%
    keep_state(nframes = 50) %>%
    mutate(.frame = .frame + 1 * 150),
  tween_state(
    points %>% filter(temperature == "10°C"),
    points %>% filter(temperature == "5°C"),
    ease = "cubic-in-out", nframes = 100,
    enter = enter, exit = exit
  ) %>%
    keep_state(nframes = 50) %>%
    mutate(.frame = .frame + 2 * 150),
  tween_state(
    points %>% filter(temperature == "5°C"),
    points %>% filter(temperature == "18°C"),
    ease = "cubic-in-out", nframes = 100,
    enter = enter, exit = exit
  ) %>%
    keep_state(nframes = 50) %>%
    mutate(.frame = .frame + 3 * 150)
) %T>%
  print()

points_short_ani %$% n_distinct(.frame) # 600 frames

points_long_ani <- bind_rows( 
  tween_state(
    points %>% filter(temperature == "18°C"),
    points %>% filter(temperature == "15°C"),
    ease = "cubic-in-out", nframes = 100,
    enter = enter, exit = exit
  ) %>%
    keep_state(nframes = 50),
  tween_state(
    points %>% filter(temperature == "15°C"),
    points %>% filter(temperature == "10°C"),
    ease = "cubic-in-out", nframes = 100,
    enter = enter, exit = exit
  ) %>%
    keep_state(nframes = 50) %>%
    mutate(.frame = .frame + 1 * 150),
  tween_state(
    points %>% filter(temperature == "10°C"),
    points %>% filter(temperature == "5°C"),
    ease = "cubic-in-out", nframes = 100,
    enter = enter, exit = exit
  ) %>%
    keep_state(nframes = 200) %>% # this is where models transition
    mutate(.frame = .frame + 2 * 150),
  tween_state(
    points %>% filter(temperature == "5°C"),
    points %>% filter(temperature == "10°C"),
    ease = "cubic-in-out", nframes = 100,
    enter = enter, exit = exit
  ) %>%
    keep_state(nframes = 50) %>%
    mutate(.frame = .frame + 4 * 150),
  tween_state(
    points %>% filter(temperature == "10°C"),
    points %>% filter(temperature == "15°C"),
    ease = "cubic-in-out", nframes = 100,
    enter = enter, exit = exit
  ) %>%
    keep_state(nframes = 50) %>%
    mutate(.frame = .frame + 5 * 150),
  tween_state(
    points %>% filter(temperature == "15°C"),
    points %>% filter(temperature == "18°C"),
    ease = "cubic-in-out", nframes = 100,
    enter = enter, exit = exit
  ) %>%
    keep_state(nframes = 200) %>% # this is where models transition
    mutate(.frame = .frame + 6 * 150)
) %T>%
  print()

points_long_ani %$% n_distinct(.frame) # 1200 frames

# Tween text
text_short_ani <- bind_rows( 
  tween_state(
    text %>% filter(temperature == "18°C"),
    text %>% filter(temperature == "15°C"),
    ease = "cubic-in-out", nframes = 100
  ) %>%
    keep_state(nframes = 50),
  tween_state(
    text %>% filter(temperature == "15°C"),
    text %>% filter(temperature == "10°C"),
    ease = "cubic-in-out", nframes = 100
  ) %>%
    keep_state(nframes = 50) %>%
    mutate(.frame = .frame + 1 * 150),
  tween_state(
    text %>% filter(temperature == "10°C"),
    text %>% filter(temperature == "5°C"),
    ease = "cubic-in-out", nframes = 100
  ) %>%
    keep_state(nframes = 50) %>%
    mutate(.frame = .frame + 2 * 150),
  tween_state(
    text %>% filter(temperature == "5°C"),
    text %>% filter(temperature == "18°C"),
    ease = "cubic-in-out", nframes = 100
  ) %>%
    keep_state(nframes = 50) %>%
    mutate(.frame = .frame + 3 * 150)
) %T>%
  print()

text_short_ani %$% n_distinct(.frame) # 600 frames

text_long_ani <- bind_rows( 
  tween_state(
    text %>% filter(temperature == "18°C"),
    text %>% filter(temperature == "15°C"),
    ease = "cubic-in-out", nframes = 100
  ) %>%
    keep_state(nframes = 50),
  tween_state(
    text %>% filter(temperature == "15°C"),
    text %>% filter(temperature == "10°C"),
    ease = "cubic-in-out", nframes = 100
  ) %>%
    keep_state(nframes = 50) %>%
    mutate(.frame = .frame + 1 * 150),
  tween_state(
    text %>% filter(temperature == "10°C"),
    text %>% filter(temperature == "5°C"),
    ease = "cubic-in-out", nframes = 100
  ) %>%
    keep_state(nframes = 200) %>% # this is where models transition
    mutate(.frame = .frame + 2 * 150),
  tween_state(
    text %>% filter(temperature == "5°C"),
    text %>% filter(temperature == "10°C"),
    ease = "cubic-in-out", nframes = 100
  ) %>%
    keep_state(nframes = 50) %>%
    mutate(.frame = .frame + 4 * 150),
  tween_state(
    text %>% filter(temperature == "10°C"),
    text %>% filter(temperature == "15°C"),
    ease = "cubic-in-out", nframes = 100
  ) %>%
    keep_state(nframes = 50) %>%
    mutate(.frame = .frame + 5 * 150),
  tween_state(
    text %>% filter(temperature == "15°C"),
    text %>% filter(temperature == "18°C"),
    ease = "cubic-in-out", nframes = 100
  ) %>%
    keep_state(nframes = 200) %>% # this is where models transition
    mutate(.frame = .frame + 6 * 150)
) %T>%
  print()

text_long_ani %$% n_distinct(.frame) # 1200 frames

# Tween lines
k_line_ani <- bind_rows(
  tween_path(
    old_k_line %>% filter(temperature == "18°C"), 
    old_k_line %>% filter(temperature == "15°C"),
    ease = "cubic-in-out", nframes = 100
  ) %>%
    keep_state(nframes = 50),
  tween_path(
    old_k_line %>% filter(temperature == "15°C"), 
    old_k_line %>% filter(temperature == "10°C"),
    ease = "cubic-in-out", nframes = 100
  ) %>%
    keep_state(nframes = 50) %>%
    mutate(.frame = .frame + 1 * 150),
  tween_path(
    old_k_line %>% filter(temperature == "10°C"), 
    old_k_line %>% filter(temperature == "5°C"),
    ease = "cubic-in-out", nframes = 100
  ) %>%
    keep_state(nframes = 50) %>%
    mutate(.frame = .frame + 2 * 150),
  tween_path(
    old_k_line %>% filter(temperature == "5°C"), 
    new_k_line %>% filter(temperature == "5°C"),
    ease = "cubic-in-out", nframes = 100
  ) %>%
    keep_state(nframes = 50) %>%
    mutate(.frame = .frame + 3 * 150),
  tween_path(
    new_k_line %>% filter(temperature == "5°C"), 
    new_k_line %>% filter(temperature == "10°C"),
    ease = "cubic-in-out", nframes = 100
  ) %>%
    keep_state(nframes = 50) %>%
    mutate(.frame = .frame + 4 * 150),
  tween_path(
    new_k_line %>% filter(temperature == "10°C"), 
    new_k_line %>% filter(temperature == "15°C"),
    ease = "cubic-in-out", nframes = 100
  ) %>%
    keep_state(nframes = 50) %>%
    mutate(.frame = .frame + 5 * 150),
  tween_path(
    new_k_line %>% filter(temperature == "15°C"), 
    new_k_line %>% filter(temperature == "18°C"),
    ease = "cubic-in-out", nframes = 100
  ) %>%
    keep_state(nframes = 50) %>%
    mutate(.frame = .frame + 6 * 150),
  tween_path(
    new_k_line %>% filter(temperature == "18°C"), 
    old_k_line %>% filter(temperature == "18°C"),
    ease = "cubic-in-out", nframes = 100
  ) %>%
    keep_state(nframes = 50) %>%
    mutate(.frame = .frame + 7 * 150)
) %T>%
  print()

k_line_ani %$% n_distinct(.frame) # 1200 frames

m_line_ani <- bind_rows(
  tween_path(
    old_m_line %>% filter(temperature == "18°C"), 
    old_m_line %>% filter(temperature == "15°C"),
    ease = "cubic-in-out", nframes = 100
  ) %>%
    keep_state(nframes = 50),
  tween_path(
    old_m_line %>% filter(temperature == "15°C"), 
    old_m_line %>% filter(temperature == "10°C"),
    ease = "cubic-in-out", nframes = 100
  ) %>%
    keep_state(nframes = 50) %>%
    mutate(.frame = .frame + 1 * 150),
  tween_path(
    old_m_line %>% filter(temperature == "10°C"), 
    old_m_line %>% filter(temperature == "5°C"),
    ease = "cubic-in-out", nframes = 100
  ) %>%
    keep_state(nframes = 50) %>%
    mutate(.frame = .frame + 2 * 150),
  tween_path(
    old_m_line %>% filter(temperature == "5°C"), 
    new_m_line %>% filter(temperature == "5°C"),
    ease = "cubic-in-out", nframes = 100
  ) %>%
    keep_state(nframes = 50) %>%
    mutate(.frame = .frame + 3 * 150),
  tween_path(
    new_m_line %>% filter(temperature == "5°C"), 
    new_m_line %>% filter(temperature == "10°C"),
    ease = "cubic-in-out", nframes = 100
  ) %>%
    keep_state(nframes = 50) %>%
    mutate(.frame = .frame + 4 * 150),
  tween_path(
    new_m_line %>% filter(temperature == "10°C"), 
    new_m_line %>% filter(temperature == "15°C"),
    ease = "cubic-in-out", nframes = 100
  ) %>%
    keep_state(nframes = 50) %>%
    mutate(.frame = .frame + 5 * 150),
  tween_path(
    new_m_line %>% filter(temperature == "15°C"), 
    new_m_line %>% filter(temperature == "18°C"),
    ease = "cubic-in-out", nframes = 100
  ) %>%
    keep_state(nframes = 50) %>%
    mutate(.frame = .frame + 6 * 150),
  tween_path(
    new_m_line %>% filter(temperature == "18°C"), 
    old_m_line %>% filter(temperature == "18°C"),
    ease = "cubic-in-out", nframes = 100
  ) %>%
    keep_state(nframes = 50) %>%
    mutate(.frame = .frame + 7 * 150)
) %T>%
  print()

m_line_ani %$% n_distinct(.frame) # 1200 frames

# Tween ribbons
k_ribbon_ani <- bind_rows(
  tween_path(
    old_k_ribbon %>% filter(temperature == "18°C"), 
    old_k_ribbon %>% filter(temperature == "15°C"),
    ease = "cubic-in-out", nframes = 100
  ) %>%
    keep_state(nframes = 50),
  tween_path(
    old_k_ribbon %>% filter(temperature == "15°C"), 
    old_k_ribbon %>% filter(temperature == "10°C"),
    ease = "cubic-in-out", nframes = 100
  ) %>%
    keep_state(nframes = 50) %>%
    mutate(.frame = .frame + 1 * 150),
  tween_path(
    old_k_ribbon %>% filter(temperature == "10°C"), 
    old_k_ribbon %>% filter(temperature == "5°C"),
    ease = "cubic-in-out", nframes = 100
  ) %>%
    keep_state(nframes = 50) %>%
    mutate(.frame = .frame + 2 * 150),
  tween_path(
    old_k_ribbon %>% filter(temperature == "5°C"), 
    new_k_ribbon %>% filter(temperature == "5°C"),
    ease = "cubic-in-out", nframes = 100
  ) %>%
    keep_state(nframes = 50) %>%
    mutate(.frame = .frame + 3 * 150),
  tween_path(
    new_k_ribbon %>% filter(temperature == "5°C"), 
    new_k_ribbon %>% filter(temperature == "10°C"),
    ease = "cubic-in-out", nframes = 100
  ) %>%
    keep_state(nframes = 50) %>%
    mutate(.frame = .frame + 4 * 150),
  tween_path(
    new_k_ribbon %>% filter(temperature == "10°C"), 
    new_k_ribbon %>% filter(temperature == "15°C"),
    ease = "cubic-in-out", nframes = 100
  ) %>%
    keep_state(nframes = 50) %>%
    mutate(.frame = .frame + 5 * 150),
  tween_path(
    new_k_ribbon %>% filter(temperature == "15°C"), 
    new_k_ribbon %>% filter(temperature == "18°C"),
    ease = "cubic-in-out", nframes = 100
  ) %>%
    keep_state(nframes = 50) %>%
    mutate(.frame = .frame + 6 * 150),
  tween_path(
    new_k_ribbon %>% filter(temperature == "18°C"), 
    old_k_ribbon %>% filter(temperature == "18°C"),
    ease = "cubic-in-out", nframes = 100
  ) %>%
    keep_state(nframes = 50) %>%
    mutate(.frame = .frame + 7 * 150)
) %T>%
  print()

k_ribbon_ani %$% n_distinct(.frame) # 1050 frames

m_ribbon_ani <- bind_rows(
  tween_path(
    old_m_ribbon %>% filter(temperature == "18°C"), 
    old_m_ribbon %>% filter(temperature == "15°C"),
    ease = "cubic-in-out", nframes = 100
  ) %>%
    keep_state(nframes = 50),
  tween_path(
    old_m_ribbon %>% filter(temperature == "15°C"), 
    old_m_ribbon %>% filter(temperature == "10°C"),
    ease = "cubic-in-out", nframes = 100
  ) %>%
    keep_state(nframes = 50) %>%
    mutate(.frame = .frame + 1 * 150),
  tween_path(
    old_m_ribbon %>% filter(temperature == "10°C"), 
    old_m_ribbon %>% filter(temperature == "5°C"),
    ease = "cubic-in-out", nframes = 100
  ) %>%
    keep_state(nframes = 50) %>%
    mutate(.frame = .frame + 2 * 150),
  tween_path(
    old_m_ribbon %>% filter(temperature == "5°C"), 
    new_m_ribbon %>% filter(temperature == "5°C"),
    ease = "cubic-in-out", nframes = 100
  ) %>%
    keep_state(nframes = 50) %>%
    mutate(.frame = .frame + 3 * 150),
  tween_path(
    new_m_ribbon %>% filter(temperature == "5°C"), 
    new_m_ribbon %>% filter(temperature == "10°C"),
    ease = "cubic-in-out", nframes = 100
  ) %>%
    keep_state(nframes = 50) %>%
    mutate(.frame = .frame + 4 * 150),
  tween_path(
    new_m_ribbon %>% filter(temperature == "10°C"), 
    new_m_ribbon %>% filter(temperature == "15°C"),
    ease = "cubic-in-out", nframes = 100
  ) %>%
    keep_state(nframes = 50) %>%
    mutate(.frame = .frame + 5 * 150),
  tween_path(
    new_m_ribbon %>% filter(temperature == "15°C"), 
    new_m_ribbon %>% filter(temperature == "18°C"),
    ease = "cubic-in-out", nframes = 100
  ) %>%
    keep_state(nframes = 50) %>%
    mutate(.frame = .frame + 6 * 150),
  tween_path(
    new_m_ribbon %>% filter(temperature == "18°C"), 
    old_m_ribbon %>% filter(temperature == "18°C"),
    ease = "cubic-in-out", nframes = 100
  ) %>%
    keep_state(nframes = 50) %>%
    mutate(.frame = .frame + 7 * 150)
) %T>%
  print()

m_ribbon_ani %$% n_distinct(.frame) # 1200 frames

# 2.5 Animated figures ####
# Top panel v1
( ggplot() +
    geom_hline(yintercept = 1) +
    geom_point(
      data = points_short_ani,
      aes(x, y, colour = colour, alpha = alpha),
      shape = 16, size = 2.35
    ) +
    geom_text(
      data = text_short_ani,
      aes(x = 2, y = 4, label = label, colour = colour),
      hjust = 0, vjust = 1, size.unit = "pt", 
      size = 12, family = "Futura"
    ) +
    scale_alpha_identity() +
    scale_colour_identity() +
    scale_x_continuous(breaks = seq(0, 240, by = 30)) +
    scale_y_continuous( # match y axis text spacing of bottom panel
      labels = c("<span style='color: transparent'>−00</span>0", 1:4)
    ) +
    labs(x = "Detrital age (days)", 
         # match y axis title spacing of bottom panel
         y = "Mass (*m*/*m*<sub><span style='font-size:8.4pt'>0</span></sub>)
         <sup><span style='font-size:8.4pt;color:transparent'>−1</span></sup>") +
    coord_cartesian(xlim = c(0, 240), ylim = c(0, 4), expand = F, clip = "off") +
    transition_manual(.frame) +
    mytheme +
    theme(
      axis.title.y = element_markdown(), # name axis explicitly
      axis.text.y = element_markdown()
    ) ) %>%
  animate(
    fps = 25, # optimal gif frame rate (4 centiseconds per frame) with reasonable file size
    duration = 4 * 3, # number of transitions (4) * time per transition (3 s) to set speed
    detail = 2, # 25*4*3 is 1/2 of all frames (600), so the skipped frames can add detail
    width = 20, height = 5,
    units = "cm", res = 300,
    renderer = gifski_renderer(),
    device = "ragg_png" # necessary for italic!
  ) %>%
  anim_save(
    filename = "example_top_1.gif", 
    path = here("Figures", "Animation")
  )

# Top panel v2
( ggplot() +
    geom_hline(yintercept = 1) +
    geom_point(
      data = points_long_ani,
      aes(x, y, colour = colour, alpha = alpha),
      shape = 16, size = 2.35
    ) +
    geom_line(
      data = m_line_ani,
      aes(x, y, colour = colour)
    ) +
    geom_polygon(
      data = m_ribbon_ani,
      aes(x, y, fill = fill, alpha = alpha, group = alpha)
    ) +
    geom_text(
      data = text_long_ani,
      aes(x = 2, y = 4, label = label, colour = colour),
      hjust = 0, vjust = 1, size.unit = "pt", 
      size = 12, family = "Futura"
    ) +
    scale_alpha_identity() +
    scale_colour_identity() +
    scale_fill_identity() +
    scale_x_continuous(breaks = seq(0, 240, by = 30)) +
    scale_y_continuous(
      labels = c("<span style='color:transparent'>−00</span>0", 1:4)
    ) +
    labs(x = "Detrital age (days)", 
         y = "Mass (*m*/*m*<sub><span style='font-size:8.4pt'>0</span></sub>)
         <sup><span style='font-size:8.4pt;color:transparent'>−1</span></sup>") +
    coord_cartesian(xlim = c(0, 240), ylim = c(0, 4), expand = F, clip = "off") +
    transition_manual(.frame) +
    mytheme +
    theme(
      axis.title.y = element_markdown(),
      axis.text.y = element_markdown()
    ) ) %>%
  animate(
    fps = 25, duration = 8 * 3, detail = 2,
    width = 20, height = 5, units = "cm", res = 300,
    renderer = gifski_renderer(), device = "ragg_png"
  ) %>%
  anim_save(
    filename = "example_top_2.gif", 
    path = here("Figures", "Animation")
  )

# Bottom panel
( ggplot() +
    geom_hline(yintercept = 0) +
    geom_line(
      data = k_line_ani,
      aes(x, y, colour = colour)
    ) +
    geom_polygon(
      data = k_ribbon_ani,
      aes(x, y, fill = fill, alpha = alpha, group = alpha)
    ) +
    scale_alpha_identity() +
    scale_colour_identity() +
    scale_fill_identity() +
    scale_x_continuous(breaks = seq(0, 240, by = 30)) +
    scale_y_continuous(
      breaks = seq(-0.05, 0.01, 0.02),
      labels = scales::label_number(style_negative = "minus")
    ) +
    labs(x = "Detrital age (days)", 
         y = "*k* (day<sup><span style='font-size:8.4pt'>−1</span></sup>)") +
    coord_cartesian(xlim = c(0, 240), ylim = c(-0.05, 0.01), expand = F, clip = "off") +
    transition_manual(.frame) +
    mytheme +
    theme( axis.title.y = element_markdown() ) ) %>%
  animate(
    fps = 25, duration = 8 * 3, detail = 2,
    width = 20, height = 5, units = "cm", res = 300,
    renderer = gifski_renderer(), device = "ragg_png"
  ) %>%
  anim_save(
    filename = "example_bottom.gif", 
    path = here("Figures", "Animation")
  )