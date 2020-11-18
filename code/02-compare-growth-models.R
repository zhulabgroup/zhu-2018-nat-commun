# Compare different growth models to show they're almost the same
library(tidyverse)
library(modelr)
theme_set(theme_bw())

# Fit different growth models ---------------------------------------------

byft.dat <- read_rds("Data/all_dat.rds") %>%
  select(src:agb) %>%
  filter(cen == "new") %>%
  group_by(ft) %>%
  nest()

byft.mod <- byft.dat %>%
  mutate(
    linear = map(data, function(dat) lm(agb ~ sag - 1, data = dat)),
    expon = map(data, function(dat) lm(agb ~ log(sag + 1) - 1, data = dat)),
    chrich = map(data, function(dat) nls(agb ~ SSasympOrig(sag, Asym, lrc), data = dat)),
    monod = map(data, function(dat) nls(agb ~ SSmicmen(sag, Vm, K), data = dat)),
    logis = map(data, function(dat) nls(agb ~ SSlogis(sag, Asym, xmid, scal), data = dat))
  ) %>%
  mutate(
    agb.linear = map2(data, linear, add_predictions, var = "agb.linear"),
    agb.expon = map2(data, expon, add_predictions, var = "agb.expon"),
    agb.chrich = map2(data, chrich, add_predictions, var = "agb.chrich"),
    agb.monod = map2(data, monod, add_predictions, var = "agb.monod"),
    agb.logis = map2(data, logis, add_predictions, var = "agb.logis")
  ) %>%
  mutate(
    aic.linear = map(linear, AIC),
    aic.expon = map(expon, AIC),
    aic.chrich = map(chrich, AIC),
    aic.monod = map(monod, AIC),
    aic.logis = map(logis, AIC)
  )

# Predict growth curves ---------------------------------------------------

pred.dat <- byft.mod %>%
  select(ft, agb.linear) %>%
  unnest(cols = c(agb.linear)) %>%
  inner_join(byft.mod %>%
    select(ft, agb.expon) %>%
    unnest(cols = c(agb.expon))) %>%
  inner_join(byft.mod %>%
    select(ft, agb.chrich) %>%
    unnest(cols = c(agb.chrich))) %>%
  inner_join(byft.mod %>%
    select(ft, agb.monod) %>%
    unnest(cols = c(agb.monod))) %>%
  inner_join(byft.mod %>%
    select(ft, agb.logis) %>%
    unnest(cols = c(agb.logis)))

mod.col <- c("black", "blue", "orange", "red")

pred.dat %>%
  select(ft, sag, agb.linear, agb.expon, agb.chrich, agb.monod) %>%
  gather("model", "agb", agb.linear:agb.monod) %>%
  mutate(model = factor(
    model,
    c("agb.linear", "agb.expon", "agb.chrich", "agb.monod"),
    c("Linear", "Exponential", "Chapman-Richards", "Monod")
  )) %>%
  ggplot(aes(sag, agb, group = model, col = model)) +
  geom_smooth(se = F) +
  facet_wrap(~ft, scale = "free_y", nrow = 6, ncol = 4) +
  scale_y_sqrt() +
  labs(
    x = "Stand age (yr)", y = expression("Aboveground biomass (Mg" ~ ha^-1 * ")"),
    col = "", group = ""
  ) +
  scale_color_manual(values = mod.col) +
  theme(legend.position = c(0.9, 0.075))
ggsave("Figures/Compare growth model curves.pdf", w = 10, h = 12)

# Calculate AICs ----------------------------------------------------------

byft.mod %>%
  select(ft, aic.linear, aic.expon, aic.chrich, aic.monod) %>%
  unnest(cols = c(aic.linear, aic.expon, aic.chrich, aic.monod)) %>%
  gather("model", "aic", -ft) %>%
  mutate(model = factor(
    model,
    c("aic.linear", "aic.expon", "aic.chrich", "aic.monod"),
    c("Linear", "Exponential", "Chapman-Richards", "Monod")
  )) %>%
  ggplot(aes(model, aic, col = model)) +
  geom_point() +
  facet_wrap(~ft, scale = "free_y", nrow = 6, ncol = 4) +
  labs(x = "Model", y = "AIC score") +
  scale_color_manual(values = mod.col) +
  guides(col = F) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = .5))
ggsave("Figures/Compare growth model AICs.pdf", w = 10, h = 12)
