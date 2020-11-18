# extra prediction exercies, DO NOT PUBLISH
library(tidyverse)
library(R2jags)
library(ggmcmc)
theme_set(theme_bw())

all.dat <- read_rds("Data/all_dat.rds")
ft.dat <- read_rds("Data/ft_dat.rds")


mcmc.dat <- read_rds("Models/old_jags_2e2.rds") %>% # try 2e2 first, slow
  coda::as.mcmc() %>%
  ggmcmc::ggs()

coef.mcmc <- mcmc.dat %>%
  filter(grepl("beta|gamma", Parameter)) %>%
  separate(Parameter, c("para", "coef.id", "ft.id"), extra = "drop") %>%
  mutate(coef.id = as.numeric(coef.id), ft.id = as.numeric(ft.id)) %>%
  full_join(tibble(coef.id = 1:3, coef = c("int", "tmp", "ppt"))) %>%
  full_join(ft.dat %>% select(ft.id = id, ft)) %>%
  select(ft, iter = Iteration, chain = Chain, para, coef, val = value) %>%
  unite(para_coef, para, coef, sep = ".") %>%
  spread(para_coef, val)

cur.dat <- all.dat %>%
  filter(cen == "new")

wis.dat <- cur.dat %>%
  full_join(coef.mcmc, by = "ft") %>%
  mutate(
    mu.hat = beta.int + beta.tmp * tmp.std + beta.ppt * ppt.std,
    k.hat = gamma.int + gamma.tmp * tmp.std + gamma.ppt * ppt.std,
    agb.hat = mu.hat * sag / (k.hat + sag)
  ) %>%
  group_by(plt) %>%
  summarize(
    mu.mean = mean(mu.hat), mu.med = median(mu.hat), mu.lwr = quantile(mu.hat, .025), mu.upr = quantile(mu.hat, .975),
    agb.mean = mean(agb.hat), agb.med = median(agb.hat), agb.lwr = quantile(agb.hat, .025), agb.upr = quantile(agb.hat, .975)
  ) %>%
  inner_join(cur.dat, ., by = "plt")

ggplot(wis.dat, aes(agb)) +
  geom_pointrange(aes(y = agb.mean, ymin = agb.lwr, ymax = agb.upr), alpha = .1) +
  facet_wrap(~ft, scale = "free", nrow = 6, ncol = 4)

wis.dat %>%
  ggplot(aes(sag)) +
  geom_point(aes(y = agb), alpha = .1) +
  geom_pointrange(aes(y = agb.mean, ymin = agb.lwr, ymax = agb.upr), col = "red", alpha = .1) +
  scale_y_sqrt() +
  facet_wrap(~ft, scale = "free", nrow = 6, ncol = 4)
