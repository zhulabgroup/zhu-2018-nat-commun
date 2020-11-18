library(tidyverse)
library(R2jags)
library(ggmcmc)

ft.dat <- read_rds("data/ft_dat.rds")
env.stat <- read_rds("data/env_stat.rds")

# Coefficient means and CI ------------------------------------------------

coef.std.ci <- read_rds("models/new_jags_2e5.rds") %>%
  coda::as.mcmc() %>%
  ggs() %>%
  ci() %>%
  filter(grepl("beta|gamma", Parameter))

coef.ci <- coef.std.ci %>%
  separate(Parameter, c("para", "coef.id", "ft.id"), extra = "drop") %>%
  mutate(coef.id = as.numeric(coef.id), ft.id = as.numeric(ft.id)) %>%
  full_join(tibble(coef.id = 1:3, coef = c("int", "tmp", "ppt"))) %>%
  full_join(ft.dat %>% select(ft.id = id, ft)) %>%
  select(para, coef, ft, low, Low, median, High, high) %>% # low and high are 95% CI, Low and High are 90% CI
  arrange(para, coef, ft)
write_rds(coef.ci, "models/new_jags_2e5_sum.rds")

# Make a table ------------------------------------------------------------

coef.tab <- coef.ci %>%
  full_join(env.stat) %>%
  mutate(
    low = ifelse(coef == "tmp", low / tmp.sd, ifelse(coef == "ppt", low / ppt.sd, low)),
    high = ifelse(coef == "tmp", high / tmp.sd, ifelse(coef == "ppt", high / ppt.sd, high)),
    median = ifelse(coef == "tmp", median / tmp.sd, ifelse(coef == "ppt", median / ppt.sd, median))
  ) %>%
  mutate(
    coef = factor(coef,
      levels = c("int", "tmp", "ppt"),
      labels = c("Intercept", "Temperature", "Precipitation")
    ),
    ft = factor(ft, levels = sort(ft.dat$ft, decreasing = T))
  ) %>%
  select(para, coef, ft, low, median, high)

coef.tab %>%
  mutate(
    ft = as.character(ft),
    coef = factor(coef, c("Intercept", "Temperature", "Precipitation"), 0:2),
    nm = paste(para, coef),
    val = paste0(signif(median, 3), " (", signif(low, 3), ", ", signif(high, 3), ")")
  ) %>%
  select(ft, nm, val) %>%
  spread(nm, val)
write_csv(coef.tab, "tables/Coefficient table (2e5).csv")
