# DO NOT PUBLISH
library(tidyverse)
library(R2jags)
library(ggmcmc)

# Compare with t error distribution DO NOT PUBLISH ------------------------

coef.std.norm.ci <- read_rds("Models/new_jags_2e4.rds") %>%
  coda::as.mcmc() %>%
  ggs() %>%
  ci() %>%
  filter(grepl("beta|gamma", Parameter))

coef.std.tdistn.ci <- read_rds("Models/new_jags_tdistn_ftdf_2e4.rds") %>%
  coda::as.mcmc() %>%
  ggs() %>%
  ci() %>%
  filter(grepl("beta|gamma", Parameter))

coef.comp.dat <- coef.std.norm.ci %>%
  inner_join(coef.std.tdistn.ci, by = "Parameter", suffix = c(".nor", ".t"))

coef.comp.dat %>%
  # filter(median.nor < 800) %>%
  ggplot(aes(median.nor, median.t)) +
  geom_point() +
  # geom_errorbarh(aes(xmin = Low.nor, xmax = High.nor), height = 0, size = .5) +
  # geom_errorbar(aes(ymin = Low.t, ymax = High.t), width = 0, size = .5) +
  geom_abline(intercept = 0, slope = 1, col = "red") +
  labs(x = "Coefficients from model with normal error", y = "Coefficients from model with t error") +
  theme_bw()
ggsave("Figures/Compare normal vs t error coefs.pdf", w = 5, h = 5)

tdf.dat <- read_rds("Models/new_jags_tdistn_ftdf_2e4.rds") %>%
  coda::as.mcmc() %>%
  ggs() %>%
  ci() %>%
  filter(grepl("tdf", Parameter))

ggplot(tdf.dat, aes(median)) +
  geom_histogram(bins = 10)

summary(tdf.dat$median)
plotrix::std.error(tdf.dat$median)
