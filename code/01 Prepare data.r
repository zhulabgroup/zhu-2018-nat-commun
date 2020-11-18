# revision for Nature Comm, code to be published
library(tidyverse)
theme_set(theme_bw())

# Raw data ----------------------------------------------------------------

# Read CSV data file
all.dat <- read.csv("Data/All_data.csv", stringsAsFactors = F) %>% as_tibble() # can't use read_csv

# Regional standardization of climate variables
env.stat <- all.dat %>%
  filter(cen == "new") %>%
  group_by(ft) %>%
  summarize(
    tmp.mean = mean(tmp), tmp.sd = sd(tmp),
    ppt.mean = mean(ppt), ppt.sd = sd(ppt)
  ) %>%
  ungroup()

all.dat <- all.dat %>%
  full_join(env.stat) %>%
  mutate(
    tmp.ctr = tmp - tmp.mean, tmp.std = tmp.ctr / tmp.sd,
    ppt.ctr = ppt - ppt.mean, ppt.std = ppt.ctr / ppt.sd
  ) %>%
  select(-tmp.mean, -tmp.sd, -ppt.mean, -ppt.sd)

# JAGS data ---------------------------------------------------------------

in.dat <- all.dat %>%
  filter(cen == "new") %>%
  mutate(ft = as.factor(ft))
ft.dat <- tibble(id = 1:nlevels(in.dat$ft), ft = levels(in.dat$ft))

jags.dat <- list()

jags.dat$y <- in.dat$agb # aboveground carbon
jags.dat$x <- in.dat$sag # stand age
jags.dat$z <- cbind(1, in.dat$tmp.std, in.dat$ppt.std) # covariates, scaled or centered

jags.dat$ft <- as.numeric(in.dat$ft) # region index, vector
jags.dat$n.plt <- nrow(in.dat) # no. of plots
jags.dat$n.ft <- nrow(ft.dat) # no. of regions

write_rds(all.dat, "Data/all_dat.rds")
write_rds(env.stat, "Data/env_stat.rds")
write_rds(ft.dat, "Data/ft_dat.rds")
write_rds(jags.dat, "Data/jags_dat.rds")

# Historgams for sample sizes ---------------------------------------------

# current
all.dat %>%
  filter(cen == "new") %>%
  ggplot(aes(sag)) + # density is wrong
  geom_histogram(aes(y = (..count..) / tapply(..count.., ..PANEL.., sum)[..PANEL..]), bins = 30) +
  facet_wrap(~ft, scale = "free", nrow = 6, ncol = 4) +
  labs(x = "Stand age (yr)", y = "Percentage of plots") +
  scale_y_continuous(labels = scales::percent)
ggsave("Figures/Histogram of current plot percentage.pdf", w = 10, h = 12)

# past -- do not show
all.dat %>%
  filter(cen == "old") %>%
  ggplot(aes(sag)) + # density is wrong
  geom_histogram(aes(y = (..count..) / tapply(..count.., ..PANEL.., sum)[..PANEL..]), bins = 30) +
  facet_wrap(~ft, scale = "free", nrow = 6, ncol = 4) +
  labs(x = "Stand age (yr)", y = "Percentage of plots") +
  scale_y_continuous(labels = scales::percent)
ggsave("Figures/Histogram of past plot percentage.pdf", w = 10, h = 12)

# both current and past
all.dat %>%
  ggplot(aes(sag, y = (..count..) / tapply(..count.., ..PANEL.., sum)[..PANEL..], fill = cen)) +
  geom_histogram(data = filter(all.dat, cen == "old"), bins = 30, alpha = .2) +
  geom_histogram(data = filter(all.dat, cen == "new"), bins = 30, alpha = .2) +
  facet_wrap(~ft, scale = "free", nrow = 6, ncol = 4) +
  labs(x = "Stand age (yr)", y = "Plot percentage") +
  scale_fill_manual(
    name = "",
    values = c("red", "blue"),
    labels = c("Current period (2000 - 2016)", "Past period (1990 - 1999)"),
    guide = guide_legend(reverse = TRUE)
  ) +
  scale_y_continuous(labels = scales::percent) +
  theme(legend.position = c(0.885, 0.075))
ggsave("Figures/Histogram of current and past plot percentage.pdf", w = 10, h = 12)
