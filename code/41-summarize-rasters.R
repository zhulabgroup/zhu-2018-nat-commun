library(tidyverse)
library(sp)
library(SpatialPack)

cur.ras <- read_rds("models/cur_ras.rds")
fut.ras <- read_rds("models/fut_ras.rds")
pst.ras <- read_rds("models/pst_ras.rds")

# Spatial correlation tests -----------------------------------------------

# modified.ttest(cur.ras$agb_cur_obs, cur.ras$agb_cur_mod, coords = coordinates(cur.ras))
# modified.ttest(pst.ras$agb_pst_obs, pst.ras$agb_pst_mod, coords = coordinates(pst.ras))

# Summary statistics ------------------------------------------------------

ras.dat <- as_tibble(cur.ras) %>%
  gather() %>%
  bind_rows(as_tibble(fut.ras) %>%
    gather()) %>%
  separate(key, into = c("variable", "period", "scenario"))

ras.sum <- ras.dat %>%
  # filter(variable == 'agb') %>%
  group_by(variable, period, scenario) %>%
  summarize(
    mean = mean(value), median = median(value),
    sd = sd(value), se = plotrix::std.error(value),
    lwr = quantile(value, .25), upr = quantile(value, .75),
    min = quantile(value, .05), max = quantile(value, .95)
  ) %>%
  ungroup()

scn.col <- c("obs" = "SkyBlue", "mod" = "DarkBlue", "rcp45" = "LightSalmon", "rcp85" = "DarkRed")

ras.sum %>%
  filter(variable == "agb") %>%
  mutate(
    period = factor(
      period,
      c("cur", "2025", "2055", "2085"),
      c("2000 - 2016", "2020s", "2050s", "2080s")
    ),
    scenario = factor(
      scenario,
      c("obs", "mod", "rcp45", "rcp85")
    )
  ) %>%
  ggplot(aes(period, mean, group = scenario, col = scenario)) +
  geom_point(
    size = 4,
    position = position_dodge(width = .5)
  ) +
  geom_errorbar(aes(ymin = mean - sd, ymax = mean + sd),
    width = .1,
    position = position_dodge(width = .5)
  ) +
  geom_linerange(aes(ymin = lwr, ymax = upr),
    size = 3, alpha = .5,
    position = position_dodge(width = .5)
  ) +
  geom_linerange(aes(ymin = min, ymax = max),
    size = 1.5, alpha = .2,
    position = position_dodge(width = .5)
  ) +
  scale_y_continuous(breaks = c(0, 50, 100, 150, 200, 250)) +
  scale_color_manual(values = scn.col) +
  labs(x = "", y = expression("Aboveground biomass (Mg" ~ ha^-1 * ")")) +
  theme_classic() +
  theme(legend.position = "none", text = element_text(size = 14)) +
  annotate("text", 1 - .2, 200, label = "Observed", col = scn.col["obs"]) +
  annotate("text", 1 + .2, 200, label = "modeled", col = scn.col["mod"]) +
  annotate("text", 2 - .2, 200, label = "RCP4.5", col = scn.col["rcp45"]) +
  annotate("text", 2 + .2, 200, label = "RCP8.5", col = scn.col["rcp85"])
ggsave("figures/Current and future AGB.pdf", w = 8, h = 8 * .618)
