library(tidyverse)
theme_set(theme_bw())

dat.dir <- 'Output/Growth/Publication/Data/'
mod.dir <- 'Output/Growth/Publication/Models/'
fig.dir <- 'Output/Growth/Publication/Figures/'

all.dat  <- read_rds(paste0(dat.dir, 'all_dat.rds'))
env.stat <- read_rds(paste0(dat.dir, 'env_stat.rds'))

coef.ci <- 'old_jags_2e4_sum.rds' %>% 
  paste0(mod.dir, .) %>% 
  read_rds()

coef.med <- coef.ci %>%
  unite(para_coef, para, coef, sep = '.') %>%
  select(para_coef, ft, median) %>%
  spread(para_coef, median)

# bin data
n.cls <- 10
sag.brk <- c(seq(0, 100, by = 10), 1000) # max(wis.dat$sag) = 949

# For current data, within-sample prediction ------------------------------

wis.dat <- all.dat %>%
  filter(cen == 'new') %>%
  full_join(coef.med, by = 'ft') %>%
  mutate(mu = beta.int + beta.tmp*tmp.std + beta.ppt*ppt.std,
         k  = gamma.int + gamma.tmp*tmp.std + gamma.ppt*ppt.std,
         agb.mod = mu*sag/(k + sag)) %>% 
  mutate(r.sat   = agb.mod/mu) %>% # remove this line
  select(src, cen, ft, plt, lon, lat, sag, tmp, ppt, agb, agb.mod, mu, r.sat) %>%
  arrange(src, cen, ft) %>% 
  mutate(sag.cls = cut(sag, sag.brk, include.lowest = T, dig.lab = 10)) %>% 
  write_rds(paste0(mod.dir, 'wis_dat.rds'))

# boxplot to compare observed vs modeled
wis.dat %>%
  # filter(ft %in% c('Alder / maple', 'Douglas-fir')) %>% # examples for my talk
  ggplot() +
  geom_boxplot(aes(x = sag.cls, y = agb), col = gray(.8)) +
  geom_line(aes(x = as.numeric(sag.cls), y = agb.mod), stat = "summary", fun.y = mean, col = 'red') +
  geom_ribbon(aes(x = as.numeric(sag.cls), y = agb.mod), stat = 'summary', fun.ymin = min, fun.ymax = max, fill = 'red', alpha = .2) +
  scale_y_sqrt() +
  facet_wrap(~ ft, scale = 'free_y', nrow = 6, ncol = 4) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = .5)) +
  labs(x = 'Stand age (yr)', y = expression('Aboveground biomass (Mg'~ha^-1*')'))
ggsave(paste0(fig.dir, 'Model vs. within-sample data.pdf'),  w = 10, h = 12)

# For past data, out-of-sample prediction ---------------------------------

oos.dat <- all.dat %>%
  filter(cen == 'old') %>%
  full_join(coef.med, by = 'ft') %>%
  mutate(mu = beta.int + beta.tmp*tmp.std + beta.ppt*ppt.std,
         k  = gamma.int + gamma.tmp*tmp.std + gamma.ppt*ppt.std, 
         agb.mod = mu*sag/(k + sag)) %>%
  select(src, cen, ft, plt, lon, lat, sag, tmp, ppt, agb, agb.mod) %>%
  arrange(src, cen, ft) %>% 
  mutate(sag.cls = cut(sag, sag.brk, include.lowest = T, dig.lab = 10)) %>%
  write_rds(paste0(mod.dir, 'oos_dat.rds'))

# boxplot
oos.dat %>% 
  ggplot() +
  geom_boxplot(aes(x = sag.cls, y = agb), col = gray(.8)) +
  geom_line(aes(x = as.numeric(sag.cls), y = agb.mod), stat = "summary", fun.y = mean, col = 'red') +
  geom_ribbon(aes(x = as.numeric(sag.cls), y = agb.mod), stat = 'summary', fun.ymin = min, fun.ymax = max, fill = 'red', alpha = .2) +
  scale_y_sqrt() +
  facet_wrap(~ ft, scale = 'free_y', nrow = 6, ncol = 4) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = .5)) +
  labs(x = 'Stand age (yr)', y = expression('Aboveground biomass (Mg'~ha^-1*')'))
ggsave(paste0(fig.dir, 'Model vs. out-of-sample data.pdf'),  w = 10, h = 12)

# For future data, out-of-sample prediction (projection) ------------------

agb.fut.dat <- NULL
for (rcp.tag in c('rcp45', 'rcp85')) { # loop through rcp and future years
  for (fyr.tag in c('2025', '2055', '2085')) {

    clim.file <- paste0(dat.dir, 'CMIP_', rcp.tag, '_', fyr.tag, '.csv')
    
    pred.dat <- all.dat %>% 
      select(src:agb) %>% 
      mutate(sag.fut = sag + as.numeric(fyr.tag) - yr) %>% # extrapolate stand age
      bind_cols(read_csv(clim.file) %>% select(tmp = MAT, ppt = MAP)) %>% # get climate projections
      filter(cen == 'new',
             tmp != -9999,
             ppt != -9999) %>% # remove missing values
      mutate(cen = 'fut') %>% 
      full_join(env.stat, by = 'ft') %>% 
      mutate(tmp.std = (tmp - tmp.mean)/tmp.sd, ppt.std = (ppt - ppt.mean)/ppt.sd) %>% 
      full_join(coef.med, by = 'ft') %>% 
      mutate(mu = beta.int + beta.tmp*tmp.std + beta.ppt*ppt.std,
             k  = gamma.int + gamma.tmp*tmp.std + gamma.ppt*ppt.std,
             agb.fut = mu*sag.fut/(k + sag.fut))
    
    agb.fut.dat <- pred.dat$agb.fut %>% 
      cbind(agb.fut.dat)
    colnames(agb.fut.dat)[1] <- paste0(fyr.tag, '_', rcp.tag) # naming of different scenarios
    
  }
}

fut.dat <- pred.dat %>% 
  select(ft, plt, lon, lat) %>% 
  cbind(agb.fut.dat) %>% 
  as_tibble() %>% 
  gather('scn', 'agb.fut', -ft, -plt, -lon, -lat) %>% 
  full_join(wis.dat %>% 
              select(ft, plt, lon, lat, agb.obs = agb, agb.cur = agb.mod),
            by = c('ft', 'plt', 'lon', 'lat')) %>% 
  mutate(agb.rat = agb.cur/agb.fut) %>% # modeled current/modeled future
  filter(agb.rat > 0, agb.rat < 10) %>% 
  write_rds(paste0(mod.dir, 'fut_dat.rds'))
