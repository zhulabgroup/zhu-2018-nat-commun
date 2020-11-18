# 2nd part of spatial validation, after JAGS fit (slow)
library(tidyverse)
theme_set(theme_bw())
library(R2jags)
library(ggmcmc)
library(sp)
library(SpatialPack)
library(tmap)

dat.dir <- 'Output/Growth/Publication/Data/'
mod.dir <- 'Output/Growth/Publication/Models/'
fig.dir <- 'Output/Growth/Publication/Figures/'

train.dat <- read_rds(paste0(mod.dir, 'train_dat.rds'))
test.dat  <- read_rds(paste0(mod.dir, 'test_dat.rds'))

ft.dat <- read_rds(paste0(mod.dir, 'train_ft_dat.rds'))
monod.jags <- read_rds(paste0(mod.dir, 'spval_jags_2e3.rds'))

# Show maps of training and testing plots ---------------------------------

train.shp <- SpatialPoints(coords = train.dat[, c('lon', 'lat')], proj4string = CRS("+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0"))
test.shp  <- SpatialPoints(coords = test.dat[, c('lon', 'lat')], proj4string = CRS("+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0"))

bkgd.poly <- read_rds(paste0(dat.dir, 'NA_shp.rds'))

train.tm <- tm_shape(train.shp, projection = CRS('+proj=aea +lat_1=20 +lat_2=60 +lat_0=40 +lon_0=-96 +x_0=0 +y_0=0 +datum=NAD83 +units=m +no_defs +ellps=GRS80 +towgs84=0,0,0')) +
  tm_dots(alpha = .5) +
  tm_shape(bkgd.poly) + tm_borders('gray80') +
  tm_layout(asp = 1, # square shape
            title = paste('Training data:', nrow(train.dat), 'plots (75%)'))
print(train.tm)
save_tmap(train.tm, file = paste0(fig.dir, 'Spatial validation - training plots.pdf'), w = 10, h = 10)

test.tm <- tm_shape(test.shp, projection = CRS('+proj=aea +lat_1=20 +lat_2=60 +lat_0=40 +lon_0=-96 +x_0=0 +y_0=0 +datum=NAD83 +units=m +no_defs +ellps=GRS80 +towgs84=0,0,0')) +
  tm_dots(alpha = .5) +
  tm_shape(bkgd.poly) + tm_borders('gray80') +
  tm_layout(asp = 1, # square shape
            title = paste('Testing data:', nrow(test.dat), 'plots (25%)'))
print(test.tm)
save_tmap(test.tm, file = paste0(fig.dir, 'Spatial validation - testing plots.pdf'), w = 10, h = 10)

# Get coefficients --------------------------------------------------------

coef.ci <- monod.jags %>% 
  coda::as.mcmc() %>% 
  ggs() %>% 
  ci() %>%
  filter(grepl('beta|gamma', Parameter)) %>%
  separate(Parameter, c('para', 'coef.id', 'ft.id'), sep = '[:punct:]', extra = 'drop') %>%
  mutate(coef.id = as.numeric(coef.id), ft.id = as.numeric(ft.id)) %>%
  full_join(data_frame(coef.id = 1:3, coef = c('int', 'tmp', 'ppt'))) %>%
  full_join(ft.dat %>% select(ft.id = id, ft)) %>%
  select(para, coef, ft, low, Low, median, High, high) %>% # low and high are 95% CI, Low and High are 90% CI
  arrange(para, coef, ft)

coef.med <- coef.ci %>%
  unite(para_coef, para, coef, sep = '.') %>%
  select(para_coef, ft, median) %>%
  spread(para_coef, median)

# Hold-out prediction -----------------------------------------------------
# plot level
test.plt.dat <- test.dat %>%
  mutate(ft = as.character(ft)) %>% 
  full_join(coef.med, by = 'ft') %>%
  mutate(mu = beta.int + beta.tmp*tmp.std + beta.ppt*ppt.std,
         k  = gamma.int + gamma.tmp*tmp.std + gamma.ppt*ppt.std,
         agb.mod = mu*sag/(k + sag))

xy.max <- max(test.plt.dat[, c('agb', 'agb.mod')])

ggplot(test.plt.dat, aes(agb, agb.mod)) +
  geom_point(alpha = .1) +
  # geom_smooth(method = 'lm') +
  geom_abline(col = 'red') +
  scale_x_sqrt() +
  scale_y_sqrt() + 
  coord_equal(xlim = c(0, xy.max), ylim = c(0, xy.max)) +
  labs(x = expression('Observed aboveground biomass (Mg'~ha^-1*')'),
       y = expression('Predicted aboveground biomass (Mg'~ha^-1*')'))
ggsave(paste0(fig.dir, 'Spatial validation - obs vs pred.pdf'), w = 7, h = 7)

cor(test.plt.dat$agb, test.plt.dat$agb.mod)
modified.ttest(test.plt.dat$agb, test.plt.dat$agb.mod, 
               coords = test.plt.dat %>% 
                 select(lon, lat) %>% 
                 as.matrix())
