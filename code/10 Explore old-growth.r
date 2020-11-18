# Response to Rich Birdsey's comment
library(tidyverse)
theme_set(theme_bw())
library(tmap)
library(sp)

dat.dir <- 'Output/Growth/Publication/Data/'
mod.dir <- 'Output/Growth/Publication/Models/'
fig.dir <- 'Output/Growth/Publication/Figures/'


# data vs model summary ---------------------------------------------------

wis.dat <- read_rds(paste0(mod.dir, 'wis_dat.rds'))

# # show data age distribution
# wis.dat %>% 
#   filter(ft == 'Douglas-fir') %>% 
#   ggplot(aes(sag, y=(..count..)/tapply(..count..,..PANEL..,sum)[..PANEL..])) +
#   # geom_histogram(data = filter(all.dat, cen == 'old'), bins = 30, alpha = .2) +
#   geom_histogram(bins = 30, alpha = .2, fill = 'red') +
#   facet_wrap(~ ft, scale = 'free') +
#   labs(x = 'Stand age (yr)', y = 'Plot percentage') +
#   scale_y_continuous(labels = scales::percent)

# 2 groups
og.wis.dat <- wis.dat %>% 
  filter(ft %in% c('Douglas-fir', 'Pinyon / juniper')) %>% 
  mutate(sag.cls = cut(sag, c(seq(0, 100, by = 10), seq(200, 800, by = 100)), include.lowest = T, dig.lab = 10))

ggplot(og.wis.dat) +
  geom_boxplot(aes(x = sag.cls, y = agb), col = gray(.8)) +
  geom_line(aes(x = as.numeric(sag.cls), y = agb.mod), stat = "summary", fun.y = mean, col = 'red') +
  geom_ribbon(aes(x = as.numeric(sag.cls), y = agb.mod), stat = 'summary', fun.ymin = min, fun.ymax = max, fill = 'red', alpha = .2) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = .5)) +
  labs(x = 'Stand age (yr)', y = expression('Aboveground biomass (Mg'~ha^-1*')')) +
  facet_wrap(~ft, scales = 'free') +
  scale_y_sqrt()
ggsave(paste0(fig.dir, 'Old-growth data vs model sqrt.pdf'),  w = 10, h = 5)



# map summary -------------------------------------------------------------

all.dat <- read_rds(paste0(dat.dir, 'all_dat.rds'))

# map
all.shp <- SpatialPointsDataFrame(coords = all.dat[, c('lon', 'lat')], 
                                  data = all.dat %>% 
                                    select(ft, cen, sag) %>% 
                                    mutate(ft = ifelse(ft == 'Douglas-fir', 
                                                       'Douglas-fir', 
                                                       ifelse(ft == 'Pinyon / juniper',
                                                              'Pinyon / juniper',
                                                              'z'))) %>% 
                                    as.data.frame(),
                                  proj4string = CRS("+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0"))

bkgd.poly <- read_rds(paste0(dat.dir, 'NA_shp.rds'))

og.tm <- tm_shape(all.shp, projection = CRS('+proj=aea +lat_1=20 +lat_2=60 +lat_0=40 +lon_0=-96 +x_0=0 +y_0=0 +datum=NAD83 +units=m +no_defs +ellps=GRS80 +towgs84=0,0,0')) +
  tm_dots(size = .05,
          # col = 'ft',
          # palette = c('black', '#00000000'), # fully transparnet color
          # col = 'sag',
          # palette = col.vec, breaks = c(-Inf, seq(100, 800, by = 100), Inf), legend.hist = T,
          alpha = .5) +
  tm_facets('ft', free.coords = F) +
  tm_shape(bkgd.poly) + tm_borders('gray80') +
  tm_layout(asp = 1) +
  tm_legend(show = F)
print(og.tm)
tmap_save(og.tm, file = paste0(fig.dir, 'Old-growth plot map.pdf'), w = 10, h = 10)


# count plots and plot areas ----------------------------------------------

# # entire FIA plot table, slow
# read.csv('Input/Growth/Forest/FIA 2018/PLOT.csv') %>% 
#   as_tibble() %>% 
#   write_rds(paste0(dat.dir, 'fia_plot_table.rds'))

# read.csv('Input/Growth/Forest/FIA 2018/COND.csv') %>% 
#   as_tibble() %>% 
#   write_rds(paste0(dat.dir, 'fia_cond_table.rds'))

# On Aug 20, 2018, at 10:25 AM, Walters, Brian F -FS <bfwalters@fs.fed.us> wrote:
# Hi Kai,
# 
# Sorry my reply is so late, I was out of the office. The code you are looking for is PROP_BASIS in the condition table. 
# Most plots will have the code ‘SUBP’ because the plot design is the normal 24-foot radius subplots. For the plots in 
# the west that use the 58.9 foot radius macroplots to capture larger trees, the PROP_BASIS code will be ‘MACR’.
# 
# Brian

og.fia.dat <- read_rds(paste0(dat.dir, 'fia_cond_table.rds')) %>% 
  select(plt = PLT_CN, PROP_BASIS) %>% 
  mutate(plt = paste0('US@', as.character(plt))) %>% 
  right_join(all.dat %>% 
               filter(ft %in% c('Douglas-fir', 'Pinyon / juniper'),
                      src == 'fia'),
             by = c('plt')) %>% 
  mutate(area.m2 = ifelse(PROP_BASIS == 'SUBP', # unit = m2
                          4*pi*(7.3152^2), # 1 subplot = 24 ft radius = 7.3152 m radius, 4 subplots in total
                          4*pi*(17.95272^2)), # 1 macroplot = 58.9 ft radius = 17.95272 m radius, 4 macroplots in total
         area.ha = area.m2*0.0001) %>% # unit = ha
  select(src, cen, plt, ft, area = area.ha)

og.psp.dat <- read_csv('Input/Growth/Forest/Zhang/biomass.pspAfter99.csv') %>% 
  mutate(cen = 'new') %>% 
  select(plt = PlotProv, cen, area = plotsize) %>% 
  bind_rows(read_csv('Input/Growth/Forest/Zhang/biomass.psp9099.csv') %>% 
              mutate(cen = 'old') %>% 
              select(plt = PlotProv, cen, area = plotsize)) %>% 
  right_join(all.dat %>% 
               filter(ft %in% c('Douglas-fir', 'Pinyon / juniper'),
                      src == 'psp'),
             by = c('plt', 'cen')) %>% 
  select(src, cen, plt, ft, area)

# count no of plots and get total plot area
bind_rows(og.fia.dat, og.psp.dat) %>% 
  group_by(ft) %>% 
  summarize(n = n(), tot.area = sum(area))

# how many >100 yr?
all.dat %>% 
  filter(ft %in% c('Douglas-fir', 'Pinyon / juniper')) %>% 
  group_by(ft) %>% 
  summarize(mean(sag > 100))

# # Combine panels ----------------------------------------------------------
# # library(cowplot)
# pal.a <- dougfir.gg
# pal.b <- ggdraw() + draw_image(magick::image_read_pdf(paste0(fig.dir, 'Douglas-fir plot map.pdf')))
# plot_grid(pal.a, pal.b, nrow = 1,
#           labels = c('a', 'b'), vjust = 2.5)
# ggsave(paste0(fig.dir, 'Douglas-fir combined fig.pdf'), h = 5, w = 10)
