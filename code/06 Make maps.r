library(tidyverse)
library(tmap)

dat.dir <- 'Output/Growth/Publication/Data/'
mod.dir <- 'Output/Growth/Publication/Models/'
fig.dir <- 'Output/Growth/Publication/Figures/'

cur.dat <- 'wis_dat.rds' %>% 
  paste0(mod.dir, .) %>% 
  read_rds()

fut.dat <- 'fut_dat.rds' %>%
  paste0(mod.dir, .) %>% 
  read_rds()

pst.dat <- 'oos_dat.rds' %>% # past obs and modeled
  paste0(mod.dir, .) %>% 
  read_rds()

# Rasterize data ----------------------------------------------------------

rasterize_north_america <- function(xyz.dat, fun = mean, res = 10, dl.path = '~/Downloads') {
  # function to rasterize at 10 min by 10 min resolution, using WorldClim as reference
  
  ref.ras <- raster::getData('worldclim', var = 'bio', res = res, download = T, path = dl.path)
  
  xyz.dat <- as.data.frame(xyz.dat)
  xyz.ras <- raster::rasterize(xyz.dat[, 1:2], ref.ras, xyz.dat[, -(1:2)], fun = fun) # fun = mean
  raster::projection(xyz.ras) <- raster::crs(ref.ras)
  
  xyz.ras.prj <- xyz.ras %>%
    raster::crop(raster::extent(c(-130, -60, 25, 60))) %>%
    raster::projectRaster(crs = '+proj=aea +lat_1=20 +lat_2=60 +lat_0=40 +lon_0=-96 +x_0=0 +y_0=0 +datum=NAD83 +units=m +no_defs +ellps=GRS80 +towgs84=0,0,0')
  
  xyz.ras.prj
  
}

cur.ras <- cur.dat %>%
  select(x = lon, y = lat, agb_cur_obs = agb, agb_cur_mod = agb.mod) %>% # current obs and modeled
  rasterize_north_america(res = 10) %>% # res = 2.5, 5, 10. small = slow
  raster::aggregate(fact = 2) %>%
  raster::rasterToPolygons() %>% 
  write_rds(paste0(mod.dir, 'cur_ras.rds'))

fut.ras <- fut.dat %>% 
  select(ft:scn, agb.fut) %>% 
  mutate(scn = paste0('agb_', scn)) %>% 
  spread(key = scn, value = agb.fut) %>% 
  full_join(fut.dat %>% 
              select(ft:scn, agb.rat) %>% 
              mutate(scn = paste0('rat_', scn)) %>% 
              spread(key = scn, value = agb.rat),
            by = c('ft', 'plt', 'lon', 'lat')) %>% 
  select(x = lon, y = lat, contains('rcp')) %>%
  na.omit() %>% 
  rasterize_north_america(res = 10) %>% # res = 2.5, 5, 10. small = slow
  raster::aggregate(fact = 2) %>%
  raster::rasterToPolygons() %>% 
  write_rds(paste0(mod.dir, 'fut_ras.rds'))

pst.ras <- pst.dat %>% 
  select(x = lon, y = lat, agb_pst_obs = agb, agb_pst_mod = agb.mod) %>%
  rasterize_north_america(res = 10) %>% # res = 2.5, 5, 10. small = slow
  raster::aggregate(fact = 2) %>%
  raster::rasterToPolygons() %>% 
  write_rds(paste0(mod.dir, 'pst_ras.rds'))

# Plot maps ---------------------------------------------------------------

col.vec <- c("#00008F", "#0000EA", "#0015FF", "#003DFF", "#0051FF", "#0070FF", 
             "#0084FF", "#0098FF", "#00ACFF", "#00B6FF", "#00CBFF", "#00DFFF", 
             "#00E9FF", "#00F4FF", "#08FFF7", "#13FFEC", "#1DFFE2", "#31FFCE", 
             "#3CFFC3", "#46FFB9", "#50FFAF", "#5AFFA5", "#64FF9B", "#6FFF90", 
             "#79FF87", "#8CFF73", "#96FF69", "#96FF68", "#A0FF5F", "#ABFF54", 
             "#B5FF4A", "#BFFF40", "#C9FF36", "#D3FF2C", "#DEFF21", "#E7FF18", 
             "#F2FF0D", "#F2FF0D", "#FDFF02", "#FFF900", "#FFED00", "#FFE300", 
             "#FFE300", "#FFD900", "#FFCF00", "#FFC500", "#FFBB00", "#FFBB00", 
             "#FFB000", "#FFA600", "#FFA600", "#FF9C00", "#FF9200", "#FF8800", 
             "#FF8800", "#FF7F00", "#FF7400", "#FF7400", "#FF6A00", "#FF6000", 
             "#FF6000", "#FF5600", "#FF4C00", "#FF4C00", "#FF4100", "#FF4100", 
             "#FF3700", "#FF2D00", "#FF2D00", "#FF2300", "#FF1900", "#FF1900", 
             "#FF0E00", "#FF0E00", "#FF0300", "#FF0300", "#FB0000", "#EF0000", 
             "#EF0000", "#E50000", "#E50000", "#DB0000", "#DB0000", "#D00000", 
             "#C60000", "#C60000", "#BC0000", "#BC0000", "#B20000", "#B20000", 
             "#A80000", "#A80000", "#9E0000", "#9E0000", "#930000", "#930000", 
             "#890000", "#890000", "#800000", "#800000")

# north america maps
bkgd.poly <- read_rds(paste0(dat.dir, 'NA_shp.rds'))
col.brk.bio <- c(-Inf, seq(50, 550, by = 50), Inf)
col.brk.rat <- c(-Inf, seq(.1, .9, by = .1), Inf)*100
col.brk.bio.past <- c(-Inf, seq(50, 400, by = 50), Inf)

# map 1, current obs and mod AGB
cur.tm <- tm_shape(cur.ras) +
  tm_fill(c('agb_cur_obs', 'agb_cur_mod'),
          title = expression('(Mg'~ha^-1*')'),
          breaks = col.brk.bio,
          palette = col.vec) +
  tm_shape(bkgd.poly) + tm_borders('gray80') + # light gray; 'gray20' is dark gray
  tm_layout(asp = 1, # square shape
            title = c('Observed aboveground biomass (2000 - 2016)',
                      'Modeled aboveground biomass (2000 - 2016)'),
            legend.title.size = 1,
            legend.text.size = 0.5,
            legend.position = c("left","bottom"))
print(cur.tm)
save_tmap(cur.tm, file = paste0(fig.dir, 'Map of current AGB.pdf'), w = 10, h = 5)

# map 2, future AGB and ratio
fut.ras$agb_fut_mod <- fut.ras$agb_2085_rcp85
fut.ras$rat_percent <- fut.ras$rat_2085_rcp85*100 # from ratio to percent
fut.tm <- tm_shape(fut.ras) +
  tm_fill(c('agb_fut_mod', 'rat_percent'),
          title = c(expression('(Mg'~ha^-1*')'), '(%)'),
          breaks = list(col.brk.bio, col.brk.rat),
          palette = col.vec) +
  tm_shape(bkgd.poly) + tm_borders('gray80') + # light gray; 'gray20' is dark gray
  tm_layout(asp = 1, # square shape
            title = c('Predicted aboveground biomass (2080s, RCP8.5)',
                      'Current (2000 - 2016) vs. future (2080s, RCP8.5) ratio'),
            legend.title.size = 1,
            legend.text.size = 0.5,
            legend.position = c("left","bottom"))
print(fut.tm)
save_tmap(fut.tm, file = paste0(fig.dir, 'Map of future AGB.pdf'), w = 10, h = 5)

# map 3, observed and modeled prior-1999
pst.tm <- tm_shape(pst.ras) +
  tm_fill(c('agb_pst_obs', 'agb_pst_mod'),
          title = expression('(Mg'~ha^-1*')'),
          breaks = col.brk.bio.past,
          palette = col.vec) +
  tm_shape(bkgd.poly) + tm_borders('gray80') + # light gray; 'gray20' is dark gray
  tm_layout(asp = 1, # square shape
            title = c('Observed aboveground biomass (1990 - 1999)',
                      'Modeled aboveground biomass (1990 - 1999)'),
            legend.title.size = 1,
            legend.text.size = 0.5,
            legend.position = c("left","bottom"))
print(pst.tm)
save_tmap(pst.tm, file = paste0(fig.dir, 'Map of past AGB.pdf'), w = 10, h = 5)

# # Interactive maps? -------------------------------------------------------
# # only lat lon projection
# tmap_mode("view")
# fut.tm
