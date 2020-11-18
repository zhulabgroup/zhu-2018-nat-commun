# Nature Comms Reviewer 1 comments, 2nd round
# (mu2081 - mu2079)/2
# I don't think it's meaningful
library(tidyverse)
library(sp)
library(tmap)
theme_set(theme_bw())

all.dat <- read_rds("Data/all_dat.rds")
env.stat <- read_rds("Data/env_stat.rds")

coef.med <- read_rds("Models/old_jags_2e4_sum.rds") %>%
  unite(para_coef, para, coef, sep = ".") %>%
  select(para_coef, ft, median) %>%
  spread(para_coef, median)

# Calculate mu for 2079, 2081 ---------------------------------------------

calc_fut_mu <- function(clim.dat, fut.yr) {
  pred.dat <- all.dat %>%
    select(src:agb) %>%
    mutate(sag.fut = sag + as.numeric(fut.yr) - yr) %>% # extrapolate stand age
    bind_cols(select(clim.dat, tmp, ppt)) %>% # get climate projections
    filter(
      cen == "new",
      tmp != -9999,
      ppt != -9999
    ) %>% # remove missing values
    mutate(cen = "fut") %>%
    full_join(env.stat, by = "ft") %>%
    mutate(tmp.std = (tmp - tmp.mean) / tmp.sd, ppt.std = (ppt - ppt.mean) / ppt.sd) %>%
    full_join(coef.med, by = "ft") %>%
    mutate(
      mu = beta.int + beta.tmp * tmp.std + beta.ppt * ppt.std,
      k = gamma.int + gamma.tmp * tmp.std + gamma.ppt * ppt.std,
      agb.fut = mu * sag.fut / (k + sag.fut)
    )

  out <- pred.dat %>%
    select(plt, lon, lat, mu)
  out
}


# compare climate ---------------------------------------------------------

rcp45.dat <- read_csv("Data/Plots_CanESM2_rcp45_r1i1p1_2079-2081YT.csv") %>%
  select(fut.yr = Year, plt = ID1, lat = Latitude, lon = Longitude, tmp = MAT, ppt = MAP)

rcp85.dat <- read_csv("Data/Plots_CanESM2_rcp85_r1i1p1_2079-2081YT.csv") %>%
  select(fut.yr = Year, plt = ID1, lat = Latitude, lon = Longitude, tmp = MAT, ppt = MAP)

# # compare temp data
# clim.comp <- rcp45.dat %>%
#   filter(fut.yr == 2079) %>%
#   inner_join(rcp45.dat %>% filter(fut.yr == 2081),
#     by = c("plt", "lon", "lat")
#   ) %>%
#   filter(tmp.x != -9999, tmp.y != -9999, ppt.x != -9999, ppt.y != -9999)


# calculate plot-level mu -------------------------------------------------

mu.2079 <- calc_fut_mu(filter(rcp85.dat, fut.yr == 2079), 2079)
mu.2081 <- calc_fut_mu(filter(rcp85.dat, fut.yr == 2081), 2081)

mu.comp <- inner_join(mu.2079, mu.2081, by = c("plt", "lon", "lat"), suffix = c(".2079", ".2081"))
plot(mu.comp[, c("mu.2079", "mu.2081")])


# raster and maps ---------------------------------------------------------

rasterize_north_america <- function(xyz.dat, fun = mean, res = 10, dl.path = tempdir()) {
  # function to rasterize at 10 min by 10 min resolution, using WorldClim as reference

  ref.ras <- raster::getData("worldclim", var = "bio", res = res, download = T, path = dl.path)

  xyz.dat <- as.data.frame(xyz.dat)
  xyz.ras <- raster::rasterize(xyz.dat[, 1:2], ref.ras, xyz.dat[, -(1:2)], fun = fun) # fun = mean
  raster::projection(xyz.ras) <- raster::crs(ref.ras)

  xyz.ras.prj <- xyz.ras %>%
    raster::crop(raster::extent(c(-130, -60, 25, 60))) %>%
    raster::projectRaster(crs = "+proj=aea +lat_1=20 +lat_2=60 +lat_0=40 +lon_0=-96 +x_0=0 +y_0=0 +datum=NAD83 +units=m +no_defs +ellps=GRS80 +towgs84=0,0,0")

  xyz.ras.prj
}

mu.ras <- mu.comp %>%
  select(x = lon, y = lat, contains("mu.")) %>%
  # select(-plt) %>% # current obs and modeled
  rasterize_north_america(res = 10) %>% # res = 2.5, 5, 10. small = slow
  raster::aggregate(fact = 2) %>%
  raster::rasterToPolygons()

col.vec <- c(
  "#00008F", "#0000EA", "#0015FF", "#003DFF", "#0051FF", "#0070FF",
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
  "#890000", "#890000", "#800000", "#800000"
)

# north america maps
bkgd.poly <- read_rds("Data/NA_shp.rds")
mu.col.brk <- c(-Inf, seq(50, 800, by = 50), Inf)

# map 1, future y hat
mu.tm <- tm_shape(mu.ras) +
  tm_fill(c("mu.2079", "mu.2081"),
    title = expression("(Mg" ~ ha^-1 * ")"),
    breaks = mu.col.brk,
    palette = col.vec
  ) +
  tm_shape(bkgd.poly) + tm_borders("gray80") + # light gray; 'gray20' is dark gray
  tm_layout(
    asp = 1, # square shape
    title = c("RCP8.5, 2079", "RCP8.5, 2081"),
    legend.title.size = 1,
    legend.text.size = 0.5,
    legend.position = c("left", "bottom")
  )
print(mu.tm)
# tmap_save(yhat.tm, file = "Figures/Map of yhat.pdf", w = 15, h = 10)
