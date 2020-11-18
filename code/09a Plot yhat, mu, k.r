# plot mu and k, also to address Nature Comms Reviewer 1 comments, 2nd round
library(tidyverse)
library(sp)
library(SpatialPack)
library(tmap)
theme_set(theme_bw())

all.dat <- read_rds("Data/all_dat.rds")
env.stat <- read_rds("Data/env_stat.rds")

coef.med <- read_rds("Models/old_jags_2e4_sum.rds") %>%
  unite(para_coef, para, coef, sep = ".") %>%
  select(para_coef, ft, median) %>%
  spread(para_coef, median)


# Calculate plot mu, k -------------------------------------------------------

calc_plt_para <- function(clim.dat) {
  pred.dat <- all.dat %>%
    select(src:agb) %>%
    mutate(sag.fut = sag + as.numeric(fyr.tag) - yr) %>% # extrapolate stand age
    bind_cols(clim.dat) %>% # get climate projections
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
      yhat = mu * sag.fut / (k + sag.fut),
      r = yhat / mu * 100
    )

  out <- pred.dat %>%
    select(plt, lon, lat, yhat, mu, k, r)
  # colnames(out)[4] <- paste0(fyr.tag, '_', rcp.tag)
  out
}

# get all paras
plt.para.dat <- NULL
for (rcp.tag in c("rcp45", "rcp85")) {
  for (fyr.tag in c("2025", "2055", "2085")) {
    clim.file <- paste0("Data/CMIP_", rcp.tag, "_", fyr.tag, ".csv")
    clim.dat <- read_csv(clim.file) %>% select(tmp = MAT, ppt = MAP)

    plt.para <- calc_plt_para(clim.dat)
    colnames(plt.para)[4] <- paste0("yhat_", rcp.tag, "_", fyr.tag)
    colnames(plt.para)[5] <- paste0("mu_", rcp.tag, "_", fyr.tag)
    colnames(plt.para)[6] <- paste0("k_", rcp.tag, "_", fyr.tag)
    colnames(plt.para)[7] <- paste0("r_", rcp.tag, "_", fyr.tag)

    if (is.null(plt.para.dat)) {
      plt.para.dat <- plt.para
    } else {
      plt.para.dat <- inner_join(plt.para.dat, plt.para, by = c("plt", "lon", "lat"))
    }
  }
}

# Rasterize maps ----------------------------------------------------------


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

yhat.ras <- plt.para.dat %>%
  select(x = lon, y = lat, contains("yhat_")) %>%
  # select(-plt) %>% # current obs and modeled
  rasterize_north_america(res = 10) %>% # res = 2.5, 5, 10. small = slow
  raster::aggregate(fact = 2) %>%
  raster::rasterToPolygons()

mu.ras <- plt.para.dat %>%
  select(x = lon, y = lat, contains("mu_")) %>%
  # select(-plt) %>% # current obs and modeled
  rasterize_north_america(res = 10) %>% # res = 2.5, 5, 10. small = slow
  raster::aggregate(fact = 2) %>%
  raster::rasterToPolygons()

k.ras <- plt.para.dat %>%
  select(x = lon, y = lat, contains("k_")) %>%
  # select(-plt) %>% # current obs and modeled
  rasterize_north_america(res = 10) %>% # res = 2.5, 5, 10. small = slow
  raster::aggregate(fact = 2) %>%
  raster::rasterToPolygons()

r.ras <- plt.para.dat %>%
  select(x = lon, y = lat, contains("r_")) %>%
  # select(-plt) %>% # current obs and modeled
  rasterize_north_america(res = 10) %>% # res = 2.5, 5, 10. small = slow
  raster::aggregate(fact = 2) %>%
  raster::rasterToPolygons()

# modified.ttest(mu.ras$`X2085_rcp85`, mu.ras$`X2055_rcp85`, coords = coordinates(mu.ras))
# modified.ttest(mu.ras$`X2085_rcp45`, mu.ras$`X2055_rcp45`, coords = coordinates(mu.ras))

# Make maps ---------------------------------------------------------------


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
yhat.col.brk <- c(-Inf, seq(50, 550, by = 50), Inf)
mu.col.brk <- c(-Inf, seq(50, 550, by = 50), Inf)
delta.mu.col.brk <- c(-Inf, seq(-30, 30, by = 10), Inf)
k.col.brk <- c(-Inf, seq(50, 400, by = 50), Inf)
r.col.brk <- c(-Inf, seq(.1, .9, by = .1), Inf) * 100

# map 1, future y hat
yhat.tm <- tm_shape(yhat.ras) +
  tm_fill(c("yhat_rcp45_2025", "yhat_rcp45_2055", "yhat_rcp45_2085", "yhat_rcp85_2025", "yhat_rcp85_2055", "yhat_rcp85_2085"),
    title = expression("(Mg" ~ ha^-1 * ")"),
    breaks = yhat.col.brk,
    palette = col.vec
  ) +
  tm_shape(bkgd.poly) + tm_borders("gray80") + # light gray; 'gray20' is dark gray
  tm_layout(
    asp = 1, # square shape
    title = c(
      "RCP4.5, 2020s", "RCP4.5, 2050s", "RCP4.5, 2080s",
      "RCP8.5, 2020s", "RCP8.5, 2050s", "RCP8.5, 2080s"
    ),
    legend.title.size = 1,
    legend.text.size = 0.5,
    legend.position = c("left", "bottom")
  )
print(yhat.tm)
tmap_save(yhat.tm, file = "Figures/Map of yhat.pdf", w = 15, h = 10)


# map 2, current obs and mod AGB
mu.tm <- tm_shape(mu.ras) +
  tm_fill(c("mu_rcp45_2025", "mu_rcp45_2055", "mu_rcp45_2085", "mu_rcp85_2025", "mu_rcp85_2055", "mu_rcp85_2085"),
    title = expression("(Mg" ~ ha^-1 * ")"),
    breaks = mu.col.brk,
    palette = col.vec
  ) +
  tm_shape(bkgd.poly) + tm_borders("gray80") + # light gray; 'gray20' is dark gray
  tm_layout(
    asp = 1, # square shape
    title = c(
      "RCP4.5, 2020s", "RCP4.5, 2050s", "RCP4.5, 2080s",
      "RCP8.5, 2020s", "RCP8.5, 2050s", "RCP8.5, 2080s"
    ),
    legend.title.size = 1,
    legend.text.size = 0.5,
    legend.position = c("left", "bottom")
  )
print(mu.tm)
tmap_save(mu.tm, file = "Figures/Map of mu.pdf", w = 15, h = 10)

# # delte mu map
# dmu.tm <- tm_shape(mu.ras) +
#   tm_fill(c(
#     "mu_rcp45_2055", "mu_rcp45_2085", "delta_mu_rcp45_2085_2055",
#     "mu_rcp85_2055", "mu_rcp85_2085", "delta_mu_rcp85_2085_2055"
#   ),
#   title = expression("(Mg" ~ ha^-1 * ")"),
#   breaks = list(
#     mu.col.brk, mu.col.brk, delta.mu.col.brk,
#     mu.col.brk, mu.col.brk, delta.mu.col.brk
#   ),
#   palette = list(
#     col.vec, col.vec, fields::tim.colors(),
#     col.vec, col.vec, fields::tim.colors()
#   )
#   ) +
#   tm_shape(bkgd.poly) + tm_borders("gray80") + # light gray; 'gray20' is dark gray
#   tm_layout(
#     asp = 1, # square shape
#     title = c(
#       "RCP4.5, 2050s", "RCP4.5, 2080s", "RCP4.5, 2080s - 2050s",
#       "RCP8.5, 2050s", "RCP8.5, 2080s", "RCP8.5, 2080s - 2050s"
#     ),
#     legend.title.size = 1,
#     legend.text.size = 0.5,
#     legend.position = c("left", "bottom")
#   )
# print(dmu.tm)
# tmap_save(dmu.tm, file = "Figures/Map of delta mu.pdf", w = 15, h = 10)



# map 3, current obs and mod AGB
k.tm <- tm_shape(k.ras) +
  tm_fill(c("k_rcp45_2025", "k_rcp45_2055", "k_rcp45_2085", "k_rcp85_2025", "k_rcp85_2055", "k_rcp85_2085"),
    title = "(yr)",
    breaks = k.col.brk,
    palette = col.vec
  ) +
  tm_shape(bkgd.poly) + tm_borders("gray80") + # light gray; 'gray20' is dark gray
  tm_layout(
    asp = 1, # square shape
    title = c(
      "RCP4.5, 2020s", "RCP4.5, 2050s", "RCP4.5, 2080s",
      "RCP8.5, 2020s", "RCP8.5, 2050s", "RCP8.5, 2080s"
    ),
    legend.title.size = 1,
    legend.text.size = 0.5,
    legend.position = c("left", "bottom")
  )
print(k.tm)
tmap_save(k.tm, file = "Figures/Map of k.pdf", w = 15, h = 10)


# map 4, saturation ratio
r.tm <- tm_shape(r.ras) +
  tm_fill(c("r_rcp45_2025", "r_rcp45_2055", "r_rcp45_2085", "r_rcp85_2025", "r_rcp85_2055", "r_rcp85_2085"),
    title = "(%)",
    breaks = r.col.brk,
    palette = col.vec
  ) +
  tm_shape(bkgd.poly) + tm_borders("gray80") + # light gray; 'gray20' is dark gray
  tm_layout(
    asp = 1, # square shape
    title = c(
      "RCP4.5, 2020s", "RCP4.5, 2050s", "RCP4.5, 2080s",
      "RCP8.5, 2020s", "RCP8.5, 2050s", "RCP8.5, 2080s"
    ),
    legend.title.size = 1,
    legend.text.size = 0.5,
    legend.position = c("left", "bottom")
  )
print(r.tm)
tmap_save(r.tm, file = "Figures/Map of r.pdf", w = 15, h = 10)


# compare mu --------------------------------------------------------------

# RMSD = sqrt(sum(y1 - y2)^2/n)

mu.ras.dat <- as.data.frame(mu.ras)
n.pix <- nrow(mu.ras)
with(mu.ras.dat, sum((mu_rcp85_2055 - mu_rcp85_2085)^2) / n.pix %>% sqrt())
with(mu.ras.dat, sum((mu_rcp45_2085 - mu_rcp85_2085)^2) / n.pix %>% sqrt())
