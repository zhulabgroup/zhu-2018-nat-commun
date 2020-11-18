library(tidyverse)
library(sp)
library(tmap)
theme_set(theme_bw())

# old vs new data to show disturbances are random -------------------------

old.dat <- read_rds("Models/oos_dat.rds")
new.dat <- read_rds("Models/wis_dat.rds")

# combine data by coords
com.dat <- inner_join(old.dat, new.dat, by = c("ft", "lon", "lat")) %>%
  mutate(agb.diff = agb.y - agb.x)

# show disturbance is spatially random

com.shp <- SpatialPointsDataFrame(
  coords = com.dat[, c("lon", "lat")],
  data = data.frame(sign = ifelse(com.dat$agb.diff > 0, "Increase", "Decline")),
  proj4string = CRS("+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0")
)

bkgd.poly <- read_rds("Data/NA_shp.rds")

com.tm <- tm_shape(com.shp, projection = CRS("+proj=aea +lat_1=20 +lat_2=60 +lat_0=40 +lon_0=-96 +x_0=0 +y_0=0 +datum=NAD83 +units=m +no_defs +ellps=GRS80 +towgs84=0,0,0")) +
  tm_dots(
    col = "sign",
    alpha = .3,
    palette = c("red", "green"),
    title = "Biomass change"
  ) +
  tm_shape(bkgd.poly) + tm_borders("gray80") +
  tm_layout(asp = 1)
print(com.tm)
tmap_save(com.tm, file = "Figures/Map of biomass change.pdf", w = 10, h = 5)


# Test spatial randomness -------------------------------------------------

com.dat %>%
  mutate(sag.mean = (sag.x + sag.y) / 2) %>%
  select(lon, lat, agb.diff, sag.mean) %>%
  gather(lon, lat, sag.mean, key = "var", value = "val") %>%
  ggplot(aes(val, agb.diff)) +
  geom_point(alpha = .2) +
  # geom_smooth() +
  facet_wrap(~var,
    scales = "free_x",
    labeller = as_labeller(c(
      "lat" = "Latitude (deg)",
      "lon" = "Longitude (deg)",
      "sag.mean" = "Mean stand age (yr)"
    ))
  ) +
  labs(x = "", y = "Biomass change (Mg/ha)")
ggsave("Figures/Biomass change.pdf", w = 15, h = 5)

# com.dat %>%
#   mutate(sag.mean = (sag.x + sag.y)/2) %>%
#   select(lon, lat, agb.diff, sag.mean) %>%
#   mutate(sag.cls = cut(sag.mean, c(seq(0, 100, by = 10), 1000))) %>%
#   ggplot(aes(sag.cls, agb.diff)) +
#   geom_boxplot()
