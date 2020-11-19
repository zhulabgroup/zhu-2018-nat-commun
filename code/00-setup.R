# install packages
list.of.packages <- c(
  "tidyverse", "scales", "plotrix", "modelr",
  "R2jags", "ggmcmc", # also install jags http://mcmc-jags.sourceforge.net
  "tmap", "sp", "SpatialPack", "raster"
)
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[, "Package"])]
if (length(new.packages)) install.packages(new.packages)

# set paths for input & output
if (!file.exists("data/")) {
  dir.create("data")
  # file.symlink("/data/ZHULAB/FIA/Growth/Data/", "data")
}

if (!file.exists("figures/")) {
  dir.create("figures/")
}

if (!file.exists("models/")) {
  dir.create("models")
  # file.symlink("/data/ZHULAB/FIA/Growth/Models/", "models")
}
