# install packages
list.of.packages <- c(
  "tidyverse", "scales", "plotrix", "modelr",
  "R2jags", "ggmcmc", # also install jags http://mcmc-jags.sourceforge.net
  "tmap", "sp", "SpatialPack", "raster"
)
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[, "Package"])]
if (length(new.packages)) install.packages(new.packages)

# set paths for input & output
file.symlink("/data/ZHULAB/FIA/Growth/Data/", "data-raw")
file.symlink("/data/ZHULAB/FIA/Growth/Models/", "models")

dir.create("figures/")
