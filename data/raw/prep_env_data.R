library(tidyverse)
library(amt)

dat <- read_rds("data/trk.rds")

elevation <- raster("data/raw/elevation/ASTER ASTGTM2 Elevation-20100101120000000-0-0.tif")
landuse <- raster("data/raw/landuse/landuse_study_area.tif")
popden <- raster("data/raw/pop_den/pop_den.tif")

get_crs(dat)
landuse <- raster::projectRaster(landuse, crs = get_crs(dat), method = "ngb")
elevation <- raster::projectRaster(elevation, crs = get_crs(dat))
popden <- raster::projectRaster(popden, crs = get_crs(dat))

forest <- raster::match(landuse, 41:43, nomatch = 0) > 0
raster::plot(forest)

elevation <- raster::resample(elevation, landuse)
popden <- raster::resample(popden, landuse)

st1 <- raster::stack(elevation, landuse, popden, forest)
write_rds(st1, "data/env_covar.rds", compress = "gz")
