# Load US soilgrids data ####

library(tigris)
library(raster)
library(sp)
library(rgeos)

r.SOC <- raster("/home/shares/soilcarbon/soilc-midwest/data/soil/soc_M_sl4_100m.tif")
spplot(r.SOC)

# Filter out soil data cells that are not primarily corn-growing ####

county.boundaries <- counties(state = c(d$FIPS))
county.boundaries <- county.boundaries[county.boundaries@data$GEOID %in% d$FIPS_county,]

crop.freq <- raster("/home/shares/soilcarbon/soilc-midwest/data/crop_frequency/crop_frequency_corn_2008-2017.img")
spplot(crop.freq)



View(crop.freq@data@attributes[[1]])


r.SOC <- SpatialPix(r.SOC)
