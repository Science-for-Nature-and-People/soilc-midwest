library(raster)
library(gdalUtils)

setwd("/home/shares/soilcarbon/soilc-midwest/data/lobell_yield_2008")

# Create list of all .tif files
files <- list.files(pattern = ".tif$", full.names = TRUE)

# Read each one in as a stack and create a list
rl <- lapply(files, raster)
rl.a <- rl[1:10]

# Call 'mosaic' to stitch them all together
rl.a$fun <- mean
rl$fun <- mean
merged <- do.call(mosaic, rl.a)

