# Load packages, specify fxn for finding the mode, create a list of files ----

library(tidyverse)
library(sf)
library(fasterize)
library(raster)
library(tigris)
library(fuzzyjoin)
library(parallel)
library(gdalUtils)

mode <- function(x, na.rm = TRUE) {
  if(na.rm){
    x = x[!is.na(x)]
  }
  
  ux <- na.omit(unique(x))
  return(ux[which.max(tabulate(match(x, ux)))])
}


counties <- tigris::counties(year = 2017)
# load the corn frequency raster from CropScape - reports how many years between 
# 2008-2017 each pixel was used for maize production.
crop.freq <- brick("data/crop_frequency/crop_frequency_corn_2008-2017.img")


# create list of filenames with gSSURGO polygon data
all_counties_rds_list <- list.files("data/soil/county_gssurgo_gdbs", full.names = T, pattern = ".rds")

# define function that takes individual county gSSURGO vector, converts to raster, 
# masks/resamples that raster by corn cells,
# then computes summary stats
soil.stats.gather <- function(i){

  # Set GEOID variable for function enviroment
  temp.GEOID <- unique(i$GEOID)
  # Set boundary object for the county
  ibound <- spTransform(subset(counties,GEOID %in% temp.GEOID), CRSobj = crs(crop.freq))
  # Create reclassification matrix for converting crop frequency data into binary raster
  # binary for had 2+ years of corn data or did not. to identify pixels from any field in
  # which maize grown with relative consistency but not opportunistically 
  # while also eliminating non-crop areas of counties
  rcl.m <- matrix(c(-Inf,2, NA,
                    2, Inf,1), 
                  ncol=3, 
                  byrow=TRUE)
  
  # Create mask object
  imask <-
    reclassify(projectRaster(mask(crop(crop.freq, y = ibound), ibound), res = 30, crs = crs(ibound)), rcl.m)
  
  
  # Convert dataframe to SF object
  i <- st_as_sf(i)
  # Create blank raster to rasterize sf to
  irast <- raster(imask)
  
  # Convert the county-level data to a raster stack
  temp.raster <- raster::stack(fasterize(sf = i, raster = irast, field = "soc"),
                               fasterize(sf = i, raster = irast, field = "clay"),
                               fasterize(sf = i, raster = irast, field = "sand"),
                               fasterize(sf = i, raster = irast, field = "silt"),
                               fasterize(sf = i, raster = irast, field = "om"),
                               fasterize(sf = i, raster = irast, field = "awc"),
                               fasterize(sf = i, raster = irast, field = "aws"),
                               fasterize(sf = i, raster = irast, field = "fifteenbar"),
                               fasterize(sf = i, raster = irast, field = "cec"),
                               fasterize(sf = i, raster = irast, field = "ph"),
                               fasterize(sf = i, raster = irast, field = "droughty"),
                               fasterize(sf = i, raster = irast, field = "order"))
  
  
  # Rename layers in the raster stack
  names(temp.raster) <-
    c("soc",
      "clay",
      "sand",
      "silt",
      "om",
      "awc",
      "aws",
      "fifteenbar",
      "cec",
      "ph",
      "droughty",
      "order")
  
  # Mask temp.raster to just the corn cells in the county
  temp.raster <- mask(temp.raster, mask = imask)
  
  # Summarize soils data for each county
  soil.stats.temp <- cbind(temp.GEOID,
                           as.data.frame(t(cellStats(subset(temp.raster, c(1:10)), 'mean'))) %>%
                             rename_all(function(.) paste(.,"mean", sep = "_")),
                           as.data.frame(t(cellStats(subset(temp.raster, c(1:10)), 'median'))) %>%
                             rename_all(function(.) paste(.,"median", sep = "_")),
                           as.data.frame(t(cellStats(subset(temp.raster, c(11:12)), mode))) %>%
                             rename_all(function(.) paste(.,"mode", sep = "_")))
  
  # Return
  return(soil.stats.temp)
  gc()
}

# run soil.stats.gather function on each list
gssurgo.soil.stats <- mclapply(mc.cores = 20, all_counties_rds_list, function(x){
    soil.stats.gather(i = read_rds(x))
  })

# Merge stats dfs and write to the hard drive
gssurgo.soil.stats <- plyr::ldply(gssurgo.soil.stats) %>%
  rename("GEOID" = temp.GEOID) %>%
  rename_at(.vars = vars(2:23), .funs = function(x) paste("ssurgo", x, sep = "_")) %>%
  mutate(ssurgo_order_mode = as.character(ssurgo_order_mode),
         ssurgo_order_mode = dplyr::recode(ssurgo_order_mode, 
                                           '1' = "Alfisol",
                                           '2' = "Entisols",
                                           '3' = "Histosols",
                                           '4' = "Inceptisols",
                                           '5' = "Mollisols",
                                           '6' = "Spodosols",
                                           '7' = "Ultisols",
                                           '8' = "Vertisols",
                                           '9' = "Aridisols"))
  

saveRDS(gssurgo.soil.stats, file = "data/soil/gssurgo.soil.stats.rds")

