# Load packages, specify fxn for finding the mode, create a list of files ----

library(tidyverse)
library(sf)
library(fasterize)
library(raster)
library(tigris)
library(fuzzyjoin)
library(parallel)

mode <- function(x, na.rm = FALSE) {
  if(na.rm){
    x = x[!is.na(x)]
  }
  
  ux <- unique(x)
  return(ux[which.max(tabulate(match(x, ux)))])
}


# define function that takes individual county gSSURGO vector, converts to raster, masks/resamples that raster by corn cells,
# then computes summary stats
soil.stats.gather <- function(i){
  
  # Set GEOID variable for function enviroment
  temp.GEOID <- unique(i$GEOID)
  
  # Convert the county-level data to a raster stack
  temp.raster <- raster::stack(fasterize(sf = i, raster = raster(i, res = 10), field = "soc0_30"),
                  fasterize(sf = i, raster = raster(i, res = 10), field = "aws0_30"),
                  fasterize(sf = i, raster = raster(i, res = 10), field = "nccpi3corn"),
                  fasterize(sf = i, raster = raster(i, res = 10), field = "droughty"),
                  fasterize(sf = i, raster = raster(i, res = 10), field = "SSURGO_sandtotal_r"),
                  fasterize(sf = i, raster = raster(i, res = 10), field = "SSURGO_silttotal_r"),
                  fasterize(sf = i, raster = raster(i, res = 10), field = "SSURGO_claytotal_r"),
                  fasterize(sf = i, raster = raster(i, res = 10), field = "SSURGO_taxorder"))
  
  # Rename layers in the raster stack
  names(temp.raster) <- c("SSURGO_soc0_30",
                          "SSURGO_aws0_30",
                          "SSURGO_nccpi3corn",
                          "SSURGO_droughty",
                          "SSURGO_sandtotal_r",
                          "SSURGO_silttotal_r",
                          "SSURGO_claytotal_r",
                          "SSURGO_taxorder")
  
  # Mask raster stack with corresponding corn cells mask
  temp.raster <- mask(resample(temp.raster, corn_mask_by_county[[temp.GEOID]]), corn_mask_by_county[[temp.GEOID]])
  
  # Summarize soils data for each county
  soil.stats.temp <- cbind(temp.GEOID,
                           as.data.frame(t(cellStats(subset(temp.raster, c(1:3,5:7)), 'mean'))) %>%
                                  rename_all(function(.) paste(.,"mean", sep = "_")),
                           as.data.frame(t(cellStats(subset(temp.raster, c(1:3,5:7)), 'sd'))) %>%
                             rename_all(function(.) paste(.,"sd", sep = "_")),
                           as.data.frame(t(cellStats(subset(temp.raster, c(4,8)), mode))) %>%
                             rename_all(function(.) paste(.,"mode", sep = "_")))
  
  # Return
  return(soil.stats.temp)
}

# call in raster for masking soil data by corn cells in each county
corn_mask_by_county <- readRDS("data/soil/corn_mask_by_county.rds")

# create list of filenames with gSSURGO polygon data
all_counties_rds_list <- list.files("data/soil/gssurgo_2019/all_counties_rds/", full.names = T)

# run soil.stats.gather function on each list
soil_stats_1 <- mclapply(mc.cores = 10, readRDS(all_counties_rds_list[[1]]), function(data){
    soil.stats.gather(i = data)
  })
gc()

soil_stats_2 <- mclapply(mc.cores = 10, readRDS(all_counties_rds_list[[2]]), function(data){
  soil.stats.gather(i = data)
})
gc()

soil_stats_3 <- mclapply(mc.cores = 10, readRDS(all_counties_rds_list[[3]]), function(data){
  soil.stats.gather(i = data)
})
gc()

soil_stats_4 <- mclapply(mc.cores = 10, readRDS(all_counties_rds_list[[4]]), function(data){
  soil.stats.gather(i = data)
})
gc()

soil_stats_5 <- mclapply(mc.cores = 10, readRDS(all_counties_rds_list[[5]]), function(data){
  soil.stats.gather(i = data)
})
gc()

# Merge stats dfs and write to the hard drive
gssurgo.soil.stats <- plyr::ldply(do.call("list", mget(ls(pattern = "soil_stats"))), function(x) plyr::ldply(x)) %>%
  dplyr::select(-.id) %>%
  rename("GEOID" = temp.GEOID)

saveRDS(gssurgo.soil.stats, file = "data/soil/gssurgo.soil.stats.rds")

