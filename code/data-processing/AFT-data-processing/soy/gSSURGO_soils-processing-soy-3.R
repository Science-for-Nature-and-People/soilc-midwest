# This file is to check the ratio of data being removed at each step

## ----setup, include=FALSE--------------------------------------------------------------------------
knitr::opts_chunk$set(echo = TRUE)
# Load packages, specify fxn for finding the mode, create a list of files ----

library(tidyverse)
library(sf)
library(fasterize)
library(raster)
library(tigris)
library(fuzzyjoin)
library(parallel)
library(readr)
library(dplyr)
#library(gdalUtils)



## ----mode------------------------------------------------------------------------------------------
mode <- function(x, na.rm = TRUE) {
  if(na.rm){
    x = x[!is.na(x)]
  }
  
  ux <- na.omit(unique(x))
  return(ux[which.max(tabulate(match(x, ux)))])
}

## Calculate median
med <- function(x, na.rm = TRUE) {
  med <- as.vector(quantile(x,probs= 0.5, na.rm = TRUE))
  
  return(med)
}

## ----CropScape-------------------------------------------------------------------------------------
counties <- tigris::counties(year = 2022)
# load the corn frequency raster from CropScape - reports how many years between 
# 2008-2017 each pixel was used for maize production.
crop.freq <- brick('/home/shared/Kane_data/CropScape/crop_frequency_corn_2008-2022.tif')

## ----soil------------------------------------------------------------------------------------------
all_counties_rds_list <- list.files("/home/aysha/Kane_data/county_gssurgo_gdbs", full.names = T, pattern = ".rds")

i <- read_rds(all_counties_rds_list[100])
# define function that takes individual county gSSURGO vector, converts to raster, 
# masks/resamples that raster by corn cells,
# then computes summary stats
soil.stats.gather <- function(i){
  
  # Set GEOID variable for function enviroment
  temp.GEOID <- unique(i$GEOID)
  
  # Set boundary object for the county
  #ibound <- spTransform(subset(counties,GEOID %in% temp.GEOID), CRSobj = crs(crop.freq))
  
  # ML: updated function
  ibound <- st_transform(subset(counties,GEOID %in% temp.GEOID), st_crs(crop.freq))
  # Create reclassification matrix for converting crop frequency data into binary raster
  # binary for had 2+ years of corn data or did not. to identify pixels from any field in
  # which maize grown with relative consistency but not opportunistically 
  # while also eliminating non-crop areas of counties
  #rcl.m <- matrix(c(-Inf,2, NA,
  #                  2, Inf,1), 
  #                ncol=3, 
  #                byrow=TRUE)
  
  # ML: the original code has a bug; where the no corn cell value =255 is also considered as corn cells; this can corrected with the following reclassification matrix
  rcl.m <- matrix(c(-Inf,2, NA,
                    2, 254,1,
                    254,Inf,NA), 
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
  temp.raster <- raster::stack(#fasterize(sf = i, raster = irast, field = "soc"),
    #fasterize(sf = i, raster = irast, field = "clay"),
    #fasterize(sf = i, raster = irast, field = "sand"),
    #fasterize(sf = i, raster = irast, field = "silt"),
    fasterize(sf = i, raster = irast, field = "om")
    #fasterize(sf = i, raster = irast, field = "awc"),
    #fasterize(sf = i, raster = irast, field = "aws"),
    #fasterize(sf = i, raster = irast, field = "fifteenbar"),
    #fasterize(sf = i, raster = irast, field = "cec"),
    #fasterize(sf = i, raster = irast, field = "ph"),
    #fasterize(sf = i, raster = irast, field = "droughty"),
    #fasterize(sf = i, raster = irast, field = "order")
  )
  
  
  # Rename layers in the raster stack
  names(temp.raster) <- 'om'
  
  # Mask temp.raster to just the corn cells in the county
  temp.raster.1 <- mask(temp.raster, mask = imask)
  
  # ML: we will mask the temp.raster to just the cells with <10% om
  om_mask <- reclassify(subset(temp.raster.1, 'om'), cbind(10, Inf, NA))
  temp.raster.om <- mask(temp.raster.1, mask = om_mask)
  
  # Calculate the ratio of cells being kept at each step for each county
  soil.stats.temp <- cbind.data.frame(temp.GEOID,
                                      no_na = length(which(!is.na(getValues(temp.raster$om)),TRUE)),
                                      cs_no_na = length(which(!is.na(getValues(temp.raster.1$om)),TRUE)),
                                      cs_om10_no_na = length(which(!is.na(getValues(temp.raster.om$om)),TRUE))) %>%
    mutate(
      ratio_cs = cs_no_na/no_na,
      ratio_om10 = cs_om10_no_na/cs_no_na)
  
  # Return
  return(soil.stats.temp)
  gc()
}


# run soil.stats.gather function on each list
gssurgo.soil.stats <- mclapply(mc.cores = 8, all_counties_rds_list, function(x){
  soil.stats.gather(i = read_rds(x))
})

# Merge stats dfs and write to the hard drive
gssurgo.soil.ratio <- plyr::ldply(gssurgo.soil.stats) %>%
  rename("GEOID" = temp.GEOID) 

saveRDS(gssurgo.soil.ratio, file = "/home/aysha/Kane_data/soy/gssurgo.soy.soil.filter_noNA_ratios.rds")

#----------combine soil data and ratio------------------#

soil_data <- readRDS('/home/aysha/Kane_data/soy/soy_gssurgo.soil.stats_filter_om10.rds')
soil_ratio <- readRDS('/home/aysha/Kane_data/soy/gssurgo.soy.soil.filter_noNA_ratios.rds')

soil_ratio.1 <- soil_ratio %>%
  dplyr::select(GEOID, ratio_om10) %>%
  dplyr::rename(ratio_om10toCS = ratio_om10)

soil_data.1 <- soil_data %>%
  left_join(soil_ratio.1, by = 'GEOID') %>%
  mutate(cell_kept_perc90 = case_when(
    ratio_om10toCS >= 0.9 ~ 'Y',
    TRUE ~ 'N')
  )

saveRDS(soil_data.1, file = '/home/aysha/Kane_data/soy/soy_gssurgo.soil.stats_n_15_filter_om10.rds')
