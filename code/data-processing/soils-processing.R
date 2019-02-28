###################################
# Soil processing to county level #
# Authors: Stephen Wood           #
# Created: 10/4/18                #
###################################


#### READ PACKAGES ####
library(tidyverse)
library(raster)


#### DEFINE FUNCTIONS ####
mode <- function(x, na.rm = FALSE) {
  if(na.rm){
    x = x[!is.na(x)]
  }
  
  ux <- unique(x)
  return(ux[which.max(tabulate(match(x, ux)))])
}


#### READ DATA ####
# Generate a list of all soil rasters
grids <- list.files("data/soil/SoilGridsUS" , pattern = "*.tif")
# Subset 30 cm rasters
thirty.cm <- grids[str_detect(grids,"sl4")]
# Select soil properties to use
thirty.cm <- thirty.cm[-1]
# Create a raster brick from the input raster files 
soils.stack <- stack(paste0("data/soil/SoilGridsUS/", thirty.cm)) 
# Read county data
counties <- tigris::counties(year = 2017)
# Read yield data for determining which counties are present
load('data/yield_data_temp_05302018.RData')
rm(census.years); rm(d.acres); rm(d.acres.irrigated); rm(d.acres.summary); rm(d.acres.total); rm(d.save); rm(proj); rm(years)


#### TRANSFORM DATA ####
# Subset original data frame for the counties that have data in time series
sub.counties <- subset(counties, GEOID %in% unique(d$FIPS_county))
# Project data to same format
sub.counties <- sp::spTransform(sub.counties, crs(soils.stack))
# Crop raster by counties
crop.stack <- crop(soils.stack, extent(sub.counties))

### FILTER DATA ####
## Filter out soil data cells that are not primarily corn-growing #
# Import raster of corn frequency 2008-2017. 
# Each cell categorized based on number of years it has been used for corn over the past 10 years
crop.freq <- brick("data/crop_frequency/crop_frequency_corn_2008-2017.img")
# Split 'sub.counties' polygon object into a list of individual polygons. 
# This makes it possible to use split-apply-combine on downstream operations and not overload memory
sub.counties.list <- sapply(sub.counties$GEOID, function(x) sub.counties[sub.counties$GEOID == x,])

# Crop the corn frequency layer to each county then change projection and resolution to match soils data
crop.freq.by.county <- lapply(sub.counties.list, function(x) crop(crop.freq, x))
crop.freq.by.county <- lapply(crop.freq.by.county, function(x) projectRaster(x, crs = crs(crop.stack), res = 100))

# Reclassify cells in each crop frequency by county layer such that 0 and 1 cells are NA and all else are 1. 
rcl.m <- matrix(c(-Inf, 1, NA,
                  1, Inf,1), 
                ncol=3, 
                byrow=TRUE)
corn.cells.by.county <- lapply(crop.freq.by.county, function(x) reclassify(x, rcl.m))

# Mask the soils data to just corn growing cells for each county
soil.stack.corn.cells.by.county <- lapply(corn.cells.by.county, function(x) {
  mask(resample(crop.stack, x), x)
})

# Summarize soils data for each county
soil.corn <- plyr::ldply(soil.stack.corn.cells.by.county, function(x) cellStats(x, 'mean'))
names(soil.corn)[1] <- "FID"

#### GET TAXONOMY ####
tax <- raster("data/soil/SoilGridsUS/TAXgg_M_100m.tif")
tax.cat <- read_csv("data/soil/SoilGridsUS/TAXgg_M_100m.csv")

soil.tax.corn.cells.by.county <- lapply(corn.cells.by.county, function(x) {
  mask(resample(tax, x), x)
})

soil.tax.corn <- plyr::ldply(soil.tax.corn.cells.by.county, function(x) cellStats(x, mode))
names(soil.tax.corn)[1] <- "FID"

soil.corn %>%
  left_join(soil.tax.corn) %>%
  rename("Value" = V1) %>%
  mutate(Value = as.numeric(as.character(res.merge$Value))) %>%
  left_join(tax.cat) -> res.merge


# Create soil order category
res.merge$Order <- res.merge$Class
res.merge$Order[grep("alfs", res.merge$Order)] <- "Alfisol"
res.merge$Order[grep("ents", res.merge$Order)] <- "Entisols"
res.merge$Order[grep("ults", res.merge$Order)] <- "Ultisols"
res.merge$Order[grep("olls", res.merge$Order)] <- "Mollisols"
res.merge$Order[grep("erts", res.merge$Order)] <- "Vertisols"
res.merge$Order[grep("epts", res.merge$Order)] <- "Inceptisols"
res.merge$Order[grep("prists", res.merge$Order)] <- "Histosols"


saveRDS(res.merge, file = "data/soil/county-soilgrids.rds")
