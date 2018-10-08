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


#### EXTRACT DATA ####
results <- raster::extract(crop.stack,
        sub.counties,
        fun=mean,
        na.rm=T
)


#### GET TAXONOMY ####
tax <- raster("data/soil/SoilGridsUS/TAXgg_M_100m.tif")
tax.cat <- read_csv("data/soil/SoilGridsUS/TAXgg_M_100m.csv")

sub.counties <- sp::spTransform(sub.counties, crs(tax))
crop.tax <- crop(tax, extent(sub.counties))

tax.res <- raster::extract(crop.tax,
                           sub.counties,
                           fun=mode,
                           na.rm=T
)

results <- cbind(results, tax.res)
names(tax.cat)[1] <- "tax.res"
res.merge <- merge(results, tax.cat)

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
