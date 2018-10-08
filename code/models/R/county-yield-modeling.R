###################
# Yield modeling  #
# Stephen Wood    #
# 10/4/18         #
###################

library(tidyverse)

#### READ IN DATA ####

# Drought data
load("data/drought_by_county.RData")
drought <- data_by_county_2
rm(data_by_county_2)

# Yield data
load('data/yield_data_temp_05302018.RData')
rm(years); rm(census.years); rm(d.save); rm(d.acres.irrigated); rm(d.acres.summary); rm(d.acres.total)
yield <- d
rm(d)

# County soil data
soil <- readRDS("/home/shares/soilcarbon/soilc-midwest/data/soil/county-soilgrids.rds")

# Spatial data
counties <- tigris::counties(year = 2017)
sub.counties <- subset(counties, GEOID %in% unique(d$FIPS_county))


#### FIT MODEL ####
