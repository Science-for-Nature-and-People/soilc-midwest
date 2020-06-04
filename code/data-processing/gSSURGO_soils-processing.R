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


# Create list of .gdb files
gdb_list <- list.files("data/soil/gssurgo_2019/", full.names = T, pattern = "gdb")


# Create a series of objects that will later be used to filter and clip gSSURGO data----

# Call in FIPS information and codes for counties used in analysis

data(fips_codes)
fips_codes 
fips_codes <- fips_codes %>%
  mutate(CH.GEOID = paste(state,county_code, sep = ""), 
         GEOID = paste(state_code,county_code, sep = "")) %>%
  filter(GEOID %in% c(read.csv("data/Unique_counties.csv", colClasses = c('character'))$x)) %>%
  mutate(county = str_remove(county, " County"))

# Call in 'sacatalog' table from gSSURGO
# While the CH.GEOID column matches the FIPS code for most counties, in some states, counties are combined into the same soil survey area
# This step is to match CH.GEOID to FIPS codes

sa_catalogs <- plyr::ldply(gdb_list, function(i) sf::st_read(dsn = i, layer = "sacatalog")) %>%
  dplyr::select(-tabularversion, -tabcertstatus, -tabcertstatusdesc, -tabnasisexportdate, -tabularverest, -tabularversion, 
                -sacatalogkey, -saversion, -saverest, -fgdcmetadata) %>%
  mutate(state =str_sub(areasymbol, end = 2),
         areaname = str_remove(areaname, pattern = '\\s*,.*'))

sa_subset <- sa_catalogs %>%
  regex_right_join(fips_codes %>%
                     anti_join(sa_catalogs, by = c(CH.GEOID = "areasymbol")), 
                   by = c(state = "state", areaname = "county")) %>%
  #Drop Harford County survey area and Boyd/Greenup County in KY
  filter(!areasymbol == "MD600",
         !areaname == "Boyd and Greenup Counties") %>%
  full_join(sa_catalogs %>%
              inner_join(fips_codes, by = c(areasymbol = "CH.GEOID"))) %>%
  dplyr::select(-CH.GEOID, -state.y) %>%
  rename("state" = state.x)



# read in 'MUPOLYGON' and 'Valu1' tables from .gdb files as simple features
all_states_gssurgo <- parallel::mclapply(mc.cores = 20, gdb_list, function(i){
  sf::st_read(dsn = i, layer = "MUPOLYGON") %>%
    left_join(sf::st_read(dsn = i, layer = "Valu1"),
              by = c("MUKEY" = "mukey")) %>%
    left_join(sf::st_read(i, layer = "component") %>%
                left_join(sf::st_read(i, layer = "chorizon")) %>%
                group_by(mukey) %>%
                # filter to keep only major components
                filter(majcompflag == "Yes") %>%
                #filter out C, E, M, R, L horizon data
                droplevels() %>%
                filter(!desgnmaster %in% c("^C","C","C/B","^C/B","C/A","CA","CB","E",
                                           "E'","E and B","E/B","EB","EC","M","R", "L"),
                       !is.na(desgnmaster)) %>%
                filter(hzdept_r <= 30) %>%
                summarize(SSURGO_taxorder = mode(taxorder), 
                          SSURGO_sandtotal_r = mean(sandtotal_r),
                          SSURGO_silttotal_r = mean(silttotal_r),
                          SSURGO_claytotal_r = mean(claytotal_r)),
              by = c("MUKEY" = "mukey")) %>%
    mutate(AREASYMBOL = as.character(AREASYMBOL),
           MUSYM = as.character(MUSYM),
           MUKEY = as.character(MUKEY)) %>%
    filter(AREASYMBOL %in% c(sa_subset$areasymbol))
})


# Collapse list of sf features into one object, then split it into a list based on county code
all_counties_gssurgo <- do.call("rbind", all_states_gssurgo) %>%
  left_join(sa_subset, by = c(AREASYMBOL = "areasymbol")) %>%
  split(., f = .$GEOID)

rm(all_states_gssurgo)
gc()

# Split data into 5 lists for easier processing downstream and write to disk
saveRDS(all_counties_gssurgo[1:100], file = "data/soil/all_counties_gSSURGO_1.rds")
saveRDS(all_counties_gssurgo[101:200], file = "data/soil/all_counties_gSSURGO_2.rds")
saveRDS(all_counties_gssurgo[201:300], file = "data/soil/all_counties_gSSURGO_3.rds")
saveRDS(all_counties_gssurgo[301:400], file = "data/soil/all_counties_gSSURGO_4.rds")
saveRDS(all_counties_gssurgo[401:491], file = "data/soil/all_counties_gSSURGO_5.rds")




# EXTRA CODE



# # Read in polygons for counties that have data in yield data time series
# sub.counties <- subset(tigris::counties(year = 2017), GEOID %in% sa_subset$GEOID)
# 
# # Project data to same CRS as rasters
# sub.counties <- sp::spTransform(sub.counties, crs("+proj=aea +lat_1=29.5 +lat_2=45.5 +lat_0=23 +lon_0=-96 +x_0=0 +y_0=0 +datum=NAD83 +units=m +no_defs"))
# 
# # Split 'sub.counties' polygon object into a list of individual polygons. 
# # This makes it possible to use split-apply-combine on downstream operations and not overload memory
# sub.counties <- split(sub.counties, f = sub.counties$GEOID)

# # Import raster of corn frequency 2008-2017. 
# # Each cell categorized based on number of years it has been used for corn over the past 10 years
# crop.freq <- brick("data/crop_frequency/crop_frequency_corn_2008-2017.img")
# 
# # Crop the corn frequency layer to each county then change projection and resolution to match soils data
# crop.freq.by.county <- mclapply(mc.cores = 10, sub.counties.list, function(x) crop(crop.freq, x))
# crop.freq.by.county <- mclapply(mc.cores = 10, crop.freq.by.county, function(x) 
#   projectRaster(x, crs = crs("+proj=aea +lat_1=29.5 +lat_2=45.5 +lat_0=23 +lon_0=-96 +x_0=0 +y_0=0 +datum=NAD83 +units=m +no_defs")))
# gc()
# # Reclassify cells in each crop frequency by county layer such that 0 and 1 cells are NA and all else are 1. 
# rcl.m <- matrix(c(-Inf, 1, NA,
#                   1, Inf,1), 
#                 ncol=3, 
#                 byrow=TRUE)
# corn.cells.by.county <- mclapply(mc.cores = 10, crop.freq.by.county, function(x) reclassify(x, rcl.m))
# 
# rm(list = setdiff(ls(), c("corn.cells.by.county")))
# gc()
# saveRDS(corn.cells.by.county, file = "data/soil/corn_mask_by_county.rds")
