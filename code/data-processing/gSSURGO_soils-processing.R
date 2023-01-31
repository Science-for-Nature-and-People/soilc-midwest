# Load packages, specify fxn for finding the mode, create a list of files ----

library(tidyverse)
library(sf)
library(fasterize)
library(raster)
library(tigris)
library(fuzzyjoin)
library(parallel)
library(aqp)

mode <- function(x, na.rm = TRUE) {
  if(na.rm){
    x = x[!is.na(x)]
  }
  
  ux <- na.omit(unique(x))
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
  filter(GEOID %in% unique(read_rds("data/yield_08062020.rds")$GEOID)) %>%  ## don't have this file
  mutate(county = str_remove(county, " County"))

# Call in 'sacatalog' table from gSSURGO
# While the CH.GEOID column matches the FIPS code for most counties, 
# in some states, counties are combined into the same soil survey area
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
         !areasymbol == "TN610",
         !areaname == "Boyd and Greenup Counties") %>%
  full_join(sa_catalogs %>%
              inner_join(fips_codes, by = c(areasymbol = "CH.GEOID"))) %>%
  dplyr::select(-CH.GEOID, -state.y) %>%
  rename("state" = state.x)



all_states_gssurgo <- parallel::mclapply(mc.cores = 27, gdb_list, function(i){
  
  # Merge MUPOLYGON and valu1 tables to start
  temp1 <-  sf::st_read(dsn = i, layer = "MUPOLYGON") %>%
    left_join(sf::st_read(dsn = i, layer = "Valu1"),
              by = c("MUKEY" = "mukey")) 
  
  # Depth slices from chorizon table
  temp2 <- sf::st_read(i, layer = "chorizon")
  depths(temp2) <- cokey ~ hzdept_r + hzdepb_r
  
  temp2 <- slab(temp2, 
                fm= cokey ~ sandtotal_r+silttotal_r+claytotal_r+
                  om_r+awc_r+cec7_r+ph1to1h2o_r+wfifteenbar_r, 
                slab.structure=c(0, 30), 
                slab.fun=mean, na.rm=TRUE)  %>%
    reshape2::dcast(., cokey + bottom ~ variable, value.var = 'value') %>%
    rename_all(.funs = function(x) paste("SSURGO", x, sep = "_")) %>%
    dplyr::select(-SSURGO_bottom)  
  
  
  # Component table
  temp3 <- sf::st_read(i, layer = "component") %>%
    dplyr::select(comppct_r, taxorder, cokey, mukey,majcompflag) 
  
  # Join component and horizon data and take weighted averages for each map unit
  temp4 <- temp3 %>%
    left_join(temp2, by = c("cokey"="SSURGO_cokey")) %>%
    group_by(mukey) %>%
    mutate(order = taxorder[which(majcompflag %in% "Yes")[1]]) %>%
    summarize(clay = weighted.mean(SSURGO_claytotal_r, w = comppct_r, na.rm = TRUE),
              sand = weighted.mean(SSURGO_sandtotal_r, w = comppct_r, na.rm = TRUE),
              silt = weighted.mean(SSURGO_silttotal_r, w = comppct_r, na.rm = TRUE),
              om = weighted.mean(SSURGO_om_r, w = comppct_r, na.rm = TRUE),
              awc = weighted.mean(SSURGO_awc_r, w = comppct_r, na.rm = TRUE),
              cec = weighted.mean(SSURGO_cec7_r, w = comppct_r, na.rm = TRUE),
              ph = weighted.mean(SSURGO_ph1to1h2o_r, w = comppct_r, na.rm = TRUE),
              fifteenbar = weighted.mean(SSURGO_wfifteenbar_r, w = comppct_r, na.rm = TRUE),
              order = unique(order)) %>%
    janitor::clean_names(.)
  
  temp4[is.na(temp4)] <- NA
  
  # Join temp4 and temp1 dataframes together to match aggregated component data to mapunits
  
  temp5 <- temp1 %>%
    dplyr::select(AREASYMBOL, MUSYM, MUKEY, aws0_30, soc0_30, droughty) %>%
    janitor::clean_names(.) %>%
    rename(c("aws"="aws0_30","soc"="soc0_30")) %>%
    left_join(temp4)
  
  return(temp5)
})


write_rds(all_states_gssurgo, path = "data/soil/all_states_gssurgo.rds")

# Collapse list of sf features into one object, then split it into a list based on county code
all_counties_gssurgo <- data.table::rbindlist(all_states_gssurgo) %>%  
  filter(areasymbol %in% sa_subset$areasymbol) %>%
  left_join(sa_subset, by = c("areasymbol" = "areasymbol")) 

rm(all_states_gssurgo)
gc()

write_rds(all_counties_gssurgo, path = "data/soil/all_counties_gssurgo.rds")
all_counties_gssurgo <- read_rds(path = "data/soil/all_counties_gssurgo.rds")

mclapply(mc.cores = 4, unique(all_counties_gssurgo$GEOID), FUN = function(x){
  write_rds(all_counties_gssurgo[all_counties_gssurgo$GEOID == x,], 
            path = paste("data/soil/county_gssurgo_gdbs/GEOID_", x, ".rds", sep = ""))
})




