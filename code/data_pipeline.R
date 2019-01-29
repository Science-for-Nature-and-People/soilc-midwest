# setup chunk ####
library(rnassqs)
library(dplyr)
library(ggplot2)
library(tidyr)
library(reshape2)

setwd("/home/shares/soilcarbon/soilc-midwest/code/")

api_key <- as.character(read.csv("NASS_API_key.csv", header = F)[1,1])       # api key
# Specify the range of years across which you want to collect data
years <- as.list(1997:2017)
# Call in all corn yield data via NASS API ####
d <- plyr::ldply(years, function(x){
  nassqs(params = list("short_desc" = "CORN, GRAIN - YIELD, MEASURED IN BU / ACRE", 
                       "short_desc" = "CORN, GRAIN - ACRES HARVESTED",
                       "source_desc" = "SURVEY",
                       "year"=x) , key = api_key) 
})

d$year <- as.numeric(d$year)

# Combine state FIPS code and county code
d$FIPS_county <- paste(d$state_fips_code, d$county_code, sep = "")
d.save <- d
d <- d %>%
  filter(county_code != 998,
         agg_level_desc %in% "COUNTY") %>%
  rename(Year = year, State = state_name, State.alpha = state_alpha, County.code = county_code, 
         County.name = county_name, FIPS = state_fips_code, variable = unit_desc) %>%
  select(FIPS_county, Year, State, State.alpha, County.name, FIPS, County.code, county_ansi, variable, Value) %>%
  spread(variable, Value) %>%
  rename(Acres.harvested = ACRES, Yield.bu.acre = `BU / ACRE`) -> d


d$Yield.bu.acre <- as.numeric(gsub(pattern = ",", replacement = "", d$Yield.bu.acre))
d$Acres.harvested <- as.numeric(gsub(pattern = ",", replacement = "", d$Acres.harvested))

# Call in data on number of irrigated acres in each county in recent census years, clean ####
census.years <- as.list(c(1997,2002,2007,2012))
d.acres.irrigated <-  plyr::ldply(census.years, function(x){
  nassqs(params = list("short_desc" = "CORN, GRAIN, IRRIGATED - ACRES HARVESTED", 
                       "source_desc" = "CENSUS",
                       "year"=x) , key = api_key) 
})
 
d.acres.irrigated$FIPS_county <- paste(d.acres.irrigated$state_fips_code, d.acres.irrigated$county_code, sep = "")
d.acres.irrigated <- d.acres.irrigated[d.acres.irrigated$agg_level_desc %in% "COUNTY",]
d.acres.irrigated$Value <- as.numeric(gsub(pattern = ",", replacement = "", d.acres.irrigated$Value))
d.acres.irrigated <- d.acres.irrigated %>% 
  rename(Acres.irrigated.census = Value, Year = year) %>%
  select(FIPS_county, Acres.irrigated.census, Year)


# Call in data on total number of acres in corn harvested in recent census years, clean
d.acres.total <-  plyr::ldply(census.years, function(x){
  nassqs(params = list("short_desc" = "CORN, GRAIN - ACRES HARVESTED", 
                       "source_desc" = "CENSUS", "sector_desc" = "CROPS",
                       "year"=x) , key = api_key) 
})

d.acres.total$FIPS_county <- paste(d.acres.total$state_fips_code, d.acres.total$county_code, sep = "")
d.acres.total <- d.acres.total[d.acres.total$agg_level_desc %in% "COUNTY",]
d.acres.total$Value <- as.numeric(gsub(pattern = ",", replacement = "", d.acres.total$Value))
d.acres.total <- d.acres.total %>% 
  rename(Acres.total.census = Value, Year = year) %>%
  select(state_alpha, county_name, county_ansi, FIPS_county, Year, Acres.total.census)

# Join irrigated acres and total acres data, process ####
d.acres <- d.acres.total %>%
  left_join(d.acres.irrigated, by = c("FIPS_county", "Year"))

d.acres <- d.acres[!is.na(d.acres$Acres.total.census|d.acres$Acres.irrigated.census),]

# NOTE: assuming that NA in acres.irrigated column implies data deficient, setting to zero
d.acres$Acres.irrigated.census[is.na(d.acres$Acres.irrigated.census)] <- 0

d.acres <- d.acres %>%
  mutate(Percent.irrigated.census = 100*(Acres.irrigated.census/Acres.total.census))
# NOTE: Check on 100% irrigated counties for data quality
# UPDATE: 100% irrigated counties appear to all be in arid locations (i.e. TX, MT, CA)

d.acres %>%
  group_by(state_alpha,county_name,county_ansi,FIPS_county) %>%
  summarize(Mean.total = round(mean(Acres.total.census, na.rm = T),digits = 4),
            Total.sd = round(sd(Acres.total.census, na.rm = T),digits = 4),
            Mean.irrigated = round(mean(Percent.irrigated.census, na.rm = T),digits = 4),
            Irrigated.sd = round(sd(Percent.irrigated.census, na.rm = T),digits = 4))  -> d.acres.summary

hist(d.acres.summary$Mean.irrigated, breaks = 20)
# Create filter to select counties that are 5 percent or less irrigated, 
# choice of 5 percent based on dsitribution of percentages, vast majority of counties are 5 percent or less irrigated

d.acres.summary %>%
  filter(Mean.irrigated < 5) %>%
  filter(Irrigated.sd <= 1) -> d.acres.summary

# Create filter to select counties that are 5 percent or less irrigated, 
# choice of 5 percent based on dsitribution of percentages, vast majority of counties are 5 percent or less irrigated

d %>%
  filter(FIPS_county %in% d.acres.summary$FIPS_county) -> d

# Filter out any counties without contiguous corn yield observations

d %>%
  add_count(FIPS_county) %>%
  filter(n == 21) -> d

# Load gSSURGO data ####

library(rgdal)
library(sf)

d.valu1 <- sf::st_read(dsn = "/home/shares/soilcarbon/soilc-midwest/data/soil/valu_fy2016.gdb", layer = "valu1")

states <- paste(unique(d$State.alpha), collapse = "|")

gdb.dir <- "/home/shares/soilcarbon/soilc-midwest/data/soil/gssurgo/"
state.gdbs <- list.files(gdb.dir)[grep(x=list.files(gdb.dir), pattern = states)]

test.dir <- "/home/shares/soilcarbon/soilc-midwest/data/soil/gssurgo/gSSURGO_AL.gdb"

test <- readOGR(dsn = "/home/shares/soilcarbon/soilc-midwest/data/soil/gssurgo/gSSURGO_AL.gdb",layer = "MUPOLYGON")

test@data

# Filter out soil data cells that are not primarily corn-growing ####

library(tigris)
library(raster)
library(sp)
library(rgeos)

county.boundaries <- counties(state = c(d$FIPS))
county.boundaries <- county.boundaries[county.boundaries@data$GEOID %in% d$FIPS_county,]

crop.freq <- raster("/home/shares/soilcarbon/soilc-midwest/data/crop_frequency/crop_frequency_corn_2008-2017.img")




