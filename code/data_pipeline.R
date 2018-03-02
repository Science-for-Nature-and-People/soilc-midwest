library(rnassqs)
library(dplyr)
library(ggplot2)
library(tidyr)
library(reshape2)

setwd("/home/shares/soilcarbon/soilc-midwest/code/")

api_key <- as.character(read.csv("NASS_API_key.csv", header = F)[1,1])       # api key
# Specify the range of years across which you want to collect data
years <- as.list(1910:2017)

# Call in all corn yield data via NASS API
#d <- plyr::ldply(years, function(x){
  #nassqs(params = list("short_desc" = "CORN, GRAIN - YIELD, MEASURED IN BU / ACRE", 
                       #"short_desc" = "CORN, GRAIN - ACRES HARVESTED",
                       #"source_desc" = "SURVEY",
                       #"year"=x) , key = api_key) 
#})

load(file = "/home/shares/soilcarbon/soilc-midwest/data/corn_yield.Rdata")

# Combine state FIPS code and county code
d$FIPS_county <- paste(d$state_fips_code, d$county_code, sep = "")
# Drop data that are not at county level (i.e. summarized by region within state)
d <- d[d$agg_level_desc %in% "COUNTY",]

d <- d %>%
  rename(Year = year, State = state_name, County.code = county_code, 
         County.name = county_name, FIPS = state_fips_code, Yield.bu.acre = Value) %>%
  select(FIPS_county, Year, State, County.name, FIPS, County.code, county_ansi, Yield.bu.acre)

# Call in data on number of acres in each county that are irrigated in recent census years, clean
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

# Join irrigated acres and total acres data, process
d.acres <- d.acres.total %>%
  left_join(d.acres.irrigated, by = c("FIPS_county", "Year"))

d.acres <- d.acres[!is.na(d.acres$Acres.total.census|d.acres$Acres.irrigated.census),]

# NOTE: assuming that NA in acres.irrigated column implies data deficient, setting to zero
#d.acres$Acres.irrigated[is.na(d.acres$Acres.irrigated)] <- 0

d.acres <- d.acres %>%
  mutate(Percent.irrigated.census = 100*(Acres.irrigated.census/Acres.total.census))
# NOTE: Check on 100% irrigated counties for data quality
# UPDATE: 100% irrigated counties appear to all be in arid locations (i.e. TX, MT, CA)

d.acres %>%
  group_by(FIPS_county) %>%
  #filter(all(c(NA, "100") %in% Percent.irrigated)) -> test
  filter(all(Percent.irrigated %in% NA)) -> test

d.acres %>%
  group_by(state_alpha,county_name,county_ansi,FIPS_county) %>%
  mutate(Mean.irrigated = round(mean(Percent.irrigated, na.rm = T),digits = 4),
            Irrigated.var = round(var(Percent.irrigated, na.rm = T),digits = 4)) %>%
  ungroup() -> test


ggplot(data = d.acres, aes(x=as.numeric(Year), y= Acres.total, group=FIPS_county))+
  geom_line(size = 0.5, alpha = 0.3)

# Create filter to select counties that are 5 percent or less irrigated, 
# choice of 5 percent based on dsitribution of percentages, vast majority of counties are 5 percent or less irrigated
FIPS.filter <- d.acres$FIPS_county[d.acres$Percent.irrigated <= 5]

d <- d %>%
  filter(FIPS_county %in% FIPS.filter) %>%
  rename(Year = year, State = state_name, County.code = county_code, 
         County.name = county_name, FIPS = state_fips_code, Yield.bu.acre = Value) %>%
  select(FIPS_county, Year, State, County.name, FIPS, County.code, county_ansi, Yield.bu.acre)

d$Yield.bu.acre <- as.numeric(gsub(pattern = ",", replacement = "", d$Yield.bu.acre))

# NOTE: Need to decide minimum number of yield obs by county to filter by to remove counties that are not corn-growing areas
# alternatively filter to counties in specific USDA regions.....

d <- d %>% 
  group_by(FIPS_county) %>%
  mutate(n = length(unique(Year))) %>%
  filter(n >= 10)






