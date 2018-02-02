library(rnassqs)

setwd("/home/shares/soilcarbon/soilc-midwest/code/")

api_key <- as.character(read.table("code/NASS_API_key.txt", header = F)[1,1])       # api key
# Specify the range of years across which you want to collect data
years <- as.list(2000:2017)
d <- plyr::ldply(years, function(x){
  nassqs(params = list("short_desc" = "CORN, GRAIN - YIELD, MEASURED IN BU / ACRE", 
                       "year"=x) , key = api_key) 
})

d$FIPS_county <- paste(d$data.state_fips_code, d$data.county_code, sep = "")
# Drop data that are not at county level (i.e. summarized by region within state)
d <- d[-which(nchar(d$FIPS_county) == 2),]


d.acres.irrigated <-  nassqs(params = list("short_desc" = "CORN, GRAIN, IRRIGATED - ACRES HARVESTED", 
                                           "source_desc" = "CENSUS",
                                           "year"=2012) , key = api_key) 

d.acres.irrigated$FIPS_county <- paste(d.acres.irrigated$data.state_fips_code, d.acres.irrigated$data.county_code, sep = "")
d.acres.irrigated <- d.acres.irrigated[-which(nchar(d.acres.irrigated$FIPS_county) == 2),]
d.acres.irrigated$data.Value <- as.numeric(gsub(pattern = ",", replacement = "", d.acres.irrigated$data.Value))

d.acres.total <-  nassqs(params = list("short_desc" = "CORN, GRAIN - ACRES HARVESTED", 
                                       "source_desc" = "CENSUS", "sector_desc" = "CROPS",
                                       "year"=2012) , key = api_key) 
d.acres.total$FIPS_county <- paste(d.acres.total$data.state_fips_code, d.acres.total$data.county_code, sep = "")
d.acres.total <- d.acres.total[-which(nchar(d.acres.total$FIPS_county) == 2),]
d.acres.total$data.Value <- as.numeric(gsub(pattern = ",", replacement = "", d.acres.total$data.Value))

library(dplyr)

d.acres.irrigated <- d.acres.irrigated %>% 
  rename(Acres.irrigated = data.Value, Year = data.year) %>%
  select(FIPS_county, Acres.irrigated, Year)

d.acres.total <- d.acres.total %>% 
  rename(Acres.total = data.Value, Year = data.year) %>%
  select(FIPS_county, Acres.total, Year)

d.acres <- d.acres.total %>%
  left_join(d.acres.irrigated, by = c("FIPS_county", "Year"))

d.acres <- d.acres[!is.na(d.acres$Acres.total|d.acres$Acres.irrigated),]

# NOTE: assuming that NA in acres.irrigated column implies data deficient, setting to zero
d.acres$Acres.irrigated[is.na(d.acres$Acres.irrigated)] <- 0

d.acres <- d.acres %>%
  mutate(Percent.irrigated = 100*(Acres.irrigated/Acres.total))
# NOTE: Check on 100% irrigated counties for data quality
FIPS.filter <- d.acres$FIPS_county[d.acres$Percent.irrigated < 20]

d <- d %>%
  filter(FIPS_county %in% FIPS.filter) %>%
  rename(Year = data.year, State = data.state_name, County.code = data.county_code, 
         County.name = data.county_name, FIPS = data.state_fips_code, Yield.bu.acre = data.Value) %>%
  select(FIPS_county, Year, State, County.name, FIPS, County.code, Yield.bu.acre)

d$Yield.bu.acre <- as.numeric(gsub(pattern = ",", replacement = "", d$Yield.bu.acre))

# NOTE: Need to decide minimum number of yield obs by county to filter by to remove counties that are not corn-growing areas
# alternatively filter to counties in specific USDA regions.....

d <- d %>% 
  group_by(FIPS_county) %>%
  mutate(n = length(unique(Year))) %>%
  filter(n >= 10)






