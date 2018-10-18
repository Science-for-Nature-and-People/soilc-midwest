###################
# Yield modeling  #
# Stephen Wood    #
# 10/4/18         #
###################

library(tidyverse)
library(lme4)

#### READ IN DATA ####
# Drought data
load("data/weather/DSCI_county_by_year.RData")
drought <- drought.summary
rm(drought.summary)

# Yield data
load('data/yield_data_temp_05302018.RData')
rm(years); rm(census.years); rm(d.save); rm(d.acres.irrigated); rm(d.acres.summary); rm(d.acres.total)
yield <- d
rm(d)

# County soil data
soil <- readRDS("/home/shares/soilcarbon/soilc-midwest/data/soil/county-soilgrids.rds")

# Spatial data
counties <- tigris::counties(year = 2017)
sub.counties <- subset(counties, GEOID %in% unique(yield$FIPS_county))


#### MANIPULATE DATA ####
names(yield)[1] <- "GEOID"
names(drought)[2] <- "GEOID"

# Merge all data frames
yield %>%
  select(GEOID:FIPS,Acres.harvested:Yield.bu.acre) %>%
  left_join(soil, by = "GEOID") %>%
  select(GEOID:Yield.bu.acre,ALAND:Order) %>%
  left_join(drought, by="GEOID") %>%
  select(GEOID:Order,DSCI.mean:DSCI.mode) -> all_data


#### FIT MODEL ####
all_data$Year <- as.factor(all_data$Year.x)
all_data$GEOID <- as.factor(all_data$GEOID)
all_data$H <- exp(all_data$ph_h2o_M_sl4_100m)
all_data$H.sq <- all_data$H^2

#### Are yields lower under drought years ####
pairs(all_data[,c(8,13:16,19:21)])
# Basic model
plot(Yield.bu.acre~DSCI.median, data=all_data)
lm(Yield.bu.acre ~ DSCI.mean, data=all_data) %>%
  summary()
lmer(Yield.bu.acre ~ 
     DSCI.mean + soc_M_sl4_100m + soc_M_sl4_100m:DSCI.mean + H + sand_M_sl4_100m + 
       (1|GEOID) + (1|Order), 
   data=all_data) %>% 
  summary()

# Spatial model

# Temporal model



