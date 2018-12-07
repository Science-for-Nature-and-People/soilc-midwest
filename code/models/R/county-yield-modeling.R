###################
# Yield modeling  #
# Stephen Wood    #
# 10/4/18         #
###################

library(tidyverse)
library(lme4)
library(mgcv)
library(itsadug)

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
  dplyr::select(GEOID:FIPS,Acres.harvested:Yield.bu.acre) %>%
  left_join(soil, by = "GEOID") %>%
  dplyr::select(GEOID:Yield.bu.acre,ALAND:Order) %>%
  left_join(drought, by="GEOID") %>%
  dplyr::select(GEOID:Order,DSCI.mean:DSCI.mode) -> all_data


#### FIT MODEL ####
all_data$Year <- as.factor(all_data$Year.x)
all_data$GEOID <- as.factor(all_data$GEOID)
all_data$H <- exp(all_data$ph_h2o_M_sl4_100m)
all_data$H.sq <- all_data$H^2

#### Are yields lower under drought years ####
# Basic model
lmer(Yield.bu.acre ~ 
     DSCI.mean + soc_M_sl4_100m + soc_M_sl4_100m:DSCI.mean + H + sand_M_sl4_100m + 
       (1|GEOID) + (1|Order), 
   data=all_data) %>% 
  model %>% 
  summary()

lm(Yield.bu.acre ~ 
       DSCI.mean + soc_M_sl4_100m + soc_M_sl4_100m:DSCI.mean + H + sand_M_sl4_100m + Order, 
     data=all_data) -> model
model %>% 
  arm::standardize() %>%
  summary()

interplot::interplot(m = model, var2 = "DSCI.mean", var1 = "soc_M_sl4_100m", hist = T) + 
  xlab("DSCI Drought index") + ylab("Conditional effect of SOC on yield") + theme_bw() +
  theme(
    legend.title = element_blank(),
    panel.grid = element_blank(),
    panel.background = element_rect(fill="white"),
    plot.background = element_rect(fill="white")
  )


# Spatial model

# Temporal model
### Extract first lag in time series and assign it to A1 correlation parameter
m1 <- lm(Yield.bu.acre ~ DSCI.mean + soc_M_sl4_100m + soc_M_sl4_100m:DSCI.mean + H + sand_M_sl4_100m + Order, data=all_data)
r1 <- start_value_rho(m1, lag=2)
acf(resid(m1), plot=FALSE)$acf[1]

## Generate specific time series
simdat <- start_event(as.data.frame(all_data), column="Year.x", event="GEOID")

## Fit time series
m1AR1 <- bam(Yield.bu.acre ~ DSCI.mean + soc_M_sl4_100m + soc_M_sl4_100m:DSCI.mean + sand_M_sl4_100m, 
             rho = r1, data=simdat, AR.start=simdat$start.event)
acf_resid(m1)
acf_resid(m1AR1)



