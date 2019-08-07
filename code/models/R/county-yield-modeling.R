###################
# Yield modeling  #
# Stephen Wood    #
# 10/4/18         #
###################


#### READ PACKAGES ####
library(sf)
library(tigris)
library(tidyverse)
library(lme4)
library(mgcv)
library(itsadug)
library(reghelper)

#### READ IN DATA ####
# Drought data
load("/home/shares/soilcarbon/soilc-midwest/data/weather/DSCI_summary_stats.county.by.year.RData")

drought.summary %>%
  ungroup(.) %>%
  select(-State, -County) %>%
  mutate(Year = as.numeric(Year)) -> drought
rm(drought.summary)

# Yield data
load("/home/shares/soilcarbon/soilc-midwest/data/yield_data_temp_05302018.RData")
rm(years); rm(census.years); rm(d.save); rm(d.acres.irrigated); rm(d.acres.summary); rm(d.acres.total); rm(d.acres)
yield <- d
rm(d)

# County soil data
soil <- readRDS("/home/shares/soilcarbon/soilc-midwest/data/soil/county-soilgrids.rds")
names(soil)[1] <- "GEOID"

# Spatial data
counties <- tigris::counties(year = 2017)
sub.counties <- subset(counties, GEOID %in% unique(yield$FIPS_county))

# RMA risk data

read.csv("data/weather/RMA_risk/RMA_corn_drought_1989_2017.csv") %>%
  rename_at(.,.vars = 6:10, .funs = 
              function(x){paste("RMA.drought.", x, sep = "")}) %>%
  full_join(
    read.csv("data/weather/RMA_risk/RMA_corn_precip_1989_2017.csv") %>%
      rename_at(.,.vars = 6:10, .funs = 
                  function(x){paste("RMA.precip.", x, sep = "")})
  ) %>%
  mutate(State = sprintf(fmt = "%02d", State),
         County = sprintf(fmt = "%03d", County),
         Year = as.numeric(Year)) %>%
  unite("GEOID", State, County, sep = "") %>%
  select(-State.name, -County.name) %>%
  mutate() -> RMA

RMA$RMA.drought.Payment.indemnity..US..[RMA$RMA.drought.Payment.indemnity..US.. < 0] <- 0
RMA$RMA.precip.Payment.indemnity..US..[RMA$RMA.precip.Payment.indemnity..US.. < 0] <- 0



#### MANIPULATE DATA ####
names(yield)[1] <- "GEOID"
names(drought)[2] <- "GEOID"

# Merge all data frames
yield %>% 
  dplyr::select(GEOID:FIPS,Acres.harvested:Yield.bu.acre) %>%
  left_join(soil) %>%
#  dplyr::select(GEOID:Yield.bu.acre,ALAND:Order) %>%
  left_join(drought) %>%
  dplyr::select(GEOID:Order,DSCI.sum:DSCI.mode) %>%
  left_join(RMA) -> all_data

# Remove individual data frames
rm(drought); rm(yield); rm(counties); rm(soil): rm(RMA)

# Define new variables
all_data$GEOID <- as.factor(all_data$GEOID)
all_data$H <- exp(all_data$ph_h2o_M_sl4_100m)
all_data$H.sq <- all_data$H^2
all_data$RMA.drought.Payment.indemnity..US..[is.na(all_data$RMA.drought.Payment.indemnity..US..)] <- 0
all_data$RMA.precip.Payment.indemnity..US..[is.na(all_data$RMA.precip.Payment.indemnity..US..)] <- 0
all_data$RMA.drought.per.acre <- all_data$RMA.drought.Payment.indemnity..US../all_data$Acres.harvested
all_data$RMA.precip.per.acre <- all_data$RMA.precip.Payment.indemnity..US../all_data$Acres.harvested


# Standardize variables



std_data <- data.frame(all_data$Yield.bu.acre)
names(std_data) <- 'yield'
std_data$drought <- (all_data$DSCI.sum - mean(all_data$DSCI.sum, na.rm = T)) / 2*sd(all_data$DSCI.sum, na.rm = T)
std_data$soc <- (all_data$soc_M_sl4_100m - mean(all_data$soc_M_sl4_100m,na.rm = T)) / 2*sd(all_data$soc_M_sl4_100m, na.rm = T)
std_data$sand <- (all_data$sand_M_sl4_100m - mean(all_data$sand_M_sl4_100m, na.rm = T)) / 2*sd(all_data$sand_M_sl4_100m, na.rm = T)
std_data$H <- (all_data$H - mean(all_data$H, na.rm = T)) / 2*sd(all_data$H, na.rm = T)
std_data$RMA.drought.per.acre <- (all_data$RMA.drought.per.acre - mean(all_data$RMA.drought.per.acre, na.rm = T)) / 2*sd(all_data$RMA.drought.per.acre, na.rm = T)
std_data$RMA.precip.per.acre <- (all_data$RMA.precip.per.acre - mean(all_data$RMA.precip.per.acre, na.rm = T)) / 2*sd(all_data$RMA.precip.per.acre, na.rm = T)

std_data$GEOID <- all_data$GEOID
std_data$Order <- as.factor(all_data$Order)
std_data$Year <- as.factor(all_data$Year)
std_data <- as.data.frame(std_data)

#### FIT MODEL ####

#### Are yields lower under drought years ####
# Basic model
# N.B. Don't use county random effects because there's no sub-county data
lmerTest::lmer(yield ~ 
       drought + soc + soc:drought + sand + (1|Order), 
     data=all_data) %>% 
  summary()

lm(yield ~ 
     RMA.drought.per.acre + soc + soc:RMA.drought.per.acre + sand + Order, 
   data=std_data) -> model
model %>% 
  summary()

lmerTest::lmer(yield ~ 
                 RMA.precip.per.acre + soc + soc:RMA.precip.per.acre + sand + (1|Order), 
               data=std_data) %>% 
  summary()

lmerTest::lmer(yield ~ 
                 RMA.drought.per.acre + soc + soc:RMA.drought.per.acre + sand + (1|Order), 
               data=std_data) %>% 
  summary()

lmerTest::lmer(RMA.drought.per.acre ~ 
                 drought + soc + soc:drought + sand + (1|Order), 
               data=std_data) %>% 
  summary()

lm(RMA.drought.per.acre ~ 
     drought + soc + soc:drought + sand, 
   data=std_data) -> model.st

all_data %>%
  filter(soc_M_sl4_100m <= 20) %>%
lm(RMA.drought.per.acre ~ 
     DSCI.sum + soc_M_sl4_100m + soc_M_sl4_100m:DSCI.sum + sand_M_sl4_100m, 
   data=.) -> model
beta(model)

interactions::interact_plot(model = model, modx = DSCI.sum,modx.values = c(0, 610, 5000, 10000),
              pred = soc_M_sl4_100m, plot.points = T)

predict()

View(test$data)

## scratch

all_data %>%
  ggplot(data = ., aes(x = DSCI.mean, y = ((RMA.drought.Payment.indemnity..US../RMA.drought.Liability..US..)), color = Year)) +
  geom_point(alpha = 0.4,size = 0.4)


all_data %>%
  ggplot(data = ., aes(x = DSCI.mean, y = RMA.drought.per.acre, color = Year)) +
  geom_point(alpha = 0.4,size = 0.4)


all_data %>%
  mutate(loss_cost = ((RMA.drought.Payment.indemnity..US../RMA.drought.Liability..US..)*100)) -> all_data

m1 <- lmerTest::lmer(data = all_data, formula = loss_cost ~ DSCI.sum + soc_M_sl4_100m + soc_M_sl4_100m:DSCI.sum + (1|Order))

summary(m1)

interactions::interact_plot(model = m1, modx = DSCI.sum,modx.values = c(0, 610, 5000, 10000),
                            pred = soc_M_sl4_100m)

all_data %>%
  ggplot(data = ., aes(x = soc_M_sl4_100m, y = loss_cost, color = Order)) +
  geom_point(alpha = 0.4,size = 0.4)








