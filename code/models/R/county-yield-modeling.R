###################
# Yield modeling  #
# Stephen Wood    #
# 10/4/18         #
###################

##### ADVICE FROM TREVOR #####
# Generate model for 50% of data and predict other 50% - how well does it do?
# Take out that part of the model and re-run. How much does the predictive capacity change?


library(tidyverse)
library(lme4)
library(mgcv)
library(itsadug)
library(ggplot2)



#### READ IN DATA ####
# Drought data
load("/home/shares/soilcarbon/soilc-midwest/data/weather/DSCI_summary_stats.county.by.year.RData")
drought <- drought.summary
rm(drought.summary)

# Yield data
load("/home/shares/soilcarbon/soilc-midwest/data/yield_data_temp_05302018.RData")
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
  dplyr::select(GEOID:Order,DSCI.sum:DSCI.mode) -> all_data


#### FIT MODEL ####
all_data$Year <- as.factor(all_data$Year.x)
all_data$GEOID <- as.factor(all_data$GEOID)
all_data$H <- exp(all_data$ph_h2o_M_sl4_100m)
all_data$H.sq <- all_data$H^2

#### Are yields lower under drought years ####
# Basic model
lmer(Yield.bu.acre ~ 
     DSCI.sum + soc_M_sl4_100m + soc_M_sl4_100m:DSCI.sum + H + sand_M_sl4_100m + 
       (1|GEOID) + (1|Order), 
   data=all_data) %>% 
  summary()

lm(Yield.bu.acre ~ 
       DSCI.sum+ soc_M_sl4_100m + soc_M_sl4_100m:DSCI.sum + H + sand_M_sl4_100m + Order, 
     data=all_data) -> model
model %>% 
  arm::standardize() %>%
  summary()

interplot::interplot(m = model, var2 = "DSCI.sum", var1 = "soc_M_sl4_100m", hist = T) + 
  xlab("DSCI Drought index") + ylab("Conditional effect of SOC on yield") + theme_bw() +
  theme(
    legend.title = element_blank(),
    panel.grid = element_blank(),
    panel.background = element_rect(fill="white"),
    plot.background = element_rect(fill="white")
  )

## 1.31.2019, commenting out the spatiotemporal models
# # Spatial model
# 
# # Temporal model
# ### Extract first lag in time series and assign it to A1 correlation parameter
# m1 <- lm(Yield.bu.acre ~ DSCI.mean + soc_M_sl4_100m + soc_M_sl4_100m:DSCI.mean + H + 
#            sand_M_sl4_100m + Order, data=all_data)
# r1 <- start_value_rho(m1, lag=2)
# acf(resid(m1), plot=FALSE)$acf[1]
# 
# ## Generate specific time series
# simdat <- start_event(as.data.frame(all_data), column="Year.x", event="GEOID")
# 
# ## Fit time series
# m1AR1 <- bam(Yield.bu.acre ~ DSCI.mean + soc_M_sl4_100m + soc_M_sl4_100m:DSCI.mean + sand_M_sl4_100m, 
#              rho = r1, data=simdat, AR.start=simdat$start.event)
# acf_resid(m1)
# acf_resid(m1AR1)

  
###
library(caret)
train.index <- createDataPartition(all_data$Yield.bu.acre, p = .8,times = 1, list = F)
train.df <- all_data[train.index,]
test.df <- all_data[-train.index,]

grid.control <- trainControl(
  method = "repeatedcv",
  number = 10,
  repeats = 3,
  search = "grid")

metric <- "RMSE"

tunegrid <- expand.grid(.mtry=seq(from = 1, to = 5, by=1))

rf.yield <- train(Yield.bu.acre ~ 
                             DSCI.mean + soc_M_sl4_100m + soc_M_sl4_100m + H + sand_M_sl4_100m + 
                             Order, 
                           data = train.df,
                           method="rf",
                           metric=metric,
                           tuneGrid=tunegrid,
                           trControl=grid.control,
                           importance=T,
                           keep.forest=T)

var.imp <- varImp(rf.yield)[[1]]
var.imp$variable <- row.names(var.imp)

ggplot(data = var.imp, aes(y= variable, x= Overall))+
  geom_point()
  
