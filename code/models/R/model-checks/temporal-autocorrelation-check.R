###############################################
# Code to check autocorrelation in data set   #
# Written by: Steve Wood                      #
# Last update: May 31, 2018                   #
###############################################

# LOAD LIBRARIES
library(tidyverse)
library(itsadug)    # For checking temporal autocorrelation
library(mgcv)       # For mixed GAM modeling

# LOAD DATA
load('data/yield_data_temp_05302018.RData')

# PLOT DATA THROUGH TIME
ggplot(d,aes(y=Yield.bu.acre,x=Year)) + geom_point(cex=0.5) + theme_bw()

# CHECK TEMPORAL AUTOCORRELATION
## Aggregate all counties to one yield value per year and plot through time
d.agr <- d[,c('Year','Acres.harvested','Yield.bu.acre')] 
d.agr <- aggregate(. ~ Year, median, data=d.agr)
ggplot(d.agr,aes(y=Yield.bu.acre,x=Year)) + geom_point() + theme_bw()

## Check for temporal autocorrelation in yield
d.agr <- arrange(d.agr, desc(-Year))
d.new <- d.agr$Yield.bu.acre
a <- d.new[-length(d.new)]
b <- d.new[-1]
plot(a,b,xlab="t",ylab="t-1")
cor(a,b) #0.51
acf(d.new)

## Check for temporal autocorrelation without collapsing data
lm1 <- lm(Yield.bu.acre ~ as.factor(Year) + as.factor(FIPS_county), data=d)
plot(lm1)
acf(resid(lm1))

## Compare time series to non time series
m1 <- bam(Yield.bu.acre ~ as.factor(Year)+as.factor(FIPS_county), data=d)

acf(resid(m1), main="acf(resid(m1))")
acf(resid_gam(m1), main="acf(resid_gam(m1))")
acf_resid(m1, main="acf_resid(m1)")

### Extract first lag in time series and assign it to A1 correlation parameter
r1 <- start_value_rho(m1, plot=TRUE)
acf(resid(m1), plot=FALSE)$acf[2]

## Generate specific time series
simdat <- start_event(as.data.frame(d), column="Year", event="FIPS_county")

## Fit time series
m1AR1 <- bam(Yield.bu.acre ~ as.factor(Year)+as.factor(FIPS_county), rho = r1, data=simdat, AR.start=simdat$start.event)
acf_resid(m1)
acf_resid(m1AR1)
