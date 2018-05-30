###############################################
# Code to check autocorrelation in data set   #
# Written by: Steve Wood                      #
# Last update: May 31, 2018                   #
###############################################

# Load libraries
library(tidyverse)
library(itsadug)    # For checking temporal autocorrelation
library(mgcv)       # For mixed GAM modeling

# Load data
load('data-subset/IL_toy_data_05112018.RData')

# Plot all yield through time
ggplot(d.IL,aes(y=Yield.bu.acre,x=Year)) + geom_point(cex=0.5) + theme_bw()

# Plot acres harvested through time
ggplot(d.IL,aes(y=Acres.harvested,x=Year)) + geom_point(cex=0.5) + theme_bw()

# Aggregate counties to one yield value and plot through time
d.IL.agr <- d.IL[,c('Year','Acres.harvested','Yield.bu.acre')] 
d.IL.agr <- aggregate(. ~ Year, mean, data=d.IL.agr)
ggplot(d.IL.agr,aes(y=Yield.bu.acre,x=Year)) + geom_point() + theme_bw()
ggplot(d.IL.agr,aes(y=Acres.harvested,x=Year)) + geom_point() + theme_bw()

# Check for temporal autocorrelation in both yield and acres harvested
d.IL.agr <- arrange(d.IL.agr, desc(-Year))
d <- d.IL.agr$Yield.bu.acre
a <- d[-length(d)]
b <- d[-1]
plot(a,b,xlab="t",ylab="t-1")
cor(a,b) #0.33
acf(d)

d2 <- d.IL.agr$Acres.harvested
a <- d2[-length(d2)]
b <- d2[-1]
plot(a,b,xlab="t",ylab="t-1")
cor(a,b) #0.65
acf(d2)

# Checking for temporal autocorrelation without collapsing data
acf(resid(lm(Yield.bu.acre ~ as.factor(Year) + NAME, data=t.dat)))
acf(resid(lm(Acres.harvested ~ Year + NAME,data=t.dat)))

## Looks like acres harvested has temporal errors, but not yield
## Lets look some more
m1 <- bam(Acres.harvested ~ as.factor(Year)+NAME, data=t.dat)

par(mfrow=c(1,3), cex=1.1)
# default ACF function:
acf(resid(m1), main="acf(resid(m1))")
# resid_gam:
acf(resid_gam(m1), main="acf(resid_gam(m1))")
# acf_resid:
acf_resid(m1, main="acf_resid(m1)")

par(mfrow=c(1,1), cex=1.1)
r1 <- start_value_rho(m1, plot=TRUE)
acf(resid(m1), plot=FALSE)$acf[2]

m1AR1 <- bam(Acres.harvested ~ as.factor(Year)+NAME, rho = r1, data=t.dat, AR.start=t.dat$start.event)
par(mfrow=c(1,2), cex=1.1)
acf_resid(m1)
acf_resid(m1AR1)
