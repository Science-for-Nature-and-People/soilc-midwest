
# Load libraries
library(tidyverse)
library(tigris)
library(spacetime)

# Load data
counties <- tigris::counties("Illinois", cb = TRUE)
load('data-subset/IL_toy_data_05112018.RData')

# Subset original data frame for the counties that have data in time series
counties.new <- subset(counties, NAME %in% unique(d.IL$NAME))

# Turn temporal data into temporal variable
d.IL$Year <- as.POSIXct(paste(d.IL$Year, "-01-01", sep=""), tz = "GMT")
d.IL <- d.IL[order(d.IL['Year']),]

# Remove non temporal data
IL.time <- select(d.IL,NAME,Year,Yield.bu.acre)
IL.space.time <- gather(IL.time,measurement=Yield.bu.acre)
names(IL.space.time)[1] <- 'County'

# Create actual time variable
spatial.counties <- STFDF(counties.new, unique(d.IL$Year),data=IL.space.time)

