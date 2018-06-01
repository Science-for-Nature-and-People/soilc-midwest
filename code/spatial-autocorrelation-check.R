
# LOAD LIBRARIES
library(tidyverse)
library(spdep)
library(broom)
library(spacetime)

# LOAD DATA
counties <- tigris::counties(year = 2017)
load('data/yield_data_temp_05302018.RData')

# MANIPULATE DATA
## Subset original data frame for the counties that have data in time series
sub.counties <- subset(counties, GEOID %in% unique(d$FIPS_county))

# SPATIAL AUTOCORRELATION CHECKING
## Regress yield against year for each county
model <- d %>% 
  as.tibble() %>%
  select(FIPS_county,Year,Yield.bu.acre) %>%
  nest(-FIPS_county) %>%
  mutate(fit = map(data, ~ median(predict(loess(Yield.bu.acre ~ Year, span = 0.91, data = .)))))

## Collapse to county-level yield score
c.data <- select(d,FIPS_county,Year,Yield.bu.acre) %>% 
  spread(Year,Yield.bu.acre) %>% 
  cbind(unlist(model$fit)) %>%
  select(FIPS_county,`unlist(model$fit)`)
names(c.data)[2] <- 'median.yield'

## Merge with spatial data frame and drop NA values
combined <- sp::merge(sub.counties,c.data,by.x="GEOID10",by.y="FIPS_county") %>%
  subset(is.na(median.yield)==FALSE)

## Plot county median yield
brks <- quantile(combined$median.yield, seq(0,1,1/7))
cols <- grey((length(brks):2)/length(brks))
dens <- (2:length(brks))*3
maps::map('usa')
plot(combined, col=cols[findInterval(combined$median.yield, brks, all.inside=TRUE)] , add = T)

## Define who is neighbors with who
## Do this with basic approach and nearest neighbors with a fairly flexible k
w1 <- combined %>% poly2nb() %>% nb2listw(style="W",zero.policy=TRUE) # Even with zero.policy=T I get NAs
IDs <- row.names(as(combined, "data.frame"))
coords <- sp::coordinates(combined)
w2 <- knn2nb(knearneigh(coords, k = 4), row.names = IDs) %>% 
        nb2listw(style='B')

## Compute spatially lagged yield values
yield.lag <- lag.listw(w2, combined$median.yield)

## Use Moran's I to check for correlations
moran.test(combined$median.yield,w2)
moran.mc(combined$median.yield, w2, nsim=599) %>%
  plot()









# # Create spatio-temporal object
# # Subset original data frame for the counties that have data in time series
# counties.new <- subset(counties, NAME %in% unique(d.IL$NAME))
# 
# # Turn temporal data into temporal variable
# d.IL$Year <- as.POSIXct(paste(d.IL$Year, "-01-01", sep=""), tz = "GMT")
# d.IL <- d.IL[order(d.IL['Year']),]
# 
# # Remove non temporal data
# IL.time <- select(d.IL,NAME,Year,Yield.bu.acre)
# IL.space.time <- gather(IL.time,measurement=Yield.bu.acre)
# names(IL.space.time)[1] <- 'County'
# 
# # Create actual time variable
# spatial.counties <- STFDF(counties.new, unique(d.IL$Year),data=IL.space.time)

