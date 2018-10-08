#######################################################
# Code to check spatial autocorrelation in data set   #
# Written by: Steve Wood  & Jeff Evans                #
# Last update: June 1, 2018                           #
#######################################################

# LOAD LIBRARIES
library(tidyverse)
library(sf)
library(spdep)
#library(broom)
#library(spacetime)

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
  dplyr::select(FIPS_county,Year,Yield.bu.acre) %>%
  nest(-FIPS_county) %>%
  mutate(fit = map(data, ~ median(predict(loess(Yield.bu.acre ~ Year, span = 0.91, data = .)))))

## Collapse to county-level yield score
c.data <- dplyr::select(d,FIPS_county,Year,Yield.bu.acre) %>% 
  spread(Year,Yield.bu.acre) %>% 
  cbind(unlist(model$fit)) %>%
  dplyr::select(FIPS_county,`unlist(model$fit)`)
names(c.data)[2] <- 'median.yield'

## Merge with spatial data frame and drop NA values
combined <- sp::merge(sub.counties,c.data,by.x="GEOID",by.y="FIPS_county") %>%
  subset(is.na(median.yield)==FALSE)

## Plot county median yield
brks <- quantile(combined$median.yield, seq(0,1,1/7))
cols <- grey((length(brks):2)/length(brks))
dens <- (2:length(brks))*3
maps::map('usa')
plot(combined, col=cols[findInterval(combined$median.yield, brks, all.inside=TRUE)] , add = T)


# EVALUATE GLOBAL AND LOCAL AUTOCORRELATION
## Functions for Global and Local Moran's I and Monte Carlo simulated distribution
morans.index <- function(x, y = NULL, W){
  if(is.null(y)) y = x
  xp <- (x - mean(x, na.rm=T))/sd(x, na.rm=T)
  yp <- (y - mean(y, na.rm=T))/sd(y, na.rm=T)
  W[which(is.na(W))] <- 0
  n <- nrow(W)
  global <- (xp%*%W%*%yp)/(n - 1)
  local  <- (xp*W%*%yp)
  list(global = global, local  = as.numeric(local))
}
morans_mc <- function(x, y = NULL, W, nsims = 1000){
  if(is.null(y)) y = x
  n = nrow(W)
  IDs = 1:n
  xp <- (x - mean(x, na.rm=T))/sd(x, na.rm=T)
  W[which(is.na(W))] <- 0
  global_sims = NULL
  local_sims  = matrix(NA, nrow = n, ncol=nsims)
  ID_sample = sample(IDs, size = n*nsims, replace = T)
  y_s = y[ID_sample]
  y_s = matrix(y_s, nrow = n, ncol = nsims)
  y_s <- (y_s - apply(y_s, 1, mean))/apply(y_s, 1, sd)
  global_sims  <- as.numeric( (xp%*%W%*%y_s)/(n - 1) )
  local_sims  <- (xp*W%*%y_s)
  list(global_sims = global_sims,
       local_sims  = local_sims)
}

## Derive Moran's-I (Global and Local) index and its simulated distribution
### Queens-case adjacency matrix (Wij)
nb <- spdep::nb2listw(poly2nb(combined), style = "B", 
                      zero.policy = TRUE)
Wij <- as(nb, "symmetricMatrix")
Wij <- as.matrix(Wij/rowSums(Wij))
Wij[which(is.na(Wij))] <- 0

# Calculate Moran's index and simulated distribution	  
m <- morans.index(x = combined$median.yield, W = Wij)
m[[1]]             # global Moran's-I
( mi <- m[[2]] )   # Local Indicators of Autocorrelation (LISA)

# Simulated values  
local.sim <- morans_mc(x = combined$median.yield, W = Wij)$local_sims

# Identifying the significant values and assign to data 
alpha <- 0.05                    # for a 95% confidence interval
probs <- c(alpha / 2, 1 - alpha / 2)
intervals <- t( apply(local.sim, 1, function(x) quantile(x, probs=probs)))
sig <- ( mi < intervals[,1] )  | ( mi > intervals[,2] )
combined  <- st_as_sf(combined)
combined$sig <- sig

# Identifying the LISA clusters
y <- (combined$median.yield - mean(combined$median.yield)) /
  sd(combined$median.yield)
clusters <- as.character( interaction(y > 0, Wij%*%y > 0) ) 
clusters <- gsub("TRUE", "High", clusters)
clusters <- gsub("FALSE", "Low", clusters)
clusters[combined$sig == 0] <- "Not significant"
combined$clusters <- clusters

# Plot significant LISA clusters
lisa.cols <- ifelse(combined$clusters == "High.High", "red",
                    ifelse(combined$clusters == "High.Low", "pink",
                           ifelse(combined$clusters == "Low.High", "lightblue",
                                  ifelse(combined$clusters == "Low.Low", "darkblue",
                                         ifelse(combined$clusters == "Not significant", "grey95", NA)))))
plot(raster::extent(combined), main="Local Autocorrelation Clusters")                                                    
plot(combined, col=lisa.cols, add = TRUE)
legend("bottomright", legend=c("High-High","High-Low","Low-High","Low-Low", "Not significant"),
       fill=c("red","pink","lightblue","darkblue", "grey95"), bg="white")







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

