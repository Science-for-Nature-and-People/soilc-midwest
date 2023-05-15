#######################################
# Call to get drought data by county  #
# Dan Kane & Steve Wood               #
# 10/1/2018                           #
#######################################

library(httr)
library(tidyverse)
library(plyr)
library(parallel)

# Read in unique county FIPS codes

counties <- c(unique(read_rds("data/yield_08062020.rds")$GEOID))

# Create list of URLs for API calls for each county
#This should pull weekly data from Jan 1, 2000, to December 31, 2022 
#Should pull county DSCI statistics 
# I think x denotes county list? County codes need to be five digit FIPS 
#statistics type = {1 (traditional), 2 (categorical)} 
#not sure what sep = “” means paste everything together

#Kane original line
#URL_by_county <- lapply(counties, function(x) paste("http://usdmdataservices.unl.edu/api/CountyStatistics/GetDroughtSeverityStatisticsByAreaPercent?aoi=",x,"&startdate=1/1/1998&enddate=12/31/2017&statisticsType=2", sep = ""))

URL_by_county <- lapply(counties, function(x) paste("http://usdmdataservices.unl.edu/api/CountyStatistics/GetDSCI?aoi=",x,"&startdate=1/1/2000&enddate=12/31/2022&statisticsType=2", sep = "")) 
# Retrieve data via API and resort into dataframe

data_by_county <- mclapply(mc.cores = 30, URL_by_county, function(x) httr::content(httr::GET(url = x)))
data_by_county_2 <- ldply(data_by_county, function(x) ldply(x, function(x) data.frame(t(unlist(x)))))

# Save raw data

save(list = c("data_by_county_2"), file = "data/drought_by_county.RData")

