#######################################
# Call to get drought data by county  #
# Dan Kane & Steve Wood               #
# 10/1/2018                           #
#######################################

library(httr)
library(plyr)
library(tidyverse)
library(parallel)

# Read in unique county FIPS codes
#tidyverse package includes ability to run read_rds function
#current yield data
counties <- c(unique(read_rds("data/corn_yield_2000-2022_w_irrigation_n_15.rds")$GEOID))

#########################################################################################################
#Create list of URLs for API calls for each county
#This should pull weekly data from Jan 1, 2000, to December 31, 2022 
#Should pull county DSCI statistics-> no need to calculate DSCI codes in subsequent drought-data-processing.R code 
# County codes need to be five digit FIPS, corn_yield_2000-2022_w_irrigation_n_15.rds contains FIPS codes
#statistics type = {1 (traditional), 2 (categorical)}, for DSCI it doesn't matter if 1 or 2 is selected, returns the same results
#sep = “” means paste everything together
#https://droughtmonitor.unl.edu/DmData/DataDownload/WebServiceInfo.aspx#comp -> Information on how to create URL 

#Kane original lines
#counties <- c(unique(read_rds("data/yield_08062020.rds")$GEOID))
#URL_by_county <- lapply(counties, function(x) paste("http://usdmdataservices.unl.edu/api/CountyStatistics/GetDroughtSeverityStatisticsByAreaPercent?aoi=",x,"&startdate=1/1/2017&enddate=12/31/2017&statisticsType=2", sep = ""))
#########################################################################################################

#Updated URL
#lapply command applies a function over a list: lapply(list,fxn,...)
URL_by_county <- lapply(counties, function(x) paste("http://usdmdataservices.unl.edu/api/CountyStatistics/GetDSCI?aoi=",x,"&startdate=1/1/2000&enddate=12/31/2022&statisticsType=1", sep = "")) 
# Retrieve data via API and resort into dataframe

#Adjust cores as necessary
#mclapply command is a parallel version of lapply
data_by_county <- mclapply(mc.cores = 30, URL_by_county, function(x) httr::content(httr::GET(url = x)))

#ldply command is command that, for each element of a list, applies a function, then combined result into a data frame
data_by_county_2 <- ldply(data_by_county, function(x) ldply(x, function(x) data.frame(t(unlist(x)))))

# Save raw data

save(list = c("data_by_county_2"), file = "data/drought_by_county.RData")

