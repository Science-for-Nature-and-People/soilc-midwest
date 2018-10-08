#######################################
# Call to get drought data by county  #
# Dan Kane & Steve Wood               #
# 10/1/2018                           #
#######################################

library(httr)
library(tidyverse)
library(plyr)

# Read in unique county FIPS codes

counties <- read_csv("data/Unique_counties.csv", col_types = cols(x = col_character()))$x

# Create list of URLs for API calls for each county

URL_by_county <- lapply(counties, function(x) paste("http://usdmdataservices.unl.edu/api/CountyStatistics/GetDroughtSeverityStatisticsByAreaPercent?aoi=",x,",01001&startdate=1/1/1998&enddate=12/31/2017&statisticsType=2", sep = ""))

# Retrieve data via API and resort into dataframe

data_by_county <- lapply(URL_by_county, function(x) httr::content(httr::GET(url = x)))
data_by_county_2 <- ldply(data_by_county, function(x) ldply(x, function(x) data.frame(t(unlist(x)))))

# Save raw data

save(list = c("data_by_county_2"), file = "data/drought_by_county.RData")



# READ IN DATA
fips <- read_csv("data/Unique_counties.csv")




################################################
## DOESNT WORK BECAUSE OF URL LENGTH RESTRICTIONS

# MANIPULATE DATA
fips.string <- paste(fips$x, collapse=",")

# CREATE PARAMETERS
url <- "http://usdmdataservices.unl.edu/api/CountyStatistics/GetDroughtSeverityStatisticsByAreaPercent"

# MAKE REQUESTS
y <- GET(url, 
         query=list(
           aoi=I(fips.string),
           startdate=I("1/1/1998"),
           enddate=I("12/31/2017"),
           statisticsType="1"
           )
         )
    