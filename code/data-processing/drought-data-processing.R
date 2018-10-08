#############################
# Drought processing        #
# Stephen Wood  & Dan Kane  #
# 10/5/18                   #
#############################


library(tidyverse)
getmode <- function(v) {
  uniqv <- unique(v)
  uniqv[which.max(tabulate(match(v, uniqv)))]
}

#### READ DATA ####
load("data/drought_by_county.RData")
drought <- data_by_county_2
rm(data_by_county_2)

#### COMBINE CLASSES into DSCI per UNL's instructions ####

drought[,5:10] <- lapply(drought[,5:10], function(x) as.numeric(as.character(x)))

drought %>%
  mutate(DSCI = 
           1*D0+2*D1+3*D2+4*D3+5*D4) -> drought

### Split release date into Y-M-D and subset to growing season

drought$ReleaseDate <- as.character(drought$ReleaseDate)

drought %>%
  separate(ReleaseDate, into = c("Year","Month","Day"), sep = c(4,6)) %>%
  filter(as.numeric(Month) %in% c(4:9)) -> drought
  
drought %>%
  group_by(Year, FIPS, State, County) %>%
  dplyr::summarise(DSCI.mean = mean(DSCI), DSCI.median = median(DSCI), DSCI.mode = getmode(DSCI)) -> 
  drought.summary
  
drought %>%
  group_by(Year, FIPS, State, County) %>%
  mutate(Drought_threshold = 
  length(.[DSCI > 200,])) -> test
  










