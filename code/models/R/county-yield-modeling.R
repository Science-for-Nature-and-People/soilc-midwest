###################
# Yield modeling  #
# Stephen Wood    #
# 10/4/18         #
###################


#### READ PACKAGES ####
library(sf)
library(tigris)
library(tidyverse)
library(lme4)
library(mgcv)
library(itsadug)
library(reghelper)

#### READ IN DATA ####
# Drought data
load("/home/shares/soilcarbon/soilc-midwest/data/weather/DSCI_summary_stats.county.by.year.RData")

drought.summary %>%
  ungroup(.) %>%
  select(-State, -County) %>%
  mutate(Year = as.numeric(Year)) -> drought
rm(drought.summary)

# Yield data
load("/home/shares/soilcarbon/soilc-midwest/data/yield_data_temp_05302018.RData")
rm(years); rm(census.years); rm(d.save); rm(d.acres.irrigated); rm(d.acres.summary); rm(d.acres.total); rm(d.acres)
yield <- d
rm(d)

# Join soil grids and gSSURGO soil data into one data frame
soil <- readRDS("/home/shares/soilcarbon/soilc-midwest/data/soil/county-soilgrids.rds") %>%
  select(-Value) %>%
  rename("GEOID" = FID) %>%
  rename_at(.vars = vars(2:7), .funs = function(x) paste("soilgrids", x, sep = "_")) %>%
  left_join(readRDS("data/soil/gssurgo.soil.stats.rds")) %>%
  mutate(SSURGO_taxorder_mode = dplyr::recode(SSURGO_taxorder_mode, 
         '1' = "Alfisol",
         '2' = "Entisols",
         '3' = "Histosols",
         '4' = "Inceptisols",
         '5' = "Mollisols",
         '6' = "Spodosols",
         '7' = "Ultisols",
         '8' = "Vertisols",
         '9' = "Aridisols"))


# Spatial data
counties <- tigris::counties(year = 2017)
sub.counties <- subset(counties, GEOID %in% unique(yield$FIPS_county))

# RMA risk data

RMA <- read.csv("data/weather/RMA_risk/RMA_corn_drought_1989_2017.csv") %>%
  rename_at(.,.vars = 6:10, .funs = 
              function(x){paste("RMA.drought.", x, sep = "")}) %>%
  full_join(
    read.csv("data/weather/RMA_risk/RMA_corn_precip_1989_2017.csv") %>%
      rename_at(.,.vars = 6:10, .funs = 
                  function(x){paste("RMA.precip.", x, sep = "")})
  ) %>%
  mutate(State = sprintf(fmt = "%02d", State),
         County = sprintf(fmt = "%03d", County),
         Year = as.numeric(Year)) %>%
  unite("GEOID", State, County, sep = "") %>%
  select(-State.name, -County.name) %>%
  mutate(RMA.drought.Payment.indemnity..US.. = case_when(RMA.drought.Payment.indemnity..US.. < 0 ~ 0,
                                                         TRUE ~ as.numeric(.$RMA.drought.Payment.indemnity..US..)),
         RMA.precip.Payment.indemnity..US.. = case_when(RMA.precip.Payment.indemnity..US.. < 0 ~ 0,
                                                         TRUE ~ as.numeric(.$RMA.precip.Payment.indemnity..US..)),
         loss_cost = ((RMA.drought.Payment.indemnity..US../RMA.drought.Liability..US..)*100))

# SPEI data
spei <- as_tibble(readr::read_csv("data/weather/SPEI/SPEI.csv", col_types = list(col_character(),
                                                                                 col_character(),
                                                                                 col_character(),
                                                                                 col_double(),
                                                                                 col_double(),
                                                                                 col_double()))) %>%
  mutate(fips = case_when(str_length(fips) == 4 ~ paste("0",.$fips, sep = ""),
                          TRUE ~ .$fips)) %>%
  filter(fips %in% yield$FIPS_county,
         year >= 1997) %>%
  dplyr::select(-state, -county) %>%
  group_by(fips, year) %>%
  summarize(annual_spei = mean(spei),
         july_spei = mean(subset(spei, month == 7)),
         summer_spei = mean(subset(spei, month %in% 5:8))) %>%
  ungroup(.)


  
#### MANIPULATE DATA ####
names(yield)[1] <- "GEOID"
names(drought)[2] <- "GEOID"
names(spei)[1] <- "GEOID"
names(spei)[2] <- "Year"

# Merge all data frames
all_data <-yield %>% 
  dplyr::select(GEOID:FIPS,Acres.harvested:Yield.bu.acre) %>%
  left_join(soil) %>%
  left_join(drought) %>%
  left_join(spei) %>%
  left_join(RMA) 

# Remove individual data frames
rm(drought); rm(yield); rm(counties); rm(soil); rm(RMA); rm(spei)

# Define new variables
all_data$GEOID <- as.factor(all_data$GEOID)
all_data$H <- exp(all_data$soilgrids_ph_h2o_M_sl4_100m)
all_data$H.sq <- all_data$H^2
all_data$RMA.drought.Payment.indemnity..US..[is.na(all_data$RMA.drought.Payment.indemnity..US..)] <- 0
all_data$RMA.precip.Payment.indemnity..US..[is.na(all_data$RMA.precip.Payment.indemnity..US..)] <- 0
all_data$RMA.drought.per.acre <- all_data$RMA.drought.Payment.indemnity..US../all_data$Acres.harvested
all_data$RMA.precip.per.acre <- all_data$RMA.precip.Payment.indemnity..US../all_data$Acres.harvested


saveRDS(all_data, "data/all_data_2019.08.28.rds")
