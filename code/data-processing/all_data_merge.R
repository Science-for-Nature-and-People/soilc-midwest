###################
# Data merge      #
###################


#### READ PACKAGES ####
library(sf)
library(tigris)
library(tidyverse)
library(lme4)
library(mgcv)
library(itsadug)
library(reghelper)
library(caret)
library(parallel)

#### READ IN DATA ####
# Yield data
yield <- read_rds("data/yield_08062020.rds")

# DSCI data
load("/home/shares/soilcarbon/soilc-midwest/data/weather/DSCI_summary_stats.county.by.year.RData")

drought <- drought.summary %>%
  mutate(year = as.numeric(year))

rm(drought.summary)

# SPEI data
spei <- read_rds("data/weather/spei.rds")

# gSSURGO soil data 
soil <- read_rds("data/soil/gssurgo.soil.stats.rds")

# RMA risk data
RMA <- read_rds("data/RMA.rds")


# Merge all data frames
all_data <-yield %>% 
  left_join(RMA) %>%
  left_join(soil) %>%
  left_join(drought) %>%
  left_join(spei)
  

# Remove individual data frames
rm(drought); rm(yield); rm(soil); rm(RMA); rm(spei)

# Define new variables

all_data <- all_data %>%
  replace_na(
    list(
      indemnity_drought = 0, 
      loss_cost = 0,
      liability_drought = 0,
      subsidy_drought = 0
  )) %>%
  group_by(GEOID) %>%
  filter(!any(Yield_mg_ha == 0)) %>%
  ungroup(.) %>%
  filter(!is.na(ssurgo_soc_mean), 
         !is.na(summer_spei), 
         ssurgo_om_mean < 10, # Remove super high OM county, must be soil processing error
         ssurgo_clay_mean > 1) %>% # Remove very low clay, high sand county (PINE, MN), must be soil processing error
  distinct(.) %>%
  mutate(loss_cost = case_when(loss_cost < 0 ~ 0,
                   TRUE ~ .$loss_cost),
         GEOID = as.factor(GEOID),
         ssurgo_silt_clay_median = ssurgo_silt_median+ssurgo_clay_median,
         ssurgo_silt_clay_mean = ssurgo_silt_mean+ssurgo_clay_mean,
         ssurgo_h = 10^(-1*(ssurgo_ph_mean)),
         ssurgo_h_square = (ssurgo_h^2))

# Generate standardized dataframe ####
# basically a z-score
scale.2sd <- function(x){
  (x-mean(x))/(2*sd(x))
}

all.data.stan <- all_data %>%
  mutate_at(.vars = vars(c(17:47)),.funs = function(x) {if(is.numeric(x)) as.vector(scale.2sd(x)) else x})


spei.mean <- mean(all_data$summer_spei)
spei.2sd <- mean(all_data$summer_spei)-(2*sd(all_data$summer_spei))
spei.1sd <- mean(all_data$summer_spei)-(sd(all_data$summer_spei))


all_data <- all_data %>%
  mutate(spei.cut = cut(
    summer_spei,
    breaks = c(-Inf, spei.2sd, spei.1sd, spei.mean, Inf),
    #labels = c("Very severe", "Severe", "Moderate", "Normal")
  ))

all.data.stan <- all.data.stan %>% 
  left_join(all_data %>% select(year, GEOID, spei.cut))

saveRDS(all_data, "data/all_data_2020.08.07.rds")
saveRDS(all.data.stan, "data/all_data_stan_2020.08.07.rds")
