# setup chunk ####
library(rnassqs)
library(dplyr)
library(ggplot2)
library(tidyr)
library(reshape2)
library(caret)

api_key <- as.character(read.csv("code/NASS_API_key.csv", header = F)[1,1])       # api key
# Specify the range of years across which you want to collect data
years <- as.list(2000:2016)
# Call in all corn yield data via NASS API ####

nassqs_auth(key = api_key)

## Yields

d <- plyr::ldply(years, function(x){
  
  params <- list(
    commodity_desc = "CORN",
    util_practice_desc = "GRAIN",
    year = x,
    agg_level_desc = "COUNTY", 
    source_desc = "SURVEY",  # change source to source_desc
    domain_desc = "TOTAL"
  )
  
  return(
    nassqs_yields(params) %>%
      filter(
        prodn_practice_desc == "ALL PRODUCTION PRACTICES",
        county_ansi != ""
      ) %>%
      mutate(
        GEOID = paste(state_ansi, county_ansi, sep = ""),
        Yield_mg_ha = as.numeric(Value) * 0.0628
      ) %>%
      select(
        year,
        GEOID,
        state_alpha,
        state_ansi,
        county_ansi,
        county_name,
        Yield_mg_ha
      )
  )
})


### Total acres

census.years <- as.list(c(1997,2002,2007,2012))

d.acres.total <- plyr::ldply(census.years, function(x) {
  
  params <- list(
    commodity_desc = "CORN",
    util_practice_desc = "GRAIN",
    source_desc = "CENSUS",
    year = x,
    agg_level_desc = "COUNTY",
    short_desc = "CORN, GRAIN - ACRES HARVESTED",
    domain_desc = "TOTAL"
  )
  
  return(
    nassqs(params) %>%
      filter(county_ansi != "") %>%
      mutate(
        GEOID = paste(state_ansi, county_ansi, sep = ""),
        Acres_total = as.numeric(gsub(
          Value, pattern = ",", replacement = ""
        ))
      ) %>%
      select(
        year,
        GEOID,
        state_alpha,
        state_ansi,
        county_ansi,
        county_name,
        Acres_total
      )
  )
})


##### IRRIGATED ACRES

d.acres.irrigated <- plyr::ldply(census.years, function(x) {
  
  params <- list(
    commodity_desc = "CORN",
    util_practice_desc = "GRAIN",
    source_desc = "CENSUS",
    year = x,
    agg_level_desc = "COUNTY",
    short_desc = "CORN, GRAIN, IRRIGATED - ACRES HARVESTED",
    domain_desc = "TOTAL"
  )
  
  return(
    nassqs(params) %>%
      filter(county_ansi != "") %>%
      mutate(
        GEOID = paste(state_ansi, county_ansi, sep = ""),
        Acres_irrigated = as.numeric(gsub(
          Value, pattern = ",", replacement = ""
        ))
      ) %>%
      select(
        year,
        GEOID,
        state_alpha,
        state_ansi,
        county_ansi,
        county_name,
        Acres_irrigated
      )
  )
})

##

d.acres <- d.acres.total %>%
  left_join(d.acres.irrigated) %>%
  filter(GEOID %in% d$GEOID,!is.na(Acres_total)) %>%
  replace_na(list(Acres_irrigated = 0)) %>%
  mutate(Percent_irrigated = Acres_irrigated / Acres_total) %>%
  group_by(GEOID) %>%
  summarize(
    Mean.percent.irrigated = mean(Percent_irrigated),
    SD.percent.irrigated = sd(Percent_irrigated)
  )

## FILTER BASED ON IRRIGATED ACRES DATA

# Create filter to select counties that are 5 percent or less irrigated, 
# choice of 5 percent based on dsitribution of percentages, vast majority of counties are 5 percent or less irrigated

d.irrgiated.filter <- d.acres %>%
  filter(Mean.percent.irrigated <= 0.05) %>%
  filter(SD.percent.irrigated <= 0.01) 

d <- d %>%
  filter(GEOID %in% d.irrgiated.filter$GEOID) %>% #Filter to counties < 5% irrigated
  group_by(GEOID) %>%
  add_count(GEOID) %>%
  filter(n >= 15) %>% # Filter to >=15 corn yield observations
  ungroup(.) %>%
  select(-n)


mod <- function(df){
  df <- df
  
  grid <- expand.grid(span = seq(0.3, 0.5, len = 5), degree = seq(0,1, len=2) )
  
  grid.control <- trainControl(
    method = "repeatedcv",
    number = 10,
    repeats = 5,
    search = "grid")
  
  train_loess <- train(Yield_mg_ha ~ year, 
                       method = "gamLoess",
                       tuneGrid=grid,
                       trControl=grid.control,
                       data = df)
  
  df$Detrend_resids <- as.numeric(residuals(train_loess))
  df$Detrend_predictions <- as.numeric(predict(train_loess))
  return(df)
}


d_list <- split(d, f = d$GEOID)

d_list <- mclapply(X = d_list,FUN = mod, mc.cores = 40)

d <- dplyr::bind_rows(d_list)

d <- d %>%
  group_by(GEOID) %>%
  mutate(County_avg_yield = mean(Yield_mg_ha)) %>%
  ungroup(.) %>%
  mutate(Yield_decomp_add = County_avg_yield+Detrend_resids,
         Yield_decomp_mult = Yield_mg_ha/Detrend_predictions)


write_rds(d, path = "data/yield_08062020.rds")
