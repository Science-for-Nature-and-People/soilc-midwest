library(tidyverse)

# RMA risk data

RMA <- read.csv("data/weather/RMA_risk/RMA_corn_drought_1989_2017.csv") %>%
  mutate(State = sprintf(fmt = "%02d", State),
         County = sprintf(fmt = "%03d", County),
         Year = as.numeric(Year)) %>%
  unite("GEOID", State, County, sep = "") %>%
  select(-State.name, -County.name) %>%
  mutate(indemnity_drought = case_when(Payment.indemnity..US.. < 0 ~ 0,
                                                         TRUE ~ as.numeric(.$Payment.indemnity..US..)),
         loss_cost = ((Payment.indemnity..US../Liability..US..)*100),
         liability_drought = Liability..US..,
         subsidy_drought = Subsidy..US.., 
         year = Year) %>%
  dplyr::select(GEOID, year, indemnity_drought, loss_cost, liability_drought, subsidy_drought) %>%
  filter(year %in% 2000:2016)

write_rds(RMA, "data/RMA.rds")
