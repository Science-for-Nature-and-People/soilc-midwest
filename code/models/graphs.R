library(tidyverse)
library(lme4)
library(mgcv)
library(itsadug)
library(reghelper)


all_data <- readRDS("data/all_data_2019.08.28.rds")

all_data <- all_data %>%
  filter(!is.na(loss_cost),
         !is.na(july_spei),
         loss_cost < 200,
         !is.na(DSCI.mean)) %>%
  group_by(GEOID) %>%
  mutate(County_avg_yield = mean(Yield.bu.acre)) %>%
  ungroup() %>% 
  mutate(Yield_diff = ((Yield.bu.acre - County_avg_yield)/County_avg_yield),
         SSURGO_soc0_30_mean = SSURGO_soc0_30_mean/1000,
         SSURGO_siltclay = SSURGO_claytotal_r_mean + SSURGO_silttotal_r_mean) %>%
  dplyr::select(Year, GEOID, FIPS, County.name, State.alpha, State,
                Yield.bu.acre,loss_cost,Yield_diff,
                starts_with("soilgrids"), starts_with("SSURGO"),
                starts_with("DSCI"), ends_with("spei"))

# NOTE - this set of filters eliminates data from the year 2017, for which no SPEI data are available, 
# and data from 1997-1999, for which there are no DSCI data

# Generate standardized dataframe

all.data.stan <- all_data %>%
  mutate_at(.vars = vars(10:37),.funs = function(x) {if(is.numeric(x)) as.vector(scale(x)) else x})


## test plots

all_data %>%
  filter(july_spei < -1,
         !is.na(loss_cost), 
         !is.na(july_spei)) %>%
  ggplot(data = ., aes(x = SSURGO_soc0_30_mean, y = Yield_diff)) +
  geom_point(alpha = 1,size = 0.7)
# facet_wrap(facets = "SSURGO_taxorder_mode", nrow = 4, ncol = 4)+
# theme(legend.position = "none")
# 



all_data %>%
  filter(july_spei < -1,
         !is.na(loss_cost), 
         !is.na(july_spei)) %>%
  ggplot(data = ., aes(x = SSURGO_soc0_30_mean, y = Yield.bu.acre, color = SSURGO_taxorder_mode)) +
  geom_point(alpha = 0.4,size = 1)+
  facet_wrap(facets = "SSURGO_taxorder_mode", nrow = 4, ncol = 4)+
  theme(legend.position = "none")

all_data_summary <- all_data %>%
  group_by(GEOID) %>%
  summarize(Corn_yield = mean(Yield.bu.acre),
            SOC = mean(SSURGO_soc0_30_mean, na.rm = T)/100) 

library(urbnmapr)
library(urbnthemes)

set_urbn_defaults(style = "map")


counties_sf <- get_urbn_map("counties", sf = TRUE)

all_data_summary <- all_data_summary %>%
  right_join(counties_sf, by = c(GEOID = "county_fips"))

all_data_summary %>% 
  ggplot() +
  geom_sf(mapping = aes(fill = SOC),
          color = "#ffffff", size = 0.25) +
  scale_fill_gradientn(labels = scales::percent) +
  labs(fill = "SOC, %") +
  coord_sf(datum = NA)

all_data %>% 
  filter(Year == 2002) %>%
  group_by(GEOID) %>%
  right_join(counties_sf, by = c(GEOID = "county_fips")) %>%
  ggplot() +
  geom_sf(mapping = aes(fill = july_spei),
          color = "#ffffff", size = 0.25) +
  scale_fill_gradientn(colours = RColorBrewer::brewer.pal(n = 10,"RdYlBu")) +
  labs(fill = "SPEI") +
  coord_sf(datum = NA)

all_data %>% 
  filter(Year == 2002) %>%
  group_by(GEOID) %>%
  right_join(counties_sf, by = c(GEOID = "county_fips")) %>%
  ggplot() +
  geom_sf(mapping = aes(fill = SSURGO_taxorder_mode),
          color = "#ffffff", size = 0.25) +
  #scale_fill_gradientn(colours = RColorBrewer::brewer.pal(n = 10,"RdYlBu")) +
  labs(fill = "SPEI") +
  coord_sf(datum = NA)
