library(tidyverse)
library(lme4)
library(mgcv)
library(itsadug)
library(reghelper)
library(urbnmapr)
library(urbnthemes)

# data #####
all_data <- readRDS("data/all_data_2020.08.07.rds")
all.data.stan <- readRDS("data/all_data_stan_2020.08.07.rds")

#map settings
set_urbn_defaults(style = "map")


counties_sf <- get_urbn_map("counties", sf = TRUE)

all_data_summary <- all_data %>%
  filter(spei.cut != "Normal") %>%
  group_by(GEOID) %>%
  summarise(Loss_cost = mean(loss_cost, na.rm =T),
            SOM = mean(ssurgo_om_mean),
            Yield_anom = mean(Yield_decomp_mult),
            yield= mean(Yield_mg_ha)) %>%
  right_join(counties_sf, by = c(GEOID = "county_fips")) %>% 
  filter(state_abbv %in% c("WI", "MI", "MN","IL","IN","IA","OH", "MO","KY"), 
         !is.na(Loss_cost)) %>% 
  sf::st_as_sf(.)

ggplot(all_data_summary) +
  geom_sf(data = all_data_summary,
          mapping = aes(fill = SOM),
          color = "#ffffff", size = 0.25) +
  scale_fill_gradientn(colours = RColorBrewer::brewer.pal(n = 8,"RdYlBu")) +
  labs(fill = "Soil organic matter (%)") +
  coord_sf(crs = sf::st_crs(all_data_summary))

ggplot(all_data_summary) +
  geom_sf(data = all_data_summary,
          mapping = aes(fill = yield),
          color = "#ffffff", size = 0.25) +
  scale_fill_gradientn(breaks = scales::breaks_pretty(n = 8),
                      colours = rev(RColorBrewer::brewer.pal(n = 8,"RdYlBu"))) +
  labs(fill = "Loss cost") +
  coord_sf(crs = sf::st_crs(all_data_summary))





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
