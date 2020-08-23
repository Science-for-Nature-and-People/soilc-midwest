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
states_sf <- get_urbn_map(map = "states", sf = TRUE)


counties_sf <- get_urbn_map("counties", sf = TRUE)

all_data_summary <- all_data %>%
  filter(spei.cut != "Normal") %>%
  group_by(GEOID) %>%
  summarise(Loss_cost = mean(loss_cost, na.rm =T),
            SOM = mean(ssurgo_om_mean),
            Yield_anom = mean(Yield_decomp_mult)) %>%
  filter(!is.na(Loss_cost)) %>%
  right_join(counties_sf, by = c(GEOID = "county_fips")) %>% 
  filter(state_abbv %in% c("PA","WI", "MI", "MN","IL","IN","IA","OH", "MO","KY")) %>% 
  sf::st_as_sf(.) 

RColorBrewer::brewer.pal(colorRampPalette(c("#fff7bc", "#662506")), n=5)

pal(5)

states_cropped <-states_sf %>%
  filter(state_abbv %in% c("PA","WI", "MI", "MN","IL","IN","IA","OH", "MO","KY"))

som.map <- ggplot(all_data_summary) +
  geom_sf(data = all_data_summary,
          mapping = aes(fill = SOM),
          color = "#ffffff", size = 0.25) +
  scale_fill_gradientn(breaks = scales::breaks_pretty(n = 5),
                       colours = RColorBrewer::brewer.pal(n = 5,"RdYlBu")) +
  labs(fill = "SOM (%)") +
  coord_sf(crs = sf::st_crs(all_data_summary))+
  geom_sf(data = states_cropped, fill = "NA", color = "black", size = 0.25)+
  theme(plot.background = element_rect(colour = "black", fill = "#f2f2f2"))

lc.map <- ggplot(all_data_summary) +
  geom_sf(data = all_data_summary,
          mapping = aes(fill = Loss_cost),
          color = "#ffffff", size = 0.25) +
  scale_fill_gradientn(breaks = scales::breaks_pretty(n = 5),
                       colours = rev(RColorBrewer::brewer.pal(n = 5,"RdYlBu"))) +
  labs(fill = "Loss   \ncost") +
  coord_sf(crs = sf::st_crs(all_data_summary))+
  geom_sf(data = states_cropped, fill = "NA", color = "black", size = 0.25)+
  theme(plot.background = element_rect(colour = "black", fill = "#f2f2f2"))

ggsave(ggpubr::ggarrange(som.map, lc.map, nrow=2, labels = c("a.","b."), widths = c(1,1), hjust = -0.15 ),
       filename = "~/som_lc.map.jpg", width = 4, height = 5, units = "in")

hist(all_data_summary$Loss_cost)

ggsave(som.map, filename = "~/som.map.jpg", width = 4, height = 2.5, units = "in")
ggsave(lc.map, filename = "~/lc.map.jpg", width = 4, height = 2.5, units = "in")


