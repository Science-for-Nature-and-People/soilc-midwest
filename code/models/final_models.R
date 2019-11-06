library(tidyverse)
library(lme4)
library(mgcv)
library(itsadug)
library(reghelper)

all_data <- readRDS("data/all_data_2019.08.28.rds")

all_data <- all_data %>%
  group_by(GEOID) %>%
  mutate(County_avg_yield = mean(Yield.bu.acre)) %>%
  ungroup() %>% 
  mutate(Yield_diff = ((Yield.bu.acre - County_avg_yield)/County_avg_yield),
         SSURGO_soc0_30_mean = SSURGO_soc0_30_mean/1000,
         SSURGO_siltclay = SSURGO_claytotal_r_mean + SSURGO_silttotal_r_mean) %>%
  filter(!is.na(loss_cost), 
         !is.na(july_spei),
         loss_cost <200)



#### FIT MODELS ####

#### Are yields lower under drought years ####
# Basic model
# N.B. Don't use county random effects because there's no sub-county data
# all_data %>%
#   filter(july_spei < -1) %>%
# lmerTest::lmer(data = ., formula = Yield.bu.acre ~  soilgrids_soc_M_sl4_100m + 
#                  (1|soilgrids_Order)) %>%
# summary(.)
# 
# all_data %>%
#   filter(july_spei < -1,
#          !is.na(loss_cost), 
#          !is.na(july_spei)) %>%
#   lmerTest::lmer(data = ., formula = loss_cost ~  soilgrids_soc_M_sl4_100m + 
#                    (1|soilgrids_Order)) %>%
#   summary(.)
# 
# all_data %>%
#   filter(july_spei < -1,
#          !is.na(loss_cost), 
#          !is.na(july_spei)) %>%
#   lmerTest::lmer(data = ., formula = Yield_diff ~  SSURGO_soc0_30_mean + 
#                    (1|soilgrids_Order)) %>%
#   summary(.)

m3 <- lmerTest::lmer(data = all_data, formula = loss_cost ~ july_spei*SSURGO_soc0_30_mean*SSURGO_claytotal_r_mean + (1|SSURGO_taxorder_mode))

m4 <- lmerTest::lmer(data = all_data, formula = loss_cost ~ july_spei*SSURGO_soc0_30_mean*SSURGO_sandtotal_r_mean + (1|SSURGO_taxorder_mode))

summary(m3)

m4 <- glmer(data = all_data, formula = Yield.bu.acre ~ july_spei*SSURGO_soc0_30_mean*SSURGO_sandtotal_r_mean + (1|SSURGO_taxorder_mode),
            family = gaussian(link = "log"), mustart=pmax(all_data$Yield.bu.acre,1e-2))

summary(m4)


interactions::interact_plot(model = m3, modx = july_spei,modx.values = c(-3, -1.5, 0),
                            pred = SSURGO_soc0_30_mean, interval = T, int.type = "prediction", outcome.scale = "response",
                            mod2 = SSURGO_claytotal_r_mean,mod2.values = c(15,25,35),mod2.labels = c("Clay, 15%", "Clay, 25%", "Clay, 35%"),
                            plot.points = F, point.size = 0.2, jitter = 0.1,line.thickness = 1.25, 
                            legend.main = "July SPEI",
                            x.label = "SOC, 0-30 cm", y.label = "Loss cost")


interactions::interact_plot(model = m4, modx = july_spei,modx.values = c(-3, -1.5, 0),
                            pred = SSURGO_soc0_30_mean, interval = T, int.type = "prediction", outcome.scale = "response",
                            mod2 = SSURGO_sandtotal_r_mean,mod2.values = c(10,30,50),mod2.labels = c("Sand, 10%", "Sand, 30%", "Sand, 50%"),
                            plot.points = F, point.size = 0.2, jitter = 0.1,line.thickness = 1.25, 
                            legend.main = "July SPEI",
                            x.label = "SOC, 0-30 cm", y.label = "Loss cost")



m5 <- glmer(data = all_data, formula = Yield.bu.acre ~ july_spei + SSURGO_soc0_30_mean + SSURGO_soc0_30_mean:july_spei + (1|SSURGO_taxorder_mode),
            family = gaussian(link = "log"), mustart=pmax(all_data$Yield.bu.acre,1e-2))


interactions::interact_plot(model = m5, modx = july_spei,modx.values = c(-3, -1.5, 0),
                            pred = SSURGO_soc0_30_mean, interval = T, int.type = "prediction", outcome.scale = "response",
                            plot.points = T, point.size = 0.2, jitter = 0.1,line.thickness = 1.25, 
                            legend.main = "July SPEI",
                            x.label = "SOC, 0-30 cm", y.label = "Yield (bu/ac)")


m6 <- glmer(data = all_data, formula = loss_cost ~ july_spei + SSURGO_soc0_30_mean + SSURGO_soc0_30_mean:july_spei + (1|SSURGO_taxorder_mode),
            family = gaussian(link = "log"), mustart=pmax(all_data$loss_cost,1e-2))


interactions::interact_plot(model = m6, modx = july_spei,modx.values = c(-3, -1.5, 0),
                            pred = SSURGO_soc0_30_mean, interval = T, int.type = "prediction", outcome.scale = "response",
                            plot.points = T, point.size = 0.2, jitter = 0.1,line.thickness = 1.25, 
                            legend.main = "July SPEI",
                            x.label = "SOC, 0-30 cm", y.label = "Loss Cost")



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
  

devtools::install_github("UrbanInstitute/urbnmapr")
devtools::install_github("UrbanInstitute/urbnthemes")

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


