## libraries #####

library(tidyverse)
library(lme4)
library(mgcv)
library(itsadug)
library(reghelper)

# data #####
all_data <- readRDS("data/all_data_2019.08.28.rds")

all_data$SSURGO_taxorder_mode[is.na(all_data$SSURGO_taxorder_mode)] <- "Unknown"


all_data <- all_data %>%
  filter(!is.na(july_spei),
         !is.na(DSCI.mean), 
         SSURGO_taxorder_mode != "Entisols",
         loss_cost < 300) %>%
  rename(Yield_mg_ha = Yield.bu.acre) %>%
  mutate(Yield_mg_ha = Yield_mg_ha*0.0628) %>%
  group_by(GEOID) %>%
  mutate(County_avg_yield = mean(Yield_mg_ha)) %>%
  ungroup() %>% 
  mutate(Yield_diff = ((Yield_mg_ha - County_avg_yield)/County_avg_yield)) %>%
  dplyr::select(Year, GEOID, FIPS, County.name, State.alpha, State,
                Yield_mg_ha,loss_cost,Yield_diff,
                starts_with("soilgrids"), starts_with("SSURGO"),
                starts_with("DSCI"), ends_with("spei")) %>%
  distinct(.)

# NOTE - this set of filters eliminates data from the year 2017, for which no SPEI data are available, 
# and data from 1997-1999, for which there are no DSCI data

# Generate standardized dataframe ####

scale.2sd <- function(x){
  (x-mean(x))/(2*sd(x))
}

all.data.stan <- all_data %>%
  mutate_at(.vars = vars(10:36),.funs = function(x) {if(is.numeric(x)) as.vector(scale.2sd(x)) else x})

# yield models ####

m <- glmer(data = all.data.stan, 
           formula = Yield_mg_ha ~ july_spei*SSURGO_soc0_30_mean*SSURGO_claytotal_r_mean+(1|SSURGO_taxorder_mode),
           family = gaussian(link = "log"))

summary(m)

ggplot(data.frame(eta=predict(m,type="link"),pearson=residuals(m,type="pearson")),
       aes(x=eta,y=pearson)) +
  geom_point() +
  theme_bw()

m <- glmer(data = all.data.stan, 
      formula = Yield_mg_ha ~ DSCI.mean*SSURGO_soc0_30_mean*SSURGO_claytotal_r_mean+(1|SSURGO_taxorder_mode),
      family = gaussian(link = "log"))

summary(m)

# Initial GLMs indicate significant effects but relationship of yield with SOC is non-linear

# check if this is because of differences across soil type

ggplot(data = all_data, aes(x = SSURGO_soc0_30_mean, y = Yield_mg_ha))+
  geom_jitter()+
  geom_smooth()+
  facet_wrap(facets = "SSURGO_taxorder_mode")



# loss cost models ####

m <- glmer(data = all.data.stan, 
      formula = loss_cost ~ july_spei*SSURGO_soc0_30_mean*SSURGO_claytotal_r_mean+(1|SSURGO_taxorder_mode),
      family = gaussian(link = "log"), mustart=pmax(all.data.stan$loss_cost,1e-8))
summary(m)

# interactions::interact_plot(model = m, modx = july_spei,modx.values = c(-1.5, -1, 0),
#                             pred = SSURGO_soc0_30_mean, interval = T, int.type = "prediction", outcome.scale = "response",
#                             plot.points = FALSE, point.size = 0.2, jitter = 0.1,line.thickness = 1.25, partial.residuals = FALSE,
#                             legend.main = "July SPEI",
#                             x.label = "SOC, 0-30 cm", y.label = "Loss cost", 
#                             mod2 = SSURGO_claytotal_r_mean,mod2.values = c(-1,0,2))
# 
interactions::interact_plot(model = m, modx = july_spei,modx.values = c(-1.5, -1, 0),
                            pred = SSURGO_claytotal_r_mean, interval = T, int.type = "prediction", outcome.scale = "response",
                            plot.points = FALSE, point.size = 0.2, jitter = 0.1,line.thickness = 1.25, partial.residuals = FALSE,
                            legend.main = "July SPEI",
                            x.label = "Clay, 0-30 cm", y.label = "Loss cost")

interactions::interact_plot(model = m, modx = july_spei,modx.values = c(-1.5, -1, 0),
                            pred = SSURGO_soc0_30_mean, interval = T, int.type = "prediction", outcome.scale = "response",
                            plot.points = FALSE, point.size = 0.2, jitter = 0.1,line.thickness = 1.25, partial.residuals = FALSE,
                            legend.main = "July SPEI",
                            x.label = "Clay, 0-30 cm", y.label = "Loss cost")



m <- glmer(data = all.data.stan, 
      formula = loss_cost ~ DSCI.mean*SSURGO_soc0_30_mean*SSURGO_claytotal_r_mean+(1|SSURGO_taxorder_mode),
      family = gaussian(link = "log"), mustart=pmax(all.data.stan$loss_cost,1e-10))
summary(m)


interactions::interact_plot(model = m, modx = DSCI.mean,modx.values = c(3,2,1),
                            pred = SSURGO_claytotal_r_mean, interval = T, int.type = "prediction", outcome.scale = "response",
                            plot.points = FALSE, point.size = 0.2, jitter = 0.1,line.thickness = 1.25, partial.residuals = FALSE,
                            legend.main = "DSCI",
                            x.label = "Clay, 0-30 cm", y.label = "Loss cost")




# Loss cost by soil order models ####

# SOC:drought effect disappears for loss cost data, but a strong clay:drought effect emerges
# This mismatch seems unusual and perhaps indicates that the random effects structure is inadequate
# Re-running model across all soil types instead

mod <- function(df){
  glm(data = df, 
        formula = loss_cost ~ DSCI.mean*SSURGO_soc0_30_mean*SSURGO_claytotal_r_mean,
        family = gaussian(link = "log"),mustart=pmax(df$loss_cost,1e-8))
}

model_by_order <- all.data.stan %>%
  group_by(SSURGO_taxorder_mode) %>%
  nest() %>%
  mutate(model_by_order = map(data, mod))


View(model_by_order %>% 
  mutate(glance = map(model_by_order, broom::tidy)) %>% 
  unnest(glance))























## Soilgrids models #####
# note, may eliminate


all_data %>%
  filter(july_spei < -2) %>%
  mutate_at(.vars = vars(10:37),.funs = function(x) {if(is.numeric(x)) as.vector(scale.2sd(x)) else x}) %>%
  mutate(nodrought.soc.resids = residuals(lmer(data = ., formula = Yield_mg_ha~soilgrids_sand_M_sl4_100m+(1|soilgrids_Order)))) %>%
ggplot(data = ., aes(x = soilgrids_soc_M_sl4_100m, y = nodrought.soc.resids))+
  geom_jitter()

all_data %>%
  filter(july_spei < -2) %>%
  ggplot(., aes(x = soilgrids_soc_M_sl4_100m, y = Yield_mg_ha)) +
  geom_jitter()


m <- glmer(data = all.data.stan, 
      formula = Yield_mg_ha ~ july_spei*soilgrids_soc_M_sl4_100m*soilgrids_sand_M_sl4_100m+(1|soilgrids_Order),
      family = gaussian(link = "log")) 

summary(m)

qqnorm(residuals(m, type="deviance"))
abline(a = 0, b = 1)

ggplot(data = all.data.stan, aes(x = DSCI.mean, y= (Yield_mg_ha)))+
  geom_jitter(size = 1) +
  theme_grey() + 
  facet_wrap(facets = "soilgrids_Order", nrow = 3,ncol = 2)


interactions::interact_plot(model = m, modx = july_spei,modx.values = c(-1.5,0,1.5),
                            pred = soilgrids_soc_M_sl4_100m, interval = T, int.type = "prediction", outcome.scale = "response",
                            plot.points = F, point.size = 0.2, jitter = 0.1,line.thickness = 1.25, 
                            legend.main = "July_spei",
                            x.label = "SOC, 0-30 cm", y.label = "Yield (mg per hectare)")





glmer(data = all.data.stan, 
      formula = Yield_mg_ha ~ DSCI.mean*soilgrids_soc_M_sl4_100m+(1|soilgrids_Order),
      family = gaussian(link = "log")) %>%
  summary(.)


glmer(data = all.data.stan, 
      formula = Yield_mg_ha ~ july_spei*soilgrids_soc_M_sl4_100m*soilgrids_sand_M_sl4_100m+(1|soilgrids_Order),
      family = gaussian(link = "log"), mustart=pmax(all.data.stan$loss_cost,1e-8)) %>%
  summary(.)


glmer(data = all.data.stan, 
      formula = loss_cost ~ DSCI.mean*soilgrids_soc_M_sl4_100m*soilgrids_sand_M_sl4_100m+(1|soilgrids_Order),
      family = gaussian(link = "log"), mustart=pmax(all.data.stan$loss_cost,1e-8)) %>%
  summary(.)



