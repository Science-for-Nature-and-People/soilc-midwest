## libraries #####

library(tidyverse)
library(lme4)
library(mgcv)
library(itsadug)
library(reghelper)

# data #####
all_data <- readRDS("data/all_data_2019.08.28.rds")

all_data$SSURGO_taxorder_mode[is.na(all_data$SSURGO_taxorder_mode)] <- "Unknown"

View(all_data %>%
  filter(is.na(DSCI.mean)) %>%
  count(as.character(GEOID)))

all_data <- all_data %>%
  filter(!is.na(july_spei),
         !is.na(DSCI.mean), 
         SSURGO_taxorder_mode != "Entisols") %>%
  rename(Yield_mg_ha = Yield.bu.acre) %>%
  mutate(Yield_mg_ha = Yield_mg_ha*0.0628) %>%
  group_by(GEOID) %>%
  mutate(County_avg_yield = mean(Yield_mg_ha)) %>%
  ungroup() %>% 
  mutate(Yield_diff = ((Yield_mg_ha - County_avg_yield)/County_avg_yield),
         DSCI.mean = DSCI.mean/1000,
         SSURGO_soc0_30_mean = SSURGO_soc0_30_mean/10000,
         SSURGO_sandtotal_r_mean = SSURGO_sandtotal_r_mean/100,
         SSURGO_silttotal_r_mean = SSURGO_silttotal_r_mean/100,
         SSURGO_claytotal_r_mean = SSURGO_claytotal_r_mean/100,
         SSURGO_siltclay = SSURGO_claytotal_r_mean + SSURGO_silttotal_r_mean) %>%
  dplyr::select(Year, GEOID, FIPS, County.name, State.alpha, State,
                Yield_mg_ha,loss_cost,Yield_diff,
                starts_with("soilgrids"), starts_with("SSURGO"),
                starts_with("DSCI"), ends_with("spei")) %>%
  distinct(.)


View(all_data %>%
       dplyr::select(Yield_mg_ha,july_spei,SSURGO_soc0_30_mean,SSURGO_sandtotal_r_mean))

ggplot(all_data, aes(x =as.factor(as.character(Year)), y = DSCI.mean)) + 
  geom_boxplot()

all_data %>%
  filter(july_spei < 0)


# NOTE - this set of filters eliminates data from the year 2017, for which no SPEI data are available, 
# and data from 1997-1999, for which there are no DSCI data

# Generate standardized dataframe

scale.2sd <- function(x){
  (x-mean(x))/(2*sd(x))
}

all.data.stan <- all_data %>%
  mutate_at(.vars = vars(10:37),.funs = function(x) {if(is.numeric(x)) as.vector(scale.2sd(x)) else x})
  

# Yield, SPEI, and SSURGO #####
## unstandardized 
m <- glmer(data = all_data, 
           formula = Yield_mg_ha ~ DSCI.mean*SSURGO_soc0_30_mean*SSURGO_claytotal_r_mean+
             (1|SSURGO_taxorder_mode),
      family = gaussian(link = "log"))

summary(m)

interactions::interact_plot(model = m, modx = DSCI.mean,modx.values = c(0.1,0.2,0.3),
                            pred = SSURGO_soc0_30_mean, interval = T, int.type = "prediction", outcome.scale = "response",
                            plot.points = F, point.size = 0.2, jitter = 0.1,line.thickness = 1.25, 
                            legend.main = "DSCI",
                            x.label = "SOC, 0-30 cm", y.label = "Yield (mg per hectare)", 
                            mod2 = SSURGO_claytotal_r_mean,mod2.values = c(0.2,0.3,0.5))



interactions::interact_plot(model = m, modx = july_spei,modx.values = c(-3, -1.5, 0),
                            pred = SSURGO_soc0_30_mean, interval = T, int.type = "prediction", outcome.scale = "response",
                            plot.points = FALSE, point.size = 0.2, jitter = 0.1,line.thickness = 1.25, partial.residuals = FALSE,
                            legend.main = "July SPEI",
                            x.label = "SOC, 0-30 cm", y.label = "Yield (mg per hectare)")

## standardized
m <- glmer(data = all.data.stan, 
           formula = Yield_mg_ha ~ july_spei*SSURGO_soc0_30_mean*SSURGO_claytotal_r_mean+(1|SSURGO_taxorder_mode),
           family = gaussian(link = "log"))

summary(m)

# Loss cost, SPEI, and SSURGO #####
gm <- glmer(data = all_data, 
           formula = loss_cost ~ july_spei*SSURGO_soc0_30_mean*SSURGO_claytotal_r_mean+(1|SSURGO_taxorder_mode),
           family = gaussian(link = "log"), mustart=pmax(all_data$loss_cost,1e-3))

summary(gm)

interactions::interact_plot(model = gm, modx = july_spei,modx.values = c(-3, -1.5),
                            pred = SSURGO_soc0_30_mean, interval = T, int.type = "prediction", outcome.scale = "response",
                            plot.points = F, point.size = 0.2, jitter = 0.1,line.thickness = 1.25, 
                            legend.main = "July SPEI",
                            x.label = "SOC, 0-30 cm", y.label = "Loss cost")


lm <- lmerTest::lmer(data = all_data, 
           formula = loss_cost ~ july_spei*SSURGO_soc0_30_mean*SSURGO_claytotal_r_mean+(1|SSURGO_taxorder_mode))

summary(lm)



## standardized
gm <- glmer(data = all.data.stan, 
            formula = loss_cost ~ july_spei*SSURGO_soc0_30_mean*SSURGO_claytotal_r_mean+(1|SSURGO_taxorder_mode),
            family = gaussian(link = "log"), mustart=pmax(all_data$loss_cost,1e-3))

summary(gm)


# Yield, DSCI, and SSURGO #######
## unstandardized 
m <- glmer(data = all_data %>% filter(SSURGO_taxorder_mode != "Unknown"), 
           formula = Yield_mg_ha ~ DSCI.mean*SSURGO_soc0_30_mean*SSURGO_claytotal_r_mean+(1|SSURGO_taxorder_mode),
           family = gaussian(link = "log"))

summary(m)


interactions::interact_plot(model = m, modx = DSCI.mean,modx.values = c(0, 0.025, 0.4),
                            pred = SSURGO_soc0_30_mean, interval = T, int.type = "prediction", outcome.scale = "response",
                            plot.points = T, point.size = 0.8, jitter = 0.1,line.thickness = 1.25, 
                            legend.main = "July SPEI",
                            x.label = "SOC, 0-30 cm", y.label = "Yield (mg per hectare)", partial.residuals = F, 
                            point.shape = T)+
  facet_wrap(facets = "SSURGO_taxorder_mode", nrow = 3, ncol = 3)

## standardized
m <- glmer(data = all.data.stan, 
           formula = Yield_mg_ha ~ DSCI.mean*SSURGO_soc0_30_mean*SSURGO_claytotal_r_mean+(1|SSURGO_taxorder_mode),
           family = gaussian(link = "log"))

summary(m)
MuMIn::r.squaredGLMM(m)


interactions::interact_plot(model = m, modx = DSCI.mean,modx.values = c(-0.4, 0, 2),
                            pred = SSURGO_soc0_30_mean, interval = T, int.type = "prediction", outcome.scale = "response",
                            plot.points = F, point.size = 0.2, jitter = 0.1,line.thickness = 1.25, 
                            legend.main = "DSCI",
                            x.label = "SOC, 0-30 cm", y.label = "Yield (mg per hectare)", 
                            mod2 = SSURGO_claytotal_r_mean,mod2.values = c(-1, 0, 1))



m <- glmer(data = all.data.stan, 
           formula = Yield_mg_ha ~ DSCI.mean*SSURGO_soc0_30_mean+SSURGO_claytotal_r_mean+(1|SSURGO_taxorder_mode),
           family = gaussian(link = "log"))

summary(m)
MuMIn::r.squaredGLMM(m)


interactions::interact_plot(model = m, modx = DSCI.mean,modx.values = c(-0.3, 0, 2),
                            pred = SSURGO_soc0_30_mean, interval = T, int.type = "prediction", outcome.scale = "response",
                            plot.points = F, point.size = 0.2, jitter = 0.1,line.thickness = 1.25, 
                            legend.main = "DSCI",
                            x.label = "SOC, 0-30 cm", y.label = "Yield (mg per hectare)")



# Loss cost, DSCI, and SSURGO #####
gm <- glmer(data = all_data, 
            formula = loss_cost ~ DSCI.mean*SSURGO_soc0_30_mean*SSURGO_claytotal_r_mean+(1|SSURGO_taxorder_mode),
            family = gaussian(link = "log"), mustart=pmax(all_data$loss_cost,1e-3))

summary(gm)

interactions::interact_plot(model = gm, modx = DSCI.mean,modx.values = c(-3, -1.5),
                            pred = SSURGO_soc0_30_mean, interval = T, int.type = "prediction", outcome.scale = "response",
                            plot.points = F, point.size = 0.2, jitter = 0.1,line.thickness = 1.25, 
                            legend.main = "July SPEI",
                            x.label = "SOC, 0-30 cm", y.label = "Loss cost")


lm <- lmerTest::lmer(data = all_data, 
                     formula = loss_cost ~ DSCI.mean*SSURGO_soc0_30_mean*SSURGO_claytotal_r_mean+(1|SSURGO_taxorder_mode))

summary(lm)



## standardized
gm <- glmer(data = all.data.stan, 
            formula = loss_cost ~ DSCI.mean*SSURGO_soc0_30_mean*SSURGO_claytotal_r_mean+(1|SSURGO_taxorder_mode),
            family = gaussian(link = "log"), mustart=pmax(all_data$loss_cost,1e-3))

summary(gm)



##### Yield, SPEI, and SOILGRIDS #######


# Yield models
## unstandardized 
m <- glmer(data = all_data, 
           formula = Yield_mg_ha ~ july_spei*soilgrids_soc_M_sl4_100m*soilgrids_sand_M_sl4_100m+(1|soilgrids_Order),
           family = gaussian(link = "log"))

summary(m)


interactions::interact_plot(model = m, modx = july_spei,modx.values = c(-3, -1.5, 0),
                            pred = soilgrids_soc_M_sl4_100m, interval = T, int.type = "prediction", outcome.scale = "response",
                            plot.points = F, point.size = 0.2, jitter = 0.1,line.thickness = 1.25, 
                            legend.main = "July SPEI",
                            x.label = "SOC, 0-30 cm", y.label = "Yield (mg per hectare)")

## standardized
m <- glmer(data = all.data.stan, 
           formula = Yield_mg_ha ~ july_spei*soilgrids_soc_M_sl4_100m*soilgrids_sand_M_sl4_100m+(1|soilgrids_Order),
           family = gaussian(link = "log"))

summary(m)

m <- lmerTest::lmer(data = all.data.stan, 
           formula = Yield_mg_ha ~ july_spei*soilgrids_soc_M_sl4_100m*soilgrids_sand_M_sl4_100m+(1|soilgrids_Order))

summary(m)


# Loss cost, SPEI, and soilgrids ####
gm <- glmer(data = all_data, 
            formula = loss_cost ~ july_spei*soilgrids_soc_M_sl4_100m*soilgrids_sand_M_sl4_100m+(1|soilgrids_Order),
            family = gaussian(link = "log"), mustart=pmax(all_data$loss_cost,1e-3))

summary(gm)

interactions::interact_plot(model = gm, modx = DSCI.mean,modx.values = c(-3, -1.5),
                            pred = SSURGO_soc0_30_mean, interval = T, int.type = "prediction", outcome.scale = "response",
                            plot.points = F, point.size = 0.2, jitter = 0.1,line.thickness = 1.25, 
                            legend.main = "July SPEI",
                            x.label = "SOC, 0-30 cm", y.label = "Loss cost")


lm <- lmerTest::lmer(data = all_data, 
                     formula = loss_cost ~ july_spei*soilgrids_soc_M_sl4_100m*soilgrids_sand_M_sl4_100m+(1|soilgrids_Order))

summary(lm)



## standardized
gm <- glmer(data = all.data.stan, 
            formula = loss_cost ~ july_spei*soilgrids_soc_M_sl4_100m*soilgrids_sand_M_sl4_100m+(1|soilgrids_Order),
            family = gaussian(link = "log"), mustart=pmax(all_data$loss_cost,1e-3))

summary(gm)

