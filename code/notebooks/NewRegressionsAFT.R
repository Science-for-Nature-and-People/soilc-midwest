

# rather than butchering Dan's code I've started a new one to try things out here.
# 16 Feb 2023

library(tidyverse)
library(lme4)
library(ggplot2)


# data #####
all_data <- readRDS("data/all_data_2020.08.07.rds")  # in R projects the default working directory is the project
all.data.stan <- readRDS("data/all_data_stan_2020.08.07.rds")

# plot the data
ggplot(data = all_data, aes(x = ssurgo_om_mean, y = Yield_decomp_add))+
  geom_jitter()+
  # geom_smooth()   #looks like a saturation curve y=(x)^(1/2)
  geom_smooth(method="lm")

# conversation with Laila, she pointed out that having a linear slope 
# for SOM might not make the most sense, based on looking at how the data look 
# in Fig. 1, but also knowing that a farmer who already has 5% SOM who gets to
# 5.5% SOM will not see the same boost in yield as a farmer who starts at 1%
# SOM and gets to 1.5% SOM.

# try using square root of SOM to make the data more linear relationship
all_data$ssurgo_om_mean_sqrt <- all_data$ssurgo_om_mean^(1/2)

ggplot(data = all_data, aes(x = ssurgo_om_mean_sqrt, y = Yield_decomp_add))+
  geom_jitter()+
  geom_smooth(method = "lm")

# model from the paper
m.state <- all.data.stan %>%
  lmerTest::lmer(data = ., 
                 formula = Yield_decomp_add ~ summer_spei*ssurgo_om_mean*ssurgo_h*ssurgo_clay_mean+(1|state_alpha))

# standardize the sqrt data so that when run lm all predictors on similar scales
# standardize function from all_data_merge.R:  
scale.2sd <- function(x){
  (x-mean(x))/(2*sd(x))
}

# standardize and add to all.data.stan
all.data.stan$ssurgo_om_mean_sqrt <- as.vector(mapply(FUN=function(x){
  as.vector(scale.2sd(x))}, all_data[,"ssurgo_om_mean_sqrt"]))

# try the new data in the model
m.state.sqrt <- all.data.stan %>%
  lmerTest::lmer(data = ., 
                 formula = Yield_decomp_add ~ summer_spei*ssurgo_om_mean_sqrt*ssurgo_h*ssurgo_clay_mean+(1|state_alpha))

summary(m.state.sqrt)

broom.mixed::tidy(m.state.sqrt) %>%
  mutate(p.value = round(p.value, digits = 5))

# Plot residuals and inspect
ggplot(data.frame(eta=predict(m.state.sqrt,type="link"),pearson=residuals(m.state.sqrt,type="pearson")),
       aes(x=eta,y=pearson)) +
  geom_point() +
  theme_bw()

# Compare models using raw om and sqrt om 
anova(m.state, m.state.sqrt) # note anova does not exactly work because models have same number of predictors, but we can look at the AIC and BIC and logLik, all of which suggest m.state.sqrt is slightly better.
MuMIn::r.squaredGLMM(m.state)
MuMIn::r.squaredGLMM(m.state.sqrt)  # marginal and conditional R^2 slightly better for m.state.sqrt

# next step is to look at separating out soil order or state as non-random effects in the model.
# note these are categorical data so they will not get new slopes only intercepts.

# Model using state as a fixed effect instead of random
# these models might get dinged for being over-parameterized
m.state.sqrt.f <- all.data.stan %>%
  lm(data = ., 
                 formula = Yield_decomp_add ~ summer_spei*ssurgo_om_mean_sqrt*ssurgo_h*ssurgo_clay_mean+ state_alpha)

summary(m.state.sqrt.f)

# what states do we have anyway?
unique(all.data.stan$state_alpha)
# so that means that Alabama is the base level intercept in the summary.
# states whose estimate is not significant use the same estimate for intercept
# as the base level.
# States with significant estimate, get their estimate ADDED to the base level intercept.

print(broom.mixed::tidy(m.state.sqrt.f) %>%
  mutate(p.value = round(p.value, digits = 5)), n=100)

x <- data.frame(eta=predict(m.state.sqrt.f,type="link"),pearson=residuals(m.state.sqrt.f,type="pearson"))

# Check model assumptions
# Plot residuals and inspect
ggplot(data.frame(res= resid(m.state.sqrt.f), obs= all.data.stan$Yield_decomp_add),
       aes(x=obs,y=res)) +
  geom_point() +
  theme_bw()

resids <- resid(m.state.sqrt.f)

plot(resids)
hist(resids)
plot(density(resids))
qqnorm(resids)

# OUTLIERS
ggplot(data.frame(x=rep("1", length(resids)), y=resids), aes(x=factor(x), y=y)) +
  geom_boxplot(outlier.color="red")
# we have some outliers: recall lower and upper hinges are 25th and 75th percentiles
# and upper and lower whisker extend to 1.5*IQR (interquartile range). data beyond
# whiskers are outliers.
# which data are outliers?
all.data.stan$fits <- dffits(m.state.sqrt.f)
hist(dffits(m.state.sqrt.f))
inflm.mssf <- influence.measures(m.state.sqrt.f)
outs <- unique(which(apply(inflm.mssf$is.inf, 1, any)))
dat <- all_data[outs,]  # using non-standardized data so we can interpret continuous values
aggregate(dat, loss_cost~ssurgo_order_mode+state_alpha, FUN=length)
# outliers don't seem to belong to a particular state or soil order
aggregate(dat, ssurgo_ph_mean~state_alpha, FUN=function(x) c(mean=mean(x), min=min(x), max=max(x))) 
# or crazy pH values
# Hmm lets check Dan's model for outliers
ggplot(data.frame(x=rep("1", length(resid(m.state))), y=resid(m.state)), aes(x=factor(x), y=y))+
  geom_boxplot(outlier.color="red")
# has lots of outliers, so not going to be too concerned with our outliers

# next step re-create code below to get coefficients of interest for new model
# this table contains the slopes used in Fig. 1, caption reads "trendlines represent
# predicted yields based on that marginal effect [of SOM]."
mod <- function(df){
  lmerTest::lmer(data = df, Yield_decomp_add ~ ssurgo_om_mean*ssurgo_clay_mean*ssurgo_h+(1|state_alpha))
}

model_by_drought <- all.data.stan %>%
  group_by(spei.cut) %>%
  nest() %>%
  mutate(model_by_drought = map(data, mod))

coeff.table <- model_by_drought %>% 
  mutate(glance = map(model_by_drought, broom.mixed::tidy)) %>% 
  unnest(glance) %>%
  mutate(p.value = round(p.value, digits = 5)) %>%
  filter(effect == "fixed", 
         term != "(Intercept)") %>%
  arrange(spei.cut) %>%
  dplyr::select(-data, -model_by_drought, -effect, - group)

coeff.table





#ATR: Brought over from Dan's code from final_analysis_notebook, recreated the regression usin the m.county and ssurgo_om_mean_sqrt created by BM 

#BM: However he uses state. --in paper they argue that "since farms in same state are generally more likely to implement fert rates and management practices simimlar to other farms in their state than those out of their state, our model accounts to some degree for broad differences in management." This is because they were unable to account for differences in management in the data (not available for fertilizer rate, and tillage data only available recently in AgCensus, I'll also add,that not all respondents to the survey report tillage practices.). Also where is the code showing these tests? Need a null model without county to compare. Below we run the same code chunk as above with GEOID (county) rather than state. And make a null model to compare.

#ATR: recreated this # SPEI as drought indicator

m.county <- all.data.stan %>%
  lmerTest::lmer(data = ., 
                 formula = Yield_decomp_add ~ summer_spei*ssurgo_om_mean_sqrt*ssurgo_h*ssurgo_clay_mean+(1|GEOID))

summary(m.county)

broom.mixed::tidy(m.county) %>%
  mutate(p.value = round(p.value, digits = 5))

# Plot residuals and inspect
ggplot(data.frame(eta=predict(m.county,type="link"), # link means predictions will be on the scale of the linear predictor rather than the response variable ("response" is the other option for type=)
                  pearson=residuals(m.county,type="pearson")),
       aes(x=eta,y=pearson)) +
  geom_point() +
  theme_bw()

# Null model
m.county.h0 <- all.data.stan %>%
  lm(data = ., 
     formula = Yield_decomp_add ~ summer_spei*ssurgo_om_mean_sqrt*ssurgo_h*ssurgo_clay_mean)

# Compare null model and model with random effect of county
anova(m.county,m.county.h0)

# compare conditional R-squared estimates 
# about conditional vs. marginal R2 for mixed effects models see: https://jonlefcheck.net/2013/03/13/r2-for-linear-mixed-effects-models/
MuMIn::r.squaredGLMM(m.county)
MuMIn::r.squaredGLMM(m.county.h0)

# BM: m.county model is significantly more parsimonious than the null model (both AIC and BIC are lower); has a higher log likelihood (better); residual deviance is lower; and the R2c (conditional R-squared) is higher (better fit). So, yes, county improves model fit. 


# Start ATR stuff: Below we'll look at the coefficients. 

summary(m.county)

#ATR: This is the same as Dan's code. "There are just a few interaction effects with SPEI. We'll evaluate them here." Looking at where he is potentially finding the interactions it looks like county also has the same interaction so we will be using those as well.  

interactions::interact_plot(
  model = m.county,
  pred = ssurgo_om_mean_sqrt,
  modx = summer_spei,
  modx.values = quantile(all.data.stan$summer_spei),
  partial.residuals = T
)

#Dan: Clay:SOM:SPEI. Yields increase with SOM more strongly under drought. Clay amplifies this effect. This appears to be largely because of counties that are high clay, low SOM. 

# ATR: This seems to also be true for county, although the clay does seem to amplify the effect even more with countries than with states.

interactions::interact_plot(
  model = m.county,
  pred = ssurgo_om_mean_sqrt,
  mod2 = summer_spei,
  mod2.values = quantile(all.data.stan$summer_spei),
  modx = ssurgo_h,
  modx.values = quantile(all.data.stan$ssurgo_h)[3:5],
  partial.residuals = T
)

#Dan: OM:SPEI:H+. Yields increase with SOM more strongly under drought. As drought severity decreases, SOM has a more strongly negative effect on yields when soils are also very high H+ concentrations. This also seems to be due to very few points that are high pH.

#ATR: using the counties it seems that, as has been the case, there is a stronger effect between yield increase and SOM under drought. Also SOM has a more strongly negative effect on yields when soils are also very high H+ concentrations, but only at the 0% SPEI.


#Dan: Interaction plots indicate that SOM interacts with SPEI to mitigate the effect of low SPEI (drought) on yields. SOM has a greater positive effect on yields under all conditions when clay content is high. Higher clay content suppresses yields under drought conditions. There are additional interaction effects with SOM and SPEI for H+ and clay. The interaction with clay simply enhances the same effect, whereas the one with H+ reverses the effect at very high H+ concentrations. 

#Now we'll check if the same patterns hold when we use DSCI as the drought indicator instead of SPEI. 

#ATR: We should change the state_alpha- changed to county_name

# DSCI as drought indicator

m.state.dsci <- all.data.stan %>%
  lmerTest::lmer(data = ., 
                 formula = Yield_decomp_add ~ DSCI.mean*ssurgo_om_mean_sqrt*ssurgo_h*ssurgo_clay_mean+(1|county_name))

# View(broom.mixed::tidy(m.state.dsci) %>%
#   mutate(p.value = round(p.value, digits = 5)))

# Dan:They mostly agree. Directionality of coeffecients is the same, as are significance factors and size of coefficients. An additional three-way interaction with DSCI:CLAY:H+ emerges. Since that does not affect the main hypotheses, we're not concerned. Given agreement between DSCI and SPEI models, we'll elect to use SPEI going forward as it introduces fewer possible artefacts from observer bias and is more strongly tied to physcial metrics.

#Next, we'll generate similar plots on the subset of observations below 0 SPEI given the interesting patterns revealed by the above figures.

#ATR: we need to change the state_alpha here too

# All drought, SPEI less than mean

all.data.stan %>%
  filter(spei.cut %in% c("Very severe", "Severe", "Moderate")) %>%
  ggplot(data = ., aes(x = ssurgo_om_mean_sqrt, y = Yield_decomp_add))+ 
  geom_point()

all.drought <- all.data.stan %>%
  filter(spei.cut %in% c("Very severe", "Severe", "Moderate")) %>%
  lmerTest::lmer(data = ., formula = Yield_decomp_add ~ ssurgo_om_mean_sqrt*ssurgo_clay_mean*ssurgo_h+(1|county_name))


plot(all.drought)
summary(all.drought)

head(all.data.stan)

#ATR This is where I stopped. There is more that can go from here, but I didn't want to spend more time and effort if I'm not on the right track. 

