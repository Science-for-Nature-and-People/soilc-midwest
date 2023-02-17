

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
  select(-data, -model_by_drought, -effect, - group)
