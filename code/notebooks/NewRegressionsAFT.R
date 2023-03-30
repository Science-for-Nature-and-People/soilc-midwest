

# rather than butchering Dan's code I've started a new one to try things out here.
# 16 Feb 2023

library(tidyverse)
library(lme4)
library(ggplot2)
# install.packages("ggthemes")
library (ggthemes)


# data #####
all_data <- readRDS("data/all_data_2020.08.07.rds")  # in R projects the default working directory is the project
all.data.stan <- readRDS("data/all_data_stan_2020.08.07.rds") # the standardized data

# plot the data
windows(xpinch=200, ypinch=200, width=5, height=5)

ggplot(data = all_data, aes(x = ssurgo_om_mean, y = Yield_decomp_add))+
  geom_jitter()+
  # geom_smooth()   #looks like a saturation curve y=(x)^(1/2)
  geom_smooth(method="lm")


# plot the data with some facets 
#by state
ggplot(data = all_data, aes(x = ssurgo_om_mean, y = Yield_decomp_add))+
  geom_jitter()+ 
  # geom_smooth()   #looks like a saturation curve y=(x)^(1/2)
  geom_smooth(method="lm") + theme_bw()+
  theme(text = element_text(size = 13), axis.text = element_text(size = 12))+
  ylab("Corn yield (Mg/ha)")+ ylim(0,20) +
  xlab("Organic matter (%)")+ xlim (0,10)+
  ggtitle("Yield by state")+
  facet_wrap(~state_alpha)

ggplot(data = all.data.stan, aes(x = ssurgo_om_mean, y = Yield_decomp_add))+
  geom_jitter(size=0.7, alpha=0.6, shape=20)+ 
  # geom_smooth()   #looks like a saturation curve y=(x)^(1/2)
  geom_smooth(method="lm") + 
  # theme_bw()+
  ylab("De-trended Corn yield (Mg/ha)")+ #ylim(0,20) +
  xlab("Standardized SOM (%)")+ #xlim (0,10)+
  # ggtitle("Yield by state")+
  facet_wrap(~state_alpha) +
  theme(panel.grid.minor=element_blank(), 
        panel.grid.major=element_blank() ,
        panel.background = element_rect(fill = NA) ,
        panel.border=element_rect(color="grey50", fill=NA, linewidth=0.5),
        strip.text=element_text(size=rel(1), face="bold", vjust=-2),
        strip.background = element_rect(fill=NA),
        panel.margin=unit(0, "lines"),
        panel.spacing.y=unit(-1, "points"),
        legend.text=element_text(size=14),
        legend.title=element_text(size=14)
        # legend.title=element_text(size=12, face="bold"),
        # plot.margin = unit(c(0.5,0.5,0.5,0.5), "cm")
  )	

ggsave("code/plots/OMxyield_by_state_originaldata.png")

#by soil order
#note NY with OM 10%?
ggplot(data = all_data, aes(x = ssurgo_om_mean, y = Yield_decomp_add, color = ssurgo_order_mode))+
  geom_jitter()+ 
  # geom_smooth()   #looks like a saturation curve y=(x)^(1/2)
  geom_smooth(method="lm") + theme_bw()+
  theme(text = element_text(size = 13), axis.text = element_text(size = 12))+
  ylab("Corn yield (Mg/ha)")+ ylim(0,20) +
  xlab("Organic matter (%)")+ xlim (0,10)+
  ggtitle("Yield vs OM by soil order")+
  facet_wrap(~state_alpha)

#by soil order
#note NY with OM 10%?
ggplot(data = all_data, aes(x = ssurgo_om_mean, y = Yield_decomp_add, color = ssurgo_order_mode))+
  geom_jitter()+ 
  # geom_smooth()   #looks like a saturation curve y=(x)^(1/2)
  geom_smooth(method="lm") + theme_bw()+
  theme(text = element_text(size = 13), axis.text = element_text(size = 12))+
  ylab("Corn yield (Mg/ha)")+ ylim(0,20) +
  xlab("Organic matter (%)")+ xlim (0,10)+
  ggtitle("Yield vs OM by soil order")+
  facet_wrap(~state_alpha)

#by year
ggplot(data = all_data, aes(x = ssurgo_om_mean, y = Yield_decomp_add, color = ssurgo_sand_median))+
  geom_jitter()+ 
  # geom_smooth()   #looks like a saturation curve y=(x)^(1/2)
  #geom_smooth(method="lm") +
  theme_bw()+
  theme(text = element_text(size = 13), axis.text = element_text(size = 12))+
  ylab("Corn yield (Mg/ha)")+ ylim(0,20) +
  xlab("Organic matter (%)")+ xlim (0,10)+
  ggtitle("Yield by year ")+
  facet_wrap(~year)

#by midwest region

all_data %>%
  mutate (region =  ifelse(state_alpha %in% c('IA', 'IL', 'IN', 'KS', 'MO', 'MN', 'NE', 'MI', 'OH'),
                           'Midwest', 'Other')) %>%
  ggplot(aes(x = ssurgo_om_mean, y = Yield_decomp_add, color = state_alpha))+
  geom_jitter()+ 
  theme_bw()+
  theme(text = element_text(size = 13), axis.text = element_text(size = 12))+
  ylab("Corn yield (Mg/ha)")+ ylim(0,20) +
  xlab("Organic matter (%)")+ xlim (0,10)+
  ggtitle("Yield by region ")+
  facet_wrap(~region)

#plot of OM and clay
all_data %>%
  ggplot(aes(x = ssurgo_om_mean, y = ssurgo_clay_median , color = state_alpha))+
  geom_jitter()+ 
  #theme_bw()+
  theme(text = element_text(size = 13), axis.text = element_text(size = 12))+
  #ylab("Corn yield (Mg/ha)")+ ylim(0,20) +
  #xlab("Organic matter (%)")+ xlim (0,10)+
 # ggtitle("Yield by region ")+
 facet_wrap(~ state_alpha)


# conversation with Laila, she pointed out that having a linear slope 
# for SOM might not make the most sense, based on looking at how the data look 
# in Fig. 1, but also knowing that a farmer who already has 5% SOM who gets to
# 5.5% SOM will not see the same boost in yield as a farmer who starts at 1%
# SOM and gets to 1.5% SOM.

#### THIS QUESTION HAS BEEN RESOLVED, realizing that standardizing the data makes it no longer linear
#### Figure 1 in the paper is confusing because it plots the regression on the un-standardized data, so it looks straight

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









## BM assigning counties to USDA NASS Farm Resource REgions to group for regression
## FRR county fips codes downloaded from https://www.ers.usda.gov/data-products/arms-farm-financial-and-crop-production-practices/documentation.aspx

# read in region data
frr <- readxl::read_excel("data/reglink.xls", skip=2)
frr_key <- frr[1:9,7]
frr <- frr[,1:2]
colnames(frr) <- c("GEOID", "region")
# add leading zeros to match soil data fips, this also converts the fips codes
# into character to allow us to merge it with the soil data
frr$GEOID <- sprintf("%05d", frr$GEOID)


# add region ID to soil data
all.data.stan <- left_join(all.data.stan, frr)

# Redo regressions using region (copied and pasted from above, replacing state with region)

# make region a factor, not numeric
all.data.stan$region <- factor(as.character(all.data.stan$region))

# Model using region as a fixed effect instead of random
# these models might get dinged for being over-parameterized
m.region <- all.data.stan %>%
  lm(data = ., 
     formula = Yield_decomp_add ~ summer_spei*ssurgo_om_mean*ssurgo_h*ssurgo_clay_mean*region )

summary(m.region)

# base level (intercept) is region1, other region estimates (that are significant) are added to the intercept 
# all regions significant

print(broom.mixed::tidy(m.region) %>%
        mutate(p.value = round(p.value, digits = 5)), n=100)

x <- data.frame(eta=predict(m.region,interval="confidence")[,1],pearson=residuals(m.region,type="pearson"))

# open a window for our plots
windows(xpinch=200, ypinch=200, width=5, height=5)

# Check model assumptions - compare predictions to residuals
ggplot(data.frame(eta=predict(m.region,interval="confidence", terms="fit")[,1], 
                  pearson=residuals(m.region,type="pearson")),
       aes(x=eta,y=pearson)) +
  geom_point() +
  theme_bw()
# recall with residual plots we are looking for 
# (1) they’re pretty symmetrically distributed, tending to cluster towards the middle of the plot.
# (2) they’re clustered around the lower single digits of the y-axis (e.g., 0.5 or 1.5, not 30 or 150).
# (3) in general, there aren’t any clear patterns.
# this is because the error term in the model should be uncorrelated with the X covariates, 
# so the predicted values, dependent on those covariates, should also be uncorrelated
# with the error term (residuals) and should look random.

resids <- resid(m.region)

plot(resids)
hist(resids)
plot(density(resids))
qqnorm(resids)
# these all look pretty good

# interaction plots
# first, what is an interact_plot?
# way to detect and understand interaction effects between two factors
# fitted values of response variable on the Y axis and values of the first factor on the X axis.
# lines represent the values of the second factor of interest.


# is there a 2-way interaction between OM*SPEI?
# below, response on the y (yield), OM on the x, and second factor, summer_spei, as lines
# recall that As SPEI decreases, drought severity increases. 
interactions::interact_plot(
  model = m.region, 
  pred = ssurgo_om_mean,  # factor 1
  modx = summer_spei,  # factor 2
  modx.values = quantile(all.data.stan$summer_spei),
  partial.residuals = T,
  point.alpha=0.2, # make the points lighter so we can see the lines more clearly
  point.size=0.6,
  line.thickness=1.5
)
# The slope of the lines in these plots is more steep with more severe drought,
# stronger effect of OM on yield during drought.
# In general if the lines on the interaction plot intersect, then there is likely an interaction.


# is there a 3-way interaction for OM*SPEI*clay?
interactions::interact_plot(
  model = m.region,
  pred = ssurgo_om_mean,
  mod2 = summer_spei,
  mod2.values = quantile(all.data.stan$summer_spei),
  modx = ssurgo_clay_mean,  # this time the lines are for clay
  modx.values = quantile(all.data.stan$ssurgo_clay_mean), # facets split into clay's quantiles
  partial.residuals = T,
  point.alpha=0.2,
  point.size=0.6,
  line.thickness=1.5
)
# yes there is a 3 way interaction (lines are intersecting in the facets)

# is there a 3-way interaction for OM*SPEI*H+?
interactions::interact_plot(
  model = m.region,
  pred = ssurgo_om_mean,
  mod2 = summer_spei,
  mod2.values = quantile(all.data.stan$summer_spei),
  modx = ssurgo_h,
  modx.values = quantile(all.data.stan$ssurgo_h)[3:5],
  partial.residuals = T,
  point.alpha=0.1,
  point.size=0.6,
  line.thickness=1.5
)
# yes at more severe drought, not so much at less severe drought.
# From Dan's notebook: OM:SPEI:H+. Yields increase with SOM more strongly 
# under drought. As drought severity decreases, SOM has a more strongly negative 
# effect on yields when soils are also very high H+ concentrations. This also 
# seems to be due to very few points that are high pH.


# the coeff.table contains the slopes used in Fig. 1, caption reads "trendlines represent
# predicted yields based on that marginal effect [of SOM]."

mod <- function(df){
  lm(data = df, Yield_decomp_add ~ ssurgo_om_mean*ssurgo_clay_mean*ssurgo_h+ region)
}

model_by_drought <- all.data.stan %>%
  group_by(spei.cut) %>% # separate data out by spei groups
  nest() %>% # creates a list of the spei group dataframes
  mutate(model_by_drought = map(data, mod))  # I think this is calling purrr::map, which is applying the mod function to each of the data in the list
# A list of 4 dataframes, one for each category of drought

coeff.table <- model_by_drought %>% 
  mutate(glance = map(model_by_drought, broom.mixed::tidy)) %>% 
  unnest(glance) %>%
  mutate(p.value = round(p.value, digits = 5)) %>%
  # filter(effect == "fixed",    # we no longer have random effects
  #        term != "(Intercept)") %>%  # I think we want to see intercepts
  arrange(spei.cut) %>%
  select(-data, -model_by_drought, -spei.cut) # use names(coeff.table) to see what we can select


# make plot of data by region 
windows(xpinch=200, ypinch=200, width=5, height=5)

# how many states by region and regions do we have?
counts_states <- aggregate(summer_spei~state_alpha + region, dat=all.data.stan, FUN="length")
counts_regions <- aggregate(summer_spei~region, dat=all.data.stan, FUN="length")

lbls <- c("1"="Heartland", 
          "2"="Northern Crescent", 
          "3"="Northern Great Plains",
          "4"="Prairie Gateway", 
          "5"="Eastern Uplands", 
          "6"="Southern Seaboard",
          "7"="Fruitful Rim", 
          "8"="Basin and Range",
          "9"="Mississippi Portal") 
# there's probably a way to do this manipulating the strings with code
# but after 20 min. of failing at that, I did it this way! :)

xom <- rep(1.8, nrow(counts_regions))
yyield <- rep(2, nrow(counts_regions))
lab <- counts_regions$summer_spei
geomtext <- data.frame(region=counts_regions$region, xom, yyield, lab)
geomtext$n <- rep("n", nrow(geomtext))
geomtext$ntext <- paste0(geomtext$n, "=", geomtext$lab)


ggplot(data=all.data.stan[all.data.stan$spei.cut %in% "Normal",], aes(x=ssurgo_om_mean, y=Yield_decomp_add)) +
  geom_point(aes(col=state_alpha), size=0.8, alpha=0.7) +
  facet_wrap(vars(region),
             labeller = as_labeller(lbls)) +
  geom_text(data=geomtext, mapping=aes(x=xom, y=yyield, label=ntext)) +
  theme(
      panel.grid.minor=element_blank(), 
      panel.grid.major=element_blank() ,
      panel.background = element_rect(fill = 'white')
  )








# split the northern crescent "2" states into east and west
all.data.stan$region2 <- all.data.stan$region
all.data.stan$region2 <- ifelse(all.data.stan$region=="2",
                                (ifelse(all.data.stan$state_alpha %in% c("MN", "WI", "MI", "OH"), "2W", "2E")),
                                 all.data.stan$region)

# check
# all.data.stan[all.data.stan$state_alpha=="MI",c(3,49:50)]

# update the facet labels
lbls2 <- c("1"="Heartland", 
           "2E"="Northern Crescent-E", 
           "2W"="Northern Crescent-W", 
           "3"="Northern Great Plains",
           "4"="Prairie Gateway", 
           "5"="Eastern Uplands", 
           "6"="Southern Seaboard",
           "7"="Fruitful Rim", 
           "8"="Basin and Range",
           "9"="Mississippi Portal") 

counts_regions2 <- aggregate(summer_spei~region2, dat=all.data.stan, FUN="length")
xom2 <- rep(1.8, nrow(counts_regions2))
yyield2 <- rep(2, nrow(counts_regions2))
lab2 <- counts_regions2$summer_spei
geomtext2 <- data.frame(region2=counts_regions2$region2, xom2, yyield2, lab2)
geomtext2$n <- rep("n", nrow(geomtext2))
geomtext2$ntext <- paste0(geomtext2$n, "=", geomtext2$lab)

ggplot(data=all.data.stan[all.data.stan$spei.cut %in% "Normal",], aes(x=ssurgo_om_mean, y=Yield_decomp_add)) +
  geom_point(aes(col=state_alpha), size=0.8, alpha=0.7) +
  facet_wrap(vars(region2),
             labeller = as_labeller(lbls2)) +
  geom_text(data=geomtext2, mapping=aes(x=xom2, y=yyield2, label=ntext)) +
  theme(
    panel.grid.minor=element_blank(), 
    panel.grid.major=element_blank() ,
    panel.background = element_rect(fill = 'white')
  )

# dir.create("code/plots")

ggsave("code/plots/facets_NCrescent_split.png")




## add Eastern Uplands "5" to Northern Crescent East "2E"
all.data.stan$region3 <- all.data.stan$region2
all.data.stan$region3 <- ifelse(all.data.stan$region2=="5", "2E", all.data.stan$region2)

# check
# all.data.stan[all.data.stan$state_alpha=="MI",c(3,49:50)]

# update the facet labels
lbls3 <- c("1"="Heartland", 
           "2E"="N Crescent-E + E Uplands", 
           "2W"="Northern Crescent-W", 
           "3"="Northern Great Plains",
           "4"="Prairie Gateway", 
           # "5"="Eastern Uplands", 
           "6"="Southern Seaboard",
           "7"="Fruitful Rim", 
           "8"="Basin and Range",
           "9"="Mississippi Portal") 

counts_regions3 <- aggregate(summer_spei~region3, dat=all.data.stan, FUN="length")
xom3 <- rep(1.8, nrow(counts_regions3))
yyield3 <- rep(2, nrow(counts_regions3))
lab3 <- counts_regions3$summer_spei
geomtext3 <- data.frame(region3=counts_regions3$region3, xom3, yyield3, lab3)
geomtext3$n <- rep("n", nrow(geomtext3))
geomtext3$ntext <- paste0(geomtext3$n, "=", geomtext3$lab)

max_sreg <- aggregate(Yield_decomp_add~state_alpha + region3, dat=all.data.stan, FUN="max")

ggplot(data=all.data.stan[all.data.stan$spei.cut %in% "Normal",], aes(x=ssurgo_om_mean, y=Yield_decomp_add)) +
  geom_point(aes(col=state_alpha), size=0.8, alpha=0.7) +
  facet_wrap(vars(region3),
             labeller = as_labeller(lbls3)) +
  geom_text(data=geomtext3, mapping=aes(x=xom3, y=yyield3, label=ntext)) +
  theme(
    panel.grid.minor=element_blank(), 
    panel.grid.major=element_blank() ,
    panel.background = element_rect(fill = 'white')
  )

# dir.create("code/plots")

ggsave("code/plots/facets_NCrescE+EUplands.png")




# is the N Crescent E and E uplands actually going down 
# or is that because of the combination of states?

sub <- all.data.stan[all.data.stan$region3=="2E",]

counts_sub <- aggregate(summer_spei~state_alpha, dat=sub, FUN="length")
xomsub <- rep(1.3, nrow(counts_sub))
yyieldsub <- rep(2, nrow(counts_sub))
labsub <- counts_sub$summer_spei
geomtextsub <- data.frame(state_alpha=counts_sub$state_alpha, xomsub, yyieldsub, labsub)
geomtextsub$n <- rep("n", nrow(geomtextsub))
geomtextsub$ntext <- paste0(geomtextsub$n, "=", geomtextsub$lab)


ggplot(sub, aes(x=ssurgo_om_mean, y=Yield_decomp_add)) +
  geom_point(size=0.7, alpha=0.5) +
  facet_wrap(vars(state_alpha)) +
  geom_smooth(method="lm") +
  geom_text(data=geomtextsub, mapping=aes(x=xomsub, y=yyieldsub, label=ntext)) 

ggsave("code/plots/facets_NCrescE+EUplands_bystate.png")

