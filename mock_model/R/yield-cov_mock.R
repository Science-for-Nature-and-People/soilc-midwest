#####################################################################
# Project:      SNAPP soil carbon working group                     #
# Description:  Mock code for US-scale model of yield variability   #
# Data sources: NASS; SoilGrids                                     #
# Author:       Steve Wood                                          #
# Date:         March 12, 2018                                      #
#####################################################################

# LOAD PACKAGES
library(tidyverse)  # For reading in and manipulating data
library(rstan)      # For interfacing with Stan

# READ DATA
model_data <- read_csv("~/Documents/data.csv")

# PREPARE DATA FOR STAN
yield_list <- list(
                    N=nrow(model_data),
                    covar=model_data$covar,
                    yield_mean=model_data$yield_mean,
                    yield_sd=model_data$yield_sd
)

# CALL STAN MODEL
yield_model <- stan(
                    file = "~/Box Sync/Work/GitHub/soilc-row_crops/stan/yield-cov_mock.stan", 
                    data = yield_list, 
                    control = list(adapt_delta=0.99,max_treedepth=15), chains = 4
)

# MODEL RESULTS
##  Print results
print(
      yield_model,pars=c("beta","alpha","sigma"),
      probs=c(0.05,0.95)
)

##  Plot results
plot(yield_model,pars=c("beta","alpha","sigma"))

##  Posterior predictive check
### Extract predicted data
yield_pred <- extract(yield_model,pars=c('y_tilde','yield_cov'))
### Unlist predicted data
yield_pred <- unlist(yield_pred, use.names=FALSE)
### Create data frame with observed and predicted data
yield_pp_data <- data.frame(
                            c(yield_pred),
                            c(rep("yield_pred",length(yield_pred$y_tilde)),
                              rep("y_cov",length(yield_pred$yield_cov)))
)
### Rename data
names(yield_pp_data) <- c("y","type")
### Overlap density plot of observed vs. predicted
ggplot(yield_pp_data, aes(x=y)) + 
  geom_density(
    aes(group=type, fill=type), 
    alpha=0.75
  ) + 
  theme_bw() +
  xlab("Yield variability") + ylab("Density") +
  scale_fill_manual(values=wesanderson::wes_palette("Royal1",n=2)) +
  theme(
    legend.title = element_blank(),
    legend.position = c(0.85,0.55),
    panel.grid = element_blank(),
    panel.background = element_rect(fill="white"),
    plot.background = element_rect(fill="white")
  )
