#####################################################################
# Title:        Yield variability and SOM modeling                  #
# Description:  Look at if SOM reduces yield variability in NA ag   #
# Author:       Stephen Wood                                        #
# Last updated: 5/8/18                                              #
#####################################################################

# LOAD PACKAGES
library(tidyverse)      # For reading in data
library(rstan)          # For interfacing with Stan

# READ DATA
load("data/IL_toy_data_05112018.RData")

# SUBSET DATA
cv.data <- select(d.IL.summary,c('CV.yield','ORCDRC.sl3','PHIKCL.sl3'))
temp.data <- select(d.IL,c('Yield.bu.acre','Year','CECSOL.sl3','ORCDRC.sl3','SNDPPT.sl3','PHIKCL.sl3'))

# DEFINE DATA
## List data for Stan model
data.list <- list(
  N=nrow(d.IL.summary),
  K=4,
  y=d.IL.summary$CV.yield,
  x=d.IL.summary[,c('ORCDRC.sl3','CECSOL.sl3','SNDPPT.sl3','PHIKCL.sl3')]
)
temp.data <- list(
  N=nrow(d.IL),
  K=4,
  y=d.IL$Yield.bu.acre,
  x=d.IL[,c('Year','ORCDRC.sl3','SNDPPT.sl3','PHIKCL.sl3')]
)

## Call Stan models
cv.model <- stan(file = "code/models/stan/linear-model.stan", 
                    data = data.list, 
                    control = list(adapt_delta=0.99,max_treedepth=15), chains = 4)
temp.model <- stan(file = "code/models/stan/linear-model.stan", 
                   data = temp.data, 
                   control = list(adapt_delta=0.99,max_treedepth=15), chains = 2)

## Model results
print(model,pars='beta',probs=c(0.05,0.95))
plot(model,pars=c("beta_std"))

print(temp.model,pars='beta',probs=c(0.05,0.95))
plot(temp.model,pars=c("beta_std"))



## Posterior predictive checks
### Corn yield
y_pred <- extract(model,pars='y_tilde')
y_pred <- unlist(y_pred, use.names=FALSE)
yield.pp.data <- data.frame(
  c(y_pred,data.list$y),
  c(rep("y_pred",length(y_pred)),
    rep("y_obs",length(data.list$y)))
)
names(yield.pp.data) <- c("y","type")
ggplot(yield.pp.data, aes(x=y)) + 
  geom_density(aes(group=type, fill=type), alpha=0.75) + theme_bw() +
  xlab("Yield") + ylab("Density") +
  # scale_fill_manual(values=wes_palette("Royal1",n=2)) +
  theme(
    legend.title = element_blank(),
    legend.position = c(0.85,0.55),
    panel.grid = element_blank(),
    panel.background = element_rect(fill="white"),
    plot.background = element_rect(fill="white")
  )
rm(y_pred)




## CHECKING IN A DROUGHT YEAR
d.2012 <- filter(d.IL,Year==2012)
summary(lm(Yield.bu.acre~ORCDRC.sl3+SNDPPT.sl3+PHIKCL.sl3,data=d.2012))
d.2013 <- filter(d.IL,Year==2013)
d.2013$H <- 10^d.2013$PHIKCL.sl3

library(arm)

m <- lm(Yield.bu.acre~ORCDRC.sl3+SNDPPT.sl3+H,data=d.2013)
m <- lm(Yield.bu.acre~ORCDRC.sl3+SNDPPT.sl3+PHIKCL.sl3,data=d.2013)
arm::standardize(m)

summary(lm(Yield.bu.acre~ORCDRC.sl3+SNDPPT.sl3+PHIKCL.sl3,data=d.2013))
summary(lm(Yield.bu.acre~ORCDRC.sl3+SNDPPT.sl3+H,data=d.2013))

summary(lm(Yield.bu.acre~ORCDRC.sl3+SNDPPT.sl3+PHIKCL.sl3+Year,data=d.IL))
