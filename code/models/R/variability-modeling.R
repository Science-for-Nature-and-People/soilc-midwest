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
data <- read_csv("data/DATASET.csv")

# DEFINE DATA
## List data for Stan model
data.list <- list(
  N=nrow(DATASET),
  K=ncol(DATASET),
  y=DATASET$yield,
  x=DATASET[,c('WHATEVERVARIABLES')]
)

## Call Stan models
model <- stan(file = "code/models/stan/linear-model.stan", 
                    data = data.list, 
                    control = list(adapt_delta=0.99,max_treedepth=15), chains = 4)

## Model results
print(model,pars='beta',probs=c(0.05,0.95))
plot(model,pars=c("beta_std"))


## Posterior predictive checks
### Wheat yield
y_pred <- extract(model,pars='y_tilde')
y_pred <- unlist(y_pred, use.names=FALSE)
yield.pp.data <- data.frame(
  c(y_pred,yield.list$y),
  c(rep("y_pred",length(y_pred)),
    rep("y_obs",length(data.list$y)))
)
names(yield.pp.data) <- c("y","type")
ggplot(yield.pp.data, aes(x=y)) + 
  geom_density(aes(group=type, fill=type), alpha=0.75) + theme_bw() +
  xlab("Yield") + ylab("Density") +
  scale_fill_manual(values=wes_palette("Royal1",n=2)) +
  theme(
    legend.title = element_blank(),
    legend.position = c(0.85,0.55),
    panel.grid = element_blank(),
    panel.background = element_rect(fill="white"),
    plot.background = element_rect(fill="white")
  )
rm(y_pred)