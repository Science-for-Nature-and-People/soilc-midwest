# This script is to investigate how many years is 
# each county in each SPEI category and summarize by state.
# Started 4/12/2023

# pull in the data 

library(ggplot2)

# data #####
all_data <- readRDS("data/all_data_2020.08.07.rds")  # in R projects the default working directory is the project

# first aggregate by county
spei_co <- aggregate(loss_cost ~ spei.cut + county_name + state_alpha, data=all_data, FUN="length") # loss_cost could be substituted with any other variable in all_data
colnames(spei_co)[4] <- "county_years"

# second, aggregate by state
spei_st <- aggregate(loss_cost ~ spei.cut + state_alpha, data=all_data, FUN="length")
colnames(spei_st)[3] <- "county_years"

# third, make plot by states
windows(xpinch=200, ypinch=200, width=5, height=5)

ggplot(spei_st, aes(x=spei.cut, y=county_years)) + 
  geom_bar(stat="identity", fill="deepskyblue1") + 
  facet_wrap(~ state_alpha, ncol=9) +
  scale_x_discrete(limits=rev(levels(spei_st$spei.cut))) + # so that bars go from normal to very severe
  ylab("County-Years") +
  xlab("SPEI category") +
  theme(axis.text.x=element_text(angle=-30, hjust=0, size=7),
        plot.margin=margin(r=40, l=10, b=10),
      panel.grid.minor=element_blank(), 
      panel.grid.major=element_blank() ,
      panel.background = element_rect(fill = NA) ,
      panel.border=element_rect(color="grey50", fill=NA, linewidth=0.5),
      strip.text=element_text(size=rel(1), face="bold", vjust=-2),
      strip.background = element_rect(fill=NA),
      panel.spacing.y=unit(-1, "points"),
      legend.text=element_text(size=14),
      legend.title=element_text(size=14)
  )

ggsave("code/plots/SPEI category by county-year.png")


# make plot using % rather than counts
spei_sttot <- aggregate(county_years~state_alpha, data=spei_st, FUN="sum")
colnames(spei_sttot)[2] <- "total"

spei_st <- dplyr::left_join(spei_st, spei_sttot)
spei_st$percent <- (spei_st$county_years / spei_st$total)*100

ggplot(spei_st, aes(x=spei.cut, y=percent)) + 
  geom_bar(stat="identity", fill="gray35") + 
  facet_wrap(~ state_alpha, ncol=9) +
  scale_x_discrete(limits=rev(levels(spei_st$spei.cut))) + # so that bars go from normal to very severe
  ylab("Percent of County-Years") +
  xlab("SPEI category") +
  theme(axis.text.x=element_text(angle=-30, hjust=0, size=7),
        plot.margin=margin(r=40, l=10, b=10),
        panel.grid.minor=element_blank(), 
        panel.grid.major=element_blank() ,
        panel.background = element_rect(fill = NA) ,
        panel.border=element_rect(color="grey50", fill=NA, linewidth=0.5),
        strip.text=element_text(size=rel(1), face="bold", vjust=-2),
        strip.background = element_rect(fill=NA),
        panel.spacing.y=unit(-1, "points"),
        legend.text=element_text(size=14),
        legend.title=element_text(size=14)
  )

ggsave("code/plots/SPEI category by county-year percent.png")

write.csv(spei_co, "code/data-processing/county-years by spei category_county level.csv")
write.csv(spei_st, "code/data-processing/county-years by spei category_state level.csv")
getwd()
