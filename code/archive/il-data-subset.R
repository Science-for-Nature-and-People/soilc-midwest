load("/home/shares/soilcarbon/soilc-midwest/data/Yield_full.RData")


# Filter out soil data cells that are not primarily corn-growing ####

library(tigris)
library(raster)
library(sp)
library(rgeos)
library(GSIF)
library(dplyr)

county.boundaries <- counties(state = c(d$FIPS))
county.boundaries <- county.boundaries[county.boundaries@data$GEOID %in% d$FIPS_county,]

county.boundaries <- county.boundaries[county.boundaries@data$STATEFP %in% "17",]

centers <- data.frame(gCentroid(county.boundaries, byid=TRUE))

coordinates(centers) = ~ x + y
proj4string(centers) <- CRS('+proj=longlat +ellps=WGS84 +datum=WGS84')

## Call map unit information from SDA ####

soilgrids.r <- REST.SoilGrids("ORCDRC,CEC,CLYPPT,SLTPPT,SNDPPT,PHIKCL&depths=sl3")
soilgrids.Chemical <- GSIF::over(soilgrids.r, centers)
soilgrids.Chemical.major <- soilgrids.Chemical[,c("CECSOL.sl3","CLYPPT.sl3","ORCDRC.sl3","SLTPPT.sl3","SNDPPT.sl3","PHIKCL.sl3")] 

IL.counties <- data.frame(county.boundaries@data[,1:5],soilgrids.Chemical.major)

IL.counties %>%
  left_join(d, by = c("GEOID" = "FIPS_county")) -> test


cv <- function(x){(sd(x)/mean(x))}

str(test)

test -> d.IL

d.IL %>%
  group_by(GEOID, NAME,CECSOL.sl3,CLYPPT.sl3,ORCDRC.sl3,SLTPPT.sl3,SNDPPT.sl3,PHIKCL.sl3) %>%
  summarise(CV.yield = cv(Yield.bu.acre)) -> d.IL.summary

save(d.IL, d.IL.summary, file = "/home/shares/soilcarbon/soilc-midwest/data/IL_toy_data_05112018.RData")

