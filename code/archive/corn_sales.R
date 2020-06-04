# Call in corn sales data via NASS
# Specify API key and authorize
api_key <- as.character(read.csv("../../code/NASS_API_key.csv", header = F)[1,1])       # api key
rnassqs::nassqs_auth(api_key)

# Call in sales data and modify
corn.sales <- rnassqs::nassqs(short_desc = "CORN, GRAIN - PRODUCTION, MEASURED IN $", year__GE = 2000, source_desc = "SURVEY") 

test <- corn.sales %>%
  filter(agg_level_desc %in% "COUNTY") %>%
  dplyr::select()

unique(corn.sales$source_desc)

transmute(Corn.sales =as.numeric(gsub(pattern = ",", replacement = "", Value)),
          GEOID = paste(state_fips_code, county_code, sep = ""),
          Year = as.numeric(year))
