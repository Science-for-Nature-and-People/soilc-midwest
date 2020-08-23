library(tidyverse)

yield <- read_rds("data/yield_08062020.rds")

spei <- as_tibble(readr::read_csv("data/weather/SPEI/SPEI.csv", col_types = list(col_character(),
                                                                                 col_character(),
                                                                                 col_character(),
                                                                                 col_double(),
                                                                                 col_double(),
                                                                                 col_double()))) %>%
  mutate(GEOID = case_when(str_length(fips) == 4 ~ paste("0",.$fips, sep = ""),
                          TRUE ~ .$fips)) %>%
  filter(GEOID %in% yield$GEOID,
         year >= 2000) %>%
  dplyr::select(-state, -county) %>%
  group_by(GEOID, year) %>%
  summarize(summer_spei = mean(subset(spei, month %in% 5:8))) %>%
  ungroup(.)

write_rds(spei, "data/weather/spei.rds")
