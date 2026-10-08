################################################################################
# Assignment 2 EVR 628
################################################################################
#
# Nayonika Choudhury
# nxc864@miami.edu
# 09/30/2026
#
# Script 1 
#
################################################################################

library(tidyverse)
raw_volcanoes <- read_tsv(
  file = "data/raw/volcano.tsv",
  show_col_types = FALSE)

## 36 column names 

# Volcano type denoted by "Type" 
# VEI denoted by "VEI" 
# Fatalities denotes by "Deaths" 

clean_volcanoes <- raw_volcanoes %>%
  select(
    year = Year,
    name = Name,
    country = Country,
    latitude = Latitude, 
    longitude = Longitude, 
    elevation = `Elevation (m)`,
    volcano_type = Type,
    vei = VEI, 
    deaths = Deaths) %>%
  
  mutate(
    volcano_type_clean = case_when(
      str_detect(volcano_type, "(?i)strato") ~ "Stratovolcano",
      str_detect(volcano_type, "(?i)shield") ~ "Shield Volcano",
      str_detect(volcano_type, "(?i)caldera") ~ "Caldera",
      str_detect(volcano_type, "(?i)complex|compound") ~ "Complex Volcano",
      is.na(volcano_type) ~ "Unknown",
      TRUE ~ "Other"
    )
  ) %>%
  
  mutate(
    deaths_clean = ifelse(is.na(deaths), 0, deaths),
    vei_clean = ifelse(is.na(vei), 0, vei)
  ) %>%
  
  mutate(
    vei_category = case_when(
      vei_clean <= 2 ~ "Low Explosivity (VEI 0-2)",
      vei_clean <= 4 ~ "Moderate Explosivity (VEI 3-4)",
      vei_clean >= 5 ~ "Catastrophic (VEI 5+)"
    ),
    decade = (year %/% 10) * 10
  ) %>%
  
  # Filter out missing coordinates and focus on historical records (year >= 1500)
  filter(!is.na(latitude), !is.na(longitude), year >= 1500)

glimpse(clean_volcanoes)

write_rds(clean_volcanoes, file = "data/processed/clean_volcanoes.rds")

clean_volcanoes

