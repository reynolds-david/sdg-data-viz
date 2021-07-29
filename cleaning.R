# Load required packages
library(janitor)
library(readxl)
library(tidyverse)

# Import the data
goal3_raw <- read_excel('Goal3.xlsx', sheet = 1)
goal3_raw$Value <- as.numeric(goal3_raw$Value)
countries <- read_xlsx('Country_List.xlsx') %>% 
  clean_names()

# Remove irrelevant columns
goal3 <- goal3_raw %>% select("target" = Target, "indicator" = Indicator, 
                          "series_code" = SeriesCode, "geo_area_code" = GeoAreaCode, 
                          "geo_area_name" = GeoAreaName, "time_period" = TimePeriod, 
                          "value" = Value)

# Merge with countries to get ISO codes
goal3 <- goal3 %>% 
  left_join(countries, by = c("geo_area_name" = "country_or_area"))

goal3 <- goal3 %>% 
  select(-m49_code)

# Group by target, indicator, series_code, geo_area_code, geo_area_name, and time_period
goal3 <- goal3 %>% 
  group_by(target, indicator, series_code, geo_area_code, geo_area_name, time_period, iso_alpha3_code) %>% 
  summarize(value = mean(value))

# Convert relevant series to percentages where 0 is good
goal3 <- goal3 %>% 
  mutate(value = case_when(series_code == 'SH_STA_BRTC' ~ 100 - value,
                           series_code == 'SH_SUD_TREAT' ~ 100 - value,
                           series_code == 'SH_FPL_MTMM' ~ 100 - value,
                           series_code == 'SH_ACS_UNHC' ~ 100 - value,
                           series_code == 'SH_ACS_DTP3' ~ 100 - value,
                           series_code == 'SH_ACS_HPV' ~ 100 - value,
                           series_code == 'SH_ACS_MCV2' ~ 100 - value,
                           series_code == 'SH_ACS_PCV3' ~ 100 - value,
                           series_code == 'SH_HLF_EMED' ~ 100 - value,
                           series_code == 'SH_MED_DEN' ~ 100 - (value / 100),
                           series_code == 'SH_MED_HWRKDIS' ~ 100 - value,
                           series_code == 'SH_MED_HWRKDIS' ~ 100 - value,
                           TRUE ~ value))

# Take the negative of DC_TOF_HLTHL and DC_TOF_HLTHNT since they are not continuous but higher 
# values are better
goal3 <- goal3 %>% 
  mutate(value = case_when(series_code == 'DC_TOF_HLTHL' ~ -value,
                            series_code == 'DC_TOF_HLTHNT' ~ -value,
                            TRUE ~ value))

# Group by series_code and geo_area_code and then standardize
goal3 <- goal3 %>% 
  group_by(series_code, geo_area_code) %>% 
  mutate(norm = scale(value))

# Remove rows in which norm is NA due to lack of sample size
goal3 <- goal3 %>% filter(!is.na(norm))

# Remove rows that do not apply to countries (World, North America, etc)
# goal3 <- goal3 %>% drop_na("iso_alpha3_code") %>% as.data.frame()

# Export the data
write_csv(goal3, "goal3.csv")
