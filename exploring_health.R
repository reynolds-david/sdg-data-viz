# Exploratory Analysis of UN Sustainability Goal 3
# # What is the world overview and trend since 2010 (if historical data is available) for your selected goal? Make sure to dive deep into the targets, indicators, and series.  
# # Which targets, indicators, and series are lagging for the selected goal compared to the worldwide trend?  
# # Which countries are underperforming or overperforming? How do these countries compare to their region, income group, or world average? 
# # How does the U.S. compare in these area's of foreign assistances to China and Russia (if data is available)? Do these countries appear to pursue similar area's of foreign assistance, or contrasting? 
# # How would you advise the U.S. Government and its partners to assist countries in the endeavor to improve the metrics related to the selected goal?  
#   

################################## 1. Setup ##################################

# Import libraries
library(tidyverse)
library(janitor)
library(openxlsx)
library(caret)

# Import data
goal3_raw <- read.xlsx('Health Data\\Goal3.xlsx')

# Convert all data to a percent out of 100
  # Not percentage-able: 3.2.1, 3.2.2, 3.3.5, 3.4.1, 3.4.2, 53.5.2?, 3.8.1, 3.b.2
goal3 <- goal3_raw %>% 
  mutate(Value = as.numeric(Value)) %>% 
  mutate(Value = case_when(Units == 'PER_100000_LIVE_BIRTHS' ~ Value / 1000,
                           Units == 'PER_1000_LIVE_BIRTHS' ~ Value / 10,
                           Units == 'PER_1000_UNINFECTED_POP' ~ Value / 10,
                           Units == 'PER_100000_POP' ~ Value / 1000,
                           Units == 'PER_1000_POP' ~ Value / 10,
                           Units == 'PER_10000_POP' ~ Value / 100,
                           TRUE ~ Value))

# Convert all data to a percent where 0 is good
  # Example: take 1 - proportion of births attended by skilled health professional, because you want that proportion to be high but all others to be low
goal3 <- goal3 %>% 
  mutate(Value = case_when(SeriesCode == 'SH_STA_BRTC' ~ 100 - Value,
                           SeriesCode == 'SH_SUD_TREAT' ~ 100 - Value,
                           SeriesCode == 'SH_ACS_UNHC' ~ 100 - Value,
                           SeriesCode == 'SH_ACS_DTP3' ~ 100 - Value,
                           SeriesCode == 'SH_ACS_HPV' ~ 100 - Value,
                           SeriesCode == 'SH_ACS_MCV2' ~ 100 - Value,
                           SeriesCode == 'SH_ACS_PCV3' ~ 100 - Value,
                           SeriesCode == 'SH_HLF_EMED' ~ 100 - Value,
                           TRUE ~ Value))

goal3_perc <- goal3 %>% 
  filter(!(Units %in% c("NUMBER", "INDEX", "CON_USD"))) %>% 
  filter(TimePeriod >= 2010)

  # TODO: Get data on regions and income levels to incorporate
goal3_perc <- goal3_perc %>% 
  group_by(TimePeriod, GeoAreaCode) %>% 
  mutate(geo_mean = mean(Value)) %>% 
  ungroup() %>% 
  group_by(TimePeriod, Target) %>% 
  mutate(target_mean = mean(Value)) %>% 
  ungroup()


################################## 2. Trends ##################################

# What is the world overview trend since 2010?
  # Graph of world Target trends since 2010
  # TODO: incorporate indicators measured in numbers, not just percentages
  # TODO: have this be a drillthrough graph, where you can filter to a target and only see the associated indicators
  # TODO: have this be a drillthrough graph from indicator to series
  # TODO: make the numbers associated to the description
goal3_perc %>% 
  filter(GeoAreaName == 'World') %>% 
  group_by(TimePeriod, Target) %>% 
  summarise(target_mean = mean(Value)) %>% 
  mutate(TimePeriod = as.factor(TimePeriod)) %>% 
  ggplot() +
    geom_path(aes(x = TimePeriod, y = target_mean, group = Target, color = Target))

  # Graph of overall average of indicators since 2010
goal3_perc %>% 
  filter(GeoAreaName == 'World') %>% 
  group_by(TimePeriod) %>% 
  summarise(target_mean = mean(Value)) %>% 
  mutate(TimePeriod = as.factor(TimePeriod)) %>% 
  ggplot() +
    geom_path(aes(x = TimePeriod, y = target_mean, group = 1))


################################## 3. Lagging ##################################

# Which targets/series/indicators are lagging?
lagging_targets <- goal3_perc %>% 
  filter(GeoAreaName == 'World') %>% 
  group_by(TimePeriod) %>% 
  mutate(world_avg = mean(Value)) %>% 
  ungroup() %>% 
  group_by(TimePeriod, Target) %>% 
  mutate(target_avg = mean(Value)) %>% 
  ungroup() %>% 
  mutate(diff = world_avg - target_avg)

  # Table of targets who are doing the worst
bottom10 <- lagging_targets %>% 
  filter(TimePeriod == 2019) %>%
  distinct(TimePeriod, Target, world_avg, target_avg, diff) %>% 
  arrange(desc(diff)) %>% 
  head(5)

# Table of targets who are doing the best
top10 <- lagging_targets %>% 
  filter(TimePeriod == 2019) %>% 
  distinct(TimePeriod, Target, world_avg, target_avg, diff) %>% 
  arrange(diff) %>% 
  head(5)


################################## 4. Countries ##################################



