# Load required packages
library(caret)
library(janitor)
library(tidyverse)

## Data manipulation

# Import data
goal3_raw <- read_excel('Goal3.xlsx', sheet = 1)

# Convert relevant variables to percentages
goal3 <- goal3_raw %>% 
  mutate(Value = as.numeric(Value)) %>% 
  mutate(Value = case_when(Units == 'PER_100000_LIVE_BIRTHS' ~ Value / 1000,
                           Units == 'PER_1000_LIVE_BIRTHS' ~ Value / 10,
                           Units == 'PER_1000_UNINFECTED_POP' ~ Value / 10,
                           Units == 'PER_100000_POP' ~ Value / 1000,
                           Units == 'PER_1000_POP' ~ Value / 10,
                           Units == 'PER_10000_POP' ~ Value / 100,
                           TRUE ~ Value))

# Convert relevant variables to percentages where 0 is good
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

# Remove irrelevant variables
goal3 <- goal3 %>% 

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


