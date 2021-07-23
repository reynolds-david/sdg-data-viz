# Exploratory Analysis of UN Sustainability Goal 3
# # What is the?world overview?and?trend since 2010?(if historical data is available)?for your selected goal? Make sure to?dive deep into the targets, indicators, and series.??
# # Which targets, indicators, and series are?lagging for the selected goal compared to the worldwide trend???
# # Which?countries are underperforming or overperforming? How do these?countries compare to their region, income group, or world average??
# # How does the U.S.?compare in these?area's?of?foreign assistances to China and Russia (if data is available)??Do these countries appear to pursue similar?area's?of foreign assistance, or contrasting??
# # How would you?advise the U.S. Government and its partners?to assist countries in the endeavor to improve the metrics related to the selected goal???
#   

################################## 1. Setup ##################################

# Import libraries
library(tidyverse)
library(janitor)
library(openxlsx)
library(caret)
library(factoextra)

# Import data
goal3_raw <- read.xlsx('Goal3.xlsx')
countries <- read.xlsx('Country_List.xlsx')

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

# Keep only the indicators with a percent value
goal3_perc <- goal3 %>% 
  filter(!(Units %in% c("NUMBER", "INDEX", "CON_USD"))) %>% 
  filter(TimePeriod >= 2010)

# Convert all data to a percent where 100 is good (because this makes for better viz)
goal3_perc <- goal3_perc %>% 
  mutate(Value = 100 - Value)

  # TODO: Get data on regions and income levels to incorporate
  # TODO: Get data on foreign investment (US, China, Russia) to incorporate

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
  
  # Get the target average over time
  group_by(TimePeriod, Target) %>% 
  mutate(target_avg = mean(Value)) %>% 
  ungroup() %>% 

  mutate(diff = target_avg - world_avg)

# Graph of difference between targets and world average
  # TODO: incorporate indicators measured in numbers, not just percentages
  # TODO: have this be an interactive where you can choose target, indicator, or series from the drop-down
  # TODO: have this be an interactive where you can choose to see the top 10 or bottom 10 of the above
  # TODO: have this be an interactive where you can choose the year
lagging_targets %>% 
  filter(TimePeriod == 2019) %>%
  distinct(TimePeriod, Target, world_avg, target_avg, diff) %>% 
  arrange(diff) %>% 
  head(10) %>% 
  ggplot() +
    geom_bar(aes(x = reorder(Target, -diff), y = diff), stat = 'identity') +
    labs(title = "Difference between Targets and Overall Average",
         x = "Target",
         y = "Difference (% pts)")



################################## 4. Countries ##################################

# Merge with country data
goal3_countries <- goal3_perc %>% 
  left_join(countries, by = c("GeoAreaName" = "Country.or.Area")) %>% 
  drop_na("ISO-alpha3.code")


lagging_countries <- goal3_countries %>% 
  group_by(TimePeriod, GeoAreaCode) %>% 
  mutate(country_avg = mean(Value)) %>% 
  ungroup()

# Table of countries who are doing the worst
lagging_countries %>% 
  filter(TimePeriod == 2019) %>%
  distinct(TimePeriod, `ISO-alpha3.code`, country_avg) %>% 
  arrange(country_avg) %>% 
  head(10) %>% 
  ggplot() +
    geom_bar(aes(x = reorder(`ISO-alpha3.code`, country_avg), y = country_avg), stat = 'identity') +
    ylim(0,100) +
    labs(title = "Worst-Performing Countries",
       x = "Country",
       y = "Avg Target Score")

# Table of countries who are doing the best
lagging_countries %>% 
  filter(TimePeriod == 2019) %>% 
  distinct(TimePeriod, `ISO-alpha3.code`, country_avg) %>% 
  arrange(country_avg) %>% 
  tail(10) %>% 
  ggplot() +
    geom_bar(aes(x = reorder(`ISO-alpha3.code`, country_avg), y = country_avg), stat = 'identity') +
    ylim(0,100) +
    labs(title = "Best-Performing Countries",
       x = "Country",
       y = "Avg Target Score")

################################## 5. Foreign Assistance ##################################



################################## 6. Recommendation ##################################

# Reference: https://uc-r.github.io/kmeans_clustering


goal3_2019_countries <- goal3_countries %>% 
  filter(TimePeriod == 2019) %>% 
  group_by(GeoAreaCode) %>% 
  mutate(country_avg = mean(Value, na.rm = T)) %>% 
  ungroup() %>% 
  distinct(SeriesCode, GeoAreaCode, GeoAreaName, `ISO-alpha3.code`, country_avg) %>% 
  pivot_wider(names_from = SeriesCode, values_from = country_avg)

# Choosing # of clusters for k-means
fviz_nbclust(goal3_2019_countries, kmeans, method = "wss")
fviz_nbclust(df, kmeans, method = "silhouette")
fviz_gap_stat(gap_stat)

