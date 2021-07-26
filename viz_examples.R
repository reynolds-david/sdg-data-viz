# Load required packages
library(tidyverse)

# Import the data
goal3 <- read.csv('goal3.csv')

# Plot the first target for worldwide over time
ex1_dat <- goal3 %>% 
  filter(target == "3.1", geo_area_code == "1") %>% 
  select(time_period, norm) %>% 
  group_by(time_period) %>% 
  summarize(value = mean(norm)) %>%
  mutate(value = scale(value))

ex1_plot <- ggplot(data = ex1_dat, aes(x = time_period, y = value)) + geom_line() + 
  labs(y = "target 3.1") + theme_minimal()

# Plot all targets for worldwide over time
ex2_dat <- goal3 %>% 
  filter(geo_area_code == "1") %>% 
  select(target, time_period, norm) %>% 
  group_by(target, time_period) %>% 
  summarize(value = mean(norm)) %>% 
  group_by(target) %>% 
  mutate(value = scale(value))

ex2_plot <- ggplot(data = ex2_dat, aes(x = time_period, y = value, color = target)) + 
  geom_line() + theme_minimal()

# 
ex3_dat <- goal3 %>% 
  drop_na("iso_alpha3_code") %>% 
  select(geo_area_name, time_period, norm) %>% 
  group_by(geo_area_name, time_period) %>% 
  summarize(value = mean(norm)) %>% 
  group_by(geo_area_name) %>% 
  mutate(value = scale(value)) %>% 
  filter(time_period == 2020)

ex3_top_plot <- ggplot(data = ex3_dat %>% 
                     arrange(desc(value)) %>% 
                     head(10), 
                   aes(x = reorder(geo_area_name, -value), y = value)) + geom_bar(stat = 'identity') + theme_minimal()

ex3_bottom_plot <- ggplot(data = ex3_dat %>% 
                     arrange(desc(value)) %>% 
                     tail(10), 
                   aes(x = reorder(geo_area_name, -value), y = value)) + geom_bar(stat = 'identity') + theme_minimal()


ex3_top_plot
ex3_bottom_plot
