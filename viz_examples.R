# Load required packages
library(tidyverse)

# Read in the data
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
