# Load required packages
library(tidyverse)
library(caret)
library(factoextra)
library(cluster)

# Import the data
goal3 <- read.csv('goal3.csv')

############### Worldwide over time, per target ###############
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


############### Top and bottom countries over time ###############
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


############### Clustering ###############

ex4_dat <- goal3 %>% 
  drop_na("iso_alpha3_code") %>% 
  select(iso_alpha3_code, series_code, time_period, norm) %>% 
  group_by(iso_alpha3_code, series_code, time_period) %>%
  summarize(value = mean(norm, na.rm = T)) %>% 
  group_by(iso_alpha3_code, series_code) %>% 
  mutate(value = as.numeric(scale(value))) %>% 
  group_by(iso_alpha3_code, series_code) %>% 
  summarize(value = mean(value)) %>% 
  pivot_wider(names_from = series_code, values_from = value)

ex4_test <- ex4_dat %>% 
  # Keep only columns where there is data for 90% of columns
  purrr::discard(~sum(is.na(.x))/length(.x)* 100 >=90) %>% 
  # Fill NAs with the column mean
  na_mean() %>% 
  # Make sure all columns are numeric
  mutate_at(vars(-iso_alpha3_code), as.numeric) %>% 
  # Convert country column to names
  remove_rownames %>% column_to_rownames(var="iso_alpha3_code")


# Choose K by comparing the 3 methods 
# BC NOTE: we can ignore this part, think it's too complicated for people to try and choose their own K off of something. Was just curious myself
fviz_nbclust(ex4_test, kmeans, method = "wss")
fviz_nbclust(ex4_test, kmeans, method = "silhouette")
gap_stat <- clusGap(ex4_test, FUN = kmeans, nstart = 25,
        K.max = 10, B = 50)
fviz_gap_stat(gap_stat)

# Run the model. IN THE VIZ, HAVE K BE CHOSEN BY THE USER. BUT MAYBE BE AUTO SET AT 3
k <- 6
final <- kmeans(ex4_test, k, nstart = 25)

# Visualize the clusters
fviz_cluster(final, data = ex4_test)

### Unfortunately, remembering that there is no way to pull feature importance from unsupervised learning models, so this is all we go with. Think we should add some language in the app explaining what people are seeing
