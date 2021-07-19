# Load required packages
library(readxl)
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
goal3 <- goal3 %>% select()

# Reshape the data

# Remove male/female for relevant variables



