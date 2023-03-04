# Summary Statistics

library(dplyr)
library("tidyverse")
library(ggplot2)
library(plotly)

# Load co2 dataframe
co2_df <- read.csv("https://raw.githubusercontent.com/owid/co2-data/master/owid-co2-data.csv", stringsAsFactors = FALSE)

# Create a df with just country, year, land use co2 pc, gas co2 pc, oil co2 pc, consumption co2 pc, total co2 pc
new_co2_df <- co2_df %>% 
  select(country, year, co2_per_capita, consumption_co2_per_capita, gas_co2_per_capita,
         land_use_change_co2_per_capita, oil_co2_per_capita)

# Data frame for income classes
income_co2 <- co2_df %>% 
 filter(str_detect(co2_df$country, "countries"))

# How many total emissions did Low-income countries emit in 2021
low_inc_co2 <- income_co2 %>% 
  filter(year == "2021") %>% 
  filter(country == "Low-income countries") %>% 
  pull(co2)

# How many total emissions did Lower-middle-income countries emit in 2021
low_mid_inc_co2 <- income_co2 %>% 
  filter(year == "2021") %>% 
  filter(country == "Lower-middle-income countries") %>% 
  pull(co2)

# How many total emissions did Upper-middle-income countries emit in 2021
up_mid_inc_co2 <- income_co2 %>% 
  filter(year == "2021") %>% 
  filter(country == "Upper-middle-income countries") %>% 
  pull(co2)

# How many total emissions did High-income emit in 2021
high_inc_co2 <- income_co2 %>% 
  filter(year == "2021") %>% 
  filter(country == "High-income countries") %>% 
  pull(co2)



# Which year had the highest amount of co2_per_capita
year_highest_co2_per_capita <- new_co2_df %>% 
  group_by(year) %>% 
  summarise(total_co2_per_capita = sum(co2_per_capita, na.rm = TRUE)) %>% 
  filter(total_co2_per_capita == max(total_co2_per_capita, na.rm = TRUE)) %>% 
  pull(year)

# How many tonnes (in millions) of CO2 emissions did the year with the highest amount of co2_per_capita have
amount_year_highest_co2_per_capita <- new_co2_df %>% 
  group_by(year) %>% 
  summarise(total_co2_per_capita = sum(co2_per_capita, na.rm = TRUE)) %>% 
  filter(total_co2_per_capita == max(total_co2_per_capita, na.rm = TRUE)) %>% 
  pull(total_co2_per_capita)



# Which year had the highest amount of land_use_change_co2_per_capita
year_highest_land_use_change_co2_per_capita <- new_co2_df %>% 
  group_by(year) %>% 
  summarise(total_land_use_change_co2_per_capita = sum(land_use_change_co2_per_capita, na.rm = TRUE)) %>% 
  filter(total_land_use_change_co2_per_capita == max(total_land_use_change_co2_per_capita, na.rm = TRUE)) %>% 
  pull(year)

# Which year had the highest amount of co2_per_capita
amount_year_highest_land_use_change_co2_per_capita <- new_co2_df %>% 
  group_by(year) %>% 
  summarise(total_land_use_change_co2_per_capita = sum(land_use_change_co2_per_capita, na.rm = TRUE)) %>% 
  filter(total_land_use_change_co2_per_capita == max(total_land_use_change_co2_per_capita, na.rm = TRUE)) %>% 
  pull(total_land_use_change_co2_per_capita)




# CO2 emissions from all, except land-use change
# Which country had the highest co2_per_capita
country_highest_co2_per_capita <- co2_df %>%
  filter(co2_per_capita == max(co2_per_capita, na.rm = TRUE)) %>% 
  pull(country)

# How many emissions (in millions of tonnes) of co2 was produced at its highest
highest_amount_co2_per_capita <- co2_df %>%
  filter(co2_per_capita == max(co2_per_capita, na.rm = TRUE)) %>% 
  pull(co2_per_capita)

# What year did the country with the highest co2_per_capita happen
year_highest_co2_per_capita <- co2_df %>%
  filter(co2_per_capita == max(co2_per_capita, na.rm = TRUE)) %>% 
  pull(year)



# CO2 emissions from oil
# Which country had the highest oil_co2_per_capita
country_highest_oil_co2_per_capita <- co2_df %>%
  filter(oil_co2_per_capita == max(oil_co2_per_capita, na.rm = TRUE)) %>% 
  pull(country)

# How many emissions (in millions of tonnes) of oil co2 was produced at its highest
highest_amount_oil_co2_per_capita <- co2_df %>%
  filter(oil_co2_per_capita == max(oil_co2_per_capita, na.rm = TRUE)) %>% 
  pull(oil_co2_per_capita)

# What year did the country with the highest oil_co2_per_capita happen
year_highest_oil_co2_per_capita <- co2_df %>%
  filter(oil_co2_per_capita == max(oil_co2_per_capita, na.rm = TRUE)) %>% 
  pull(year)



# CO2 emissions from land-use change
# Which country had the highest land_use_change_co2_per_capita
country_highest_land_use_change_co2_per_capita <- co2_df %>%
  filter(land_use_change_co2_per_capita == max(land_use_change_co2_per_capita, na.rm = TRUE)) %>% 
  pull(country)

# How many emissions (in millions of tonnes) of land-use change co2 was produced at its highest
highest_amount_land_use_change_co2_per_capita <- co2_df %>%
  filter(land_use_change_co2_per_capita == max(land_use_change_co2_per_capita, na.rm = TRUE)) %>% 
  pull(land_use_change_co2_per_capita)

# What year did the country with the highest oil_co2_per_capita happen
year_highest_land_use_change_co2_per_capita <- co2_df %>%
  filter(land_use_change_co2_per_capita == max(land_use_change_co2_per_capita, na.rm = TRUE)) %>% 
  pull(year)



# CO2 emissions from consumption
# Which country had the highest consumption_co2_per_capita
country_highest_consumption_co2_per_capita <- co2_df %>%
  filter(consumption_co2_per_capita == max(consumption_co2_per_capita, na.rm = TRUE)) %>% 
  pull(country)

# How many emissions (in millions of tonnes) of consumption co2 was produced at its highest
highest_amount_consumption_co2_per_capita <- co2_df %>%
  filter(consumption_co2_per_capita == max(consumption_co2_per_capita, na.rm = TRUE)) %>% 
  pull(consumption_co2_per_capita)

# What year did the country with the highest consumption_co2_per_capita happen
year_highest_consumption_co2_per_capita <- co2_df %>%
  filter(consumption_co2_per_capita == max(consumption_co2_per_capita, na.rm = TRUE)) %>% 
  pull(year)
