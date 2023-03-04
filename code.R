# write code here before pasting to shiny
library(dplyr)
library("tidyverse")
library(ggplot2)
library(plotly)

# Load the co2 dataset into a df
co2_df <- read.csv("https://raw.githubusercontent.com/owid/co2-data/master/owid-co2-data.csv", stringsAsFactors = FALSE)

# Create a df with just country, year, land use co2 pc, gas co2 pc, oil co2 pc, consumption co2 pc, total co2 pc
new_co2_df <- co2_df %>% 
  select(country, year, co2_per_capita, consumption_co2_per_capita, gas_co2_per_capita, land_use_change_co2_per_capita, oil_co2_per_capita)

# Change the label of CO2 categories
colnames(new_co2_df) <- c("country", "year", "CO2", "Consumption CO2", "Gas CO2", "Land-Use Change CO2", "Oil CO2")

# Create a pivoted df that combines all co2 categories.
pivoted_new_co2_df <- new_co2_df %>% 
  pivot_longer(!c(year, country), # Ignored
  names_to = "co2_category",
  values_to = "amount")

# Write code that selects a certain country to view
select_country <- pivoted_new_co2_df %>%
  filter(country %in% "China")

# Create a plot
sample_select_country_plot <- ggplot(data = select_country) +
  geom_point(aes(x = year,
                 y = amount,
                 color = co2_category,
                 text = paste("Year:", year, "<br>",
                              "Medium:", co2_category, "<br>", 
                              "Amount of CO2 per capita:", amount))) +
  geom_line(aes(x = year,
                y = amount,
                color = co2_category)) +
  labs(title = "CO2 Emissions per capita for China",
       x = "Year",
       y = "CO2 Emitted Per Person (in million tonnes)",
       color = "CO2 Type")

ggplotly(sample_select_country_plot, tooltip = "text")


unique(pivoted_new_co2_df$country)

max(pivoted_new_co2_df$year)

