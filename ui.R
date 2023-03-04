library(dplyr)
library("tidyverse")
library(ggplot2)
library(plotly)


# Summary Stats
# Load co2 dataframe
co2_df <- read.csv("https://raw.githubusercontent.com/owid/co2-data/master/owid-co2-data.csv", stringsAsFactors = FALSE)
# Create a df with just country, year, land use co2 pc, gas co2 pc, oil co2 pc, consumption co2 pc, total co2 pc
new_co2_df <- co2_df %>% 
  select(country, year, co2_per_capita, consumption_co2_per_capita, gas_co2_per_capita,
         land_use_change_co2_per_capita, oil_co2_per_capita)
# Create a pivoted df that combines all co2 categories.
pivoted_new_co2_df <- new_co2_df %>% 
  pivot_longer(!c(year, country), # Ignored
               names_to = "co2_category",
               values_to = "amount")
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


# Paragraph's
intro_p <- paste0("Discussions and conversations regarding climate change are increasing as temperatures across the globe experience polarizing highs and lows. People all over the globe are trying to limit their carbon footprint, however, the average individual can only do so much. Big companies and factories are the main drivers for CO2 emissions. On a larger scale, how are individual countries contributing to the emission of CO2? Countries with policies that surround attaining a cleaner carbon footprint tend to have lower CO2 emissions. Are lower income or higher income countries contributing the most to CO2 emissions. Which countries are contributing the most to CO2 emissions? The dataset, \"CO2 and Greenhosue Gas Emissions,\" helps answer these questions. We will be exploring emissions from regular CO2, consumption CO2, gas CO2, land-use change CO2, and oil CO2. We will also explore the amounts that different countries emit of these CO2 types throughout different time ranges.")

data_p <- paste0("The dataset \"CO2 and Greenhouse Gas Emissions\" is a collection of data about CO2 emissions (annual, per capita, cumulative and consumption-based), other greenhouse gases, energy mix, and other relevant metrics. The data was collected, aggregated, and documented by Hannah Ritchie, Max Roser, Edouard Mathieu, Bobbie Macdonald, and Pablo Rosado. The dataset is maintained by Our World In Data whose mission is to create data and conduct research on the world’s largest problems while making them understandable and accessible. The data is collected and built upon a collection of other datasets. The purpose of this data is to examine which countries are emitting hazardous amounts of CO2 and which countries aren't. Additionally, this dataset was created to help make data surrounding important issues more understandable.")

data_p2 <- paste0("One possible limitation of this dataset is the use of multiple sources as the basis for their data. Since \"CO2 and Greenhouse Gas Emissions\" is built upon other datasets, are all the datasets measuring CO2 the same way? If one dataset measures CO2 emissions in a different way than the others, it can misrepresent the actual amount of CO2. The authors would have to make sure and verify that all the datasets that they're using have the same measurement procedures.")

sumstat_p1 <- paste0("There are 4 socioeconomic classes that contributed to CO2 emisions in 2021. Lower income countries emitted ", low_inc_co2, " million tonnes of CO2. Lower middle-income countries emitted ", low_mid_inc_co2, " million tonnes of CO2. Upper middle-income countries emitted " , up_mid_inc_co2, " million tonnes of CO2. Higher income countries emitted ", high_inc_co2, " million tonnes of CO2 in 2021. From these statistics, it's apparent that upper-middle income countries emit the most CO2.")

sumstat_p2 <- paste0(year_highest_co2_per_capita, " was the year with the largest number of CO2 emissions per capita with ", amount_year_highest_co2_per_capita, " million tonnes per person. ", year_highest_land_use_change_co2_per_capita, " was the year with the largest number of land-use change CO2 emissions per capita with ", amount_year_highest_land_use_change_co2_per_capita, " million tonnes per person.")

sumstat_p3 <- paste0("The country that had the highest annual amount of CO2 emissions per capita is ", country_highest_co2_per_capita, " in ", year_highest_co2_per_capita, ". ", country_highest_co2_per_capita, " emitted ", highest_amount_co2_per_capita, " million tonnes of CO2 per person. Seemingly, ", country_highest_oil_co2_per_capita, " also had the highest annual amount of CO2 emissions from oil in ", year_highest_oil_co2_per_capita, ". In ", year_highest_oil_co2_per_capita, ", each person emitted in ", country_highest_oil_co2_per_capita, " emitted ", highest_amount_oil_co2_per_capita, " million tonnes of CO2. This is important to note since CO2 per capita and CO2 from oil per capita result in the same number. However, ", country_highest_land_use_change_co2_per_capita, " had the highest annual CO2 emissions from land-use changes, with each person emitting ", highest_amount_land_use_change_co2_per_capita, " million tonnes in ", year_highest_land_use_change_co2_per_capita, ". Lastly, ", country_highest_consumption_co2_per_capita, " had the highest annual emissions of CO2 from consumption in ", year_highest_consumption_co2_per_capita, ", with each person emitting ", highest_amount_consumption_co2_per_capita, " million tonnes of CO2.")

conclusion_p <- paste0("From the visualization, we are able to attain a better understanding of different countries CO2 emissions over time. We're able to see how different types of CO2 emissions (land-use, oil, consumption, etc.) vary over time, and how they vary with each country. We can identify years where certain CO2 emissions either drastically increased, or drastically decreased. With that, we can investigate what historical events happened that year and conclude that it contributed to a country's CO2 emissions.")

viz_explanation_p1 <- paste0("This purpose of this trends over time chart is to help visualize how a country's CO2 emissions change over the years. It allows us to choose which years we want to observe through the slider. We're able to turn on/off the types of CO2's we want to see by clicking on the CO2 type. This chart also allows us to see certain patterns that arise. For example, we can pinpoint when a country's land-use change CO2 annual emissions decreases, but their oil CO2 annual emissions increases.")

viz_explanation_p2 <- paste0("This line plot allows us to see how a country's annual CO2 emissions change over time. We can also see when a country's overall CO2 emissions are the same value as another CO2 type. For example, if you choose the country Sint Maarten and toggle between CO2 and Oil CO2, we can see that those 2 types have the same number.")


# Credits
credits <- HTML('<footer>
          Visualizations designed and built by Jason Cabusao
         </footer>')

intro_credits <- HTML('<footer>
          Written by Jason Cabusao
         </footer>')


intro_tab <- tabPanel(
  "Introduction",
  h3("Introduction"),
  p(intro_p),
  h3("Data"),
  p(data_p),
  p(data_p2),
  h3("Summary Statistics"),
  p(sumstat_p1),
  p(sumstat_p2),
  p(sumstat_p3),
  h3("Visualization Conclusion"),
  p(conclusion_p),
  intro_credits
)

year_select <- sliderInput(
  inputId = "year_slider",
  label = "Select a range of years:",
  sep = "",
  min = 1750,
  max = 2021,
  value = c("1960", "2010")
)

country_select <- selectInput(
  inputId = "user_country_selection", 
  label = "Select a country",
  choices = unique(pivoted_new_co2_df$country),
  selected = "United States",
  multiple = FALSE
)

viz_tab <- tabPanel(
  "CO2 Visualization",
  sidebarPanel(year_select, 
               country_select),
  mainPanel(plotlyOutput("bar"),
            br(),
            br(),
            p(viz_explanation_p1),
            p(viz_explanation_p2)
  ),
  credits
)

my_ui <- navbarPage(
  "A4: Climate Change",
  intro_tab,
  viz_tab
)

