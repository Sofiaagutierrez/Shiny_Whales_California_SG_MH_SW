
# Load libraries
library(forecast)
library(tidyverse)
library(ggplot2)
library(dplyr)
library(tsibble)
library(lubridate)
library(tseries)
library(feasts)


# Make specific forecasting dataframe
whale_forecast <- read_csv("data/whale_cleaned.csv")

# Check structure of date columns
str(whale_forecast$year)
str(whale_forecast$month) 

# Convert 'month' (factor) to numeric
whale_forecast$month <- match(whale_forecast$month, month.abb)

# Combine 'year' and 'month' into a date format column
whale_forecast$date_column <- 
  as.Date(paste(whale_forecast$year, sprintf("%02d", whale_forecast$month), "01", sep = "-"))

# Split the data by species
whale_species_split <- whale_forecast %>%
  group_by(species) 

# Now, aggregate sightings by date for each species
whale_agg_by_species <- whale_species_split %>%
  group_by(species, date_column) %>%
  summarise(total_sightings = sum(number_sighted, na.rm = TRUE), .groups = "drop")

# Convert each species to a tsibble
whale_blue_tsibble <- whale_agg_by_species %>%
  filter(species == "Blue Whale") %>%
  mutate(date_column = lubridate::ymd(date_column)) %>%
  as_tsibble(key = NULL, index = date_column)

whale_fin_tsibble <- whale_agg_by_species %>%
  filter(species == "Fin Whale") %>%
  mutate(date_column = lubridate::ymd(date_column)) %>%
  as_tsibble(key = NULL, index = date_column)

whale_hump_tsibble <- whale_agg_by_species %>%
  filter(species == "Humpback Whale") %>%
  mutate(date_column = lubridate::ymd(date_column)) %>%
  as_tsibble(key = NULL, index = date_column)

# Convert tsibbles to tibbles and add species column
whale_blue_df <- as_tibble(whale_blue_tsibble) %>% mutate(species = "Blue Whale")
whale_fin_df <- as_tibble(whale_fin_tsibble) %>% mutate(species = "Fin Whale")
whale_hump_df <- as_tibble(whale_hump_tsibble) %>% mutate(species = "Humpback Whale")

# Combine all species data into one data frame
whale_combined <- bind_rows(whale_blue_df, whale_fin_df, whale_hump_df)

# Extract year and month from date_column and create new columns for faceting
whale_combined <- whale_combined %>%
  mutate(year = year(date_column),
         month = month(date_column, label = TRUE))

# Blue Whales: Aggregate sightings by month and year
whale_blue_agg <- whale_blue_df |>
  group_by(date_column) |>
  summarise(total_sightings = sum(total_sightings, na.rm = TRUE))

# Convert to tsibble
whale_blue_tsibble <- whale_blue_agg %>%
  as_tsibble(index = date_column)

# Fin Whales: Aggregate sightings by month and year
whale_fin_agg <- whale_fin_df |>
  group_by(date_column) |>
  summarise(total_sightings = sum(total_sightings, na.rm = TRUE))

# Humpback Whales: Aggregate sightings by month and year
whale_hump_agg <- whale_hump_df |>
  group_by(date_column) |>
  summarise(total_sightings = sum(total_sightings, na.rm = TRUE))

# Aggregate sightings across all species by date
whale_combined_agg <- whale_combined %>%
  group_by(date_column) %>%
  summarise(total_sightings = sum(total_sightings, na.rm = TRUE), .groups = "drop")


# Convert to time series objects
whale_blue_ts <- ts(whale_blue_agg$total_sightings, 
                    start = c(2014, 1), 
                    end = c(2024, 12), 
                    frequency = 12)

whale_fin_ts <- ts(whale_fin_agg$total_sightings, 
                   start = c(2014, 1), 
                   end = c(2024, 12), 
                   frequency = 12)

whale_hump_ts <- ts(whale_hump_agg$total_sightings, 
                    start = c(2014, 1), 
                    end = c(2024, 12), 
                    frequency = 12)

whale_combined_ts <- ts(whale_combined_agg$total_sightings, 
                        start = c(2014, 1), 
                        end = c(2024, 12), 
                        frequency = 12)

# Decompose the time series
whale_blue_decomp <- decompose(whale_blue_ts)
whale_fin_decomp <- decompose(whale_fin_ts)
whale_hump_decomp <- decompose(whale_hump_ts)

# Plot decomposition components
autoplot(whale_blue_decomp) + 
  ggtitle("Decomposition of Blue Whale Sightings") +
  theme_minimal()

autoplot(whale_fin_decomp) + 
  ggtitle("Decomposition of Fin Whale Sightings") +
  theme_minimal()

autoplot(whale_hump_decomp) + 
  ggtitle("Decomposition of Humpback Whale Sightings") +
  theme_minimal()

## Yearly Time Series Forecasting - Seasonal Naive Method

# Blue Whales

# Forecast using Seasonal Naive Method for Blue Whale
whale_blue_forecast <- snaive(whale_blue_ts, h = 36)  # h = 36 for 3 years of forecast

# Plot the forecast
autoplot(whale_blue_forecast) + 
  ggtitle("Seasonal Naive Forecast for Blue Whale Sightings") +
  xlab("Year") + 
  ylab("Total Sightings") +
  theme_minimal()


# Fin Whales

# Forecast using Seasonal Naive Method for Fin Whale
whale_fin_forecast <- snaive(whale_fin_ts, h = 36)  # h = 36 for 3 years of forecast

# Plot the forecast
autoplot(whale_fin_forecast) + 
  ggtitle("Seasonal Naive Forecast for Fin Whale Sightings") +
  xlab("Year") + 
  ylab("Total Sightings") +
  theme_minimal()

#  Humpback Whales

# Forecast using Seasonal Naive Method for Humpback Whale
whale_hump_forecast <- snaive(whale_hump_ts, h = 36)  # h = 36 for 3 years of forecast

# Plot the forecast
autoplot(whale_hump_forecast) + 
  ggtitle("Seasonal Naive Forecast for Humpback Whale Sightings") +
  xlab("Year") + 
  ylab("Total Sightings") +
  theme_minimal()

# Combined Forecasting

# Forecast using Seasonal Naive Method for All Species
whale_combined_forecast <- snaive(whale_combined_ts, h = 36)  # h = 36 for 3 years of forecast

# Plot the forecast
autoplot(whale_combined_forecast) + 
  ggtitle("Seasonal Naive Forecast for All Whale Sightings") +
  xlab("Year") + 
  ylab("Total Sightings") +
  theme_minimal()