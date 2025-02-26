## Time Series Forcasting
## Last update: 2/25, Marley

# Load libraries
library(forecast)
library(tidyverse)
library(ggplot2)
library(dplyr)

# Make specific forcasting dataframe
whale_forecast <- whale_cleaned

# Check structure ofdate columns
str(whale_forecast$year)
str(whale_forecast$month) 

# Convert 'month' (factor) to numeric
whale_forecast$month <- match(whale_forecast$month, month.abb)

# Combine 'year' and 'month' into a date format column
whale_forecast$date_column <- 
  as.Date(paste(whale_forecast$year, sprintf("%02d", whale_forecast$month),
                "01", sep = "-"))

# Check data
head(whale_forecast$date_column)

# Filter the data for the three species
whale_blue_forcast <- subset(whale_forecast, species == "Blue Whale")  
whale_fin_forcast <- subset(whale_forecast, species == "Fin Whale")  
whale_hump_forcast <- subset(whale_forecast, species == "Humpback Whale")  

# Convert to time series for each species

# Blue Whales
# Aggregated sightings by month and year
whale_blue_agg <- whale_blue_forcast |>
  group_by(year, month) |>
  summarise(total_sightings = sum(number_sighted, na.rm = TRUE))

# Fin Whales
# Aggregated sightings by month and year
whale_fin_agg <- whale_fin_forcast |>
  group_by(year, month) |>
  summarise(total_sightings = sum(number_sighted, na.rm = TRUE))

# For Humpback Whale
# Aggregated sightings by month and year
whale_hump_agg <- whale_hump_forcast |>
  group_by(year, month) |>
  summarise(total_sightings = sum(number_sighted, na.rm = TRUE))

# Combined Time Series for All Species
whale_combined_agg <- whale_forecast |>
  group_by(year, month) |>
  summarise(total_sightings = sum(number_sighted, na.rm = TRUE))

## Decomposition Plots ****something is wrong with the ts data - 
# - not showing full data (ending in 2020 vs. 2024) 

# Blue Whales

# Convert thee aggregated sightings into time series object
whale_blue_ts <- ts(whale_blue_agg$total_sightings, 
                    start = c(min(whale_blue_agg$year), min(whale_blue_agg$month)), 
                    frequency = 12)
whale_blue_ts

# Decompose the time series into trend, seasonal, and residual components
whale_blue_decomp <- decompose(whale_blue_ts)

# Plot decomposition components
autoplot(whale_blue_decomp) + 
  ggtitle("Decomposition of Blue Whale Sightings") +
  theme_minimal()

# Fin Whales 

# Convert the aggregated sightings into time series object
whale_fin_ts <- ts(whale_fin_agg$total_sightings, 
                   start = c(min(whale_fin_agg$year), min(whale_fin_agg$month)), 
                   frequency = 12)
whale_fin_ts

# Decompose the time series into trend, seasonal, and residual components
whale_fin_decomp <- decompose(whale_fin_ts)

# Plot decomposition components
autoplot(whale_fin_decomp) + 
  ggtitle("Decomposition of Fin Whale Sightings") +
  theme_minimal()

# Humpback Whales

# Convert the aggregated sightings into time series object
whale_hump_ts <- ts(whale_hump_agg$total_sightings, 
                    start = c(min(whale_hump_agg$year), min(whale_hump_agg$month)), 
                    frequency = 12)
whale_hump_ts

# Decompose the time series into trend, seasonal, and residual components
whale_hump_decomp <- decompose(whale_hump_ts)

# Plot the decomposition components
autoplot(whale_hump_decomp) + 
  ggtitle("Decomposition of Humpback Whale Sightings") +
  theme_minimal()

# All Species

# Combine the total sightings
whale_combined_ts <- ts(whale_combined_agg$total_sightings, 
                        start = c(min(whale_combined_agg$year), min(whale_combined_agg$month)), 
                        frequency = 12)

# Decompose the combined time series
whale_combined_decomp <- decompose(whale_combined_ts)

# Plot the decomposition
autoplot(whale_combined_decomp) + 
  ggtitle("Decomposition of All Whale Sightings") +
  theme_minimal()

## Yearly Time Series Forecasting - Seasonal Naive Method

# Blue Whales

# Forecast using Seasonal Naive Method
whale_blue_forecast <- snaive(whale_blue_ts, h = 12)  
# 'h' specifies the number of periods to forecast (12 months)

# Plot the forecast
autoplot(whale_blue_forecast) + 
  ggtitle("Seasonal Naive Forecast for Blue Whale Sightings") +
  xlab("Year") + 
  ylab("Total Sightings") +
  theme_minimal()

# Fin Whales

# Forecast using Seasonal Naive Method
whale_fin_forecast <- snaive(whale_fin_ts, h = 12)  
# 'h' specifies the number of periods to forecast (12 months)

# Plot the forecast
autoplot(whale_fin_forecast) + 
  ggtitle("Seasonal Naive Forecast for Fin Whale Sightings") +
  xlab("Year") + 
  ylab("Total Sightings") +
  theme_minimal()

#  Humpback Whales

# Forecast using Seasonal Naive Method
whale_hump_forecast <- snaive(whale_hump_ts, h = 12)  
# 'h' specifies the number of periods to forecast (12 months)

# Plot the forecast
autoplot(whale_hump_forecast) + 
  ggtitle("Seasonal Naive Forecast for Humpback Whale Sightings") +
  xlab("Year") + 
  ylab("Total Sightings") +
  theme_minimal()

## Monthly time series forecasting (???)






  