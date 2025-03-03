## Time Series Forcasting
## Last update: 2/25, Marley

# Load libraries
library(forecast)
library(tidyverse)
library(ggplot2)
library(dplyr)
library(tsibble)
library(lubridate)
library(tseries)

# Make specific forcasting dataframe
whale_forecast <- whale_cleaned

# Check structure of date columns
str(whale_forecast$year)
str(whale_forecast$month) 

# Convert 'month' (factor) to numeric
whale_forecast$month <- match(whale_forecast$month, month.abb)

# Combine 'year' and 'month' into a date format column
whale_forecast$date_column <- 
  as.Date(paste(whale_forecast$year, sprintf("%02d", whale_forecast$month),
                "01", sep = "-"))

whale_forecast_agg <- whale_forecast %>%
  group_by(species, date_column) %>%
  summarise(number_sighted = sum(number_sighted, na.rm = TRUE), .groups = "drop")


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
  group_by(date_column) |>
  summarise(total_sightings = sum(number_sighted, na.rm = TRUE))

# Fin Whales
# Aggregated sightings by month and year
whale_fin_agg <- whale_fin_forcast |>
  group_by(date_column) |>
  summarise(total_sightings = sum(number_sighted, na.rm = TRUE))

# For Humpback Whale
# Aggregated sightings by month and year
whale_hump_agg <- whale_hump_forcast |>
  group_by(date_column) |>
  summarise(total_sightings = sum(number_sighted, na.rm = TRUE))

# Combined Time Series for All Species
whale_combined_agg <- whale_forecast |>
  group_by(date_column) |>
  summarise(total_sightings = sum(number_sighted, na.rm = TRUE))

## Visualize the Data

# Plot time series for each species
ggplot(whale_blue_agg, aes(x = date_column, y = total_sightings)) +
  geom_line() +
  labs(title = "Blue Whale Sightings", x = "Date", y = "Total Sightings") +
  theme_minimal()

ggplot(whale_fin_agg, aes(x = date_column, y = total_sightings)) +
  geom_line() +
  labs(title = "Fin Whale Sightings", x = "Date", y = "Total Sightings") +
  theme_minimal()

ggplot(whale_hump_agg, aes(x = date_column, y = total_sightings)) +
  geom_line() +
  labs(title = "Humpback Whale Sightings", x = "Date", y = "Total Sightings") +
  theme_minimal()

# Plot combined sightings for all species
ggplot(whale_combined_agg, aes(x = date_column, y = total_sightings)) +
  geom_line() +
  labs(title = "Combined Whale Sightings", x = "Date", y = "Total Sightings") +
  theme_minimal()


## Check for Stationarity

# Perform Augmented Dickey-Fuller test to check for stationarity

adf.test(whale_blue_agg$total_sightings)
adf.test(whale_fin_agg$total_sightings)
adf.test(whale_hump_agg$total_sightings)
adf.test(whale_combined_agg$total_sightings)

##  Fit Time Series Forecasting Models

# Load the forecast library for ARIMA models

# Fit ARIMA model for each species
whale_blue_arima <- auto.arima(whale_blue_agg$total_sightings)
whale_fin_arima <- auto.arima(whale_fin_agg$total_sightings)
whale_hump_arima <- auto.arima(whale_hump_agg$total_sightings)

# For combined sightings
whale_combined_arima <- auto.arima(whale_combined_agg$total_sightings)

# Summary of the ARIMA models
summary(whale_blue_arima)
summary(whale_fin_arima)
summary(whale_hump_arima)
summary(whale_combined_arima)

## Forecasting

# Next 48 months (4 years)
blue_forecast <- forecast(whale_blue_arima, h=48)
fin_forecast <- forecast(whale_fin_arima, h=48)
hump_forecast <- forecast(whale_hump_arima, h=48)
combined_forecast <- forecast(whale_combined_arima, h=48)

# Plot the forecasts
autoplot(blue_forecast) + ggtitle("Blue Whale Sightings Forecast")
autoplot(fin_forecast) + ggtitle("Fin Whale Sightings Forecast")
autoplot(hump_forecast) + ggtitle("Humpback Whale Sightings Forecast")
autoplot(combined_forecast) + ggtitle("Combined Whale Sightings Forecast")


##  Model Diagnostics

# Check residuals for each model
checkresiduals(whale_blue_arima)
checkresiduals(whale_fin_arima)
checkresiduals(whale_hump_arima)
checkresiduals(whale_combined_arima)

## Review and Interpret Results

# Calculate MAE or RMSE
accuracy(whale_blue_forecast)
accuracy(whale_fin_forecast)
accuracy(whale_hump_forecast)
accuracy(whale_combined_forecast)


``

--------------------------

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






  