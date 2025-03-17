## Forecasting Whale Sightings

# Load libraries
library(forecast)
library(tidyverse)
library(ggplot2)
library(dplyr)
library(tsibble)
library(lubridate)
library(tseries)
library(feasts)

# Function to handle zero values
replace_zeros <- function(ts_data) {
  ts_data[ts_data == 0] <- 0.1
  return(ts_data)
}

# Apply zero handling to original time series
whale_blue_ts_clean <- replace_zeros(whale_blue_ts)
whale_fin_ts_clean <- replace_zeros(whale_fin_ts)
whale_hump_ts_clean <- replace_zeros(whale_hump_ts)
whale_combined_ts_clean <- replace_zeros(whale_combined_ts)

# Decompose the time series using multiplicative model
whale_blue_decomp <- decompose(whale_blue_ts_clean, type = "multiplicative")
whale_fin_decomp <- decompose(whale_fin_ts_clean, type = "multiplicative")
whale_hump_decomp <- decompose(whale_hump_ts_clean, type = "multiplicative")

# Plot improved decomposition components
autoplot(whale_blue_decomp) + 
  ggtitle("Multiplicative Decomposition of Blue Whale Sightings") +
  theme_minimal()

autoplot(whale_fin_decomp) + 
  ggtitle("Multiplicative Decomposition of Fin Whale Sightings") +
  theme_minimal()

autoplot(whale_hump_decomp) + 
  ggtitle("Multiplicative Decomposition of Humpback Whale Sightings") +
  theme_minimal()

# Create training and test sets for seasonal naive method (using 80% for training)
create_train_test <- function(ts_data, species_name) {
  train_end <- floor(length(ts_data) * 0.8)
  # Calculate years and months for windowing
  start_year <- 2014
  train_end_year <- start_year + train_end %/% 12
  train_end_month <- train_end %% 12
  if(train_end_month == 0) {
    train_end_month <- 12
    train_end_year <- train_end_year - 1
  }
  
  test_start_year <- train_end_year
  test_start_month <- train_end_month + 1
  if(test_start_month > 12) {
    test_start_month <- 1
    test_start_year <- test_start_year + 1
  }
  
  train <- window(ts_data, end = c(train_end_year, train_end_month))
  test <- window(ts_data, start = c(test_start_year, test_start_month))
  
  # Create forecast with 70% and 85% confidence intervals
  forecast_obj <- snaive(train, h = length(test), level = c(70, 85))
  
  # Calculate accuracy measures
  accuracy_metrics <- accuracy(forecast_obj, test)
  
  # Plot the forecast with actual test data and confidence intervals
  p <- autoplot(train) +
    autolayer(forecast_obj, series = "Forecast", PI = TRUE) +
    autolayer(test, series = "Actual") +
    ggtitle(paste("Forecast vs Actual -", species_name, "Sightings")) +
    xlab("Year") + 
    ylab("Total Sightings") +
    scale_color_manual(name = "Series", 
                       values = c("Forecast" = "blue", "Actual" = "red")) +
    scale_fill_manual(name = "Confidence Level", 
                      values = c("70%" = "skyblue3", "85%" = "skyblue1")) +
    theme_minimal() +
    theme(legend.position = "bottom")
  
  return(list(
    train = train,
    test = test,
    forecast = forecast_obj,
    accuracy = accuracy_metrics,
    plot = p
  ))
}

# Add this function for ARIMA forecasting with train/test split
create_train_test_arima <- function(ts_data, species_name) {
  train_end <- floor(length(ts_data) * 0.8)
  # Calculate years and months for windowing
  start_year <- 2014
  train_end_year <- start_year + train_end %/% 12
  train_end_month <- train_end %% 12
  if(train_end_month == 0) {
    train_end_month <- 12
    train_end_year <- train_end_year - 1
  }
  
  test_start_year <- train_end_year
  test_start_month <- train_end_month + 1
  if(test_start_month > 12) {
    test_start_month <- 1
    test_start_year <- test_start_year + 1
  }
  
  train <- window(ts_data, end = c(train_end_year, train_end_month))
  test <- window(ts_data, start = c(test_start_year, test_start_month))
  
  # Auto-select ARIMA model
  fit <- auto.arima(train, seasonal = TRUE, stepwise = FALSE, approximation = FALSE)
  
  # Create forecast with 70% and 85% confidence intervals
  forecast_obj <- forecast(fit, h = length(test), level = c(70, 85))
  
  # Calculate accuracy measures
  accuracy_metrics <- accuracy(forecast_obj, test)
  
  # Print ARIMA model details
  print(paste("ARIMA Model for", species_name))
  print(summary(fit))
  
  # Plot the forecast with actual test data and confidence intervals
  p <- autoplot(train) +
    autolayer(forecast_obj, series = "Forecast", PI = TRUE) +
    autolayer(test, series = "Actual") +
    ggtitle(paste("ARIMA Forecast vs Actual -", species_name, "Sightings")) +
    xlab("Year") + 
    ylab("Total Sightings") +
    scale_color_manual(name = "Series", 
                       values = c("Forecast" = "blue", "Actual" = "red")) +
    scale_fill_manual(name = "Confidence Level", 
                      values = c("70%" = "skyblue3", "85%" = "skyblue1")) +
    theme_minimal() +
    theme(legend.position = "bottom")
  
  return(list(
    train = train,
    test = test,
    model = fit,
    forecast = forecast_obj,
    accuracy = accuracy_metrics,
    plot = p
  ))
}

# Function for seasonal naive forecasting of future periods
forecast_future <- function(ts_data, species_name, h = 36) {
  # Create forecast with 70% and 85% confidence intervals
  forecast_obj <- snaive(ts_data, h = h, level = c(70, 85))
  
  # Plot the forecast with confidence intervals
  p <- autoplot(forecast_obj) + 
    ggtitle(paste("Seasonal Naive Forecast for", species_name, "Sightings")) +
    xlab("Year") + 
    ylab("Total Sightings") +
    scale_fill_manual(name = "Confidence Level", 
                      values = c("70%" = "skyblue3", "85%" = "skyblue1")) +
    theme_minimal() +
    theme(legend.position = "bottom")
  
  return(list(
    forecast = forecast_obj,
    plot = p
  ))
}

# Add this function for ARIMA future forecasting
forecast_future_arima <- function(ts_data, species_name, h = 36) {
  # Auto-select ARIMA model
  fit <- auto.arima(ts_data, seasonal = TRUE, stepwise = FALSE, approximation = FALSE)
  
  # Create forecast with 70% and 85% confidence intervals
  forecast_obj <- forecast(fit, h = h, level = c(70, 85))
  
  # Print ARIMA model details
  print(paste("ARIMA Model for", species_name, "Future Forecast"))
  print(summary(fit))
  
  # Plot the forecast with confidence intervals
  p <- autoplot(forecast_obj) + 
    ggtitle(paste("ARIMA Forecast for", species_name, "Sightings")) +
    xlab("Year") + 
    ylab("Total Sightings") +
    scale_fill_manual(name = "Confidence Level", 
                      values = c("70%" = "skyblue3", "85%" = "skyblue1")) +
    theme_minimal() +
    theme(legend.position = "bottom")
  
  return(list(
    model = fit,
    forecast = forecast_obj,
    plot = p
  ))
}

# Apply analysis to each species
blue_analysis <- create_train_test(whale_blue_ts_clean, "Blue Whale")
fin_analysis <- create_train_test(whale_fin_ts_clean, "Fin Whale")
# Use ARIMA for Humpback Whales instead of snaive
hump_analysis <- create_train_test_arima(whale_hump_ts_clean, "Humpback Whale")
combined_analysis <- create_train_test(whale_combined_ts_clean, "All Whale")

# View accuracy metrics
print("Blue Whale Accuracy Metrics:")
print(blue_analysis$accuracy)

print("Fin Whale Accuracy Metrics:")
print(fin_analysis$accuracy)

print("Humpback Whale ARIMA Accuracy Metrics:")
print(hump_analysis$accuracy)

print("Combined Whale Accuracy Metrics:")
print(combined_analysis$accuracy)

# Display plots
blue_analysis$plot
fin_analysis$plot
hump_analysis$plot
combined_analysis$plot

# Create future forecasts
blue_future <- forecast_future(whale_blue_ts_clean, "Blue Whale")
fin_future <- forecast_future(whale_fin_ts_clean, "Fin Whale")
# Use ARIMA for Humpback Whales future forecast
hump_future <- forecast_future_arima(whale_hump_ts_clean, "Humpback Whale")
combined_future <- forecast_future(whale_combined_ts_clean, "All Whale")

# Display future forecast plots
blue_future$plot
fin_future$plot
hump_future$plot
combined_future$plot

# Compare seasonal naive and ARIMA for Humpback Whales
# This allows us to see if ARIMA is truly better
hump_snaive_analysis <- create_train_test(whale_hump_ts_clean, "Humpback Whale")
print("Humpback Whale Accuracy Metrics (Seasonal Naive for comparison):")
print(hump_snaive_analysis$accuracy)

# Create a comparison plot
comparison_plot <- ggplot() +
  geom_line(data = as.data.frame(hump_analysis$test), 
            aes(x = time(hump_analysis$test), y = hump_analysis$test, color = "Actual")) +
  geom_line(data = as.data.frame(hump_analysis$forecast$mean), 
            aes(x = time(hump_analysis$forecast$mean), y = hump_analysis$forecast$mean, color = "ARIMA")) +
  geom_line(data = as.data.frame(hump_snaive_analysis$forecast$mean), 
            aes(x = time(hump_snaive_analysis$forecast$mean), y = hump_snaive_analysis$forecast$mean, color = "Seasonal Naive")) +
  ggtitle("Humpback Whale Forecast Comparison") +
  xlab("Year") + 
  ylab("Total Sightings") +
  scale_color_manual(name = "Model", 
                     values = c("Actual" = "black", "ARIMA" = "blue", "Seasonal Naive" = "red")) +
  theme_minimal() +
  theme(legend.position = "bottom")

comparison_plot

# Seasonal patterns analysis
# Convert time series to tsibble for better seasonal analysis
whale_blue_tsbl <- as_tsibble(whale_blue_ts_clean)
whale_fin_tsbl <- as_tsibble(whale_fin_ts_clean)
whale_hump_tsbl <- as_tsibble(whale_hump_ts_clean)

# Seasonal plots
whale_blue_tsbl %>%
  gg_season(value) +
  ggtitle("Seasonal Plot: Blue Whale Sightings") +
  theme_minimal()

whale_fin_tsbl %>%
  gg_season(value) +
  ggtitle("Seasonal Plot: Fin Whale Sightings") +
  theme_minimal()

whale_hump_tsbl %>%
  gg_season(value) +
  ggtitle("Seasonal Plot: Humpback Whale Sightings") +
  theme_minimal()