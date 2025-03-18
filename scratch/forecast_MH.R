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
  ggtitle("Decomposition of Blue Whale Sightings") +
  theme_minimal()

autoplot(whale_fin_decomp) + 
  ggtitle("Decomposition of Fin Whale Sightings") +
  theme_minimal()

autoplot(whale_hump_decomp) + 
  ggtitle("Decomposition of Humpback Whale Sightings") +
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
  
  return(list(
    train = train,
    test = test,
    forecast = forecast_obj,
    accuracy = accuracy_metrics
  ))
}

# Modified function to use most recent 8 years for training
create_train_test_arima <- function(ts_data, species_name) {
  # Get the time series start and end dates
  ts_start <- start(ts_data)
  ts_end <- end(ts_data)
  
  # Calculate total years of data
  start_year <- ts_start[1]
  start_month <- ts_start[2]
  end_year <- ts_end[1]
  end_month <- ts_end[2]
  
  # Calculate test start date (2 years before end)
  test_start_year <- end_year - 2
  test_start_month <- end_month
  
  # Set train end date to month before test start
  train_end_year <- test_start_year
  train_end_month <- test_start_month - 1
  
  # Handle month rollover if needed
  if(train_end_month == 0) {
    train_end_month <- 12
    train_end_year <- train_end_year - 1
  }
  
  # Create train and test windows
  train <- window(ts_data, start = c(start_year, start_month), end = c(train_end_year, train_end_month))
  test <- window(ts_data, start = c(test_start_year, test_start_month), end = c(end_year, end_month))
  
  # Auto-select ARIMA model
  fit <- auto.arima(train, seasonal = TRUE, stepwise = FALSE, approximation = FALSE)
  
  # Create forecast with 70% and 85% confidence intervals
  forecast_obj <- forecast(fit, h = length(test), level = c(70, 85))
  
  # Calculate accuracy measures
  accuracy_metrics <- accuracy(forecast_obj, test)
  
  # Print ARIMA model details
  print(paste("ARIMA Model for", species_name))
  print(summary(fit))
  
  return(list(
    train = train,
    test = test,
    model = fit,
    forecast = forecast_obj,
    accuracy = accuracy_metrics
  ))
}

# Modified function for seasonal naive with same training/testing approach
create_train_test <- function(ts_data, species_name) {
  # Get the time series start and end dates
  ts_start <- start(ts_data)
  ts_end <- end(ts_data)
  
  # Calculate total years of data
  start_year <- ts_start[1]
  start_month <- ts_start[2]
  end_year <- ts_end[1]
  end_month <- ts_end[2]
  
  # Calculate test start date (2 years before end)
  test_start_year <- end_year - 2
  test_start_month <- end_month
  
  # Set train end date to month before test start
  train_end_year <- test_start_year
  train_end_month <- test_start_month - 1
  
  # Handle month rollover if needed
  if(train_end_month == 0) {
    train_end_month <- 12
    train_end_year <- train_end_year - 1
  }
  
  # Create train and test windows
  train <- window(ts_data, start = c(start_year, start_month), end = c(train_end_year, train_end_month))
  test <- window(ts_data, start = c(test_start_year, test_start_month), end = c(end_year, end_month))
  
  # Create forecast with seasonal naive method
  forecast_obj <- snaive(train, h = length(test), level = c(70, 85))
  
  # Calculate accuracy measures
  accuracy_metrics <- accuracy(forecast_obj, test)
  
  return(list(
    train = train,
    test = test,
    forecast = forecast_obj,
    accuracy = accuracy_metrics
  ))
}

# After running these functions, verify the split
blue_analysis <- create_train_test(whale_blue_ts_clean, "Blue Whale")
print(paste("Training data from:", start(blue_analysis$train)[1], start(blue_analysis$train)[2], 
            "to", end(blue_analysis$train)[1], end(blue_analysis$train)[2]))
print(paste("Testing data from:", start(blue_analysis$test)[1], start(blue_analysis$test)[2], 
            "to", end(blue_analysis$test)[1], end(blue_analysis$test)[2]))

# Function for seasonal naive forecasting of future periods
forecast_future <- function(ts_data, species_name, h = 36) {
  # Create forecast with 70% and 85% confidence intervals
  forecast_obj <- snaive(ts_data, h = h, level = c(70, 85))
  
  # Extract forecast data for custom plotting
  fc_data <- as.data.frame(forecast_obj)
  
  # Get time information for proper x-axis
  time_info <- time(forecast_obj$mean)
  fc_data$time <- as.numeric(time(forecast_obj$mean))
  
  # Plot with ggplot2 for more control
  p <- ggplot() +
    # Plot historical data
    geom_line(data = as.data.frame(ts_data), 
              aes(x = as.numeric(time(ts_data)), y = x, color = "Historical"), 
              size = 0.5) +
    # Add 85% confidence interval - force minimum to 0
    geom_ribbon(data = fc_data, 
                aes(x = time, 
                    ymin = pmax(0, `Lo 85`), 
                    ymax = `Hi 85`, 
                    fill = "85% CI"),
                alpha = 0.3) +
    # Add 70% confidence interval - force minimum to 0
    geom_ribbon(data = fc_data, 
                aes(x = time, 
                    ymin = pmax(0, `Lo 70`), 
                    ymax = `Hi 70`, 
                    fill = "70% CI"),
                alpha = 0.5) +
    # Add forecast line
    geom_line(data = fc_data, 
              aes(x = time, y = `Point Forecast`, color = "Forecast"), 
              size = 0.5) +
    # Customize labels
    ggtitle(paste("Forecast for", species_name, "Sightings")) +
    xlab("Year") + 
    ylab("Total Sightings") +
    # Define color and fill scales with explicit legends
    scale_color_manual(name = "Series",
                       values = c("Historical" = "black", "Forecast" = "#593527FF")) +
    scale_fill_manual(name = "Confidence Interval",
                      values = c("85% CI" = "#89B7CFFF", "70% CI" = "#5C92B4FF")) +
    # Force y-axis to start at 0
    scale_y_continuous(limits = c(0, NA)) +
    # Improve theme
    theme_minimal() +
    theme(
      legend.position = "bottom",
      legend.box = "vertical",
      legend.title = element_text(face = "bold"),
      plot.title = element_text(hjust = 0.5)
    ) +
    # Ensure both legend items appear
    guides(
      color = guide_legend(order = 1),
      fill = guide_legend(order = 2)
    )
  
  return(list(
    forecast = forecast_obj,
    plot = p
  ))
}

# ARIMA version with the same improvements
forecast_future_arima <- function(ts_data, species_name, h = 36) {
  # Auto-select ARIMA model
  fit <- auto.arima(ts_data, seasonal = TRUE, stepwise = FALSE, approximation = FALSE)
  
  # Create forecast with 70% and 85% confidence intervals
  forecast_obj <- forecast(fit, h = h, level = c(70, 85))
  
  # Print ARIMA model details
  print(paste("ARIMA Model for", species_name, "Future Forecast"))
  print(summary(fit))
  
  # Extract forecast data for custom plotting
  fc_data <- as.data.frame(forecast_obj)
  
  # Get time information for proper x-axis
  time_info <- time(forecast_obj$mean)
  fc_data$time <- as.numeric(time(forecast_obj$mean))
  
  # Plot with ggplot2 for more control
  p <- ggplot() +
    # Plot historical data
    geom_line(data = as.data.frame(ts_data), 
              aes(x = as.numeric(time(ts_data)), y = x, color = "Historical"), 
              size = 0.5) +
    # Add 85% confidence interval - force minimum to 0
    geom_ribbon(data = fc_data, 
                aes(x = time, 
                    ymin = pmax(0, `Lo 85`), 
                    ymax = `Hi 85`, 
                    fill = "85% CI"),
                alpha = 0.3) +
    # Add 70% confidence interval - force minimum to 0
    geom_ribbon(data = fc_data, 
                aes(x = time, 
                    ymin = pmax(0, `Lo 70`), 
                    ymax = `Hi 70`, 
                    fill = "70% CI"),
                alpha = 0.5) +
    # Add forecast line
    geom_line(data = fc_data, 
              aes(x = time, y = `Point Forecast`, color = "Forecast"), 
              size = 0.5) +
    # Customize labels
    ggtitle(paste("Forecast for", species_name, "Sightings")) +
    xlab("Year") + 
    ylab("Total Sightings") +
    # Define color and fill scales with explicit legends
    scale_color_manual(name = "Series",
                       values = c("Historical" = "black", "Forecast" = "#593527FF")) +
    scale_fill_manual(name = "Confidence Interval",
                      values = c("85% CI" = "#89B7CFFF", "70% CI" = "#5C92B4FF")) +
    # Force y-axis to start at 0
    scale_y_continuous(limits = c(0, NA)) +
    # Improve theme
    theme_minimal() +
    theme(
      legend.position = "bottom",
      legend.box = "vertical",
      legend.title = element_text(face = "bold"),
      plot.title = element_text(hjust = 0.5)
    ) +
    # Ensure both legend items appear
    guides(
      color = guide_legend(order = 1),
      fill = guide_legend(order = 2)
    )
  
  return(list(
    model = fit,
    forecast = forecast_obj,
    plot = p
  ))
}

# Modify decomposition plots
plot_decomp_with_min_zero <- function(decomp_obj, title) {
  p <- autoplot(decomp_obj) +
    ggtitle(title) +
    theme_minimal()
  
  # Extract the grobs and modify the y-axis limits for the data panel only
  p_build <- ggplot_build(p)
  p_build$layout$panel_params[[1]]$y.range[1] <- 0  # Set minimum y to 0 for first panel
  
  return(p)
}

# Apply to decomposition plots
blue_decomp_plot <- plot_decomp_with_min_zero(whale_blue_decomp, 
                                              "Decomposition of Blue Whale Sightings")
fin_decomp_plot <- plot_decomp_with_min_zero(whale_fin_decomp, 
                                             "Decomposition of Fin Whale Sightings")
hump_decomp_plot <- plot_decomp_with_min_zero(whale_hump_decomp, 
                                              "Decomposition of Humpback Whale Sightings")

# Create future forecasts with fixed y-axis
blue_future <- forecast_future(whale_blue_ts_clean, "Blue Whale")
fin_future <- forecast_future(whale_fin_ts_clean, "Fin Whale")
hump_future <- forecast_future_arima(whale_hump_ts_clean, "Humpback Whale")

# Display future forecast plots
blue_future$plot
fin_future$plot
hump_future$plot