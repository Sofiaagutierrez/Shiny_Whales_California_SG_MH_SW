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
library(feasts)

# Make specific forecasting dataframe
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

# Example: if year = 2021, month = 3
whale_forecast$date_column <- as.Date(paste(whale_forecast$year, sprintf("%02d", whale_forecast$month), "01", sep = "-"))


### tsibble
# Split the data by species
whale_species_split <- whale_forecast %>%
  group_by(species)  # assuming 'species' column identifies the species

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

# Blue Whales
# Aggregated sightings by month and year
whale_blue_agg <- whale_blue_forcast |>
  group_by(date_column) |>
  summarise(total_sightings = sum(number_sighted, na.rm = TRUE))

# Convert to tsibble
whale_blue_tsibble <- whale_blue_agg %>%
  as_tsibble(index = date_column)

# Fin Whales
# Aggregated sightings by month and year
whale_fin_agg <- whale_fin_forcast |>
  group_by(date_column) |>
  summarise(total_sightings = sum(number_sighted, na.rm = TRUE))

## Individual plots

# Blue Whale
ggplot(whale_blue_tsibble, aes(x = date_column, y = total_sightings)) +
  geom_line() +
  labs(title = "Blue Whale Sightings", x = "Date", y = "Total Sightings") +
  theme_minimal()

# Fin Whale
ggplot(whale_fin_tsibble, aes(x = date_column, y = total_sightings)) +
  geom_line() +
  labs(title = "Fin Whale Sightings", x = "Date", y = "Total Sightings") +
  theme_minimal()

# Humpback Whale
ggplot(whale_hump_tsibble, aes(x = date_column, y = total_sightings)) +
  geom_line() +
  labs(title = "Humpback Whale Sightings", x = "Date", y = "Total Sightings") +
  theme_minimal()

## combined on the same plot
# Convert tsibbles to tibbles and add species column
whale_blue_df <- as_tibble(whale_blue_tsibble) %>% mutate(species = "Blue Whale")
whale_fin_df <- as_tibble(whale_fin_tsibble) %>% mutate(species = "Fin Whale")
whale_hump_df <- as_tibble(whale_hump_tsibble) %>% mutate(species = "Humpback Whale")

# Combine all species data into one data frame
whale_combined <- bind_rows(whale_blue_df, whale_fin_df, whale_hump_df)

# Plot combined data for all species
ggplot(whale_combined, aes(x = date_column, y = total_sightings, color = species)) +
  geom_line() +
  labs(title = "Whale Sightings by Species", x = "Date", y = "Total Sightings") +
  theme_minimal() +
  scale_color_manual(values = c("darkblue", "darkred", "darkgreen"))


# Extract year and month from date_column and create new columns for faceting
whale_combined <- whale_combined %>%
  mutate(year = year(date_column),
         month = month(date_column, label = TRUE))

# Plot the combined whale sightings with faceting by month
ggplot(whale_combined, aes(x = year, y = total_sightings, color = species)) +
  geom_line() +
  facet_wrap(~month, labeller = label_both) +  # Facet by month
  labs(x = "Year",
       y = "Total Whale Sightings",
       title = "Whale Sightings by Month and Species",
       subtitle = "Aggregated sightings across species") +
  scale_color_manual(values = c("darkblue", "darkred", "darkgreen")) +
  theme_minimal()

# Add year and month columns to each species tsibble
whale_blue_tsibble <- whale_blue_tsibble %>%
  mutate(year = year(date_column), 
         month = month(date_column, label = TRUE))

whale_fin_tsibble <- whale_fin_tsibble %>%
  mutate(year = year(date_column), 
         month = month(date_column, label = TRUE))

whale_hump_tsibble <- whale_hump_tsibble %>%
  mutate(year = year(date_column), 
         month = month(date_column, label = TRUE))

# Plot Blue Whale with faceting by month
ggplot(whale_blue_tsibble, aes(x = year, y = total_sightings)) +
  geom_line() +
  facet_wrap(~month, scales = "free_y") +  # Facet by month
  labs(title = "Blue Whale Sightings", x = "Year", y = "Total Sightings") +
  theme_minimal()

# Plot Fin Whale with faceting by month
ggplot(whale_fin_tsibble, aes(x = year, y = total_sightings)) +
  geom_line() +
  facet_wrap(~month, scales = "free_y") +  # Facet by month
  labs(title = "Fin Whale Sightings", x = "Year", y = "Total Sightings") +
  theme_minimal()

# Plot Humpback Whale with faceting by month
ggplot(whale_hump_tsibble, aes(x = year, y = total_sightings)) +
  geom_line() +
  facet_wrap(~month, scales = "free_y") +  # Facet by month
  labs(title = "Humpback Whale Sightings", x = "Year", y = "Total Sightings") +
  theme_minimal()

## Decomposition Plots ****something is wrong with the ts data - 
# - not showing full data (ending in 2020 vs. 2024) 

# Blue Whales
whale_blue_agg %>%
  group_by(year, month) %>%
  summarise(count = n())

whale_blue_ts <- ts(whale_blue_agg$total_sightings, 
                    start = c(2014, 1),  # assuming data starts from Jan 2014
                    end = c(2024, 12),    # assuming data ends in Dec 2024
                    frequency = 12)
whale_blue_ts

# Decompose the time series into trend, seasonal, and residual components
whale_blue_decomp <- decompose(whale_blue_ts)

# Plot decomposition components
autoplot(whale_blue_decomp) + 
  ggtitle("Decomposition of Blue Whale Sightings") +
  theme_minimal()

# Fin Whales 

# Extract year and month from the date column in the whale_fin_agg dataset
whale_fin_agg <- whale_fin_agg %>%
  mutate(year = year(date_column), month = month(date_column))

# Now you can group by year and month
whale_fin_agg %>%
  group_by(year, month) %>%
  summarise(count = n())

# Convert the total sightings of fin whales into a time series object
whale_fin_ts <- ts(whale_fin_agg$total_sightings, 
                   start = c(2014, 1),  # assuming data starts from Jan 2014
                   end = c(2024, 12),    # assuming data ends in Dec 2024
                   frequency = 12)

# Decompose the time series into trend, seasonal, and residual components
whale_fin_decomp <- decompose(whale_fin_ts)

# Plot decomposition components
autoplot(whale_fin_decomp) + 
  ggtitle("Decomposition of Fin Whale Sightings") +
  theme_minimal()


# Humpback Whales
# Extract year and month from the date column in the whale_humpback_agg dataset
whale_hump_agg <- whale_hump_agg %>%
  mutate(year = year(date_column), month = month(date_column))

# Now group by year and month and summarize the count
whale_hump_agg %>%
  group_by(year, month) %>%
  summarise(count = n())

# Convert the total sightings of humpback whales into a time series object
whale_hump_ts <- ts(whale_hump_agg$total_sightings, 
                        start = c(2014, 1),  # assuming data starts from Jan 2014
                        end = c(2024, 12),    # assuming data ends in Dec 2024
                        frequency = 12)

# Decompose the time series into trend, seasonal, and residual components
whale_hump_decomp <- decompose(whale_hump_ts)

# Plot decomposition components
autoplot(whale_hump_decomp) + 
  ggtitle("Decomposition of Humpback Whale Sightings") +
  theme_minimal()


# All Species

# Extract year and month from the date column in the whale_combined_agg dataset
whale_combined_agg <- whale_combined_agg %>%
  mutate(year = year(date_column), month = month(date_column))

# Group by year and month and summarize the total sightings
whale_combined_agg %>%
  group_by(year, month) %>%
  summarise(count = n())

# Create a time series object
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



## Monthly time series forecasting (???)

__________________________________________________

# If it's not already a Date type, convert it
whale_forecast$date_column <- as.Date(whale_forecast$date_column)

# Verify it has been converted correctly
str(whale_forecast$date_column)

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

# Convert to tsibble
whale_blue_tsibble <- whale_blue_agg %>%
  as_tsibble(index = date_column)

# Fin Whales
# Aggregated sightings by month and year
whale_fin_agg <- whale_fin_forcast |>
  group_by(date_column) |>
  summarise(total_sightings = sum(number_sighted, na.rm = TRUE))

# Convert to tsibble
whale_fin_ts <- whale_fin_agg %>%
  as_tsibble(index = date_column)

# For Humpback Whale
# Aggregated sightings by month and year
whale_hump_agg <- whale_hump_forcast |>
  group_by(date_column) |>
  summarise(total_sightings = sum(number_sighted, na.rm = TRUE))

# Convert to tsibble
whale_hump_ts <- whale_hump_agg %>%
  as_tsibble(index = date_column)

# Combined Time Series for All Species
whale_combined_agg <- whale_forecast |>
  group_by(date_column) |>
  summarise(total_sightings = sum(number_sighted, na.rm = TRUE))

# Convert to tsibble
whale_combined_ts <- whale_combined_agg %>%
  as_tsibble(index = date_column)

## Visualize the Data

# Plot time series for each species
ggplot(whale_blue_ts, aes(x = date_column, y = total_sightings)) +
  geom_line() +
  labs(title = "Blue Whale Sightings", x = "Date", y = "Total Sightings") +
  theme_minimal()

ggplot(whale_fin_ts, aes(x = date_column, y = total_sightings)) +
  geom_line() +
  labs(title = "Fin Whale Sightings", x = "Date", y = "Total Sightings") +
  theme_minimal()

ggplot(whale_hump_ts, aes(x = date_column, y = total_sightings)) +
  geom_line() +
  labs(title = "Humpback Whale Sightings", x = "Date", y = "Total Sightings") +
  theme_minimal()

# Plot combined sightings for all species
ggplot(whale_combined_tsibble, aes(x = date_column, y = total_sightings)) +
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




  