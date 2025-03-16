## Time series work 


# Load in the necessary libraries 
library(tidyverse)
library(here)
library(janitor)
library(dplyr) 
library(ggplot2)

# Load in the data 

whale_data <- read_csv(here("data", "whale_cleaned.csv"))


# Time series plot test 

# first create a new df with grouped by count of whale type 

whale_sightings <- whale_data %>%
  group_by(species, year, month) %>%
  summarize(total_sighted = sum(number_sighted), .groups = "drop")

ggplot(whale_sightings, aes(x = interaction(year, month), y = total_sighted, color = species, group = species)) +
  geom_line() +  # Use geom_line() for time series plot
  labs(title = "Total Whale Sightings per Species Over Time",
       x = "Month-Year",
       y = "Total Sightings") +
  scale_color_manual(values = c(
    "Humpback Whale" = "steelblue",  # Light blue
    "Fin Whale" = "deepskyblue",    # Medium blue
    "Blue Whale" = "royalblue"      # Dark blue
  )) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))  # Rotate x-axis labels by 45 degrees

#######################################################################
#Summary stats table
# Summarize the total sightings by species and year
whale_sightings_summary <- whale_data |>
  group_by(species, year) |>
  summarize(total_sighted = sum(number_sighted), .groups = "drop") |>
  pivot_wider(names_from = species, values_from = total_sighted, values_fill = list(total_sighted = 0))|> 
  rename(Year = year)
whale_sightings_summary


whale_relevant <- whale_data |> 
  filter(species %in% c("Humpback Whale", "Fin Whale", "Blue Whale")) |> 
  group_by(species) |> 
  summarize(Total_Value = sum(number_sighted), .groups = "drop")



plot <- ggplot(data = whale_relevant, aes(x = month, y = Total_Value, color = species)) + 
  geom_line()
plot

# Create the plot with custom blue colors
ggplot(whale_relevant, aes(x = month, y = Total_Value, color = species)) +
  geom_line() +  # Use geom_line() for a time series line plot
  geom_point() + # Add points for clarity
  labs(title = "Whale Sightings Over Time",
       x = "Year",
       y = "Number of Sightings") +
  scale_color_manual(values = c(
    "Humpback Whale" = "steelblue",  # Light blue
    "Fin Whale" = "steelblue2",       # Medium blue
    "Blue Whale" = "steelblue4"       # Dark blue
  )) +
  theme_minimal()

#########################################################################################
# Pt 2 with all data 

whale_test <- read_csv(here("data", "raw_data_stella.csv"))

whale_date <- whale_test %>%
  mutate(create_date = ymd_hms(create_date),        # Parse the datetime
    date = as.Date(create_date)) %>%
  select(-create_date) |> 
  filter(whale_alert_species %in% c("Humpback Whale", "Fin Whale", "Blue Whale")) |> 
  group_by(whale_alert_species, date) |> 
  summarize(Total_Value = sum(number_sighted), .groups = "drop")

whale_plot_detialed <- ggplot(whale_date, aes(x = date, y = Total_Value, color = whale_alert_species)) +
  geom_line() +  # Use geom_line() for a time series line plot
  geom_point() + # Add points for clarity
  labs(title = "Whale Sightings Over Time",
       x = "Year",
       y = "Number of Sightings") +
  scale_color_manual(values = c(
    "Humpback Whale" = "steelblue",  # Light blue
    "Fin Whale" = "steelblue2",       # Medium blue
    "Blue Whale" = "steelblue4"       # Dark blue
  )) +
  theme_minimal()
whale_plot_detialed


##########################################################################################
#Seasonality Plot 

# want to create a new widget where you can summer, winter, spring, fall and then show the population dynaics for those month 
# stacked bar 

library(tidyverse)

# Sample Data
whale_raw <- read_csv("data/whale_cleaned.csv")

# 1. Create a 'season' column
whale_mutate <- whale_raw %>%
  mutate(season = case_when(
    month %in% c("Dec", "Jan", "Feb") ~ "Winter",  # December, January, February
    month %in% c("Mar", "Apr", "May") ~ "Spring",   # March, April, May
    month %in% c("Jun", "Jul", "Aug") ~ "Summer",   # June, July, August
    month %in% c("Sep", "Oct", "Nov") ~ "Fall"      # September, October, November
  ))

# 2. Summarize the data to get total sightings per species and season
whale_season_sightings <- whale_mutate %>%  # Use 'whale_mutate' which contains the 'season' column
  filter(species %in% c("Humpback Whale", "Fin Whale", "Blue Whale")) %>%
  group_by(species, season) %>%
  summarize(total_sighted = sum(number_sighted), .groups = "drop")

# 3. Plot the stacked bar chart
ggplot(whale_season_sightings, aes(x = season, y = total_sighted, fill = species)) +
  geom_bar(stat = "identity") +
  labs(
    title = "Whale Sightings by Species and Season",
    x = "Season",
    y = "Total Sightings",
    fill = "Species"
  ) +
  scale_fill_manual(values = c("Humpback Whale" = "steelblue1",  # Blue for Humpback Whale
                               "Fin Whale" = "steelblue3",     # Lighter blue for Fin Whale
                               "Blue Whale" = "steelblue4")) +  # Even lighter blue for Blue Whale
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))  # Rotate x-axis labels for better readability












