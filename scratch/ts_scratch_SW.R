## Time series work 


# Load in the necessary libraries 
library(tidyverse)
library(here)
library(janitor)
library(dplyr)

# Load in the data 

whale_data <- read_csv(here("data", "whale_cleaned.csv"))

# Time series plot test 

# first create a new df with grouped by count of whale type 


whale_relevant <- whale_data |> 
  filter(species %in% c("Humpback Whale", "Fin Whale", "Blue Whale")) |> 
  group_by(species, year) |> 
  summarize(Total_Value = sum(number_sighted), .groups = "drop")



# Create the plot with custom blue colors
ggplot(whale_relevant, aes(x = year, y = Total_Value, color = species)) +
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
