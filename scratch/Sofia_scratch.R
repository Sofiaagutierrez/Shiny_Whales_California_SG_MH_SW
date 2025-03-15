# Load required libraries
library(shiny)
library(ggplot2)
library(dplyr)
library(tidyr)
library(lubridate)
library(tmap)
library(shinythemes)
library(readr)
library(sf)
library(forecast)

# Read in the Whale Alert CSV
whale_raw <- read_csv("data/whale_cleaned.csv")

# Expand rows based on number_sighted (for map)
whale_expanded <- whale_raw %>%
  uncount(weights = number_sighted)  # Duplicates rows based on the value in number_sighted

# Create a 'season' column
whale_mutate <- whale_raw %>%
  mutate(season = case_when(
    month %in% c("Dec", "Jan", "Feb") ~ "Winter",  # December, January, February
    month %in% c("Mar", "Apr", "May") ~ "Spring",   # March, April, May
    month %in% c("Jun", "Jul", "Aug") ~ "Summer",   # June, July, August
    month %in% c("Sep", "Oct", "Nov") ~ "Fall"      # September, October, November
  ))

# Summarize the data to get total sightings per species and season
whale_season_sightings <- whale_mutate %>%
  filter(species %in% c("Humpback Whale", "Fin Whale", "Blue Whale")) %>%
  group_by(species, season) %>%
  summarize(total_sighted = sum(number_sighted), .groups = "drop")

# Summarize data for time series analysis
whale_sightings <- whale_raw %>%
  group_by(species, year, month) %>%
  summarize(total_sighted = sum(number_sighted), .groups = "drop")

whale_relevant <- whale_raw %>% 
  filter(species %in% c("Humpback Whale", "Fin Whale", "Blue Whale")) %>% 
  group_by(species, year) %>% 
  summarize(Total_Value = sum(number_sighted), .groups = "drop")

# For map, converting data into sf
whale_sf <- whale_expanded %>%
  filter(species %in% c("Humpback Whale", "Fin Whale", "Blue Whale")) %>% 
  st_as_sf(coords = c("longitude", "latitude"), crs = 4326)

#for map, calling zones file
zones_sf <- st_read("data/zones_shapefile.shp")


# Create the user interface (this is the front end side of the Shiny App)
ui <- fluidPage(
  
  # Add custom CSS for background color and other styling
  tags$head(
    tags$style(HTML("
      body {
        background-color: #e0f7fa; /* Light blue background */
      }
      .navbar-default {
        background-color: #0277bd; /* Blue bar for the navbar */
      }
      .navbar-default .navbar-nav > li > a {
        color: white; /* White text on the navbar */
      }
    "))
  ),
  
  # Apply the cerulean theme using shinythemes
  theme = shinytheme("cerulean"),
  
  titlePanel("Whale Alert - Endangered Species Monitoring"), 
  
  tabsetPanel( # Add tabsetPanel for tabs
    tabPanel("Data Information",  # First tab now
             h3("Project Motivation"),
             p("The motivation for this project is ..."),
             h3("Data Summary"), 
             p("This dataset was provided to us by Anastasia Kunz, a NOAA affiliate, and details spatial whale sighting data over time across California. 
               As detailed by Anastaia, the relevant columns for this Shiny App include the X, Y position of the whale observation, 
               the date and time of the whale sighting, the whale alert species, and the number of sighted individuals per single record time.")
    ),
    tabPanel("Whale Sightings Trends and Seasonality",  # Second tab (was first)
             sidebarLayout(
               sidebarPanel(
                 radioButtons(
                   inputId = "whale_species", 
                   label = "Choose whale species", 
                   choices = c("Humpback Whale", "Fin Whale", "Blue Whale", "All Species")  # Add "All Species" option
                 ), 
                 h3("Summary Statistics"),  # Place the title here for the table
                 tableOutput(outputId = "whale_sum_table"), 
                 plotOutput(outputId = "whale_plot") # Display summary table in the sidebar
               ), 
               mainPanel(
                 plotOutput(outputId = "whale_plot2"), # Second plot (Total Sightings per Month-Year)
                 plotOutput(outputId = "whale_season_plot") # New stacked bar chart plot
               )
             )
    ), 
    
    tabPanel("Interactive Map",              
             sidebarLayout(
               sidebarPanel(
                 p("This section will display a map of different zones."),
                 
                 # Dropdown to select which map to display
                 selectInput("map_select", "Select Map to Display:",
                             choices = c("Map 1 - Whale Sightings", "Map 2 - Kernel Density"),
                             selected = "Map 1 - Whale Sightings"
                 ),
                 
                 # Dropdown for Whale Species Selection, including "All Species"
                 selectInput("species", "Select Whale Species:",
                             choices = c("All Species", unique(whale_sf$species)),
                             selected = "All Species"
                 ),
                 
                 # Conditional UI: Show different options based on selected map type
                 uiOutput("map_options")
               ),
               
               mainPanel(
                 # Dynamically display the selected map
                 tmapOutput("whale_map")  # Display the selected map
               )
             )
    ),
    
    
    tabPanel("Whale Migration Forecast and Time Series Analysis", 
             sidebarLayout(
               sidebarPanel(
                 radioButtons(inputId = "forecast_species", 
                              label = "Select Whale Species", 
                              choices = c("Blue Whale", "Fin Whale", "Humpback Whale", "All Species"), 
                              selected = "All Species"),
                 checkboxInput(inputId = "show_decomposition", 
                               label = "Show Decomposition", 
                               value = TRUE)  # Checkbox to control visibility of decomposition
               ),
               mainPanel(
                 # Output for the decomposition plot and forecasts
                 plotOutput("whale_forecast_plot"),
                 plotOutput("whale_decomp_plot")
               )
             )
    ),
    tabPanel("Additional Resources", 
             h4("For more information on reducing whale strikes, check out the following document:"),
             a("Reduce Whale Strikes - Top 5 Things You Should Know", 
               href = "https://media.fisheries.noaa.gov/dam-migration/reduce-whale-strikes-top5things.pdf", 
               target = "_blank"), 
             p("Learn more about whales on the Ocean.org website:"),
             a("Whales - Ocean.org", 
               href = "https://ocean.org/whales/", 
               target = "_blank"), 
             br(),
             p("For more information, check out this Whale Alert Smartphone App flyer:"),
             a("Whale Alert Smartphone App Flyer", 
               href = "https://media.fisheries.noaa.gov/dam-migration/whale-alert-smartphone-app-flier.pdf", 
               target = "_blank"), 
             h3("Contact Information"),
             p("*Here have the contact info for relevant people*")
    ) 
  )
)


# Create the server function 
server <- function(input, output) {
  
  # Reactive expression for the filtered whale data based on the selected species
  whale_select <- reactive({
    if (input$whale_species == "All Species") {
      return(whale_relevant)  # Return all species data
    } else {
      return(whale_relevant %>% filter(species == input$whale_species))  # Filter by selected species
    }
  })
  
  # Reactive expression for the filtered whale sightings data based on the selected species
  whale_sightings_select <- reactive({
    if (input$whale_species == "All Species") {
      return(whale_sightings)  # Return all species data
    } else {
      return(whale_sightings %>% filter(species == input$whale_species))  # Filter by selected species
    }
  })
  
  # Render the first plot (Time Series)
  output$whale_plot <- renderPlot({
    ggplot(whale_select(), aes(x = year, y = Total_Value, color = species)) + 
      geom_line(color = "steelblue2") +  
      theme_bw() +
      labs(title = paste(input$whale_species, "Annual Sightings (2014-2024)"), 
           x = "Year", 
           y = "Number of Sightings") +
      scale_x_continuous(breaks = seq(min(whale_select()$year), max(whale_select()$year), by = 1))  # Show every year on the x-axis
  })
  
  # Render the second plot (Total Whale Sightings per Species by Month-Year)
  output$whale_plot2 <- renderPlot({
    ggplot(whale_sightings_select(), aes(x = interaction(year, month), y = total_sighted, color = species, group = species)) +
      geom_line(color = "steelblue2") +  # Line plot for sightings over time
      labs(title = paste(input$whale_species, "Monthly Sightings (2014-2024)"),
           x = "Date",
           y = "Total Sightings") +
      theme_minimal() +
      theme(axis.text.x = element_text(angle = 45, hjust = 1))  # Rotate x-axis labels by 45 degrees
  })
  
  # Render the stacked bar chart (Sightings by Species and Season)
  output$whale_season_plot <- renderPlot({
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
  })
  
  # Reactive summary table for whale sightings
  whale_sum_table <- reactive({
    # Filter the data based on the selected species
    data_filtered <- if (input$whale_species == "All Species") {
      whale_sightings  # If "All Species" is selected, return all species data
    } else {
      whale_sightings %>% filter(species == input$whale_species)  # Filter for the selected species
    }
    
    # Summarize the filtered data
    data_filtered %>%
      group_by(species, year) %>%
      summarize(total_sighted = sum(total_sighted), .groups = "drop") %>%
      pivot_wider(names_from = species, values_from = total_sighted, values_fill = 0) |> 
      rename(Year = year) |> 
      mutate(Year = as.integer(Year)) 
  })
  
  # Render the summary table
  output$whale_sum_table <- renderTable({
    whale_sum_table()
  })
 
    # Reactive expression for dynamically updating the species, year, and month options
    observe({
      # Update the available species in the dropdown based on filtered data
      available_species <- unique(whale_sf$species)
      updateSelectInput(session, "species", choices = c("All Species", available_species))
      
      # Update the available years in the dropdown based on filtered data
      available_years <- sort(unique(whale_sf$year))
      updateSelectInput(session, "year", choices = c("All Years", available_years))
      
      # Update the available months in the dropdown based on filtered data
      available_months <- month.name
      updateSelectInput(session, "month", choices = c("All Months", available_months))
    })
    
    # Reactive expression for whale map filtering based on selected species, year, and month
    filtered_whale_sf <- reactive({
      req(input$species, input$year, input$month)
      
      print("Reactive function triggered")
      
      # Convert full month name (e.g., "February") to 3-letter format (e.g., "Feb")
      input_month_abbr <- format(as.Date(paste0("1 ", input$month), "%d %B"), "%b")
      
      # Debug prints
      print(paste("Converted Month:", input_month_abbr))
      print(paste("Year Input:", input$year))
      print(paste("Species selected:", input$species))
      
      # Convert year to numeric if it's not already
      year_input <- as.numeric(input$year)
      print(paste("Year input (numeric):", year_input))   # Check the numeric conversion
      
      # Filter the data based on selected species, year, and month
      filtered_data <- whale_sf %>%
        mutate(month = trimws(month)) %>%
        filter(
          (input$species == "All Species" | species == input$species),
          (input$year == "All Years" | year == year_input),
          (input$month == "All Months" | month == input_month_abbr)
        )
      
      print(paste("Filtered data row count:", nrow(filtered_data)))  # Check filtered data row count
      print(head(filtered_data))  # Preview filtered data
      return(filtered_data)
    })
    
    # Ensure tmap is in view mode
    observe({
      tmap_mode("view")
    })
    
    # Render whale sightings map
    output$whale_map <- renderTmap({
      data <- filtered_whale_sf()
      
      if (nrow(data) == 0) {
        showNotification("No data available for the selected filters.", type = "warning")
        return(NULL)  # Prevents error when data is empty
      }
      
      tm_shape(zones_sf) +  # The shapefile data (zones_sf)
        tm_polygons(
          col = "lightblue",  # Color for polygons
          border.col = "darkblue",  # Color for borders
          alpha = 0.3
        ) +
        tm_borders() +  # Add borders for the polygons
        tm_shape(data) +
        tm_dots(
          col = "pink",
          size = 0.5,
          alpha = 0.8,   # Adjust transparency for visibility
          shape = 21,    # Use a circle with fill
          border.col = "black",  # Ensure there's an outline
          border.lwd = 0.5) +
        tm_basemap(server = "Esri.WorldImagery")  # Add basemap without max.native.zoom
    })
    
    # Render second map (for Kernel Density or other types)
    output$second_map <- renderTmap({
      # Render logic for second map, e.g., Kernel Density or another dataset
      tm_shape(some_other_data) + 
        tm_bubbles(col = "other_column")  # Example visualization
    })
    
  }
  
# Run the Shiny app
shinyApp(ui = ui, server = server)
  