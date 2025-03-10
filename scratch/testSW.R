library(tidyverse)
library(palmerpenguins)
library(sf)
library(tmap)
library(tmaptools)
library(shiny)
library(shinythemes)
library(here)
library(tseries)
library(feasts)
library(forecast)


# Read in the Whale Alert CSV
whale_raw <- read_csv("data/whale_cleaned.csv")

whale_sightings <- whale_raw %>%
  group_by(species, year, month) %>%
  summarize(total_sighted = sum(number_sighted), .groups = "drop")

whale_relevant <- whale_raw |> 
  filter(species %in% c("Humpback Whale", "Fin Whale", "Blue Whale")) |> 
  group_by(species, year) |> 
  summarize(Total_Value = sum(number_sighted), .groups = "drop")

# for map, converting data into sf
#whale_sf <- whale_raw %>% 
#filter(species %in% c("Humpback Whale", "Fin Whale", "Blue Whale")) %>% 
#st_as_sf(coords = c("longitude", "latitude"), crs = 4326) 

#for map, calling zones file
zones_sf <- st_read("data/zones_shapefile.shp")

# Custom CSS to incorporate elements from the "lumen" theme
custom_css <- "
  /* Custom styles to mimic some Lumen-like elements */
  .navbar {
    background-color: #f7f7f7;  /* Light background like lumen theme */
  }
  .panel {
    border-color: #d9edf7;  /* Lumen-style borders */
  }
  .btn-primary {
    background-color: #428bca;  /* Primary button color from lumen */
    border-color: #357ebd;  /* Button border */
  }
  .btn-primary:hover {
    background-color: #3071a9;  /* Hover state */
    border-color: #285e8e;
  }
  .content-wrapper {
    background-color: #f5f5f5;  /* Light gray background for content */
  }
"

# Create the user interface (this is the front end side of the Shiny App)
ui <- fluidPage(
  
  # Apply the cerulean theme using shinythemes and add custom CSS
  theme = shinytheme("cerulean"),
  tags$head(tags$style(HTML(custom_css))),  # Add custom CSS
  
  titlePanel("NOAA - Whale Alert"), 
  
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
                 selectInput(inputId = "pt_color", 
                             label = "Select Point Color", 
                             choices = c("Roses are red" = "red",
                                         "Violets are purple" = "purple",
                                         "Oranges are ..." = "orange"))
               ), 
               mainPanel(
                 plotOutput(outputId = "whale_plot"),  # First plot
                 plotOutput(outputId = "whale_plot2"), # Second plot
                 h3("Summary Table"), 
                 tableOutput(outputId = "whale_sum_table")  # Display summary table
               )
             )
    ), 
    
    # Tab for Zones Map
    tabPanel("Interactive Map ", 
             sidebarLayout(
               sidebarPanel(
                 p("This section will display a map of different zones.")
               ),
               mainPanel(
                 tmapOutput("whale_map")  # Display the map
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

#Create the server function 
server <- function(input, output) {
  
  # Reactive expression for the filtered whale data based on the selected species
  whale_select <- reactive({
    if (input$whale_species == "All Species") {
      return(whale_relevant)  # Return all species data
    } else {
      return(whale_relevant |> filter(species == input$whale_species))  # Filter by selected species
    }
  })
  
  # Reactive expression for the filtered whale sightings data based on the selected species
  whale_sightings_select <- reactive({
    if (input$whale_species == "All Species") {
      return(whale_sightings)  # Return all species data
    } else {
      return(whale_sightings |> filter(species == input$whale_species))  # Filter by selected species
    }
  })
  
  # Render the first plot (Time Series)
  output$whale_plot <- renderPlot({
    ggplot(whale_select(), aes(x = year, y = Total_Value, color = species)) + 
      geom_line() +  # Line plot for time series
      geom_point(color = input$pt_color) +  # Point color based on user input
      theme_bw() +
      labs(title = paste(input$whale_species, "Sightings Over Time"), 
           x = "Year", 
           y = "Number of Sightings") +
      scale_x_continuous(breaks = seq(min(whale_select()$year), max(whale_select()$year), by = 1))  # Show every year on the x-axis
  })
  
  # Render the second plot (Total Whale Sightings per Species by Month-Year)
  output$whale_plot2 <- renderPlot({
    ggplot(whale_sightings_select(), aes(x = interaction(year, month), y = total_sighted, color = species, group = species)) +
      geom_line() +  # Line plot for sightings over time
      labs(title = paste(input$whale_species, "Total Whale Sightings per Species Over Time"),
           x = "Month-Year",
           y = "Total Sightings") +
      theme_minimal() +
      theme(axis.text.x = element_text(angle = 45, hjust = 1))  # Rotate x-axis labels by 45 degrees
  })
  
  # Reactive summary table for whale sightings
  whale_sum_table <- reactive({
    whale_sightings_summary <- whale_select() %>%
      group_by(species, year) %>%
      summarize(
        total_sighted = sum(total_sighted),  # Total sightings
        .groups = "drop"
      )
    return(whale_sightings_summary)  # Return the summary table
  })
  
  output$whale_sum_table <- renderTable({
    whale_sum_table()  # Use the reactive function to get the data
  })
  
  # Reactive expression to filter whale data for mapping
  #whale_map_data <- reactive({
  #if (input$map_species == "All Species") {
  #return(whale_sf)  # Return all species data
  #} else {
  # Filter by species (if map_species input is implemented)
  #}
  #})
  
  # Reactive expression for reading zones shapefile
  #zones_sf <- reactive({
  #req(input$zones_sf)  # Ensure that file is uploaded
  # Construct the file path to read the shapefile
  #zone_file <- input$zones_sf$datapath
  # Read the shapefile using sf::st_read
  #st_read(zone_file)
  #})

  # Render tmap
  output$whale_map <- renderTmap({
    tmap_mode("view")  # Enable interactive mode
    
    # Use tm_shape() correctly for defining the spatial data, then apply the relevant layers.
    tm_shape(zones_sf) +  # The shapefile data (zones_sf)
      tm_polygons(
        col = "lightblue",  # Color for polygons
        border.col = "darkblue",  # Color for borders
        alpha = 0.3
      ) +
      tm_borders() +  # Add borders for the polygons
      tm_basemap(server = "Esri.WorldImagery")  # Add basemap without max.native.zoom
  })
  
  # For species selection, filter the data
  filtered_whale_data <- reactive({
    if (input$forecast_species == "Blue Whale") {
      return(whale_blue_agg)
    } else if (input$forecast_species == "Fin Whale") {
      return(whale_fin_agg)
    } else if (input$forecast_species == "Humpback Whale") {
      return(whale_hump_agg)
    } else {
      return(whale_combined_agg)  # For "All Species"
    }
  })
  
  # The time series and perform decomposition for the selected species
  output$whale_decomp_plot <- renderPlot({
    whale_data <- filtered_whale_data()
    
    # Convert to time series object
    whale_ts <- ts(whale_data$total_sightings, start = c(2014, 1), end = c(2024, 12), frequency = 12)
    whale_decomp <- decompose(whale_ts)
    
    if (input$show_decomposition) {
      autoplot(whale_decomp) + 
        ggtitle(paste("Decomposition of", input$forecast_species, "Sightings")) +
        theme_minimal()
    } else {
      NULL  # Don't show the decomposition plot if unchecked
    }
  })
  
  # Forecasting (Seasonal Naive Method) for the selected species
  output$whale_forecast_plot <- renderPlot({
    whale_data <- filtered_whale_data()
    
    # Convert to time series object
    whale_ts <- ts(whale_data$total_sightings, start = c(2014, 1), end = c(2024, 12), frequency = 12)
    whale_forecast <- snaive(whale_ts, h = 36)  # Forecast 3 years ahead
    
    autoplot(whale_forecast) + 
      ggtitle(paste("Seasonal Naive Forecast for", input$forecast_species, "Sightings")) +
      theme_minimal() +
      xlab("Year") + 
      ylab("Total Sightings")
  })
  
}

# Combine them into an app
shinyApp(ui = ui, server = server)
