library(tidyverse)
library(palmerpenguins)
library(sf)
library(tmap)
library(tmaptools)
library(shiny)
library(shinythemes)
library(here)

# Read in the Whale Alert CSV
whale_raw <- read_csv(here("data", "whale_cleaned.csv"))

whale_relevant <- whale_raw |> 
  filter(species %in% c("Humpback Whale", "Fin Whale", "Blue Whale")) |> 
  group_by(species, year) |> 
  summarize(Total_Value = sum(number_sighted), .groups = "drop")

whale_sightings <- whale_raw %>%
  group_by(species, year, month) %>%
  summarize(total_sighted = sum(number_sighted), .groups = "drop")

#for map,converting data into sf
whale_sf <- whale_raw |> 
  filter(species %in% c("Humpback Whale", "Fin Whale", "Blue Whale")) |> 
  st_as_sf(coords = c("longitude", "latitude"), crs = 4326) 

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
# Modify the UI to include an option for displaying all species or a specific species
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
               ), 
               mainPanel(
                 plotOutput(outputId = "whale_plot"), 
                 h3("Summary Table"), 
                 p("This table summarizes the total number of whale sightings for each species across the years. 
                   The data is aggregated by year and species to help identify trends and patterns in whale populations."), 
                 tableOutput(outputId = "whale_sum_table")
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
             h3("Advanced Exploratory Data Analysis"),
             p("This section will contain advanced exploratory data analysis features. Currently, it is under development.")
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
      return(whale_sightings)  # Return all species data
    } else {
      return(whale_sightings %>% filter(species == input$whale_species))  # Filter by selected species
    }
  })
  
  # Render the plot based on the filtered data
  output$whale_plot <- renderPlot({
    ggplot(whale_select(), aes(x = interaction(year, month), y = total_sighted, color = species, group = species)) +
      geom_line() +  # Use geom_line() for time series plot
      labs(title = paste(input$whale_species, "Sightings Over Time"), 
           x = "Year", 
           y = "Number of Sightings") +
      theme_minimal() +
      theme(axis.text.x = element_text(angle = 45, hjust = 1))  # Rotate x-axis labels by 45 degrees
  })
  

  # Example reactive table (if required)
  whale_sum_table <- reactive({
    whale_sightings_summary <- whale_sightings %>%
      group_by(species, year) %>%
      summarize(
        total_sighted = sum(total_sighted),  # Sum of the total_sighted
        .groups = "drop") |> 
      pivot_wider(names_from = species, values_from = total_sighted, values_fill = list(total_sighted = 0))
    
    return(whale_sightings_summary)  # Explicitly return the summary table
  })
  
  # Render the table based on the reactive data
  output$whale_sum_table <- renderTable({
    whale_sum_table()  # Use the reactive function to get the data
  })
  
  
  
  #Reactive expression to filter whale data for mapping
  whale_map_data <- reactive({
    if (input$map_species == "All Species") {
      return(whale_sf)  # Return all species data
    } else {
      #return(whale_sf |> filter(species == input$map_species))  # Filter by species
    }
  })
  
  #Reactive expression for reading zones shapefile
  zones_sf <- reactive({
    req(input$zones_shapefile)  # Ensure that file is uploaded
    #Construct the file path to read the shapefile
    zone_file <- input$zones_shapefile$datapath
    #Read the shapefile using sf::st_read
    st_read(zone_file)
  })
  
  #Render tmap 
  output$whale_map <- renderTmap({
    tmap_mode("view")  # Enable interactive mode
    tm_shape(zones_sf()) +
      tm_dots(col = "species", palette = "Set1", size = 0.3) +
      tm_basemap(server = "Esri.WorldImagery")  # Use an Esri basemap
  })
}

# Combine them into an app
shinyApp(ui = ui, server = server)
