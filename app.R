# Load required libraries
library(shiny)
library(ggplot2)
library(dplyr)
library(tidyr)
library(lubridate)
library(tmap)
library(shinythemes)
library(readr)
library(tsibble)
library(sf)
library(forecast)
library(bslib)

# Read in the Whale Alert CSV
whale_raw <- read_csv("data/whale_cleaned.csv")

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
whale_sf <- whale_raw %>% 
  filter(species %in% c("Humpback Whale", "Fin Whale", "Blue Whale")) %>% 
  st_as_sf(coords = c("longitude", "latitude"), crs = 4326) 

# forecasting data
whale_forecast <- whale_raw

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

# Decompose the time series
whale_blue_decomp <- decompose(whale_blue_ts)
whale_fin_decomp <- decompose(whale_fin_ts)
whale_hump_decomp <- decompose(whale_hump_ts)

# For map, calling zones file
zones_sf <- st_read("data/zones_shapefile.shp")

# Create the user interface (this is the front end side of the Shiny App)
ui <- navbarPage(
  title = div(style = "color: white; font-weight: bold; font-size: 40px", "Whale Alert - Endangered Species Monitoring"), 
  theme = bslib::bs_theme(bootswatch = "flatly", primary = "#1874CD"),  # Apply the custom theme
  
  # Add custom CSS for background color and other styling
  tags$head(
    tags$link(rel = "stylesheet", href = "https://fonts.googleapis.com/css2?family=Playfair+Display&display=swap"),
    tags$style(HTML("
    body {
      font-family: 'Playfair Display', serif;  /* Apply Playfair Display font globally */
      background-color: #C6E2FF;  /* Set the background color to light blue */
    }
    h1, h2, h3, h4, h5, h6 {
      font-family: 'Playfair Display', serif;  /* Apply Playfair Display to headings */
    }
    p {
      font-family: 'Playfair Display', serif;  /* Apply Playfair Display to paragraphs */
    }
    .navbar-nav .nav-item {
      color: #1874CD !important; /* Set the navbar tab text color to the same blue as the title bar */
    }
    .navbar-nav .nav-item a {
      color: #1874CD !important;  /* Make sure links within the tabs have the same color */
    }
    .tab-content {
      padding-top: 60px; /* Add space between the title bar and the content */
    }
  "))
  ),
  
  # Apply the navbarPage structure for navigation
  tabsetPanel(  # Add tabsetPanel for tabs
    tabPanel("Data Information",  
             # Center the entire content using div and CSS
             tags$div(
               style = "display: flex; flex-direction: column; align-items: center; justify-content: center; text-align: center;",
               h3("Project Motivation"),
               p("The motivation for this project is to spatially and statistically assess endangered whale species population dynamics across California. 
                 This information can aid in informing vessel speed reduction (VSR) efforts, as well as concerns surrounding entanglement. We hope that our Shiny App might inform 
                 future compliance efforts, as well as provide a useful interface for future citizen scientists."),
               h3("Data Summary"), 
               p("This dataset was provided to us by Anastasia Kunz, a NOAA affiliate, and details spatial whale sighting data over time across California. 
               Whale Alert is a citizen science database, and therefore requires extensive cleaning and review before its use. 
               The relevant columns for this Shiny App include the X, Y position of the whale observation, 
                 the date and time of the whale sighting, the whale alert species, and the number of sighted individuals per single record time."),
               # The image with proper styling
               tags$img(src = "www/noaa_logo.png", height = "50px", style = "margin-right: 10px;")
             )
    ),
    tabPanel("Whale Sightings Trends and Seasonality",  
             sidebarLayout(
               sidebarPanel(
                 radioButtons(
                   inputId = "whale_species", 
                   label = "Choose whale species", 
                   choices = c("Humpback Whale", "Fin Whale", "Blue Whale", "All Species")  
                 ),
                 selectInput(inputId = "time_series",
                             label = "Choose Time Series View",
                             choices = c("Annual", "Monthly")),
                 h3("Summary Statistics"),  
                 tableOutput(outputId = "whale_sum_table"), 
               ), 
               mainPanel(
                 plotOutput(outputId = "whale_plot"), 
                 plotOutput(outputId = "whale_plot2"), 
                 plotOutput(outputId = "whale_season_plot") 
               )
             )
    ),
    tabPanel("Interactive Map ", 
             # Use fluidRow and column to control map space
             fluidRow(
               column(6,  # Set the left column to hold content or can be left empty
                      p("This is an interactive map of whale sightings and zones.") 
               ),
               column(6,  # Set the map to occupy the right side
                      tmapOutput("whale_map", height = "600px")  # Set map height to 600px for larger map
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
                               value = TRUE)  
               ),
               mainPanel(
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
      return(whale_relevant)  
    } else {
      return(whale_relevant %>% filter(species == input$whale_species))  
    }
  })
  
  # Reactive expression for the filtered whale sightings data based on the selected species
  whale_sightings_select <- reactive({
    if (input$whale_species == "All Species") {
      return(whale_sightings)  
    } else {
      return(whale_sightings %>% filter(species == input$whale_species))  
    }
  })
  
  # Render the plot (Time Series)
  output$whale_plot <- renderPlot({
    if (input$time_series == "Annual") {
      # Annual Time Series Plot
      ggplot(whale_select(), aes(x = year, y = Total_Value, color = species)) + 
        geom_line() + scale_color_manual(values = c("Humpback Whale" = "steelblue1", "Fin Whale" = "steelblue3", "Blue Whale" = "steelblue4")) +  # QUESTION HERE 
        theme_bw() +
        labs(title = paste(input$whale_species, "Annual Sightings (2014-2024)"), 
             x = "Year", 
             y = "Whale Sightings") +
        scale_x_continuous(breaks = seq(min(whale_select()$year), max(whale_select()$year), by = 1))  
    } else if (input$time_series == "Monthly") {
      # Monthly Time Series Plot
      ggplot(whale_sightings_select(), aes(x = interaction(year, month), y = total_sighted, color = species, group = species)) +
        geom_line() + scale_color_manual(values = c("Humpback Whale" = "steelblue1", "Fin Whale" = "steelblue3", "Blue Whale" = "steelblue4")) + 
        labs(title = paste(input$whale_species, "Monthly Sightings (2014-2024)"),
             x = "Date",
             y = "Whale Sightings") +
        theme_minimal() +
        scale_x_discrete(
          breaks = function(x) x[seq(1, length(x), by = 12)]) 
    }
  })
  
  # Render the stacked bar chart (Sightings by Species and Season)  # QUESTION HERE - why so far down? 
  output$whale_season_plot <- renderPlot({
    ggplot(whale_season_sightings, aes(x = season, y = total_sighted, fill = species)) +
      geom_bar(stat = "identity") +
      labs(
        title = "Whale Sightings by Species and Season",
        x = "Season",
        y = "Total Sightings",
        fill = "Species"
      ) +
      scale_fill_manual(values = c("Humpback Whale" = "steelblue1",  
                                   "Fin Whale" = "steelblue3",     
                                   "Blue Whale" = "steelblue4")) +  
      theme_minimal() +
      theme(axis.text.x = element_text(angle = 45, hjust = 1))  
  })
  
  # Reactive summary table for whale sightings
  whale_sum_table <- reactive({
    data_filtered <- if (input$whale_species == "All Species") {
      whale_sightings  
    } else {
      whale_sightings %>% filter(species == input$whale_species)  
    }
    
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
  
  # Render tmap
  output$whale_map <- renderTmap({
    tmap_mode("view")  
    
    tm_shape(zones_sf) +  
      tm_polygons(
        col = "lightblue",  
        border.col = "darkblue",  
        alpha = 0.3
      ) +
      tm_borders() +  
      tm_basemap(server = "Esri.WorldImagery")  
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

# Run the application 
shinyApp(ui = ui, server = server)
