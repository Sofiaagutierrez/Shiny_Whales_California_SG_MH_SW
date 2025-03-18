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
library(adehabitatHR)

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

#for map kernel density 
#Filtering data for each species (if needed)
humpback_whales <- whale_expanded %>% filter(species == "Humpback Whale")
blue_whales <- whale_expanded %>% filter(species == "Blue Whale")
fin_whales <- whale_expanded %>% filter(species == "Fin Whale")

#Create a projection, Create SPDF and Check CRS
#CRS projection, variable prj
prj <- "+init=epsg:4326"

#Creating a SpatialPointsDataFrame

whale_exp <- SpatialPointsDataFrame(coords = coordinates(cbind(whale_expanded$longitude,
                                                               whale_expanded$latitude)),  
                                    data = whale_expanded,  
                                    proj4string = CRS(prj))

humpback_w <- SpatialPointsDataFrame(coords = coordinates(cbind(humpback_whales$longitude,
                                                                humpback_whales$latitude)),  
                                     data = humpback_whales,  
                                     proj4string = CRS(prj))

blue_w <- SpatialPointsDataFrame(coords = coordinates(cbind(blue_whales$longitude,
                                                            blue_whales$latitude)),  
                                 data = blue_whales,  
                                 proj4string = CRS(prj))

fin_w <- SpatialPointsDataFrame(coords = coordinates(cbind(fin_whales$longitude,
                                                           fin_whales$latitude)),  
                                data = fin_whales,  
                                proj4string = CRS(prj))
#kernel density 
kernel_whales <- kernelUD(xy = whale_exp,
                          h = "href")
kernel_blue <- kernelUD(xy = blue_w,
                        h = "href")
kernel_fin <- kernelUD(xy = fin_w,
                       h = "href")
kernel_humpack <- kernelUD(xy = humpback_w,
                           h = "href")
kernel_blue <- kernelUD(xy = blue_w,
                        h = "href")
kernel_fin <- kernelUD(xy = fin_w,
                       h = "href")

#extracting 75% and 90%
hr_90_humpback <- getverticeshr(x = kernel_humpack, percent = 90) 
hr_75_humpback <- getverticeshr(x = kernel_humpack, percent = 75)
hr_90_blue <- getverticeshr(x = kernel_blue, percent = 90) 
hr_75_blue <- getverticeshr(x = kernel_blue, percent = 75)
hr_90_fin <- getverticeshr(x = kernel_fin, percent = 90) 
hr_75_fin <- getverticeshr(x = kernel_fin, percent = 75)

#Converting into SF, so it can be plotted in Tmap and clipped to zones_sf

hr_90_humpback_sf <- st_as_sf(hr_90_humpback)
hr_75_humpback_sf <- st_as_sf(hr_75_humpback)
hr_90_blue_sf <- st_as_sf(hr_90_blue)
hr_75_blue_sf <- st_as_sf(hr_75_blue)
hr_90_fin_sf <- st_as_sf(hr_90_fin)
hr_75_fin_sf <- st_as_sf(hr_75_fin)

#Clipping home range to Ocean Polygon (zones_sf)
zones_sf <- st_read("data/zones_shapefile.shp")

#Transform projection of zones_sf
zones_sf <- st_transform(zones_sf, crs = 4326)

# Combine all polygons into one single geometry (for the entire zones_sf dataset)
zones_combined <- zones_sf %>%
  summarize(geometry = st_union(geometry), .groups = "drop")


# Clip home range to zones, unifying polygons

#humpback 90
# Apply st_make_valid() to fix invalid geometries before the union operation
hr_90_humpback_sf <- st_make_valid(hr_90_humpback_sf)

# Union the geometries and apply st_make_valid() again after union
hr_90_humpback_sf_united <- hr_90_humpback_sf %>%
  summarize(geometry = st_union(geometry), .groups = "drop")
hr_90_humpback_sf_united <- st_make_valid(hr_90_humpback_sf_united)

# Check for validity of the geometry
if (!st_is_valid(hr_90_humpback_sf_united)) {
  cat("The geometry is still invalid after union and cleaning.")
} else {
  cat("The geometry is valid.")
}

#humpback 75 
# Apply st_make_valid() to fix invalid geometries before the union operation
hr_75_humpback_sf <- st_make_valid(hr_75_humpback_sf)

# Union the geometries and apply st_make_valid() again after the union
hr_75_humpback_sf_united <- hr_75_humpback_sf %>%
  summarize(geometry = st_union(geometry), .groups = "drop")
hr_75_humpback_sf_united <- st_make_valid(hr_75_humpback_sf_united)

# Check for validity of the geometry
if (!st_is_valid(hr_75_humpback_sf_united)) {
  cat("The geometry is still invalid after union and cleaning for 75%.\n")
} else {
  cat("The geometry is valid for 75%.\n")
}


# Clip home range to zones, unifying polygons

#blue 90
# Apply st_make_valid() to fix invalid geometries before the union operation
hr_90_blue_sf <- st_make_valid(hr_90_blue_sf)

# Union the geometries and apply st_make_valid() again after union
hr_90_blue_sf_united <- hr_90_blue_sf %>%
  summarize(geometry = st_union(geometry), .groups = "drop")
hr_90_blue_sf_united <- st_make_valid(hr_90_blue_sf_united)

# Check for validity of the geometry
if (!st_is_valid(hr_90_blue_sf_united)) {
  cat("The geometry is still invalid after union and cleaning.")
} else {
  cat("The geometry is valid.")
}

#blue 75 
# Apply st_make_valid() to fix invalid geometries before the union operation
hr_75_blue_sf <- st_make_valid(hr_75_blue_sf)

# Union the geometries and apply st_make_valid() again after the union
hr_75_blue_sf_united <- hr_75_blue_sf %>%
  summarize(geometry = st_union(geometry), .groups = "drop")
hr_75_blue_sf_united <- st_make_valid(hr_75_blue_sf_united)

# Check for validity of the geometry
if (!st_is_valid(hr_75_blue_sf_united)) {
  cat("The geometry is still invalid after union and cleaning for 75%.\n")
} else {
  cat("The geometry is valid for 75%.\n")
}

#Fin Whale


# Clip home range to zones, unifying polygons

#Fin 90
# Apply st_make_valid() to fix invalid geometries before the union operation
hr_90_fin_sf <- st_make_valid(hr_90_fin_sf)

# Union the geometries and apply st_make_valid() again after union
hr_90_fin_sf_united <- hr_90_fin_sf %>%
  summarize(geometry = st_union(geometry), .groups = "drop")
hr_90_fin_sf_united <- st_make_valid(hr_90_fin_sf_united)

# Check for validity of the geometry
if (!st_is_valid(hr_90_fin_sf_united)) {
  cat("The geometry is still invalid after union and cleaning.")
} else {
  cat("The geometry is valid.")
}

#Fin 75 
# Apply st_make_valid() to fix invalid geometries before the union operation
hr_75_fin_sf <- st_make_valid(hr_75_fin_sf)

# Union the geometries and apply st_make_valid() again after the union
hr_75_fin_sf_united <- hr_75_fin_sf %>%
  summarize(geometry = st_union(geometry), .groups = "drop")
hr_75_fin_sf_united <- st_make_valid(hr_75_fin_sf_united)

# Check for validity of the geometry
if (!st_is_valid(hr_75_fin_sf_united)) {
  cat("The geometry is still invalid after union and cleaning for 75%.\n")
} else {
  cat("The geometry is valid for 75%.\n")
}


#Intersect whales with  zones
#humpback

zones_combined <- st_make_valid(zones_combined)
#hr_90_humpback_zones <- st_make_valid(hr_90_humpback_zones)
hr_90_humpback_zones <- st_intersection(hr_90_humpback_sf_united, zones_combined)

#hr_75_humpback_zones <- st_make_valid(hr_75_humpback_zones)
hr_75_humpback_zones <- st_intersection(hr_75_humpback_sf_united, zones_combined)

#blue
zones_combined <- st_make_valid(zones_combined)
#hr_90_blue_zones <- st_make_valid(hr_90_blue_zones)
hr_90_blue_zones <- st_intersection(hr_90_blue_sf_united, zones_combined)

#hr_75_blue_zones <- st_make_valid(hr_75_blue_zones)
hr_75_blue_zones <- st_intersection(hr_75_blue_sf_united, zones_combined)

#fin
zones_combined <- st_make_valid(zones_combined)
#hr_90_fin_zones <- st_make_valid(hr_90_fin_zones)
hr_90_fin_zones <- st_intersection(hr_90_fin_sf_united, zones_combined)

#hr_75_fin_zones <- st_make_valid(hr_75_fin_zones)
hr_75_fin_zones <- st_intersection(hr_75_fin_sf_united, zones_combined)

#intersecting whale-sightings with fishing zones
whale_zones <- st_intersection(whale_sf, zones_sf)

whale_sf <- whale_zones

# Define a color palette for different whale species
whale_colors <- c("Humpback Whale" = "lightpink",   # Light Pink for Humpback Whale
                  "Blue Whale" = "#FF3399",       # Medium Pink for Blue Whale
                  "Fin Whale" = "hotpink3")        # Darker Pink for Fin Whale

# Simplify and make valid before intersection
hr_75_blue_zones <- st_simplify(st_make_valid(hr_75_blue_zones))
hr_75_humpback_zones <- st_simplify(st_make_valid(hr_75_humpback_zones))
hr_75_fin_zones <- st_simplify(st_make_valid(hr_75_fin_zones))

# Intersect 75% home ranges of blue, humpback, and fin whales
hr_75_intersection <- st_intersection(hr_75_blue_zones, hr_75_humpback_zones)
hr_75_intersection <- st_intersection(hr_75_intersection, hr_75_fin_zones)


# Step 1: Simplify and make valid the geometries
hr_90_blue_zones <- st_simplify(st_make_valid(hr_90_blue_zones))
hr_90_humpback_zones <- st_simplify(st_make_valid(hr_90_humpback_zones))
hr_90_fin_zones <- st_simplify(st_make_valid(hr_90_fin_zones))

# Step 2: Perform the first intersection between blue and humpback
hr_90_blue_humpback_intersection <- st_intersection(hr_90_blue_zones, hr_90_humpback_zones)

# Union the result if multiple geometries are returned
hr_90_blue_humpback_intersection_combined <- st_union(hr_90_blue_humpback_intersection)

# Step 3: Perform the second intersection with Fin Whale zones
hr_90_intersection <- st_intersection(hr_90_blue_humpback_intersection_combined, hr_90_fin_zones)


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
                 p(""),
                 
                 # Dropdown to select which map to display
                 selectInput("map_select", "Select Map to Display:",
                             choices = c("Whale Sightings", "Kernel Density"),
                             selected = "Whale Sightings"
                 ),
                 
                 # Conditional UI to select species, with additional filters for Whale Sightings map
                 conditionalPanel(
                   condition = "input.map_select == 'Whale Sightings'",
                   selectInput("species", "Select Whale Species:",
                               choices = c("All Species", unique(whale_sf$species)),
                               selected = "All Species"
                   ),
                   selectInput("year", "Select Year:",
                               choices = c("All Years", sort(unique(whale_sf$year))),
                               selected = "All Years"
                   ),
                   selectInput("month", "Select Month:",
                               choices = c("All Months", month.name),
                               selected = "All Months"
                   )
                 ),
                 
                 # Conditional UI to select species for Kernel Density map
                 conditionalPanel(
                   condition = "input.map_select == 'Kernel Density'",
                   selectInput("species", "Select Whale Species:",
                               choices = c("All Species", unique(whale_sf$species),
                               selected = "All Species")
                   )
                 )
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
server <- function(input, output, session) {
  
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
    
    # Update the available species in the dropdown based on filtered data (reactive)
    observe({
      # When the map is changed, we also need to reset the available options
      if (input$map_select == "Whale Sightings") {
        # For Map 1, enable all options (species, year, and month)
        available_species <- unique(whale_sf$species)
        updateSelectInput(session, "species", choices = c("All Species", available_species))
        
        # Update available years based on the data
        available_years <- sort(unique(whale_sf$year))
        updateSelectInput(session, "year", choices = c("All Years", available_years))
        
        # Update available months (we use full month names)
        available_months <- month.name
        updateSelectInput(session, "month", choices = c("All Months", available_months))
        
      } else if (input$map_select == "Kernel Density") {
        # For Map 2, only enable species selection
        available_species <- unique(whale_sf$species)
        updateSelectInput(session, "species", choices = c("All Species", available_species))
        
        # Disable year and month inputs as they're not required for Map 2
        updateSelectInput(session, "year", choices = NULL)
        updateSelectInput(session, "month", choices = NULL)
      }
    })
    
    # Reactive expression for whale map filtering based on selected species, year, and month
    filtered_whale_sf <- reactive({
      req(input$species)  # Ensure that species is selected
      
      # Handle "All Species" selection
      if (input$species == "All Species") {
        species_filter <- TRUE  # All species
      } else {
        species_filter <- whale_sf$species == input$species
      }
      
      # Filter by year if Map 1 is selected (otherwise, skip year filtering for Map 2)
      if (input$map_select == "Whale Sightings" && input$year != "All Years") {
        year_filter <- whale_sf$year == as.numeric(input$year)
      } else {
        year_filter <- TRUE  # Include all years if "All Years" is selected
      }
      
      # Filter by month if Map 1 is selected (otherwise, skip month filtering for Map 2)
      if (input$map_select == "Whale Sightings" && input$month != "All Months") {
        month_abbr <- format(as.Date(paste0("1 ", input$month), "%d %B"), "%b")
        month_filter <- whale_sf$month == month_abbr
      } else {
        month_filter <- TRUE  # Include all months if "All Months" is selected
      }
      
      # Apply the filters and return the filtered data
      filtered_data <- whale_sf %>%
        filter(species_filter, year_filter, month_filter)
      
      return(filtered_data)
    })
    
    # Ensure tmap is in "view" mode for interactivity
    observe({
      tmap_mode("view")
    })
    
    # Render whale sightings map based on filtered data for Map 1
    output$whale_map <- renderTmap({
      data <- filtered_whale_sf()
      
      # Handle case when no data is available after filtering
      if (nrow(data) == 0) {
        showNotification("No data available for the selected filters.", type = "warning")
        return(NULL)  # Prevent errors when no data is available
      }
      

      if (input$map_select == "Whale Sightings") {
        tm_shape(zones_sf) +  # Background shapefile data (zones_sf)
          tm_polygons(col = "lightblue", border.col = "darkblue", alpha = 0.3) +
          tm_borders() +  # Add borders for polygons
          tm_shape(data) +  # Whale sightings data
          tm_dots(col = "species", palette = whale_colors,
                  size = 0.5, alpha = 0.8, shape = 21, border.col = "black", border.lwd = 0.5) +
          tm_basemap(server = "Esri.WorldImagery")  # Basemap
      } else if (input$map_select == "Kernel Density") {
        
        # Kernel Density Map logic (for Kernel Density map)
        # Check the selected species and render the corresponding 75% and 90% HR zones
   
        # Humpback Whale Kernel Density Map     
        if (input$species == "Humpback Whale") {
          tm_shape(zones_sf) +  # Background shapefile data (zones_sf)
            tm_polygons(col = "lightblue", border.col = "steelblue", alpha = 0.3) +
          tm_shape(hr_75_humpback_zones) +
            tm_polygons(col = "steelblue1", border.col = "darkblue", alpha = 0.5) +
            tm_borders() +
            tm_shape(hr_90_humpback_zones) +
            tm_polygons(col = "steelblue3", border.col = "darkblue", alpha = 0.5) +
            tm_borders() +
            tm_basemap(server = "Esri.WorldImagery")+
            tm_add_legend(
              type = "fill",
              labels = c("90% Kernel density", "75% Kernel density"),
              col = c("steelblue1", "steelblue3"))
        } 
        
        # Blue Whale Kernel Density Map  
        else if (input$species == "Blue Whale") {
          tm_shape(zones_sf) +  # Background shapefile data (zones_sf)
            tm_polygons(col = "lightblue", border.col = "steelblue", alpha = 0.3) +
          tm_shape(hr_75_blue_zones) +
            tm_polygons(col = "steelblue1", border.col = "darkblue", alpha = 0.5) +
            tm_borders() +
            tm_shape(hr_90_blue_zones) +
            tm_polygons(col = "steelblue3", border.col = "darkblue", alpha = 0.5) +
            tm_borders() +
            tm_basemap(server = "Esri.WorldImagery")+
            tm_add_legend(
              type = "fill",
              labels = c("90% Kernel density", "75% Kernel density"),
              col = c("steelblue1", "steelblue3"))
          
          
        } 
        # Fin Whale Kernel Density Map     
        else if (input$species == "Fin Whale") {
          tm_shape(zones_sf) +  # Background shapefile data (zones_sf)
            tm_polygons(fill  = "lightblue", border.col = "steelblue", fill_alpha = 0.3) +
          tm_shape(hr_75_fin_zones) +
            tm_polygons(fill = "steelblue1", border.col = "darkblue", fill_alpha = 0.5)+
            tm_borders() +
            tm_shape(hr_90_fin_zones) +
            tm_polygons(fill = "steelblue3", border.col = "darkblue", fill_alpha = 0.5)+
            tm_borders() +
            tm_basemap(server = "Esri.WorldImagery") + 
            tm_add_legend(
              type = "fill",
              labels = c("90% Kernel density", "75% Kernel density"),
              col = c("steelblue1", "steelblue3"))
          
        } 
        
        # Intersection Kernel Density Map (3 species)  
        else if (input$species == "All Species") {
          tm_shape(zones_sf) +  # Background shapefile data (zones_sf)
            tm_polygons(fill  = "lightblue", border.col = "steelblue", fill_alpha = 0.3) +
            tm_shape(hr_75_intersection) +
            tm_polygons(fill = "steelblue1", border.col = "darkblue", fill_alpha = 0.5)+
            tm_borders() +
            tm_shape(hr_90_intersection) +
            tm_polygons(fill = "steelblue3", border.col = "darkblue", fill_alpha = 0.5)+
            tm_borders() +
            tm_basemap(server = "Esri.WorldImagery") + 
            tm_add_legend(
              type = "fill",
              labels = c("90% Kernel density", "75% Kernel density"),
              col = c("steelblue1", "steelblue3"))
          
        
        
        
        }
      }
    })
}    
      
 
  # Run the Shiny app
  shinyApp(ui = ui, server = server)
  