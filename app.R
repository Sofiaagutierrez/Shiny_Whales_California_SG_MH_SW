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
library(adehabitatHR)


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

# For map, calling zones file
zones_sf <- st_read("data/zones_shapefile.shp")

# Expand rows based on number_sighted
whale_expanded <- whale_raw %>%
  uncount(weights = number_sighted)  # Duplicates rows based on the value in number_sighted

# For map, converting data into sf
whale_sf <- whale_expanded %>%
  filter(species %in% c("Humpback Whale", "Fin Whale", "Blue Whale")) %>% 
  st_as_sf(coords = c("longitude", "latitude"), crs = 4326)


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

#Reproject to a projected CRS (change CRS based on your region!)
zones_sf_projected <- st_transform(zones_sf, crs = 32610)  # Example: UTM Zone 10N (adjust as needed)

#Calculate area in square meters
zones_sf_projected$area_m2 <- st_area(zones_sf_projected)

#Convert to square kilometers (1,000,000 m² = 1 km²)
zones_sf_projected$area_km2 <- as.numeric(zones_sf_projected$area_m2) / 1e6  

#head(shp_projected[, c("area_m2", "area_km2")])

#Reproject back to WGS 84 for mapping
zones_sf <- st_transform(zones_sf_projected, crs = 4326)


# Create the user interface (this is the front end side of the Shiny App)
ui <- navbarPage(
  title = div(style = "color: white; font-weight: bold; font-size: 60px",
              "Whale Alert - Endangered Species Monitoring", 
              tags$img(src = "noaa_logo.png", height = "120px", width = "120px",  style = "margin-left: 50px;")), 
  theme = bslib::bs_theme(bootswatch = "flatly", primary = "#1874CD"), 
  
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
  tabsetPanel(  
    # Add an image to the Data Information tab label
    tabPanel(
      tagList(tags$img(src = "home.png", height = "20px", width = "20px", style = "margin-right: 10px;"), "Data Information"),  # Add image to the tab label
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
           the date and time of the whale sighting, the whale alert species, and the number of sighted individuals per single record time. 
          The map below details the six RAMP fishing zones and spatial range of the California State Waters."),
        h3(""), 
        tags$img(src = "map.png", height = "450", width = "400", style = "margin-top: 10px;") # Adjust the margin-top to move the image down
      )
    ), 
    
    tabPanel(
      tagList(tags$img(src = "sun.png", height = "20px", width = "20px", style = "margin-right: 10px;"),"Trends and Seasonality"),  
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
          plotOutput(outputId = "whale_plot", height = "420px"), 
          plotOutput(outputId = "whale_plot2", height = "420px"), 
          plotOutput(outputId = "whale_season_plot") 
        )
      )
    ),
    
    tabPanel(
      tagList(tags$img(src = "loupe.png", height = "20px", width = "20px", style = "margin-right: 10px;"),"Interactive Map"),
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
                                    selected = "Fin Whale")
            )
          )
        ),
        
        mainPanel(
          # Dynamically display the selected map
          tmapOutput("whale_map")  # Display the selected map
        )
      )
    ),
    
    tabPanel(
      tagList(tags$img(src = "clock.png", height = "20px", width = "20px", style = "margin-right: 10px;"),"Whale Migration Forecast"), 
      sidebarLayout(
        sidebarPanel(
          radioButtons(inputId = "forecast_species", 
                       label = "Select Whale Species", 
                       choices = c("Blue Whale", "Fin Whale", "Humpback Whale"), 
                       selected = "Blue Whale"),
          checkboxInput(inputId = "show_decomposition", 
                        label = "Show Decomposition", 
                        value = TRUE)  
        ),
        mainPanel(
          plotOutput("whale_forecast_plot", height = "500px"),
          # Conditional rendering for decomposition plot
          conditionalPanel(
            condition = "input.show_decomposition == true", 
            plotOutput("whale_decomp_plot", height = "500px"))
        )
      )
    ),
    tabPanel(
      tagList(tags$img(src = "whale.png", height = "20px", width = "20px", style = "margin-right: 10px;"),"Additional Resources and Links") , 
      tags$div(
        style = "display: flex; flex-direction: column; align-items: center; justify-content: center; text-align: center;",
        h4("For more information on reducing whale strikes, check out the following:"),
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
        tags$img(src = "humpback.jpg", height = "300px", width = "650px",  style = "margin-top: 30px;"))
    )
  )
)


# Create the server function 
server <- function(input, output, session) {
  
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
        geom_line() +  # Line for individual species
        scale_color_manual(values = c("Humpback Whale" = "steelblue1", "Fin Whale" = "steelblue3", "Blue Whale" = "steelblue4")) +
        theme_bw() +  # Apply black-and-white theme
        labs(title = paste(input$whale_species, "Annual Sightings (2014-2024)"), 
             x = "Year", 
             y = "Whale Sightings") +
        scale_x_continuous(breaks = seq(min(whale_select()$year), max(whale_select()$year), by = 1)) + 
        theme(
          panel.border = element_rect(color = "dodgerblue4", size = 1.2), 
          plot.title = element_text(size = 16, face = "bold", hjust = 0.5) # Bold and centered title
        )
      
    } else if (input$time_series == "Monthly") {
      # Monthly Time Series Plot with a red trendline
      ggplot(whale_sightings_select(), aes(x = interaction(month, year), y = total_sighted, color = species, group = species)) +
        geom_line() +  # Line for individual species
        geom_smooth(aes(group = 1), method = "lm", color = "red3", se = FALSE, size = 0.5, linetype = "dashed") + 
        scale_color_manual(values = c("Humpback Whale" = "steelblue1", "Fin Whale" = "steelblue3", "Blue Whale" = "steelblue4")) + 
        labs(title = paste(input$whale_species, "Monthly Sightings (2014-2024)"),
             x = "Date",
             y = "Whale Sightings") +
        scale_x_discrete(breaks = function(x) x[seq(1, length(x), by = 12)]) +
        theme_minimal() +
        theme(
          axis.text.x = element_text(angle = 45, hjust = 1), 
          plot.title = element_text(size = 16, face = "bold", hjust = 0.5) # Bold and centered title
        )
    }
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
      mutate(Year = as.integer(Year)) %>%
      # Apply rounding here to remove decimals from numeric columns
      mutate(across(-Year, ~ round(. , 0)))  # Round all columns except 'Year'
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
      tm <- tm_shape(zones_sf) +  # Background shapefile data (zones_sf)
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
  # For species selection, filter the data
  filtered_whale_data <- reactive({
    if (input$forecast_species == "Blue Whale") {
      return(whale_blue_agg)
    } else if (input$forecast_species == "Fin Whale") {
      return(whale_fin_agg)
    } else if (input$forecast_species == "Humpback Whale") {
      return(whale_hump_agg)
    } 
  })
  
  # The time series and perform decomposition for the selected species
  output$whale_decomp_plot <- renderPlot({
    whale_data <- filtered_whale_data()
    
    # Convert to time series object
    whale_ts <- ts(whale_data$total_sightings, start = c(2014, 1), end = c(2024, 12), frequency = 12)
    
    if (input$show_decomposition) {
      # Create decomposition using STL (more robust than decompose)
      whale_decomp <- stl(whale_ts, s.window = "periodic")
      
      # Set larger font size for title and labels
      par(mar = c(5, 5, 5, 5))  # Adjust margins if necessary
      par(cex.main = 2)  # Increase title size
      par(cex.lab = 2)  # Increase axis labels size
      par(cex.axis = 1.5)  # Axis label size
      
      # Plot decomposition
      plot(whale_decomp, main = paste(input$forecast_species, "Sightings - Time Series Decomposition"),
           col.main = "darkblue", col.lab = "darkblue",
           font.main = 1)
      
    } else {
      NULL  # Don't show the decomposition plot if unchecked
    }
  })
  
  # Forecasting with enhanced visualization for the selected species
  output$whale_forecast_plot <- renderPlot({
    whale_data <- filtered_whale_data()
    
    # Convert to time series object
    whale_ts <- ts(whale_data$total_sightings, start = c(2014, 1), end = c(2024, 12), frequency = 12)
    
    # Check species and create the forecast accordingly
    if (input$forecast_species == "Blue Whale" | input$forecast_species == "Fin Whale") {
      # Create forecast with 70% and 85% confidence intervals using Seasonal Naive method
      forecast_obj <- snaive(whale_ts, h = 36, level = c(70, 85))
      
      # Extract forecast data for custom plotting
      fc_data <- as.data.frame(forecast_obj)
      
      # Get time information for proper x-axis
      fc_data$time <- as.numeric(time(forecast_obj$mean))
      
      # Plot with ggplot2 for more control
      ggplot() +
        # Plot historical data
        geom_line(data = as.data.frame(whale_ts), 
                  aes(x = as.numeric(time(whale_ts)), y = x, color = "Historical"), 
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
        ggtitle(paste("Forecast for", input$forecast_species, "Sightings")) +
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
          legend.title = element_text(face = "bold", size = 12),
          plot.title = element_text(face = "bold", hjust = 0.5, size = 18),    
          axis.title.x = element_text(size = 14),     
          axis.title.y = element_text(size = 14) 
        ) +
        # Ensure both legend items appear
        guides(
          color = guide_legend(order = 1),
          fill = guide_legend(order = 2)
        )
    }
    else if (input$forecast_species == "Humpback Whale") {
      # Function to forecast future ARIMA for sightings
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
            legend.title = element_text(face = "bold", size = 12),
            plot.title = element_text(face = "bold", hjust = 0.5, size = 18),    
            axis.title.x = element_text(size = 14),     
            axis.title.y = element_text(size = 14) 
          ) +
          # Ensure both legend items appear
          guides(
            color = guide_legend(order = 1),
            fill = guide_legend(order = 2)
          )
        
        # Return the plot
        return(list(plot = p, forecast_obj = forecast_obj))
      }
      
      # Generate ARIMA forecast plot
      forecast_results <- forecast_future_arima(whale_ts, species_name = "Humpback Whale", h = 36)
      
      # Return the ARIMA forecast plot
      forecast_results$plot
    }
  })
}


# Run the application 
shinyApp(ui = ui, server = server)