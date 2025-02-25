library(tidyverse)
library(palmerpenguins)
library(shiny)
library(shinythemes)
library(here)

# Read in the Whale Alert CSV
whale_raw <- read_csv("data/whale_cleaned.csv")

whale_relevant <- whale_raw |> 
  filter(species %in% c("Humpback Whale", "Fin Whale", "Blue Whale")) |> 
  group_by(species, year) |> 
  summarize(Total_Value = sum(number_sighted), .groups = "drop")

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
               sidebarPanel("Put my widgets here", 
                            radioButtons(
                              inputId = "whale_species", 
                              label = "Choose whale species", 
                              choices = c("Humpback Whale", "Fin Whale", "Blue Whale") 
                            ), 
                            selectInput(inputId = "pt_color",  # Changed from inputID to inputId
                                        label = "Select Point Color", 
                                        choices = c("Roses are red" = "red",
                                                    "Violets are purple" = "purple",
                                                    "Oranges are ..." = "orange"))), 
               mainPanel("Put my graph here", 
                         plotOutput(outputId = "whale_plot"), 
                         h3("Summary Table"), 
                         tableOutput(outputId = "penguin_table"))  # Changed outputID to outputId
             )
    ), 
    tabPanel("Whale Migration Forecast and Time Series Analysis",  # Moved tab before Resources
             h3("Advanced Exploratory Data Analysis"),
             p("This section will contain advanced exploratory data analysis features. Currently, it is under development.")
    ),
    tabPanel("Additional Resources",  # Resources tab now moved after Advanced EDA
             h4("For more information on reducing whale strikes, check out the following document:"),
             a("Reduce Whale Strikes - Top 5 Things You Should Know", 
               href = "https://media.fisheries.noaa.gov/dam-migration/reduce-whale-strikes-top5things.pdf", 
               target = "_blank"), 
             # Link to the second URL
             p("Learn more about whales on the Ocean.org website:"),
             a("Whales - Ocean.org", 
               href = "https://ocean.org/whales/", 
               target = "_blank"), 
             br(),
             # Link to the third PDF
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
# Create the server function 
server <- function(input, output) {
  
  # Reactive expression for the filtered whale data based on the selected species
  whale_select <- reactive({
    whale_relevant |> 
      filter(species == input$whale_species)  # Filter data by the selected species
  })
  
  # Render the plot based on the filtered data
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
  
  
  # Example reactive table (if required)
  penguin_sum_table <- reactive({
    penguin_summary_df <- penguins |> 
      filter(species == input$penguin_species) |> 
      group_by(sex) |> 
      summarize(mean_flip = mean(flipper_length_mm, na.rm = TRUE), 
                mean_mass = mean(body_mass_g, na.rm = TRUE))
  })
  
  output$penguin_table <- renderTable({
    penguin_sum_table()
  })
}

# Combine them into an app
shinyApp(ui = ui, server = server)
