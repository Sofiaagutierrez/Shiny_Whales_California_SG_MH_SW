library(tidyverse)
library(palmerpenguins)
library(shiny)
library(here)

# Read in the Whale Alert CSV here 

whale_raw <- read_csv(here("data", "whale_data.csv"))


# Create the user interface (this is the front end side of the Shiny App)
ui <- fluidPage(
  titlePanel("NOAA - Whale Alert"), 
  tabsetPanel( # Add tabsetPanel for tabs
    tabPanel("Data Information",  # First tab now
             h3("Project Motivation"),
             p("The motivation for this project is ..."),
             h3("Data Summary"), 
             p("This dataset was provided to us by Anastasia Kunz, a NOAA affiliate, and details spatial whale sighting data over time across California. 
               As detailed by Anastaia, the relevant columns for this Shiny App include the X, Y position of the whale observation, 
               the date and time of the whale sighting, the whale alert species, and the number of sighted individuals per single record time. ")
    ),
    tabPanel("Data Explorer",  # Second tab (was first)
             sidebarLayout(
               sidebarPanel("Put my widgets here", 
                            radioButtons(
                              inputId = "penguin_species", 
                              label = "Choose penguin species", 
                              choices = c("Adelie", "Gentoo", "Cool Chinstrap Penguins!" = "Chinstrap") 
                            ), 
                            selectInput(inputId = "pt_color",  # Changed from inputID to inputId
                                        label = "Select Point Color", 
                                        choices = c("Roses are red" = "red",
                                                    "Violets are purple" = "purple",
                                                    "Oranges are ..." = "orange"))), 
               mainPanel("Put my graph here", 
                         plotOutput(outputId = "penguin_plot"), 
                         h3("Summary Table"), 
                         tableOutput(outputId = "penguin_table"))  # Changed outputID to outputId
             )
    ), 
    tabPanel("Advanced EDA",  # Moved tab before Resources
             h3("Advanced Exploratory Data Analysis"),
             p("This section will contain advanced exploratory data analysis features. Currently, it is under development.")
    ),
    tabPanel("Resources",  # Resources tab now moved after Advanced EDA
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
server <- function(input, output) {
  penguin_select <- reactive({
    penguins_df <- penguins |> 
      filter(species == input$penguin_species)
  })
  
  output$penguin_plot <- renderPlot({
    ggplot(data = penguin_select()) + 
      geom_point(aes(x = flipper_length_mm, y = body_mass_g),
                 color = input$pt_color) 
  })
  
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
