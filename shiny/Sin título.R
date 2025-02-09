library(tidyverse)
library(palmerpenguins)
library(shiny)


git config --global user.name "Sofiaagutierrez"
git config --global user.email "slgutierrez@uc.cl"


#create user interface

ui <- fluidPage(
  titlePanel("WhaleAlert: Citizen science to understand whale seassonalities in SB Channel"),
  sidebarLayout( #when we build a widget we have to give it the name id (name of the widget that r is going to recognize, the label, and what the choices are going to be, 
    sidebarPanel('put my widgets here', 
                 radioButtons(
                   inputId = 'penguin_species' #this needs to match the dataset of penguin, 
                   label = 'choose penguin species'#this is what the user needs to see"
                 )), 
    mainPanel('put my graph here')
    
  ))

#create the server function (all the magic happens for the data analysis)

server <-function(input.output){
  
}

#combine them into an app:

shinyApp(ui=ui, server=server)

## add reactive elements to the fluidpage to make the shiny app powerful 