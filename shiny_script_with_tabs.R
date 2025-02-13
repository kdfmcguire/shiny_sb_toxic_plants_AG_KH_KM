# Shiny Script

library(shiny)
library(tidyverse)
library(DT)
library(shinyWidgets)
library(shinycssloaders)
library(markdown)
library(rsconnect)
library(here)
library(bslib)


poison_codes <- read_csv(here("UCANR Poisonous Plants Metadata Key.csv")) |>
  filter(Column =="Toxins")

toxin_type <- poison_codes$Meaning


ui <- page_navbar(
  title = "Should I touch that plant?",
  bg = "#2D89C8",
  inverse = TRUE,
  nav_panel(title = "Overview",     # overview tab
            accordion(              # creating an accordion format for the overview tab
              accordion_panel(
                "Data Summary"           # figure out how to add text here
              ),
              accordion_panel(
                "App Overview"           # figure out how to add more detailed text
              ),
              accordion_panel(
                "Background Information" # figure out how to add more detailed text
              )
            )),
  nav_panel(title = "SB County Map", 
            layout_sidebar(
              sidebarPanel(
                checkboxGroupInput(
                  inputId = 'toxin_type',
                  label = "Choose toxin type",
                  choices = toxin_type
                )
              ),
              
              mainPanel( 
                textOutput("selected_toxin"),
                plotOutput("map_output")  
              ))
  ),
  
  nav_panel(title = "Elevation",
            layout_sidebar( # confirm that this is the right function
              sidebarPanel(
                sliderInput(
                  inputId = 'elevation_ft',
                  label = "Choose elevation range",
                  min=0,
                  max=7000,
                  value=c(0,7000),
                  step=100
                )
              ),
              
              mainPanel( 
                textOutput("selected_elevation"),
                plotOutput("elevation_plot_output")  
              )
            )),
  # nav_panel(title = "Time Series", 
  #           layout_sidebar(
  #             sidebarPanel("Native Status",
  #                          radioButtons(inputId = "Native Status", # R variable name
  #                                       label = "Native Status", 
  #                                       choices = c("Native" = "native", "Non-Native" = "rare")), # confirm whether rare means non-native
  # 
  #                          checkboxGroupInput(inputId = "Lifeform", 
  #                                             label = "Lifeform Type", 
  #                                             choices = c("Perennial Herb", "Annual Herb")) # add all of the choices here (this is just a few)
  #                                                     
  #                          ),
  #             
  #             mainPanel("Title - Count over Time",
  #                       plotOutput(outputId = "")) # add later
  #           )),
  nav_panel(title = "Game", p("Third page content.")),
  nav_spacer()
)

server <- function(input, output) {
  
  # Display selected toxin(s)
  output$selected_toxin <- renderText({
    paste("You selected:", paste(input$toxin_type, collapse = ", "))
  })
  
  # Example placeholder map (you can replace with an actual map later)
  output$map_output <- renderPlot({
    plot(x=1:10,y=1:10,main = "Santa Barbara County Map")
  })
  
  output$selected_elevation <- renderText({
    paste("You selected:", paste(input$elevation_ft, collapse = ", "))
  })
  
  output$elevation_plot_output <- renderPlot({
    plot(x=1:10,y=1:10,main = "Plants by Elevation")
  })
  
}

shinyApp(ui = ui, server = server)


