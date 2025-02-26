# Shiny Script
#Abbey Guilliat, Karlie Hayes, Kylie McGuire

##########################################################################################

library(shiny)
library(tidyverse)
library(DT)
library(shinyWidgets)
library(shinycssloaders)
library(markdown)
library(rsconnect)
library(here)
library(bslib)
library(bsicons)

##########################################################################################

#THEME

sittp_theme <- bs_theme(bootswatch = "cerulean") |>
  bs_theme_update(bg = "#FFFFFC", fg = "#576B47", 
                  primary = "#353E3D", secondary = "#DFFFC7", success = "#00AFE6", 
                  info = "#244C24", base_font = font_google("Signika"), heading_font = font_google("Crete Round"), 
                  
                  font_scale = NULL, preset = "cerulean")

##########################################################################################

#DATA

#extract input options for widgets
poison_codes <- read_csv(here("data", "UCANR Poisonous Plants Metadata Key.csv"))

toxin_type <- poison_codes |>
  filter(Column =="Toxins") |>
  pull(Meaning)

toxic_part <- poison_codes |>
  filter(Column =="Toxic Part") |>
  pull(Meaning)

##########################################################################################

#USER INTERFACE

ui <- fluidPage(
  theme = sittp_theme,
  page_navbar(
    title = "Should I touch that plant?",
    inverse = TRUE,
    
    ##############  INFO PAGE  ##############
    
    nav_panel(title = "Overview",     # overview tab
              accordion(              # creating an accordion format for the overview tab
                accordion_panel(
                  icon = bs_icon("window-sidebar"),
                  title = "App Overview",
                  "This app allows the user to explore the characteristics and distribution of dermally toxic plants in Santa Barbara County."
                ),
                accordion_panel(
                  icon = bs_icon("info-square"),
                  title = "Background Information",
                  HTML("We’ve all been there – wandering through a flowering meadow, stumbling
                  upon an unfamiliar plant, and wondering, “Hmm… should I touch that plant?”
                  Like most people, we love plants, but not all of them are safe to touch.
                  We created this Shiny app to explore the distribution and characteristics
                  of dermally toxic plants in Santa Barbara County. We hope it helps you
                  learn something new, or at least reminds you to touch with caution.
                  <br><br>Happy plant hunting!
                       <br>– Abbey, Karlie, & Kylie")
                  ),
                accordion_panel(
                  icon = bs_icon("clipboard-data"),
                  title = "Data Citations",
                  HTML("
                  Information on Plant Toxins:<br>
                  Alsop, J. A., & Karlik, J. F. (2016). Poisonous plants.
                  University of California Agriculture and Natural Resources.
                  https://doi.org/10.3733/ucanr.8560 <br> <br>
                  
                  Plant Observations and Characteristics:<br>
                  Calflora: Information on California plants for education, research and conservation.
                  [web application]. 2019. Berkeley, California: The Calflora Database [a non-profit organization].
                  Available: https://www.calflora.org/ (Accessed: Jan, 30 2025).")
                  )
                )
              ),
    
    ##############  MAP  ##############
    
    nav_panel(title = "Plant Distribution Map", 
              titlePanel("Plant Distribution Map"),
              sidebarLayout(
                sidebarPanel(
                  checkboxGroupInput(
                    inputId = 'toxin_type',
                    label = "Choose toxin type",
                    choices = toxin_type
                    )
                  ),
                mainPanel(
                  textOutput("selected_toxin"),
                  leafletOutput(outputId = "map_output")
                  )
                )
              ),
    
    ##############  ELEVATION PLOT  ##############
    
    nav_panel(title = "Elevation",
              titlePanel("Elevation"),
              sidebarLayout(
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
                )
              ),
    
    ##############  TIME SERIES  ##############
    
    nav_panel(title = "Time Series", 
              titlePanel("Time Series"),
              sidebarLayout(
                sidebarPanel(
                            radioButtons(inputId = "Native Status", # R variable name
                                         label = "Native Status", 
                                         choices = c("Native" = "native", "Non-Native" = "rare")), # confirm whether rare means non-native
                            checkboxGroupInput(inputId = "Lifeform",
                                               label = "Lifeform Type",
                                               choices = c("Perennial Herb", "Annual Herb")) # add all of the choices here (this is just a few)
                                                         
                              ),
                 mainPanel("Title - Count over Time",
                           plotOutput(outputId = "time_plot_output")) # add later
               )
              ),
    
    ##############  GAME  ##############
    
    nav_panel(title = "Game", p("Put your plant intuition to the test!"),
              titlePanel("Game"),
              sidebarLayout(
                sidebarPanel(
                  selectInput("select_game", 
                              label = "Which part of the plant is safe to touch?", 
                              choices = toxic_part, 
                              selected = 1),
                  actionButton("guess_game", label = "Guess",icon = icon("seedling")),
                  actionButton("new_game", label = "Play Again",icon = icon("leaf"))
                ),
                mainPanel( 
                  textOutput("select_game"),
                  # placeholder image
                  img(src = "https://www.calflora.org/app/up/entry/245/73766.jpg", height = "300px", width = "300px"),
                  textOutput("guess_message")
                  )
                )
              )
    )
)

##########################################################################################

#SERVER

server <- function(input, output) {

  ##############  MAP  ##############
  
  ## Display selected toxin(s)
  output$selected_toxin <- renderText({
    paste("You selected:", paste(input$toxin_type, collapse = ", "))
  })
  
  ### Use selected toxins to generate intensity ratio raster
  
  #### find common bandwidth- mean of default bandwidths obtained when using density()
  #### to estimate the intensity of toxic and nontoxic plants separately.
  bw_toxic <- attr(density(sb_county_toxic_plant_obs_ppp), "sigma")
  bw_nontoxic <- attr(density(sb_county_nontoxic_plant_obs_ppp), "sigma")
  bw <- (bw_toxic + bw_nontoxic)/2
  
  #use selected bandwidth to compute the smoothed intensity estimates
  int_toxic<- density(sb_county_toxic_plant_obs_ppp, sigma = bw)
  int_nontoxic <- density(sb_county_nontoxic_plant_obs_ppp, sigma = bw)
  
  #estimate α as the ratio of number of toxic observations to the number of nontoxic observations
  #to account for there being far mor nontoxic plant observations
  alpha <- sb_county_toxic_plant_obs_ppp$n/sb_county_nontoxic_plant_obs_ppp$n
  
  #create intensity ratio raster
  int_ratio <- int_toxic$v/(alpha * int_nontoxic$v)
  int_ratio_raster <- rast(int_ratio)
  
  #transpose the image values returned by density(), since they are stored in transposed form
  #save as raster and plot
  int_ratio_raster_flip <- flip(int_ratio_raster, direction="vertical")
  plot(int_ratio_raster_flip)
  
  #set raster's spatial extent to be the same as the individual kernel density rasters
  #and add crs info to raster so it can be mapped
  ext(int_ratio_raster_flip) <- ext(sb_county_nontoxic_plant_obs_raster)
  crs(int_ratio_raster_flip) <- "+init=epsg:2229"
  
  
    output$map_output <- renderLeaflet({
    leaflet() |>
      addTiles() |>
      addRasterImage(int_ratio_raster_flip, colors="YlOrRd", opacity = 0.7) |> #add opacity slider?
      setView(lng = -120.2, lat = 34.5, zoom = 8)   })
  
  ##############  ELEVATION PLOT  ##############
    
  output$selected_elevation <- renderText({
    paste("You selected:", paste(input$elevation_ft, collapse = ", "))
  })
  
  output$elevation_plot_output <- renderPlot({
    plot(x=1:10,y=1:10,main = "Plants by Elevation")
  })
  
  ##############  TIME SERIES  ##############
  
    
  ##############  GAME  ##############
  
  output$select_game <- renderText({
    paste("You selected:", paste(input$select_game, collapse = ", "))
  })
  
  #when guess is made
  ################################################
  guess_message <- reactiveVal("") # initialize empty string
  
  observeEvent(input$guess_game, {
    guess_message("Oh no! You got a rash and your son was eaten by wolves ):") # populates string when clicked
  })
  
  output$guess_message <- renderText({
    guess_message()  # outputs message to user
  })
  
  
  ################################################
  
  output$time_plot_output <- renderPlot({
    plot(x=1:10,y=1:10,main = "Plants over time")
  })
  
}

##########################################################################################

shinyApp(ui = ui, server = server)
