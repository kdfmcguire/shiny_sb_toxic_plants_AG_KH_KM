# ##########################################################################################
# 
# library(shiny)
# library(tidyverse)
# library(DT)
# library(shinyWidgets)
# library(shinycssloaders)
# library(markdown)
# library(rsconnect)
# library(here)
# library(bslib)
# library(bsicons)
# library(janitor)
# library(rvest)
# 
# ##########################################################################################
# 
# #DATA
# #extract input options for widgets
# poison_codes <- read_csv(here("data", "UCANR Poisonous Plants Metadata Key.csv"))
# 
# toxin_type <- poison_codes |>
#   filter(Column =="Toxins") |>
#   pull(Meaning)
# 
# toxic_part <- poison_codes |>
#   filter(Column =="Toxic Part") |>
#   pull(Meaning)
# 
# ##########################################################################################
# 
# # GAME IMAGES
# 
# game_plants <- read_csv(here("data", "UCANR Skin Irritant Plants Clean.csv")) |>
#   clean_names()
# 
# main_url <- read_html("https://www.calflora.org/app/ipl?list_id=px3140&family=t&fmt=simple")
# 
# plant_pages_id <- main_url |>
#   html_nodes("a") |>
#   html_attr("href")
# 
# plant_pages <- gsub(" ","",paste("https://www.calflora.org",plant_pages_id))
# 
# plant_pages <- plant_pages[grepl("taxon", plant_pages, ignore.case = TRUE)]
# 
# #options(max.print = 10000)
# #print(plant_pages)
# 
# ##########
# #test
# plant_page <- read_html("https://www.calflora.org/app/taxon?crn=10166")
# 
# plant_name <- plant_page |>
#   html_node(xpath = '//*[@id="c-name"]/font/span/font/span/text()') |>
#   html_text(trim = TRUE)
# 
# image_nodes <- plant_page |>
#   html_nodes(xpath = '//*[@id="c-photohook"]/div/div[3]/div/div/table/tbody/tr[1]/td/a/div/img')
# 
# image_urls <- plant_page |>
#   html_node(xpath = '//*[@id="c-photohook"]/div/div[3]/div/div/table/tbody/tr[1]/td/a/div/img') |>
#   html_attr("src")
# 
# image_url <- plant_page |>
#   html_nodes("img") |>
#   .[2] |> 
#   html_attr("src")
# ##########
# 
# plant_images <- data.frame(plant_name = character(), image_url = character(), stringsAsFactors = FALSE)
# base_url <- "https://www.calflora.org"
# 
# for (page in plant_pages) {
#   
#   plant_page <- read_html(page)
#   
#   if (!is.null(plant_page)) {
#     
#     # extract plant name using xpath
#     plant_name <- plant_page |>
#       html_node(xpath = '//*[@id="c-name"]/font/span/font/span/text()') |>
#       html_text(trim = TRUE)
#     
#     if (!(plant_name %in% game_plants$scientific_name)) {
#       next # skips over if we don't have toxic part data
#     }
#     
#     # extract image urls
#     image_urls <- plant_page |>
#       html_nodes("img") |>
#       .[2] |>  # selects 2nd image on page
#       html_attr("src")
#     
#     # add base to some urls
#     if (!grepl("^https?://", image_urls)) { # if https is missing
#       image_urls <- paste0(base_url, image_urls)
#     }
#     
#     if (!is.na(plant_name) && !is.na(image_urls)) {
#       plant_images <- rbind(plant_images, 
#                             data.frame(plant_name = plant_name, 
#                                        image_url = image_urls, 
#                                        stringsAsFactors = FALSE))
#     }
#     
#   }
# }
# 
# write.csv(plant_images, file = here("data", "plant_images.csv"), row.names = FALSE)
# 
# # 
# poison_codes <- read_csv(here("data", "UCANR Poisonous Plants Metadata Key.csv")) |>
#   filter(Column=="Toxic Part")
# 
# game_images <- read_csv(here("data", "plant_images.csv")) |>
#   left_join(select(game_plants,scientific_name,toxic_part_1,toxic_part_2),by=c("plant_name"="scientific_name")) |>
#   left_join(select(poison_codes,Code,Meaning),by=c("toxic_part_1"="Code")) |>
#   left_join(select(poison_codes,Code,Meaning),by=c("toxic_part_2"="Code")) |>
#   rename(
#     toxic_part1 = Meaning.x,
#     toxic_part2 = Meaning.y
#   ) |>
#   select(plant_name,image_url,toxic_part1,toxic_part2)
  

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

poison_codes <- read_csv(here("data", "UCANR Poisonous Plants Metadata Key.csv")) |>
   filter(Column=="Toxic Part")

 game_images <- read_csv(here("data", "plant_images.csv")) |>
   left_join(select(game_plants,scientific_name,toxic_part_1,toxic_part_2),by=c("plant_name"="scientific_name")) |>
   left_join(select(poison_codes,Code,Meaning),by=c("toxic_part_1"="Code")) |>
   left_join(select(poison_codes,Code,Meaning),by=c("toxic_part_2"="Code")) |>
   rename(
     toxic_part1 = Meaning.x,
     toxic_part2 = Meaning.y
   ) |>
   select(plant_name,image_url,toxic_part1,toxic_part2)

##########################################################################################

ui <- fluidPage(
  theme = sittp_theme,
  
  page_navbar(
    title = "Should I touch that plant?",
    inverse = TRUE,
    nav_panel(title = "Overview",     # overview tab
              accordion(              # creating an accordion format for the overview tab
                accordion_panel(
                  icon = bs_icon("window-sidebar"),
                  title = "App Overview",
                  "This app allows the user to explore the dermally toxic plants of Santa Barbara County"
                ),
                accordion_panel(
                  icon = bs_icon("info-square"),
                  title = "Background Information",
                  HTML("We’ve all been there – wandering through a flowering meadow, stumbling upon an unfamiliar plant, and wondering, “Hmm… should I touch that plant?” Like most people, we love plants, but not all of them are safe to touch. We created this Shiny app to explore the distribution and characteristics of dermally toxic plants in Santa Barbara County. We hope it helps you learn something new, or at least reminds you to touch with caution.<br><br>Happy plant hunting!<br>– Abbey, Karlie, & Kylie")
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
                  Available: https://www.calflora.org/ (Accessed: Jan, 30 2025)."
                  )
                )
              )
    ),
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
    nav_panel(title = "Time Series", 
              layout_sidebar(
                sidebarPanel("Native Status",
                             radioButtons(inputId = "Native Status", # R variable name
                                          label = "Native Status", 
                                          choices = c("Native" = "native", "Non-Native" = "rare")), # confirm whether rare means non-native
                             
                             checkboxGroupInput(inputId = "Lifeform", 
                                                label = "Lifeform Type", 
                                                choices = c("Perennial Herb", "Annual Herb")) # add all of the choices here (this is just a few)
                             
                ),
                
                mainPanel("Title - Count over Time",
                          plotOutput(outputId = "time_plot_output")) # add later
              )),
    nav_panel(title = "Game", p("Put your plant intuition to the test!"),
              layout_sidebar(
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
                  uiOutput("display_image"),
                  textOutput("guess_message"),
                )
              ))
  )
)

##########################################################################################


server <- function(input, output) {
  
  # Display selected toxin(s)
  output$selected_toxin <- renderText({
    paste("You selected:", paste(input$toxin_type, collapse = ", "))
  })
  
  # Placeholder map
  output$map_output <- renderPlot({
    plot(x=1:10,y=1:10,main = "Santa Barbara County Map")
  })
  
  output$selected_elevation <- renderText({
    paste("You selected:", paste(input$elevation_ft, collapse = ", "))
  })
  
  output$elevation_plot_output <- renderPlot({
    plot(x=1:10,y=1:10,main = "Plants by Elevation")
  })
  
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
  
  #display new image
  ################################################
  current_plant <- reactiveVal(NULL)
  
  observeEvent(input$new_game, {
    # select random new plant
    new_plant <- game_images[sample(nrow(game_images), 1), ]
    current_plant(new_plant)
  })
  
  output$display_image <- renderUI({
    img(src = current_plant()$image_url, height = "300px")
  })
  ################################################
  
  output$time_plot_output <- renderPlot({
    plot(x=1:10,y=1:10,main = "Plants over time")
  })
  
}

##########################################################################################

shinyApp(ui = ui, server = server)