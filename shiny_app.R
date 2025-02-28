# Shiny Script
#Abbey Guilliat, Karlie Hayes, Kylie McGuire

######################################LIBRARIES####################################################

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
library(sf)
library(sp)
library(spatstat)
library(terra)
library(leaflet)
library(janitor)

#########################################THEME#################################################

#THEME

sittp_theme <- bs_theme(bootswatch = "cerulean") |>
  bs_theme_update(bg = "#FFFFFC", fg = "#576B47", 
                  primary = "#353E3D", secondary = "#DFFFC7", success = "#00AFE6", 
                  info = "#244C24", base_font = font_google("Signika"), heading_font = font_google("Crete Round"), 
                  
                  font_scale = NULL, preset = "cerulean")

##########################################DATA################################################

#DATA

##############  MAP DATA ##############
plant_obs <- read_csv(here("data","sb_obs_w_characteristics_toxins.csv"))
ca_counties_sf <- read_sf(here("data","ca_counties_shapefile", "CA_counties.shp"))

#retrieve sb county outline
sb_county_sf <- ca_counties_sf |>
  janitor::clean_names() |>
  filter(name=="Santa Barbara") |>
  st_transform(crs = 2229)

#create spatial observation window  of sb county
sb_county_owin <- as.owin(sb_county_sf)

### TOXIC PLANTS FOR MAP (NOT FILTERED BY TOXIN TYPE) ###
#drop rows with NA lat or lon, filter to only dermally toxic plants
toxic_plant_obs <- plant_obs |>
  drop_na(Latitude) |>
  drop_na(Longitude) |>
  filter(!is.na(`Toxic parts`))
#make dataframe into sf, assign WGS84 CRS (based on info from the source, CalFlora)          
toxic_plant_obs_sf <- st_as_sf(toxic_plant_obs, coords = c("Longitude","Latitude"), crs = "WGS84")
#transform data to projected coordinate system, NAD83 California state plane zone 5
toxic_plant_obs_sf <- toxic_plant_obs_sf |>
  st_transform(crs = 2229)

### NONTOXIC PLANTS FOR MAP ###
#drop rows with NA lat or lon, filter to only NON dermally toxic plants
nontoxic_plant_obs <- plant_obs |>
  drop_na(Latitude) |>
  drop_na(Longitude) |>
  filter(is.na(`Toxic parts`))
#make dataframe into sf, assign WGS84 CRS (based on info from the source, CalFlora)          
nontoxic_plant_obs_sf <- st_as_sf(nontoxic_plant_obs, coords = c("Longitude","Latitude"), crs = "WGS84")
#transform data to projected coordinate system, NAD83 California state plane zone 5
nontoxic_plant_obs_sf <- nontoxic_plant_obs_sf |>
  st_transform(crs = 2229)
#create spatial point pattern of nontoxic plant observation
nontoxic_plant_obs_ppp <- as.ppp(nontoxic_plant_obs_sf)
#make full point pattern object
sb_county_nontoxic_plant_obs_ppp <- ppp(nontoxic_plant_obs_ppp$x, nontoxic_plant_obs_ppp$y, window = sb_county_owin)
#remove duplicates
sb_county_nontoxic_plant_obs_ppp <- unique(sb_county_nontoxic_plant_obs_ppp)

#extract input options for Map
toxin_type_list <- toxic_plant_obs_sf$`Toxin 1` |>
  unique() |>
  na.omit()

##############  ELEVATION DATA ##############
characteristics_data <- read_csv(here("data", "sb_species_w_characteristics_toxins.csv")) 

characteristics_elevation_clean <- characteristics_data |>
  janitor::clean_names() |>
  select(lifeform, lower_elevation, upper_elevation) |>
  cbind(lifeform_clean = NA) |>
  mutate(lifeform = tolower(lifeform))

unique(characteristics_elevation_clean$lifeform)

# create broader groups :tree, herb, shrub, grass, moss, hornwort, vine, seaweed, fern

characteristics_elevation_clean$lifeform_clean[str_detect(characteristics_elevation_clean$lifeform, "grass")] = "Grass"
characteristics_elevation_clean$lifeform_clean[str_detect(characteristics_elevation_clean$lifeform, "herb")] = "Herb"
characteristics_elevation_clean$lifeform_clean[str_detect(characteristics_elevation_clean$lifeform, "shrub")] = "Shrub"
characteristics_elevation_clean$lifeform_clean[str_detect(characteristics_elevation_clean$lifeform, "tree")] = "Tree"
characteristics_elevation_clean$lifeform_clean[str_detect(characteristics_elevation_clean$lifeform, "moss")] = "Herb"
characteristics_elevation_clean$lifeform_clean[str_detect(characteristics_elevation_clean$lifeform, "hornwort")] = "Hornwort"
characteristics_elevation_clean$lifeform_clean[str_detect(characteristics_elevation_clean$lifeform, "vine")] = "Vine"
characteristics_elevation_clean$lifeform_clean[str_detect(characteristics_elevation_clean$lifeform, "seaweed")] = "Seaweed"
characteristics_elevation_clean$lifeform_clean[str_detect(characteristics_elevation_clean$lifeform, "fern")] = "Fern"

characteristics_elevation_clean <- characteristics_elevation_clean |> drop_na() |>
  mutate(lifeform_clean = as.factor(lifeform_clean))

unique(characteristics_elevation_clean$lifeform)


##############  TIME SERIES DATA ##############

#once data is loaded, replace "non-native invasive" with "non-native"
# and replace "rare" with "native"

#observations <- observations |>
#  mutate(`Native Status` = case_when(
#    `Native Status` == "non-native invasive" ~ "non-native",
#    `Native Status` == "rare" ~ "native",
#    TRUE ~ `Native Status`))

##############  GAME DATA ##############

poison_codes <- read_csv(here("data", "UCANR Poisonous Plants Metadata Key.csv"))

game_plants <- read_csv(here("data", "UCANR Skin Irritant Plants Clean.csv")) |>
  clean_names()

game_images <- read_csv(here("data", "plant_images.csv")) |>
  left_join(select(game_plants,scientific_name,toxic_part_1,toxic_part_2),by=c("plant_name"="scientific_name")) |>
  left_join(select(poison_codes,Code,Meaning),by=c("toxic_part_1"="Code")) |>
  left_join(select(poison_codes,Code,Meaning),by=c("toxic_part_2"="Code")) |>
  rename(
    toxic_part1 = Meaning.x,
    toxic_part2 = Meaning.y
  ) |>
  select(plant_name,image_url,toxic_part1,toxic_part2)

toxic_part <-  na.omit(unique(c(game_images$toxic_part1, game_images$toxic_part2)))
toxic_part <- append(toxic_part, "none")
toxic_part <- Filter(function(x) x != "whole plant", toxic_part)

##########################################UI################################################

#USER INTERFACE

ui <- fluidPage(
  theme = sittp_theme,
  page_navbar(
    title = "Should I touch that plant?",
    inverse = TRUE,
    
    ##############  INFO PAGE UI  ##############
    
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
    
    ##############  MAP UI ##############
    
    nav_panel(title = "Plant Distribution Map", 
              titlePanel("Plant Distribution Map"),
              sidebarLayout(
                sidebarPanel(
                  sliderInput(inputId = "map_opacity",
                              label = "Opacity",
                              min = 0,
                              max = 1,
                              value = 0.5,
                              step = 0.05
                              ),
                  checkboxGroupInput(
                    inputId = 'toxin_type',
                    label = "Choose toxin type",
                    choices = toxin_type_list
                    )
                  ),
                mainPanel(
                  textOutput(outputId = "selected_toxin"),
                  leafletOutput(outputId = "map_output")
                  )
                )
              ),
    
    ##############  ELEVATION UI ##############
    
    nav_panel(title = "Elevation",
                sidebarPanel(
                  sliderInput(
                    inputId = "upper_elevation", # change to between range
                    label = "Choose elevation range", 
                    min=-1000,
                    max=10000,
                    value=0,
                    step=250
                  )
                ),
                
                mainPanel( 
                  textOutput(outputId = "selected_elevation"),
                  plotOutput(outputId = "elevation_plot_output")  
                )
              ),
    
    ##############  TIME SERIES UI ##############
    
    nav_panel(title = "Time Series", 
              titlePanel("Time Series"),
              sidebarLayout(
                sidebarPanel(
                  checkboxGroupInput(inputId = "native_status_choice",
                                     label = "Native Status", 
                                     choices = c("Native" = "native", "Non-Native" = "non-native")
                                     ), 
                   checkboxGroupInput(inputId = "duration_choice",
                                      label = "Duration",
                                      choices = c("Perennial", "Annual")
                                      )
                  ),
                 mainPanel("Count of Observations Over Time",
                           textOutput(outputId = "ts_native_choice"),
                           textOutput(outputId = "ts_duration_choice"),
                           plotOutput(outputId = "time_plot_output")
                           )
               )
              ),
    
    ##############  GAME UI ##############
    
    nav_panel(title = "Game", p("Put your plant intuition to the test!"),
              titlePanel("Game"),
              sidebarLayout(
                sidebarPanel(
                  selectInput("select_game", 
                              label = "Which part of the plant is safe to touch?", 
                              choices = toxic_part, 
                              selected = "none"),
                  actionButton("guess_game", label = "Guess",icon = icon("seedling")),
                  actionButton("new_game", label = "Play Again",icon = icon("leaf"))
                ),
                mainPanel( 
                  textOutput("select_game"),
                  uiOutput("display_image"),
                  textOutput("guess_message")
                  )
                )
              )
    )
)

###########################################SERVER###############################################

#SERVER

server <- function(input, output) {

  ##############  MAP SERVER ##############
  
  ## Display selected toxin(s)
  output$selected_toxin <- renderText({
    paste("You selected:", paste(input$toxin_type, collapse = ", "))
  })
  
  int_ratio_raster_reactive <- reactive({
    toxic_plant_obs_sf_filtered <- toxic_plant_obs_sf |>
      filter(`Toxin 1` %in% input$toxin_type | `Toxin 2` %in% input$toxin_type)
    #create spatial point pattern of plant observation
    toxic_plant_obs_ppp <- as.ppp(toxic_plant_obs_sf_filtered)
    #make full point pattern object
    sb_county_toxic_plant_obs_ppp <- ppp(toxic_plant_obs_ppp$x, toxic_plant_obs_ppp$y, window = sb_county_owin)
    #remove duplicates
    sb_county_toxic_plant_obs_ppp <- unique(sb_county_toxic_plant_obs_ppp)
    ###INTENSITY RATIO
    #find common bandwidth- mean of default bandwidths obtained when using density()
    #to estimate the intensity of toxic and nontoxic plants separately.
    bw_toxic <- attr(density(sb_county_toxic_plant_obs_ppp), "sigma")
    bw_nontoxic <- attr(density(sb_county_nontoxic_plant_obs_ppp), "sigma")
    bw <- (bw_toxic + bw_nontoxic)/2
    #use selected bandwidth to compute the smoothed intensity estimates
    int_toxic <- density(sb_county_toxic_plant_obs_ppp, sigma = bw)
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
    #set raster's spatial extent to be the same as the individual kernel density rasters
    #and add crs info to raster so it can be mapped
    ext(int_ratio_raster_flip) <- ext(sb_county_sf)
    crs(int_ratio_raster_flip) <- "+init=epsg:2229"
    return(int_ratio_raster_flip)
  })
  output$map_output <- renderLeaflet({
    if(length(input$toxin_type)>0){
    leaflet() |>
      addTiles() |>
      addRasterImage(int_ratio_raster_reactive(),
                     colors="YlOrRd",
                     opacity = input$map_opacity) |>
      setView(lng = -120.2, lat = 34.5, zoom = 8) |>
      addLegend(pal = colorNumeric(palette="YlOrRd",
                                   values(int_ratio_raster_reactive())),
                      values = values(int_ratio_raster_reactive(), na.rm = T),
                title = HTML("Intensity Ratio <br> of Toxic to <br> NonToxic Plants")
      )
    }
    else{
      leaflet() |>
        addTiles() |>
        setView(lng = -120.2, lat = 34.5, zoom = 8)
    }
  })
  
  ##############  ELEVATION SERVER  ##############
    
  elevation_select <- reactive({
    characteristics_df <- characteristics_elevation_clean |> # sum is.na
      mutate(within_elevation = if_else((input$upper_elevation > lower_elevation & input$upper_elevation < upper_elevation), 1, 0)) |>
      group_by(lifeform_clean) |>
      summarize(count = sum(within_elevation))
  })
  
  output$selected_elevation <- renderText({
    paste("You selected:", paste(input$upper_elevation, collapse = ", "))
  })
  
  output$elevation_plot_output <- renderPlot({
    ggplot(data = elevation_select()) +
      geom_col(aes(x = lifeform_clean, y = count)) +
      scale_fill_manual(values = c("#576B47")) +
      ylim(0, 10) +
      labs(x = "Lifeform", y = "Number of Species", title = "Number of species per lifeform category") +
      theme_bw()
  })
  
  ##############  TIME SERIES SERVER ##############

    
  ##############  GAME SERVER ##############
  current_plant <- reactiveVal(NULL)
  guess_message <- reactiveVal("") # initialize empty string
  
  # sets first image when user clicks on tab
  observe({
    if (is.null(current_plant())) {
      new_plant <- game_images[sample(nrow(game_images), 1), ]
      current_plant(new_plant)
    }
  })
  
  # updates image when user selects 'play again'
  observeEvent(input$new_game, {
    new_plant <- game_images[sample(nrow(game_images), 1), ] # randomize
    current_plant(new_plant)
    guess_message("")
  })
  
  output$display_image <- renderUI({
    img(src = current_plant()$image_url, height = "400px")
  })
  
  # sets response message based on user guess
  observeEvent(input$guess_game, {
    # whole plant is toxic, user does not choose 'none'
    if (current_plant()$toxic_part1 == "whole plant"
        && input$select_game != 'none') {
      guess_message(paste("Oh no, you got a rash! All parts of ",current_plant()$plant_name," are toxic."))
    } 
    # whole plant is toxic, user does chooses 'none'
    else if (current_plant()$toxic_part1 == "whole plant") {
      guess_message(paste("Phew! All parts of ",current_plant()$plant_name," are toxic. You safely admired from afar."))
    } # user chooses 'none', 1 toxic part
    else if (input$select_game=="none" && is.na(current_plant()$toxic_part2)) {
      guess_message(paste("Oh no! You played it safe by not touching ",current_plant()$plant_name,". Only the ",current_plant()$toxic_part1, " is toxic. You missed out on a magical moment with nature ):"))
    } # user chooses 'none', 2 toxic parts
    else if (input$select_game=="none" && !is.na(current_plant()$toxic_part2)) {
      guess_message(paste("Oh no! You played it safe by not touching ",current_plant()$plant_name,". Only the ",current_plant()$toxic_part1," and ",current_plant()$toxic_part2, " are toxic. You missed out on a magical moment with nature ):"))
    }
    else if (input$select_game==current_plant()$toxic_part1 && is.na(current_plant()$toxic_part2)) {
      guess_message(paste("Oh no! You touched ",current_plant()$plant_name," and got a rash. The ",current_plant()$toxic_part1," is toxic."))
    }
    else if (input$select_game==current_plant()$toxic_part1
             || (!is.na(current_plant()$toxic_part2) &&
                 input$select_game==current_plant()$toxic_part2)) {
      guess_message(paste("Oh no! You touched ",current_plant()$plant_name," and got a rash. The ",current_plant()$toxic_part1, " and ",current_plant()$toxic_part2, "are toxic.")) 
    } 
    else {
      guess_message(paste("Phew! You touched a safe part of ",current_plant()$plant_name," and had a magical moment with nature (:"))
    }
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
