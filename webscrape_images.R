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
library(janitor)
library(rvest)

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

# GAME IMAGES

game_plants <- read_csv(here("data", "UCANR Skin Irritant Plants Clean.csv")) |>
  clean_names()

main_url <- read_html("https://www.calflora.org/app/ipl?list_id=px3140&family=t&fmt=simple")

plant_pages_id <- main_url |>
  html_nodes("a") |>
  html_attr("href")

plant_pages <- gsub(" ","",paste("https://www.calflora.org",plant_pages_id))

plant_pages <- plant_pages[grepl("taxon", plant_pages, ignore.case = TRUE)]

#options(max.print = 10000)
#print(plant_pages)

##########
#test
plant_page <- read_html("https://www.calflora.org/app/taxon?crn=10166")

plant_name <- plant_page |>
  html_node(xpath = '//*[@id="c-name"]/font/span/font/span/text()') |>
  html_text(trim = TRUE)

image_nodes <- plant_page |>
  html_nodes(xpath = '//*[@id="c-photohook"]/div/div[3]/div/div/table/tbody/tr[1]/td/a/div/img')

image_urls <- plant_page |>
  html_node(xpath = '//*[@id="c-photohook"]/div/div[3]/div/div/table/tbody/tr[1]/td/a/div/img') |>
  html_attr("src")

image_url <- plant_page |>
  html_nodes("img") |>
  .[2] |> 
  html_attr("src")
##########

plant_images <- data.frame(plant_name = character(), image_url = character(), stringsAsFactors = FALSE)
base_url <- "https://www.calflora.org"

for (page in plant_pages) {
  
  plant_page <- read_html(page)
  
  if (!is.null(plant_page)) {
    
    # extract plant name using xpath
    plant_name <- plant_page |>
      html_node(xpath = '//*[@id="c-name"]/font/span/font/span/text()') |>
      html_text(trim = TRUE)
    
    if (!(plant_name %in% game_plants$scientific_name)) {
      next # skips over if we don't have toxic part data
    }
    
    # extract image urls
    image_urls <- plant_page |>
      html_nodes("img") |>
      .[2] |>  # selects 2nd image on page
      html_attr("src")
    
    # add base to some urls
    if (!grepl("^https?://", image_urls)) { # if https is missing
      image_urls <- paste0(base_url, image_urls)
    }
    
    if (!is.na(plant_name) && !is.na(image_urls)) {
      plant_images <- rbind(plant_images, 
                            data.frame(plant_name = plant_name, 
                                       image_url = image_urls, 
                                       stringsAsFactors = FALSE))
    }
    
  }
}

write.csv(plant_images, file = here("data", "plant_images.csv"), row.names = FALSE)