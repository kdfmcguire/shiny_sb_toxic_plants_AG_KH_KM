# timeseries testing

library(tidyverse)
library(here)


#### DATA PROCESSING

time_obs <- read_csv(here("data","sb_obs_w_characteristics_toxins.csv"))
  
time_obs_ts <- time_obs |>
  clean_names() |>
  select(date, native_status, toxic_parts, taxon, latitude, longitude, duration) |>
  mutate(native_status = case_when(
    native_status == "non-native invasive" ~ "non-native",
    native_status == "rare" ~ "native",
    TRUE ~ native_status)) |>
  unique() |> # confirm that it is ok to do this - was having issues with duplicate data
  mutate(date = ymd(date)) |>
  as_tsibble(key = c(native_status, toxic_parts, taxon, latitude, longitude, duration),
             index = date)
  

#####

date_vec <- c(date("1975-01-01"),date("1985-01-01"))

time_series_plot_data <-
  time_obs_ts |> 
    filter(native_status %in% c("native")) |>
    index_by(date_selected = ~get(c("year"))(.)) |>
    filter_index(as.character(date_vec[1]) ~ as.character(date_vec[2])) |>
    summarize(toxic_count = sum(!is.na(toxic_parts)),
              non_toxic_count = sum(is.na(toxic_parts))
    ) |>
    ungroup()

ggplot(data = time_series_plot_data) +
  geom_line(aes(x=date_selected, y=toxic_count/non_toxic_count))



##### UI
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
                ),
                selectInput(inputId = "time_scale", 
                            label = "Select option below:", 
                            choices = c("Weekly" = "yearweek", "Monthly" = "yearmonth", "Yearly" = "year") 
                ), 
              ),
              mainPanel("Count of Observations Over Time",
                        textOutput("ts_native_choice"),
                        textOutput("ts_duration_choice"),
                        plotOutput(outputId = "time_plot_output")
              )
            )
  )

  
  
##### SERVER
  
  time_series_tsibble <- reactive({
    obs_df <- observations |> 
      filter(native_status %in% input$native_status_choice) |>
      filter(duration %in% input$duration_choice) |>
      index_by(date = ~input$time_scale(.)) |>
      summarize(toxic_count = count(!is.na(toxic_parts)),
                non_toxic_count = count(is.na(toxic_parts))
                ) |>
      ungroup()
  })

output$ts_native_choice <- renderText({
  paste("You selected:", paste(input$native_status_choice,
                               collapse = ", ")
  )
})

output$ts_duration_choice <- renderText({
  paste("You selected:", paste(input$duration_choice,
                               collapse = ", ")
  )
})

output$time_plot_output <- renderPlot({
  ggplot(data = time_series()) +
    geom_line()
  

})
