# timeseries testing

library(tidyverse)
library(here)


#### DATA PROCESSING

observations <- read_csv(here("data", "sb_obs_w_characteristics_toxins.csv"))

observations <- observations |>
  mutate(`Native Status` = case_when(
    `Native Status` == "non-native invasive" ~ "non-native",
    `Native Status` == "rare" ~ "native",
    TRUE ~ `Native Status`)) |>
  
  
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
      filter(`Native Status` %in% input$native_status_choice) |>
      filter(`Duration` %in% input$duration_choice) |>
      index_by(Date = ~input$time_scale(.)) |>
      summarize(`Toxic Count` = count(!is.na(`Toxic Parts`)),
                `Non-toxic Count` = count(is.na(`Toxic Parts`))
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
