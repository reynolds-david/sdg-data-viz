#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
#

library(shiny)
library(shinydashboard)
library(shinyWidgets)
library(dplyr)
library(tidyr)
library(tibble)
library(imputeTS)
library(ggplot2)
library(plotly)
library(factoextra)
library(cluster)

goal3 <- read.csv('..\\goal3.csv') %>% 
  drop_na("iso_alpha3_code")

############# DEFINE UI LOOK
ui <- dashboardPage(

    ### Application title
    dashboardHeader(title = "UN SDG, Goal 3"),

    ### Create the sidebar
    dashboardSidebar(
      
        ## Choose a timeframe (variable: input$time)
        sliderInput(
          inputId = "time", 
          label = "Timeframe:",
          min = min(goal3$time_period),
          max = max(goal3$time_period),
          value = c(2010, max(goal3$time_period)),
          step = 1,
          sep = ""
        ),
          
          ## Choose area to look at (variable: input$area)
          pickerInput(
            inputId = "area", 
            label = "Country or Area:",
            choices = unique(goal3$geo_area_name),
            multiple = T,
            selected = unique(goal3$geo_area_name),
            options = list(`actions-box` = TRUE)
          ),
          
          ## Choose target (variable: input$target)
          pickerInput(
            inputId = "target", 
            label = "Target:",
            choices = unique(goal3$target),
            multiple = T,
            selected = unique(goal3$target),
            options = list(`actions-box` = TRUE)
          ),
          
          ## Choose indicator (variable: input$indicator)
          pickerInput(
            inputId = "indicator", 
            label = "Indicator:",
            choices = unique(goal3$indicator),
            multiple = T,
            selected = unique(goal3$indicator),
            options = list(`actions-box` = TRUE)
          ),
        
        ## Choose number of clusters (variable: input$k)
        numericInput(
          inputId = "k", 
          label = "# of Clusters:",
          value = 4,
          min = 1,
          step = 1
        )
          
    ),

    ### Create the body of the dash
    dashboardBody(
      
      
      fluidRow(valueBox(length(unique(goal3$geo_area_name)), "Countries"),
               valueBox(length(unique(goal3$target)), "Targets"),
               valueBox(length(unique(goal3$indicator)), "Indicators")
      ),
      
      # Overall description of what user should see and do
      fluidRow(box("Each Goal has several indicators. These indicators help determine the success of the goals.",
               width = 12)),
      # Line charts of targets and indicators over time
      fluidRow(box("Each Goal has several indicators. These indicators help determine the success of the goals.",
                   plotlyOutput("targets_over_time")),
               box("Each Target has several indicators. These are measurable things that people keep track of.",
                   plotlyOutput("indicators_over_time"))),
      # Bar charts of best and worst countries
      fluidRow(box("Each Goal has several indicators. These indicators help determine the success of the goals.",
                   plotlyOutput("best_countries")),
               box("Each Target has several indicators. These are measurable things that people keep track of.",
                   plotlyOutput("worst_countries"))),
      fluidRow(box("Each Goal has several indicators. These indicators help determine the success of the goals.",
                   width = 12,
                   plotOutput("clusters"))),
      
      tags$footer("Visit SDG WEBSITE for more information on UN Sustainable Development Goals")
    )
)

############# Define server logic
server <- function(input, output) {
  
  # Get reactive dataset based on selected filters.
  goal3_reactive <- reactive({
    goal3 %>% 
      filter(geo_area_name %in% input$area,
             target %in% input$target,
            indicator %in% input$indicator,
            time_period >= input$time)
  })

  ### Line graph of targets over time
  output$targets_over_time <- renderPlotly({
    ggplotly(
      ggplot(data = goal3_reactive() %>% 
               select(target, time_period, norm) %>% 
               group_by(target, time_period) %>% 
               summarize(value = mean(norm)) %>% 
               group_by(target) %>% 
               mutate(value = scale(value)),
             aes(x = time_period, y = value, color = target)) + 
        geom_line() + 
        theme_minimal()
    )
  })
  
  ### Line graph of indicators over time
  output$indicators_over_time <- renderPlotly({
    ggplotly(
      ggplot(data = goal3_reactive() %>% 
               select(indicator, time_period, norm) %>% 
               group_by(indicator, time_period) %>% 
               summarize(value = mean(norm)) %>% 
               group_by(indicator) %>% 
               mutate(value = scale(value)),
             aes(x = time_period, y = value, color = indicator)) +
        geom_line() +
        theme_minimal()
    )
  })
  
  # Further structure the data for the bar plots
  goal3_countries_reactive <- reactive({
    goal3_reactive() %>% 
      select(geo_area_name, time_period, norm) %>% 
      group_by(geo_area_name, time_period) %>% 
      summarize(value = mean(norm)) %>% 
      group_by(geo_area_name) %>% 
      mutate(value = scale(value))
  })
  
  ### Bar chart of best performing countries
  output$best_countries <- renderPlotly({
    ggplotly(
      ggplot(data = goal3_countries_reactive() %>% 
               arrange(desc(value)) %>% 
               head(10),
             aes(x = reorder(geo_area_name, -value), y = value)) + 
        geom_bar(stat = 'identity') + 
        theme_minimal() +
        theme(axis.text.x = element_text(angle = 45, vjust = 0.5, hjust=1))
    )
  })
  
  ### Bar chart of worst performing countries
  output$worst_countries <- renderPlotly({
    ggplotly(
      ggplot(data = goal3_countries_reactive() %>% 
               arrange(desc(value)) %>% 
               tail(10),
             aes(x = reorder(geo_area_name, -value), y = value)) + 
        geom_bar(stat = 'identity') + 
        theme_minimal() +
        theme(axis.text.x = element_text(angle = 45, vjust = 0.5, hjust=1))
    )
  })
  
  # Further structure the data for clustering
  
  goal3_clustering <- reactive({
    goal3_reactive() %>%  
      select(geo_area_name, series_code, time_period, norm) %>% 
      group_by(geo_area_name, series_code, time_period) %>%
      summarize(value = mean(norm, na.rm = T)) %>% 
      group_by(geo_area_name, series_code) %>% 
      mutate(value = as.numeric(scale(value))) %>% 
      group_by(geo_area_name, series_code) %>% 
      summarize(value = mean(value)) %>% 
      pivot_wider(names_from = series_code, values_from = value) %>% 
      purrr::discard(~sum(is.na(.x))/length(.x)* 100 >=90) %>% 
      na_mean() %>% 
      mutate_at(vars(-geo_area_name), as.numeric) %>% 
      remove_rownames %>% column_to_rownames(var="geo_area_name")
  })
  
  clustering <- reactive({
    kmeans(goal3_clustering(), input$k, nstart = 25)
  })
  
  output$clusters <- renderPlot({
    fviz_cluster(clustering(), data = goal3_clustering())
  })
  
}

# Run the application 
shinyApp(ui = ui, server = server)
