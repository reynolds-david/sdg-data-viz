# Load required packages
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

# Read in the data
goal3 <- read.csv('goal3.csv') %>% 
  mutate(geo_area_name = ifelse(geo_area_name == 'United Kingdom of Great Britain and Northern Ireland', 'United Kingdom', geo_area_name))

############# DEFINE UI APPEARANCE
ui <- dashboardPage(

    ### Application title
    dashboardHeader(title = "UN Sustainable Development Goal 3",
                    titleWidth = 450),

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
          label = "# of clusters:",
          value = 4,
          min = 1,
          step = 1
        )
          
    ),

    ### Create the body of the dash
    dashboardBody(
      
      # Overview of what the SDGs are
      fluidRow(box('According to the United Nations website, "The 2030 Agenda for Sustainable Development, adopted by all United Nations member states in 2015, provides a shared blueprint for peace and prosperity for people and the planet, now and into the future. At its heart are the 17 Sustainable Development Goals (SDGs), which are an urgent call for action by all countries - developed and developing - in a global partnership. They recognize that ending poverty and other deprivations must go hand in hand with strategies that improve health and education, reduce inequality, and spur economic growth, all while tackling climate change and working to preserve our oceans and forests."',
                   width = 12)),
      
      # Set cards with numbers of countries, target, indicators, etc up top
      fluidRow(valueBox(length(unique(goal3$geo_area_name)), "Countries"),
               valueBox(length(unique(goal3$target)), "Targets"),
               valueBox(length(unique(goal3$indicator)), "Indicators")
      ),
      
      # Overall description of goals
      fluidRow(box('This dashboard focuses on the third Sustainable Development Goal, which aims to "Ensure healthy lives and promote well-being for all at all ages." Scroll down to see how the components of this goal change over time, how well countries are doing on the goal, and clusters of countries with similar component scores',
               width = 12)),
      
      # Line charts of targets and indicators over time
      fluidRow(box(plotlyOutput("targets_over_time"),
                   "There are several targets within each goal. Targets are a quantified sub-component of a goal that contribute in a major way to its achievement. A target should be an outcome. For the visualization above, targets have been normalized to the same scale, and shown over time. Use the filters in the sidebar to examine individual targets."),
               box(plotlyOutput("indicators_over_time"),
                   "Within each target, there are several indicators. Indicators are precise metrics to assess if a target is being met. There may be more than one indicator associated with a target. For the visualization above, indicators have been normalized to the same scale and shown over time. Use the filters in the sidebar to examine individual indicators."
                   )),
      
      # Bar charts of best and worst countries
      fluidRow(box(plotlyOutput("best_countries"),
                   "Some countries are doing very well in attaining the goal of ensuring healthy lives and promoting well-being for all. Use the filters in the sidebar to see how the top 10 countries change with time."),
               box(plotlyOutput("worst_countries"),
                   "Some countries need improvement in attaining the goal of ensuring healthy lives and promoting well-being for all. Use the filters in the sidebar to see how the worst 10 countries change with time.")),
      
      # Cluster viz
      fluidRow(box("The visualization below is the result of a k-means cluster analysis. This unsupervised machine learning technique groups, or clusters, countries together based on their similar responses to each of the series/indicators/targets. See how the groupings change when adjusting the number of clusters in the sidebar. Countries that do not easily fall into a cluster are of particular interest, as this could indicate a country is doing particularly well or particularly poorly on attaining an SDG goal.",
                   width = 12,
                   plotOutput("clusters"))),
      
      tags$footer("Visit https://sdgs.un.org/goals for more information on the UN Sustainable Development Goals.")
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
               select(target, target_name, time_period, norm) %>% 
               group_by(target, target_name, time_period) %>% 
               summarize(value = mean(norm)) %>% 
               group_by(target, target_name) %>% 
               mutate(value = round(scale(value), 2), time_period = as.Date(as.character(time_period), format = '%Y')),
             aes(x = time_period, 
                 y = value, 
                 color = target,
                 group = target,
                 label = target)) + 
        geom_line() + 
        scale_x_date(name = 'Year', date_breaks = '3 years', date_labels = '%Y') +
        theme_minimal() +
        labs(title = 'Targets over time', x = 'Year', y = 'Value') +
        theme(title = element_text(face = "bold"), axis.title = element_text(face = "bold"), 
              legend.title = element_blank()),
      tooltip = c("value", "time_period", "label")
    )
  })
  
  ### Line graph of indicators over time
  output$indicators_over_time <- renderPlotly({
    ggplotly(
      ggplot(data = goal3_reactive() %>% 
               select(indicator, indicator_name, time_period, norm) %>% 
               group_by(indicator, indicator_name, time_period) %>% 
               summarize(value = mean(norm)) %>% 
               group_by(indicator, indicator_name) %>% 
               mutate(value = round(scale(value), 2), time_period = as.Date(as.character(time_period), format = '%Y')),
             aes(x = time_period, 
                 y = value, 
                 color = indicator,
                 group = indicator,
                 label = indicator)) +
        geom_line() +
        scale_x_date(name = 'Year', date_breaks = '3 years', date_labels = '%Y') +
        theme_minimal() +
        labs(title = 'Indicators over time',x = 'Year', y = 'Value') +
        theme(title = element_text(face = "bold"), axis.title = element_text(face = "bold"), 
              legend.title = element_blank()),
      tooltip = c("time_period", "value", "label")
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
        theme(title = element_text(face = "bold"), axis.title = element_text(face = "bold"),
              axis.text.x = element_text(angle = 45, vjust = 0.5, hjust = 1)) +
        labs(title = 'Best-performing countries',
             x = 'Country', y = 'Value') 
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
        theme(title = element_text(face = "bold"), axis.title = element_text(face = "bold"), 
              axis.text.x = element_text(angle = 45, vjust = 0.5, hjust = 1)) +
        labs(title = 'Worst-performing countries',
             x = 'Country', y = 'Value')
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
      purrr::discard(~sum(is.na(.x))/length(.x)* 100 >= 90) %>% 
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
