library(dplyr)
library(shiny)
library(ggplot2)
library(stringr)
library(markdown)
library(lubridate)
library(plotly)

flights <- readr::read_csv("flights_data.csv")
airports <- readr::read_csv("airport_data.csv")
plot_data <- left_join(flights, airports, by = c("Airport Code" = "Code"))

# Define UI for app that draws a histogram ----
ui <- fluidPage(
  
  tags$head(
    # Note the wrapping of the string in HTML()
    tags$style(HTML("
      body {
        padding-left: 40px;
        padding-right: 0px;
        padding-top: 20px;
      }"))
  ),
  
  # App title ----
  titlePanel("European Flights"),
  
  # Sidebar layout with input and output definitions ----
  sidebarLayout(
    
    position = "right", 
    
    
    # Sidebar panel for inputs ----
    sidebarPanel(
      
      # Title
      includeMarkdown("controls.md"),
      
      br(),
      
      # Date slider
      dateRangeInput(inputId = "date",
                     label = "Date",
                     start = "2016-01-01",
                     end = "2022-05-31", 
                     min = "2016-01-01",
                     max = "2022-05-31"),
      
      br(),
      
      # Input: Slider for the number of bins ----
      checkboxGroupInput(
        inputId = "country",
        label = "Country",
        choices = c("Belgium", "France", "Ireland", "Luxembourg", "Netherlands", "United Kingdom"),
        selected = c("Belgium", "France", "Ireland", "Luxembourg", "Netherlands", "United Kingdom")
      )
      
    ), 
    
    # Main panel for displaying outputs ----
    mainPanel(
      
      # Subtitle
      includeMarkdown("subtitle.md"),
      
      fluidRow(
        column(6, 
               # Bar chart
               div(style='height:400px; overflow-y: scroll',
                   plotlyOutput(outputId = "barplot", height = "1200px")
               )
        ), 
        column(6, 
               # area plot
               div(style='height:400px;',
                   plotlyOutput(outputId = "areaplot", height = "400px")
               ))
      ),
      
      
    )
  )
)

# Define server logic required to draw a histogram ----
server <- function(input, output) {
  
  # filter data
  barplot_df <- reactive({
    plot_df1 <- plot_data %>% 
      filter(Country %in% input$country)  %>% 
      filter(dmy_hms(Date) >= as.Date(input$date[1], origin = "1970-01-01") & dmy_hms(Date) <= as.Date(input$date[2], origin = "1970-01-01")) %>% 
      select(c(Total, Name, Date, Country)) %>% 
      group_by(Name, Country) %>% 
      summarise(n = round(mean(Total, na.rm = T))) %>% 
      arrange(-n) %>% 
      mutate(`Airport ` = paste0(Name, 
                                 "\nCountry : ",
                                 Country, 
                                 "\nAverage : ", 
                                 n))
    plot_df1
  })
  
  area_df <- reactive({
    plot_df2 <- plot_data %>% 
      filter(Country %in% input$country) %>% 
      filter(dmy_hms(Date) >= as.Date(input$date[1], origin = "1970-01-01") & dmy_hms(Date) <= as.Date(input$date[2], origin = "1970-01-01")) %>% 
      select(c(Total, Date, Country)) %>% 
      mutate(yr_week = floor_date(dmy_hms(Date), unit = "weeks")) %>% 
      select(-Date) %>% 
      group_by(yr_week, Country) %>% 
      summarise(n = round(mean(Total, na.rm = T))) %>% 
      arrange(-n) %>% 
      mutate(`Country ` = paste0(Country, 
                                 "\nWeek Commencing : ", 
                                 yr_week,
                                 "\nAverage : ", 
                                 n))
    plot_df2
  })
  
  # plots
  output$barplot <- renderPlotly({
    barplot_data <- barplot_df()
    g1 <- ggplot(data = barplot_data, 
           mapping = aes(x = n, 
                         y = forcats::fct_reorder(Name, n), 
                         fill = Country)) +
      geom_col(aes(label = `Airport `)) +
      scale_fill_manual("", 
                        values = c("Belgium" = "#8175aa", 
                                   "France" = "#6fb899", 
                                   "Ireland" = "#31a1b3", 
                                   "Luxembourg" = "#ccb22b", 
                                   "Netherlands" = "#a39fc9", 
                                   "United Kingdom" = "#94d0c0")) +
      labs(x = "",
           y = "", 
           title = "") +
      theme_minimal() +
      theme(legend.position = "none",
            plot.margin = unit(c(0.5, 1, 0.5, 0.5), unit = "cm"),
            panel.grid.major.y = element_blank(),
            panel.grid.minor.y = element_blank())
    ggplotly(g1, tooltip = c("Airport "))  
    }
  )
  
  output$areaplot <- renderPlotly({
    area_data <- area_df()
    g2 <- ggplot(data = area_data, 
           mapping = aes(x = yr_week,
                         y = n,
                         fill = Country)) +
      geom_area(aes(label = `Country `), alpha = 0.7) +
      labs(x = "",
           y = "Average number of flights per day", 
           title = "") +
      scale_fill_manual("", 
                        values = c("Belgium" = "#8175aa", 
                                   "France" = "#6fb899", 
                                   "Ireland" = "#31a1b3", 
                                   "Luxembourg" = "#ccb22b", 
                                   "Netherlands" = "#a39fc9", 
                                   "United Kingdom" = "#94d0c0"), 
                        labels = function(x) str_wrap(x, width = 7)) +
      guides(fill = guide_legend(nrow = 1)) +
      theme_minimal() +
      theme(legend.position = "bottom", 
            legend.title = element_blank(), 
            plot.margin = unit(c(0.5, 0.5, 0.5, 0.5), unit = "cm"), 
            panel.grid.major.x = element_blank(),
            panel.grid.minor.x = element_blank())
    ggplotly(g2, tooltip = c("Country "))  %>%
      layout(legend = list(
        orientation = "h"
      ))
    
  })

}

shinyApp(ui, server)
