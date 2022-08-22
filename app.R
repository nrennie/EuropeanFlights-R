library(dplyr)
library(shiny)
library(ggplot2)
library(stringr)
library(markdown)
library(lubridate)

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
                   plotOutput(outputId = "barplot")
               )
        ), 
        column(6, 
               # area plot
               div(style='height:400px;',
                   plotOutput(outputId = "areaplot")
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
      arrange(-n)
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
      arrange(-n)
    plot_df2
  })
  
  # plots
  output$barplot <- renderPlot({
    barplot_data <- barplot_df()
    ggplot(data = barplot_data, 
           mapping = aes(x = n, 
                         y = forcats::fct_reorder(Name, n), 
                         fill = Country)) +
      geom_col() +
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
            plot.margin = unit(c(0.5, 1, 0.5, 0.5), unit = "cm"))
    },
    height = function() {nrow(barplot_df())*13 + 30}
  )
  
  output$areaplot <- renderPlot({
    area_data <- area_df()
    ggplot(data = area_data, 
           mapping = aes(x = yr_week, y = n, fill = Country)) +
      geom_area(alpha = 0.7) +
      labs(x = "",
           y = "Average number of flights per day", 
           title = "") +
      scale_fill_manual("", 
                        values = c("Belgium" = "#8175aa", 
                                   "France" = "#6fb899", 
                                   "Ireland" = "#31a1b3", 
                                   "Luxembourg" = "#ccb22b", 
                                   "Netherlands" = "#a39fc9", 
                                   "United Kingdom" = "#94d0c0")) +
      guides(fill = guide_legend(nrow = 1)) +
      theme_minimal() +
      theme(legend.position = "bottom", 
            legend.title = element_blank(), 
            plot.margin = unit(c(0.5, 0.5, 0.5, 0.5), unit = "cm"), 
            panel.grid.major.x = element_blank(),
            panel.grid.minor.x = element_blank())
    
  }, 
  height = 400
  )

}

shinyApp(ui, server)
