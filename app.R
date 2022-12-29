library(dplyr)
library(shiny)
library(ggplot2)
library(stringr)
library(markdown)
library(plotly)

flights <- readr::read_csv("flights_data.csv")

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
      markdown("### **Controls**
               
               Use the selectors below to choose set of countries to explore.
               "),
      
      br(),

      # Input: Select a country ----
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
      markdown("
               The number of flights arriving or leaving from European airports saw a dramatic decrease with the onset of the Covid-19 pandemic in March 2020. Amsterdam - Schipol remains the busiest airport, averaging 1,150 flights per day since January 2016.
               
               Data: [Eurocontrol](https://ansperformance.eu/data/)
               
               ### Total number of flights per week
               "),
      
      fluidRow(
        column(12, 
               # area plot
               div(style='height:400px;',
                   plotlyOutput(outputId = "areaplot", height = "400px")
               ))
      ),
      
      
    )
  )
)

# Function to define server
server <- function(input, output) {
  
  # Filer the data and add tooltip
  area_df <- reactive({
    req(input$country)
    plot_df <- flights %>% 
      filter(Country %in% input$country) %>% 
      mutate(`Flights` = paste0(Total, 
                                "\nWeek Commencing : ", 
                                Date,
                                "\nCountry : ", 
                                Country))
    plot_df
  })
  
  # Make the area chart
  output$areaplot <- renderPlotly({
    area_data <- area_df()
    g2 <- ggplot(data = plot_df, 
                 mapping = aes(x = Date,
                               y = Total,
                               fill = Country,
                               label = Flights)) +
      geom_col(alpha = 0.7) +
      labs(x = "",
           y = "Total number of flights per week", 
           title = "") +
      scale_fill_manual(values = c("Belgium" = "#8175aa", 
                                   "France" = "#6fb899", 
                                   "Ireland" = "#31a1b3", 
                                   "Luxembourg" = "#ccb22b", 
                                   "Netherlands" = "#a39fc9", 
                                   "United Kingdom" = "#94d0c0")) +
      theme_minimal() +
      theme(plot.margin = unit(c(0.5, 0.5, 0.5, 0.5), unit = "cm"), 
            panel.grid.major.x = element_blank(),
            panel.grid.minor.x = element_blank())
    ggplotly(g2, tooltip = c("label"))
  })

}

shinyApp(ui, server)
