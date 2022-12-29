library(dplyr)
library(shiny)
library(ggplot2)
library(markdown)

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
               "),
      
      fluidRow(
        column(12, 
               # bar plot
               div(style='height:400px;',
                   plotOutput(outputId = "barplot", height = "400px")
               ))
      ),
      
      
    )
  )
)

# Function to define server
server <- function(input, output) {
  
  # Filer the data
  plot_df <- reactive({
    req(input$country)
    plot_df <- flights %>% 
      filter(Country %in% input$country) 
    plot_df
  })
  
  # Make the bar chart
  output$barplot <- renderPlot({
    plot_df <- plot_df()
    ggplot(data = plot_df, 
                 mapping = aes(x = Date,
                               y = Total,
                               fill = Country)) +
      geom_col() +
      labs(x = "",
           y = "Total number of flights per week", 
           title = "Total number of flights per week") +
      scale_fill_brewer(palette = "Dark2") +
      theme_minimal() +
      theme(panel.grid.major.x = element_blank(),
            panel.grid.minor.x = element_blank())
  })

}

shinyApp(ui, server)
