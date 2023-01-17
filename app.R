library(dplyr)
library(shiny)
library(ggplot2)
library(markdown)

flights <- readr::read_csv("flights_data.csv")

# Define UI for app that draws a histogram ----
ui <- fluidPage(
  
  # Add CSS styling
  tags$head(
    tags$style(HTML("
      @import url('https://fonts.googleapis.com/css2?family=Roboto:wght@100&display=swap');
      body {
        color: #505050;
        font-family: 'Roboto', sans-serif;
        padding-left: 20px;
        padding-right: 20px;
      }
      h2 {
        font-family: 'Roboto', sans-serif;
        color: #black;
      }"))
  ),
  
  # App title ----
  titlePanel("European Flights"),
  
  # Subtitle
  markdown("
           The number of flights arriving or leaving from European airports saw a dramatic decrease with the onset of the Covid-19 pandemic in March 2020. Amsterdam - Schipol remains the busiest airport, averaging 1,150 flights per day since January 2016.
           
           Data: [Eurocontrol](https://ansperformance.eu/data/)
           "),
  
  # Row for plot
  fluidRow(
    # bar chart output
    column(10, 
           plotOutput(outputId = "barplot")
           ),
    # controls
    column(2,
           markdown("### **Controls**
               
               Use the selectors below to choose a set of countries to explore.
               "),
           checkboxGroupInput(
             inputId = "country",
             label = "Country",
             choices = c("Belgium", "France", "Ireland", "Luxembourg", "Netherlands", "United Kingdom"),
             selected = c("Belgium", "France", "Ireland", "Luxembourg", "Netherlands", "United Kingdom")
           )
           )
    )
  )
  

# Function to define server
server <- function(input, output) {
  
  # Filter the data
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
      scale_fill_manual(values = c("Belgium" = "#F2C57C",
                                   "France" = "#DDAE7E",
                                   "Ireland" = "#7FB685",
                                   "Luxembourg" = "#426A5A",
                                   "Netherlands" = "#EF6F6C",
                                   "United Kingdom" = "#AC9FBB")) +
      theme_minimal() +
      theme(panel.grid.major.x = element_blank(),
            panel.grid.minor.x = element_blank())
  })

}

shinyApp(ui, server)
