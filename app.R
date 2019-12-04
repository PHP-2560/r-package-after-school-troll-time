library(shiny)
library(ggplot2)
library(plyr)
library(corrplot)
library(RColorBrewer)
source("Visualization.R")


ui <- fluidPage(
  
  titlePanel("Visualize Data"),
  # or, headerPanel("Visualize Data"),  
  # "NULL"is character!!!!!!!!!!!!!
  
  fluidRow( column(12,
                    radioButtons(
                      "Plot",
                      h4("Choose a Plot"),
                      choices = list(
                        "Scatterplot" ,  "Histogram" ,     "Density Plot",   "Barplot",     "Barplot with Aggregation",
                        "Boxplot",      "Correlation Plot"
                      ),
                      selected = "Histogram", inline=T
                    )
                    )

  ),
  
sidebarLayout(
    sidebarPanel( 
      
      # conditionalPanel("input.plot === 'Histogram'",
      #                    selectInput('xvar', 'X-axis Variable', names(data)),
      #                    selectInput('yvar', 'Y-axis Variable', names(data), selected=names(data)[[2]]),
      #                    selectInput('fill', 'Color Variable', c("NULL", names(data)), selected="NULL"),
      #                    selectInput('group.mean', 'Add Mean Lines for Which Variable: ', c("NULL", names(data)), selected="NULL"),
      #                    selectInput('position', 'Position: ', c("identity", "dodge","stack"), selected="identity"),
      #                    sliderInput(inputId = "alpha",
      #                                label = "Adjust Transparency (Alpha)",
      #                                value = 0.5, min = 0.1, max = 1),
      #                    numericInput(inputId = "binwidth",
      #                                 label = "Adjust Binwidth",
      #                                 value = 20)
      #   )
                          selectInput('xvar', 'X-axis Variable', names(data)),
                          selectInput('yvar', 'Y-axis Variable', names(data), selected=names(data)[[2]]),
                          selectInput('fill', 'Color Variable', c("NULL", names(data)), selected="NULL"),
                          selectInput('group.mean', 'Add Mean Lines for Which Variable: ', c("NULL", names(data)), selected="NULL"),
                          selectInput('position', 'Position: ', c("identity", "dodge","stack"), selected="identity"),
                          sliderInput(inputId = "alpha",
                                      label = "Adjust Transparency (Alpha)",
                                      value = 0.5, min = 0.1, max = 1),
                          numericInput(inputId = "binwidth",
                                       label = "Adjust Binwidth",
                                       value = 20)    
      
      ),

    
    mainPanel(
      plotOutput('Histogram')
    )
)
)




server <- function(input, output) {


output$Histogram <- renderPlot({
  Histogram(data, input$xvar, input$fill, input$group.mean, input$binwidth, input$alpha, input$position )
})



}




shinyApp(ui = ui, server = server)