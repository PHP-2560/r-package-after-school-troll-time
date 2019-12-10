library(shiny)
library(ggplot2)
library(plyr)
library(corrplot)
library(RColorBrewer)
source("Visualization.R")


ui <- fluidPage(
  titlePanel("Visualize Data"),
  
  navbarPage("Choose a Plot:",
             tabPanel("Scatterplot",
                      sidebarLayout(
                        sidebarPanel(
                          selectInput('scatter_xvar', 'X-axis Variable', names(data)),
                          selectInput('scatter_yvar', 'Y-axis Variable', names(data), selected=names(data)[[2]]),
                          selectInput('scatter_color', 'Color Variable', c("NULL", names(data)), selected="NULL"),
                          selectInput('scatter_shape', 'Shape Variable', c("NULL", names(data)), selected="NULL"),
                          selectInput('scatter_size', 'Size Variable', c("NULL", names(data)), selected="NULL"),
                          numericInput(inputId = "scatter_jitter_width",
                                       label = "Adjust Jitter Width",
                                       value = 0),
                          numericInput(inputId = "scatter_jitter_height",
                                       label = "Adjust Jitter Height",
                                       value = 0)
                        ),
                        mainPanel(
                          plotOutput("scatter")
                        )
                      )
             ),
             tabPanel("Histogram",
                      sidebarLayout(
                        sidebarPanel(
                          h6("Note: X-axis Variable has to be a continuous variable."),
                          selectInput('hist_xvar', 'X-axis Variable', names(data)),
                          selectInput('hist_group', 'Histogram By Group', c("NULL", names(data)), selected="NULL"),
                          selectInput('hist_position', 'Position: ', c("identity", "dodge","stack"), selected="identity"),
                          sliderInput(inputId = "hist_alpha",
                                      label = "Adjust Transparency (Alpha)",
                                      value = 0.5, min = 0.1, max = 1),
                          numericInput(inputId = "hist_binwidth",
                                       label = "Adjust Binwidth",
                                       value = 20)
                        ),
                        mainPanel(
                          plotOutput("hist")
                        )
                      )
             ),
             tabPanel("Density Plot",
                      sidebarLayout(
                        sidebarPanel(
                          selectInput('dens_xvar', 'X-axis Variable', names(data)),
                          h6("Note: Choose a categorical variable to identify groups."),
                          selectInput('dens_group', 'Density Plot By Group', c("NULL", names(data)), selected="NULL"),
                          sliderInput(inputId = "dens_alpha",
                                      label = "Adjust Transparency (Alpha)",
                                      value = 0.2, min = 0.1, max = 1)
                        ),
                        mainPanel(
                          plotOutput("dens")
                        )
                      )
             ),
             tabPanel("Bar Chart",
                      sidebarLayout(
                        sidebarPanel(
                          selectInput('bar_xvar', 'X-axis Variable', names(data)),
                          selectInput('bar_yvar', 'Y-axis Variable', names(data), selected=names(data)[[2]]),
                          selectInput('bar_fill', 'Color Variable', c("NULL", names(data)), selected="NULL"),
                          selectInput('bar_position', 'Position: ', c("identity", "dodge","stack"), selected="identity"),
                          sliderInput(inputId = "bar_alpha",
                                      label = "Adjust Transparency (Alpha)",
                                      value = 0.5, min = 0.1, max = 1),
                          numericInput(inputId = "bar_width",
                                       label = "Adjust width",
                                       value = 0.7)
                          # ,
                          # # radioButtons("bar_coord_flip",
                          # #              choices = list("TRUE" ,  "FALSE"), selected = "FALSE", inline=T),
                          # radioButtons("bar_facet",
                          #              choices = list("TRUE" ,  "FALSE"), selected = "FALSE", inline=T),
                          # selectInput('bar_fvar', 'Faceting Variable', c("NULL", names(data)), selected="NULL")
                        ),
                        mainPanel(
                          plotOutput("bar")
                        )
                      )
             ),
             tabPanel("Box Plot",
                      sidebarLayout(
                        sidebarPanel(
                          selectInput('box_xvar', 'X-axis Variable', names(data)),
                          selectInput('box_yvar', 'Y-axis Variable', names(data), selected=names(data)[[2]]),
                          selectInput('box_fill', 'Color Variable', c("NULL", names(data)), selected="NULL"),
                          sliderInput(inputId = "box_alpha",
                                      label = "Adjust Transparency (Alpha)",
                                      value = 0.5, min = 0.1, max = 1)
                          # ,
                          # radioButtons("box_coord_flip",
                          #              choices = list("TRUE" ,  "FALSE"), selected = "FALSE", inline=T)

                        ),
                        mainPanel(
                          plotOutput("box")
                        )
                      )
              ) ,
             tabPanel("Correlation Plot",
                      sidebarLayout(
                        sidebarPanel(
                          selectInput('sig.level', 'Choose Significance Level', c(0.01,0.05,0.1))
                        ),
                        mainPanel(
                          plotOutput("corr")
                        )
                      )
             )
  )
)




server <- function(input, output) {
  output$scatter <- renderPlot({
    scatterplot(data, input$scatter_xvar, input$scatter_yvar, input$scatter_color, input$scatter_shape, 
                input$scatter_size, input$scatter_jitter_width, input$scatter_jitter_height)})
  
  output$hist <- renderPlot({
    histogram(data, input$hist_xvar, input$hist_group, input$hist_binwidth, input$hist_alpha, input$hist_position)})

  output$dens <- renderPlot({
    density(data, input$dens_xvar, input$dens_group, input$dens_alpha)})

  output$bar <- renderPlot({
    barplot(data, input$bar_xvar, input$bar_yvar, input$bar_fill, input$bar_position, input$bar_width,
            input$bar_alpha)})    # , input$bar_coord_flip, input$bar_facet, input$bar_fvar

  output$box <- renderPlot({
    boxplot(data, input$box_xvar, input$box_yvar, input$box_fill,  input$box_alpha)})   #, input$box_coord_flip

  output$corr <- renderPlot({
    correlation_plot(data, input$sig.level)})
  
  #output$pvalue <- renderText({cor_pvalue(data, input$sig.level)})

}



shinyApp(ui = ui, server = server)




