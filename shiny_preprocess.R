library(shiny) 
library(DT)
source("preprocess.R")

if (interactive()) {
  
  ui <- fluidPage(
    sidebarLayout(
      sidebarPanel(
        fileInput("file1", "Choose CSV File",
                  accept = c(
                    "text/csv",
                    "text/comma-separated-values,text/plain",
                    ".csv")
        ),
        tags$hr(),
        checkboxInput("header", "Header", TRUE),
        actionButton("reset","reset")
      ),
      mainPanel(
        tableOutput("contents")
        
      )
    )
  )
  
  server <- function(input, output, session) {
    
    getdat <- reactive({
      req(input$file1)
      read.csv(file = input$file1$datapath, header = input$header)
    })
    
    output$contents <- renderTable({
      df = req(getdat())
      head(df)
    })
    
    observeEvent(input$reset, {
      output$contents <- renderTable({
        df = req(getdat())
        head(df)
      })

    })
    
    observeEvent(input$file1, {
      df = req(getdat())
      insertUI(
        selector = "#contents",
        where = "afterEnd",
        ui = mainPanel(
          checkboxGroupInput("onehotselector","select columns to one hot encode",colnames(df),inline=TRUE),
          checkboxGroupInput("ordinalselector","select columns to convert to ordinal",colnames(df),inline=TRUE),
          checkboxGroupInput("stdselector","select columns to standardize",colnames(df),inline=TRUE),
          numericInput("testp", "test data percentage", 0.2, min = 0, max = 1, step = 0.1, width = NULL),
          actionButton("confirmonehot", "confirm")
        )
      )
    })
    
#  to use training data, simply call  
#  df = req(getdat2()$train)
#  to use testing data, simply call  
#  df = req(getdat2()$test)

    getdat2 <- eventReactive(input$confirmonehot, {
        df = req(getdat())
        tp = to_ordinal(df, input$ordinalselector) 
        tp = one_hot(tp, input$onehotselector)
        splited = train_test_split(tp,test_perc = input$testp) 
        standardizer = standardization(splited$train, input$stdselector) 
        splited$train = predict(standardizer, splited$train)
        splited$test = predict(standardizer, splited$test)
        return(splited)
    })

    
    observeEvent(input$confirmonehot, {
      # removeUI(selector = "#getdat")
      
      output$contents <- renderTable({
        df = req(getdat2()$train)
        head(df)  
      })
      
      insertUI(
        selector = "#contents",
        where = "afterBegin",
        ui = "Training data selected")
    })
    
  }
  
  shinyApp(ui, server)
}
