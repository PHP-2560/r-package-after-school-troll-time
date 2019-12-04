# source("preprocess.R")
library("preprocessHTT")
source('variable_importance_plot.R')
source('model_yijie.R')


library(shiny)
ui <- navbarPage(
  'Mega Project',
  id = "inTabset",
  
  ### Define the title of different pages;
  ### each tabPanel defines a new page
  
  #### Define the first page to read in our dataset
  tabPanel(
    title = 'Read in dataset',
    value = 'panel1',
    
    sidebarLayout(
      sidebarPanel(
        fileInput(
          "file1",
          "Choose CSV File",
          accept = c("text/csv",
                     "text/comma-separated-values,text/plain",
                     ".csv")
        ),
        tags$hr(),  
        checkboxInput("header", "Header", TRUE)
        # actionButton("reset", "reset")
      ),
      mainPanel(tableOutput("contents"))
    )
  ),
  
  tabPanel(
    title = 'Variable Importance Plot',
    value = "panel2",
    sidebarPanel(tabsetPanel(tabPanel(
      selectInput(
        inputId = 'var_imp_preference',
        label = 'Select the variable importance plot you would like to see:',
        choices =  c(
          'Gradient Boosting Machine' = "gbm",
          "Neural Network Based" = "dnn"
        ),
        selected = 'gbm'
      )
    ))),
    
    #### Selection of dataset no longer necessary
    #### Default to training dataset
    mainPanel(
      textOutput("print"),
      textOutput("print2"),
      plotOutput(outputId = "var_imp_plot"),
      actionButton('jumpToP1', 'Back'),
      actionButton('jumpToP3', 'Next')
    )
  ),
  
  
  #### Define the second page to be the preview for a quick model
  tabPanel(
    title = 'Modeling',
    value = "panel3",
    
    sidebarPanel(
      fluidRow(
        column(
          10,
          radioButtons(
            "model",
            h4("Model"),
            choices = list(
              "Xgboost (lighter)" ,
              "Xgboost (deeper)" ,
              "Gradient Boost",
              "Random Forest",
              "SVM",
              "Naive Bayes"
            ),
            selected = "SVM"
          ),
          conditionalPanel(
            "input.model === 'SVM'",
            sliderInput(
              "hyper_param",
              h5("C"),
              min = 0,
              max = 100,
              value = 1
            )
          ),
          
          conditionalPanel(
            "input.model == 'Gradient Boost' || input.model == 'Xgboost (lighter)'|| input.model == 'Xgboost (deeper)'",
            sliderInput(
              "max_depth",
              h5("max depth"),
              min = 1,
              max = 20,
              value = 5
            ),
            sliderInput(
              "ntrees",
              h5("number of trees"),
              min = 1,
              max = 100,
              value = 30
            ),
            sliderInput(
              "learn_rate",
              h5("learning rate"),
              min = 0,
              max = 1,
              value = 0.2
            )
          )
        )
      ),
      
      
      fluidRow(column(
        3,
        checkboxGroupInput(
          "metrics",
          h4("Metrics"),
          choices = list("AUC", "Accuracy",
                         "F1"),
          selected = "AUC"
        )
      )),
      
      
      actionButton("select", "Select")
    ),
    mainPanel(#textOutput("selected"),
      textOutput("result"),
      actionButton('jumpToP2', 'Back'))
  )
)
  
  
  #### Define the third panel to be variable importance plot
  ##### CAUTION: DID NOT CONNECT WITH INPUT FILE
  


server <- function(input, output, session) {
  
  
  # Build a selector for the features that are most important
  
  observeEvent(input$jumpToP2, {
    updateTabsetPanel(session, "inTabset",
                      selected = "panel2")
  })
  
  observeEvent(input$jumpToP1, {
    updateTabsetPanel(session, "inTabset",
                      selected = "panel1")
  })
  observeEvent(input$jumpToP3, {
    updateTabsetPanel(session, "inTabset",
                      selected = "panel3")
  })
  
  #### Implementation from Hao
  
  getdat <- reactive({
    req(input$file1)
    read.csv(file = input$file1$datapath, header = input$header)
  })
  
  output$contents <- renderTable({
    df = req(getdat())
    head(df)
  })
  
 # observeEvent(input$reset, {
 #   output$contents <- renderTable({
 #     df = req(getdat())
 #     head(df)
 #   })
    
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
        selectInput(inputId = 'y',label = 'Select target column',choices = colnames(df)),
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
  

  ## yijie added
  observeEvent(input$confirmonehot, {
    updateTabsetPanel(session, "inTabset",
                      selected = "panel2")
  })
  
  
  #### Select dataset to render variance importance plot
  output$print <- renderText({ paste("Now, the training dataset has",dim(getdat2()$train)[1],"rows and",dim(getdat2()$train)[2],"columns.The target label is:",input$y,". The column names are:")
    })
  output$print2 <- renderText({ paste("Now, the training dataset has",dim(getdat2()$train)[1],"rows and",dim(getdat2()$train)[2],"columns.The target label is:",input$y,".")
    names(getdat2()$train)})
  output$var_imp_plot <- renderPlot({
    variable_importance(getdat2()$train, input$var_imp_preference,input$y)})
  
  
  ##### implement from yijie 
  model.metrics <- eventReactive(input$select, {
    input$metrics
  })
  model.model<-eventReactive(input$select, {
    input$model
  })
  model.hyper_param<- eventReactive(input$select, {
    input$hyper_param
  })
  model.max_depth<- eventReactive(input$select, {
    input$max_depth
  })
  model.ntrees<- eventReactive(input$select, {
    input$ntrees
  })
  model.learn_rate<- eventReactive(input$select, {
    input$learn_rate
  })
  
  
  output$result <- renderText({
  train<-getdat2()$train
  test <-getdat2()$test
  y <- input$y
  x <- setdiff(names(train), y)
  # For binary classification, response should be a factor
  train[, y] <- as.factor(train[, y])
  test[, y] <- as.factor(test[, y])
  h2o.init(nthreads = -1)
  h2o.no_progress()
  train <- as.h2o(train)
  test <- as.h2o(test)
  
  result <-new_model(train=train,test=test,y = y,
                     metrics=model.metrics(),
                     model=model.model(),
                     hyper_param=model.hyper_param(),ntrees = model.ntrees(),max_depth = model.max_depth(),learn_rate = model.learn_rate())
  result
})
  
  
  
}

shinyApp(ui, server)