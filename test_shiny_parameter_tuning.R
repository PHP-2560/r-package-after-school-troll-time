library(shiny)
library(caret)
library(dplyr)
library(h2o)
library(devtools)
source("model_yijie.R")
#install_github("h2oai/h2o-3/h2o-r/ensemble/h2oEnsemble-package")

# Define UI ----
ui <- fluidPage(
  titlePanel("Classification Models"),
  sidebarPanel(
    fluidRow(column(
      10,
      radioButtons(
        "model",
        h2("Model"),
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
      conditionalPanel("input.model === 'SVM'",
                       sliderInput("hyper_param", h3("C"),
                                   min = 0, max = 100, value = 1
                       )),
      
      conditionalPanel("input.model === 'Gradient Boost'|'Xgboost (lighter)'|'Xgboost (deeper)'",
                       sliderInput("max_depth", h3("max depth"),
                                   min = 1, max = 20, value = 5
                       ),
                       sliderInput("ntrees", h3("number of trees"),
                                   min = 1, max = 100, value = 30
                       ),
                       sliderInput("learn_rate", h3("learning rate"),
                                   min = 0, max = 1, value = 0.2
                       ))
    )),
    
    
    fluidRow(column(
      3,
      checkboxGroupInput(
        "metrics",
        h3("Metrics"),
        choices = list("AUC", "Accuracy",
                       "F1"),
        selected = "AUC"
      )
    )),
    

    actionButton("select", "Select")
  ),
  mainPanel(#textOutput("selected"),
    textOutput("result"),
    textOutput("selected2"))
)



# Define server logic ----
server <- function(input, output) {
  # Define a reactive expression for the document term matrix
  metrics <- eventReactive(input$select, {
    input$metrics
  })
  model<-eventReactive(input$select, {
    input$model
  })
  
  
  hyper_param<- eventReactive(input$select, {
    input$hyper_param
  })
  max_depth<- eventReactive(input$select, {
    input$max_depth
  })
  ntrees<- eventReactive(input$select, {
    input$ntrees
  })
  learn_rate<- eventReactive(input$select, {
    input$learn_rate
  })
  
  
  
  output$result <- renderText({
    train_test_val_split <-
      function(data,
               train_perc = 0.6,
               test_perc = 0.2) {
        spec = c(
          train = train_perc,
          test = test_perc,
          validate = 1 - train_perc - test_perc
        )
        g = sample(cut(seq(nrow(data)),
                       nrow(data) * cumsum(c(0, spec)),
                       labels = names(spec)))
        return(split(data, g))
      }
    one_hot <- function(data, col) {
      tp <- data
      for (name in col) {
        tp[, name] = as.factor(tp[, name])
      }
      dmy <-
        dummyVars(paste("~", paste(col, collapse = "+"), sep = ""), data = tp)
      trsf <-
        data.frame(predict(dmy, newdata = tp, na.action = na.omit))
      return(cbind(tp[,!(colnames(tp) %in% col)], trsf))
    }
    
    to_ordinal <- function(data, col) {
      tp <- data
      for (name in col) {
        tp[, name] = factor(tp[, name])
      }
      return(tp)
    }
    
    standardization <- function(data, col) {
      tp <- data
      std_trained <-
        preProcess(data[, col],
                   method = c("center", "scale"),
                   na.remove = TRUE)
      std_trained
      # return(cbind(tp[,!(colnames(tp) %in% col)], trsf))
    }
    
    data = to_ordinal(mtcars, c("carb"))
    
    splited = train_test_val_split(data)
    
    traindat = one_hot(splited$train, c("cyl", "gear"))
    valdat = one_hot(splited$validate, c("cyl", "gear"))
    testdat = one_hot(splited$test, c("cyl", "gear"))
    
    standardizer = standardization(traindat, c("mpg", "disp", "hp", "drat", "wt", "qsec"))
    
    stdtraindat = predict(standardizer, traindat)
    stdvaldat = predict(standardizer, valdat)
    stdtestdat = predict(standardizer, testdat)
    
    train <- stdtraindat
    test <- stdtestdat
    val <- stdvaldat
    y <- "vs"
    x <- setdiff(names(train), y)
    # For binary classification, response should be a factor
    train[, y] <- as.factor(train[, y])
    test[, y] <- as.factor(test[, y])
    val[, y] <- as.factor(val[, y])
    h2o.init(nthreads = -1)
    h2o.no_progress()
    train <- as.h2o(train)
    test <- as.h2o(test)
    val <- as.h2o(val)
    
    result <-
      new_model(
        train,
        test,
        val,
        y = "vs",
        model = model,
        metrics = metrics,svm.hyper_param=hyper_param,ntrees = ntrees,
        max_depth = max_depth,
        learn_rate = learn_rate
      )
    result
  })
  
  
  output$selected2 <- renderText({"Done!"
  })
  
}

# Run the app ----
shinyApp(ui = ui, server = server)










