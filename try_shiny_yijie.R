library(shiny)
library(caret)
library(dplyr)
library(h2o)
library(devtools)
#install_github("h2oai/h2o-3/h2o-r/ensemble/h2oEnsemble-package")

# Define UI ----
ui <- fluidPage(
  titlePanel("Classification Models"),
  sidebarPanel(
    fluidRow(column(
      2,
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
      )
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
    
    actionButton("update", "Select")
  ),
  mainPanel(#textOutput("selected"),
    textOutput("result"),
    textOutput("selected2"))
)



# Define server logic ----
server <- function(input, output) {
  # Define a reactive expression for the document term matrix
  terms <- reactive({
    # Change when the "update" button is pressed...
    input$update
    # ...but not for anything else
    isolate({
      withProgress({
        setProgress(message = "Processing corpus...")
        getTermMatrix(input$model, input$metrics)
      })
    })
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
    
    
    
    new_model <-
      function(train,
               test,
               val,
               y,
               model = "Xgboost (lighter)",
               nfolds = 10,
               metrics = 'AUC')
      {
        x <- setdiff(names(train), y)
        if (model == "Xgboost (lighter)") {
          my_model <- h2o.xgboost(
            x = x,
            y = y,
            training_frame = train,
            validation_frame = val,
            distribution = "bernoulli",
            ntrees = 100,
            max_depth = 5,
            min_rows = 2,
            learn_rate = 0.2,
            nfolds = nfolds,
            fold_assignment = "Modulo",
            keep_cross_validation_predictions = TRUE,
            seed = 1
          )
        }
        else if (model == 'Xgboost (deeper)') {
          my_model <- h2o.xgboost(
            x = x,
            y = y,
            training_frame = train,
            validation_frame = val,
            distribution = "bernoulli",
            ntrees = 100,
            max_depth = 5,
            min_rows = 2,
            learn_rate = 0.2,
            sample_rate = 0.7,
            col_sample_rate = 0.9,
            nfolds = nfolds,
            fold_assignment = "Modulo",
            keep_cross_validation_predictions = TRUE,
            seed = 1
          )
        }
        else if (model == 'Gradient Boost') {
          my_model <- h2o.gbm(
            x = x,
            y = y,
            training_frame = train,
            validation_frame = val,
            distribution = "bernoulli",
            max_depth = 3,
            min_rows = 2,
            learn_rate = 0.2,
            nfolds = nfolds,
            fold_assignment = "Modulo",
            keep_cross_validation_predictions = TRUE,
            seed = 1
          )
          
        }
        else if (model == 'Random Forest') {
          my_model <- h2o.randomForest(
            x = x,
            y = y,
            training_frame = train,
            validation_frame = val,
            nfolds = nfolds,
            fold_assignment = "Modulo",
            keep_cross_validation_predictions = TRUE,
            seed = 1
          )
          
        }
        
        else if (model == 'SVM') {
          my_model <- h2o.psvm(
            x = x,
            y = y,
            training_frame = train,
            validation_frame = val,
            hyper_param = 1,
            kernel_type = c("gaussian"),
            gamma = -1,
            max_iterations = 200,
            seed = 1
          )
        }
        
        else if (model == 'Naive Bayes') {
          my_model <- h2o.naiveBayes(
            x = x,
            y = y,
            training_frame = train,
            validation_frame = val,
            nfolds = nfolds,
            fold_assignment =  "Modulo",
            laplace = 0
          )
        }
        
        perf <- h2o.performance(my_model, newdata = test)
        auc <- round(as.numeric(h2o.auc(perf)),2)
        acc <- round(as.numeric(h2o.accuracy(perf, 0.5)),2)
        f1 <- round(as.numeric(h2o.F1(perf,0.5)),2)
        
        result <- c()
        if ( 'AUC' %in% metrics) {
          result <- c(result, paste("\nThe AUC of the " , input$model, "is", auc,".\n"))
        }
        if ('Accuracy' %in% metrics) {
          result <-
            c(result,
              paste("\nThe accuracy of the " , input$model, "is", acc,".\n"))
        }
        if ('F1' %in% metrics) {
          result <-
            c(result,
              paste("\nThe F1 score of the " , input$model, "is", f1,".\n"))
        }
        return(result)
      }
    
    
    result <-
      new_model(
        train,
        test,
        val,
        y = "vs",
        model = input$model,
        metrics = input$metrics
      )
    result
  })
  
  
  output$selected2 <- renderText({"Done!"
  })
  
}

# Run the app ----
shinyApp(ui = ui, server = server)










