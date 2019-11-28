library(shiny)
library(caret)
library(dplyr)
library(h2o)

new_model <-
  function(train,
           test,
           val,
           y,
           model = "Xgboost (lighter)",
           nfolds = 10,
           metrics = 'AUC',svm.hyper_param=1,max_depth=5,ntrees=30,learn_rate=0.2)
  {
    x <- setdiff(names(train), y)
    if (model == "Xgboost (lighter)") {
      my_model <- h2o.xgboost(
        x = x,
        y = y,
        training_frame = train,
        validation_frame = val,
        distribution = "bernoulli",
        min_rows=2,
        ntrees = ntrees,
        max_depth = max_depth,
        learn_rate = learn_rate,
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
        ntrees = ntrees,
        max_depth = max_depth,
        learn_rate = learn_rate,
        min_rows=2,
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
        min_rows=2,
        ntrees = ntrees,
        max_depth = max_depth,
        learn_rate = learn_rate,
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
        hyper_param = svm.hyper_param,
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
