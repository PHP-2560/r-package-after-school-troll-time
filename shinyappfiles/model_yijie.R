library(shiny)
library(caret)
library(dplyr)
library(h2o)

new_model <-
  function(train,
           test,
           y,
           model = "Xgboost (lighter)",nfolds = 10,hyper_param=1,max_depth=5,ntrees=30,learn_rate=0.2)
  {
    x <- setdiff(names(train), y)
    if (model == "Xgboost (lighter)") {
      my_model <- h2o.xgboost(
        x = x,
        y = y,
        training_frame = train,
        
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
        hyper_param = hyper_param,
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
        nfolds = nfolds,
        fold_assignment =  "Modulo",
        laplace = 0
      )
    }}



print_result<-function(mymodel,test,metrics,model){
    
    perf <- h2o.performance(mymodel,  test)
    auc <- round(as.numeric(h2o.auc(perf)),2)
    acc <- round(as.numeric(h2o.accuracy(perf, 0.5)),2)
    f1 <- round(as.numeric(h2o.F1(perf,0.5)),2)
    
    result <- c()
    if ( 'AUC' %in% metrics) {
      result <- c(result, paste("The AUC of the " , model, "is", "<font color='#7ED13E'>", auc,'</font>.'))
    }
    if ('Accuracy' %in% metrics) {
      result <-
        c(result,
          paste("The accuracy of the " , model, "is","<font color='#7ED13E'>", acc,'</font>.'))
    }
    if ('F1' %in% metrics) {
      result <-
        c(result,
          paste("The F1 score of the " ,model, "is", "<font color='#7ED13E'>", f1,'</font>.'))
    }
    return(result)
  }






