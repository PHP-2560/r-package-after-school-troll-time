library(h2o)
library(tidyverse)
h2o.init()

data<-mtcars
y<-'vs'
data[,y]
length(unique(data[,y])) 



variable_importance <- function(data, plot_choice,y,probelm='classification'){
  #data<-mtcars
  data_hf <- as.h2o(data)
  #y<-'vs'
  #plot_choice<-'gbm'
  x <- setdiff(names(data_hf), y)
  # By default we use deep learning to plot variable importance
  
  if (plot_choice == 'dnn'){
    mod <- h2o.deeplearning(x = x, y = y, training_frame = data_hf,
                            variable_importances = TRUE)
    h2o.varimp_plot(mod)}
  
  
  # We then use XGBoost; we decide based on the class of the response variable
  if (plot_choice == 'gbm') {
    if (probelm=='regression'){
      mod_gbm_numeric <- h2o.gbm(x = x, y = y, training_frame = data_hf, distribution = "gaussian")
      h2o.varimp_plot(mod_gbm_numeric)
    }
    if (probelm=='classification'){
      data_hf[, y] <- as.factor(data_hf[, y])
      if (length(unique(data[,y])) == 2) { 
        factor_dist = 'bernoulli'
      } else {
        factor_dist = 'multinomial'
      }
      mod_gbm_factor <- h2o.gbm(x = x, y = y, training_frame = data_hf, distribution = factor_dist)
      h2o.varimp_plot(mod_gbm_factor)
    }
  }
}