library(caret)
library(dplyr)

train_test_split <- function(data, test_perc=0.2) {
  spec = c(train = 1-test_perc, test = test_perc)
  g = sample(cut(
    seq(nrow(data)), 
    nrow(data)*cumsum(c(0,spec)),
    labels = names(spec)
  ))   
  return(split(data, g))
}

one_hot <- function(data, col) {
  if(length(col)==0){return(data)}
  tp <- data
  for (name in col){
    tp[,name] = as.factor(tp[,name])
  }
  dmy <- caret::dummyVars(paste("~", paste(col, collapse="+"), sep = ""), data = tp)
  trsf <- data.frame(predict(dmy, newdata = tp, na.action = na.omit))
  return(cbind(tp[,!(colnames(tp) %in% col)], trsf))
}

to_ordinal <- function(data, col) {
  tp <- data
  for (name in col){
    tp[,name] = factor(tp[,name])
  }
  return(tp)
}

standardization <- function(data, col) {
  tp <- data   
  std_trained <- preProcess(data[col], method = c("center", "scale"), na.remove = TRUE)
  std_trained
  # return(cbind(tp[,!(colnames(tp) %in% col)], trsf))
}


