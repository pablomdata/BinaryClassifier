
################################
##  READING AND DATA CLEANING
################################

trainTestSplit <- function(rawData, labelColumn){
  
  set.seed(123)
  sample_size <- floor(0.75*nrow(rawData))
  train_idxs <- sample(seq_len(nrow(rawData)),size = sample_size)
  
  if(missing(labelColumn)){
    trainData <- rawData[train_idxs,]
    testData <- rawData[-train_idxs,]
    invisible(list(trainData=trainData, testData=testData))
  }
  else{
    
    x_train <- rawData[train_idxs,names(rawData)!=labelColumn]
    y_train <- rawData[train_idxs,names(rawData)==labelColumn]
    x_test <- rawData[-train_idxs,names(rawData)!=labelColumn]
    y_test <- rawData[-train_idxs,names(rawData)==labelColumn]
    
    trainData <- rawData[train_idxs,]
    invisible(list(trainData=trainData, x_test=x_test, y_test=y_test))
  }
}