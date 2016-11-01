################################
## TRAIN MODELS
################################

scoreModels <- function(splitData, yColumn, trainFormula){
  

  trainDF = as.data.frame(splitData$trainData)
  
  
  ## Linear regression
  lrTrain = glm(trainFormula, data = trainDF, family="binomial")
  lrPredProbs = predict(lrTrain,splitData$x_test, type="response")
  lrPredLabels = rep("no", length(lrPredProbs))
  lrPredLabels[lrPredProbs > 0.5] = "yes"
  
  ## Tree model
  
  dtTrain = rpart(trainFormula, data = trainDF, method="class")
  dtPredProbs = predict(dtTrain,splitData$x_test, type="prob")[,"yes"]
  dtPredLabels = predict(dtTrain, splitData$x_test, type="class")  
  

  ## Random Forest
  rfTrain = randomForest(trainFormula, data = trainDF)
  rfPredProbs = predict(rfTrain,splitData$x_test, type="prob")[,"yes"]
  rfPredLabels = predict(rfTrain, splitData$x_test, type="class")
  
  
  ## Prob output
  labelsMatrix = cbind(lrLabel=lrPredLabels, dtLabel = dtPredLabels, rfLabel=rfPredLabels)
  probsMatrix = cbind(lrProb=lrPredProbs, dtProb = dtPredProbs, rfProb=rfPredProbs)
  
  as.data.frame(cbind(labelsMatrix,probsMatrix))
  
  }
  


################################
## ROC, LIFT CHART
################################

dataFromROCR <- function(preds,labels,modelName, meas, x.meas){
  
  # Returns data to make ROCR package plots in ggplot
  
  pred.obj <- prediction(predictions=preds,
                         labels=labels)
  # Get data for ROC curve
  perf.obj <- performance(pred.obj, measure=meas, x.measure=x.meas)

  roc.data <- data.frame(x=unlist(perf.obj@x.values),
                         y=unlist(perf.obj@y.values))
  roc.data$Model = modelName
  roc.data
}




rocData <- function(pred.s, true.y)
{
  # Calculates the roc curve from predictions

  cutoff <- Inf # start with all instances classified as negative
  tp <- fp <- 0
  tn <- sum(2-as.integer(true.y)) # all negative instances
  fn <- sum(as.integer(true.y)-1) # all positive instances
  rt <- data.frame()
  sord <- order(pred.s, decreasing=TRUE) # score ordering
  for (i in 1:length(sord))
  {
    if (pred.s[sord[i]] < cutoff)
    {
      rt <- rbind(rt, data.frame(tpr=tp/(tp+fn), fpr=fp/(fp+tn)))
      cutoff <- pred.s[sord[i]]
    }
    p <- as.integer(true.y[sord[i]])-1 # next positive classified as positive
    n <- 2-as.integer(true.y[sord[i]]) # next negative classified as positive
    tp <- tp+p
    fp <- fp+n
    tn <- tn-n
    fn <- fn-p
  }
  rt <- rbind(rt, data.frame(tpr=tp/(tp+fn), fpr=fp/(fp+tn)))
}

