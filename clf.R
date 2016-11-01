library('ROCR')
library('caret')




################################
##  REPORTING
################################
decisionBoundary <-function(model,x_test,y_test){
  x_test<-as.data.frame(x_test)
  predLogProbs <- log(predict(model,x_test,type="prob"))
  predLabel <- predict(model,x_test,type="raw")
  preds<-cbind(predLogProbs,predLabel)
  outPlot <- ggplot(preds, aes(yes, no)) + geom_point(aes(colour = factor(predLabel)), position = "jitter") + geom_abline(intercept=0, slope=1)
  invisible(outPlot)
  
}







generatePlots <- function(evalResults,lbFit){

  
  liftData = lift(Class~GBM+SVM+LB, data = evalResults)
  
  liftPlot <- plot(liftData, values = 60, auto.key = list(columns = 3, lines = TRUE, points = FALSE))

  pred <- prediction(evalResults$LR,evalResults$Class)
  
  auc <- performance(pred, measure = "auc")
  auc <- auc@y.values[[1]]
  
  
  roc <- performance(pred,measure="tpr",x.measure="fpr")  
  
  
  
  roc.data <- data.frame(fpr=unlist(roc@x.values),
                         tpr=unlist(roc@y.values),
                         model="GLM")
  
  rocPlot <- ggplot(roc.data, aes(x=fpr, ymin=0, ymax=tpr)) +
                    geom_ribbon(alpha=0.2) +
                    geom_line(aes(y=tpr)) +
                    ggtitle(paste0("ROC Curve w/ AUC=", auc))
                  
  
    
  invisible(liftPlot=liftPlot, rocPlot=rocPlot))

}




comparePreds <- function(preds){
  
  # Given a prediction table from the four models, 
  # return the comparative histograms per class
  # Input data has to be labeled (no class probabilities)
  
  par(mfrow=c(2,4))
  # Fraction
  counts_gbm<-table(preds$GBM)
  barplot(counts_gbm/sum(counts_gbm), main="GBM", col="gray")
  
  counts_svm<-table(preds$SVM)
  barplot(counts_svm/sum(counts_svm), main = "SVM", col = "pink")
  
  counts_lb<-table(preds$LB)
  barplot(counts_lb/sum(counts_lb), main = "Logistic Boost Reg.", col="violet")
  
  counts_lr<-table(preds$LR)
  barplot(counts_lr/sum(counts_lr), main = "Logistic Reg.", col="violet")
  
  # Total
  barplot(counts_gbm, main="GBM", col="gray")
  barplot(counts_svm, main = "SVM", col = "pink")
  barplot(counts_lb, main = "Logistic Boost Reg.", col="violet")
  barplot(counts_lr, main = "Logistic Reg.", col="violet")
  
  
}


################################
##  SCORING
################################

scoreNewData<-function(x_new,model){
  x_new<-as.data.frame(x_new)
  preds <- predict(model,x_new,type="raw")
  invisible(preds)
}




################################
################################
################################
## Main program: For offline evaluation
################################
################################
################################
#library(caret)


#rawData <- read.csv("~/R/Apps/BinaryClassifierDashboard/data/bank.csv", sep=";")
#newData <- read.csv("~/R/Apps/BinaryClassifierDashboard/data/new.csv", sep=";")


#labelColumn <- "y"

#d<-trainTestSplit(rawData,labelColumn)
#m<-trainModels(as.data.frame(d$trainData),labelColumn)
#e<-generateProbs(m$gbmFit,m$svmFit,m$lbFit,m$lrFit,d$x_test,d$y_test)

#l<-generateLabels(m$gbmFit,m$svmFit,m$lbFit, m$lrFit, d$x_test,d$y_test, threshold=0.5)
#p<-generatePlots(e,m$lrFit)
#g<-generateProbs(m$gbmFit,m$svmFit,m$lbFit,m$lrFit,d$x_test,d$y_test)

#comp <- comparePreds(l)
#s<-scoreNewData(newData,m$lrFit)


