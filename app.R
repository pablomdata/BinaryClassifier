## app.R ##
#setwd("~/R/Apps/BinaryClassifierDashboard")

library(ineq)
library(ggplot2)
library(rpart)
library(GGally)
library(shiny)
library(shinydashboard)
library(ggvis)
library(ROCR)
library(caret)
library(randomForest)
library(ggfortify)
source("preprocessing.R")
source("main.R")


sidebar <-  dashboardSidebar(
  sidebarMenu(
    menuItem("Dashboard", icon = icon("dashboard"), tabName = "dashboard"),
    menuItem("Load train data", icon = icon("database"), tabName = "load"),
    menuItem("Explore train data", icon = icon("th"), tabName = "explore"),
    menuItem("Train and compare models", icon = icon("cogs"), tabName = "train"),
    menuItem("Sample models", icon = icon("bar-chart-o"), tabName = "indiv"),
    menuItem("Score new data", icon =icon("sort-amount-desc"), tabName ="score"),
    menuItem("Generate report", icon = icon("pie-chart"), tabName = "report", badgeLabel = "upcoming", badgeColor = "green")
  )
)
  
body <-  dashboardBody(
  

  tabItems(
    
    tabItem(
      tabName = "dashboard",
      h2("Main dashboard"),
      
      box("Binary classifier app")
    ),
    
    tabItem(
      tabName = "load",
      h2("Select training data source"),
      
      box(
        fileInput('datafile', 'Choose train file', 
                  accept = c('text/csv', 'text/comma-separated-values'
                                                              ,'text/plain','.csv')),
        
        checkboxInput('header', 'Header', TRUE),
        radioButtons('sep', 'Separator',
                     c(Comma=',',
                       Semicolon=';',
                       Tab='\t'),
                     ','),
        radioButtons('quote', 'Quote',
                     c(None='',
                       'Double Quote'='"',
                       'Single Quote'="'"),
                     '"')
      ), 
      
      box(
        title = "Choose response variable",
        htmlOutput("selectY")
      )
      
      
    ),

    tabItem(
      tabName = "explore",
      h2("Explore train data"),
      
   
      box(
        title = "Pair plots", htmlOutput("selectXvars"), collapsed = T, collapsible = T,
        plotOutput('ggpairs')
      ),
      
        
      box(title = "Inspect individual distribution", 
          htmlOutput('varName'), plotOutput('IndividualPlot'), collapsed = T, collapsible = T),
      
      
      box(title = "Feature importance summary", verbatimTextOutput('VarImpList'), collapsed = T, collapsible = T),
      
      box(title='Data Preview',tableOutput('contents'), collapsed = T, collapsible = T)  
      ),
    
        
    tabItem(
      tabName = "train",
      h2("Train model"),
      
      fluidRow(
     
      
      tabBox(title = 'Model Comparison', id = 'compare',
             tabPanel('ROC', 'ROC curves',plotOutput("rocPlots")),
             tabPanel('Lift', 'Lift curves',plotOutput("liftPlots")),
             tabPanel('Prec. vs Rec.', 'Precision vs Recall',plotOutput("precvsrecPlots")),
             tabPanel('Lorenz', 'Lorenz curves',plotOutput("lorenzPlots"))
             ), 
      
      tabBox(title = "Confusion Matrix", id="ConfMats", 
             tabPanel('Log. Reg.', 'Summary', verbatimTextOutput("lrConfMat")), 
             tabPanel('Decision Tree', 'Summary', verbatimTextOutput("dtConfMat")), 
             tabPanel('Random Forest', 'Summary', verbatimTextOutput("rfConfMat"))
             ),
      
      box(title="Model metrics", collapsible = T, verbatimTextOutput("metricsSummary")), 
      box(title = "Select variables to train the model", 
          htmlOutput('IndepVars'), collapsible = T , actionButton('trainButton', 'Train model'))
      )
    ),
    
    
    
    tabItem(
      tabName = "indiv",
      h2("Individual model metrics"),
      box(title = "Logistic regression", collapsed = T, collapsible = T,
             tableOutput('glmPlots')),
      box(title = "Decision Tree", collapsed = T, collapsible = T,
          plotOutput('dtPlots'))
    ),
  
    
    tabItem(
      tabName = "score",
      h2("Upload new data to score"),
      
      box(
        fileInput('newfile', 'Choose train file', 
                  accept = c('text/csv', 'text/comma-separated-values'
                             ,'text/plain','.csv')),
        
        checkboxInput('header', 'Header', TRUE),
        radioButtons('sep', 'Separator',
                     c(Comma=',',
                       Semicolon=';',
                       Tab='\t'),
                     ','),
        radioButtons('quote', 'Quote',
                     c(None='',
                       'Double Quote'='"',
                       'Single Quote'="'"),
                     '"')
      ), 
      
      box(
        title = "Choose model",
        radioButtons('selectModel', 'Model',
                     c(None='',
                       'Logistic Regression'='lr',
                       'Random Forest'="dt",
                       'Decision Tree'="rf"),
                     '"'),
        actionButton('scoreButton', 'Score data')
      ),
      box(title="Preview score data",verbatimTextOutput('previewScores'),
          downloadButton('downloadData', 'Download'), collapsible = T),
      box(title="Summary of the selected model",verbatimTextOutput('modelSummary'), collapsible = T)
    ),
    
    
    tabItem(
      tabName = "report",
      h2("Create a report")
    )
    

    
  )

                  
)
  



ui <- dashboardPage(skin="yellow",
  dashboardHeader(title = "Binary Classifier"),
  sidebar,
  body

  )


## SERVER SIDE
server <- function(input, output) { 
  theData = reactive({
    infile <- input$datafile
    if (is.null(infile))
      return(NULL)
    d<-read.csv(infile$datapath, header = input$header,
                sep = input$sep, quote = input$quote)
    d
  })
  

  

  
  labelColumn = reactive({input$y})
  
  Xvars = reactive({ choices()[choices()!=input$y] })
  
  output$contents = renderTable({head(theData(), n=5)})
  
  
  #########################    
  # Exploration
  #########################  
  
  

  
  choices = reactive({
    if(is.null(theData)){
      return(c("y","age","job"))
    }
    return(names(theData()))
  })
  
  output$selectY <- renderUI({selectInput('y','Dependent variable','y', choices=choices())})
  
  pairplotChoices <- reactive({checkboxGroupInput('chosenPlot','Independent variables', 
                                                  inline = T, choices=Xvars())})
  
  output$selectXvars <- renderUI({pairplotChoices()})
  
  output$varName <- renderUI({selectInput('chooseVarName','Variable to inspect', choices=choices())})
  
  output$IndividualPlot <- renderPlot({ggplot(data=theData(),
                                                        aes_string(x=input$chooseVarName,
                                                                   fill = labelColumn()))+geom_bar()})
  
  output$ggpairs <- renderPlot({ggpairs(theData(),
                                        columns = which(names(theData())%in%input$chosenPlot), 
                                        colour = labelColumn())})
    
  
  
  
  regFormula = reactive({paste(labelColumn(),'~','.')})
  
  
  
  ## Variable importance
  output$VarImpList = renderPrint({
    trainDF = isolate({theData()})
    trainFormula = as.formula(isolate({regFormula()}))
    rfTrain = randomForest(trainFormula, data = trainDF)
    importance(rfTrain)
    
  })
    
  
  #########################    
  # Training and validation
  #########################  


  
  regFormula = reactive({paste(labelColumn(),'~','.')})
  

  chooseIndepVars <- reactive({checkboxGroupInput('IndepVars','Independent variables', inline = T,
                                                  choices=Xvars(), selected = Xvars())})
  
  output$IndepVars <- renderUI({chooseIndepVars()})
  
  
  splitData = reactive({
    originalData = isolate({theData()})
    chosenIndepVars = isolate({input$IndepVars})
    yColumn = isolate({labelColumn()})
    rawData = originalData[,names(originalData)%in%c(chosenIndepVars,yColumn)]
    trainTestSplit(rawData,yColumn)
  })
  
  
  
  trainDF = reactive({as.data.frame(splitData()$trainData)})
  
  
  ## Train the models
  lrTrain = reactive({
    glm(regFormula(), data = trainDF(), family="binomial")
  })
  
  dtTrain = reactive({
    rpart(regFormula(), data = trainDF(), method="class")
    })
  
  rfTrain = reactive({
    randomForest(formula = as.formula(regFormula()), data = trainDF())
  })
  
  
  
  
  ## Predicted probs
  
  
  scoreDF = reactive({splitData()$x_test})
  trueLabels = reactive({splitData()$y_test})
  
  
  
  lrPredProbs = reactive({
    predict(lrTrain(),scoreDF(), type="response")
    })
  
  dtPredProbs = reactive({
    predict(dtTrain(),scoreDF(), type="prob")[,"yes"]	
  })
  
  rfPredProbs = reactive({
    predict(rfTrain(),scoreDF(), type="prob")[,"yes"]
  })
  
  
  
  ## Predicted labels
  
  lrPredLabels = reactive({
    preds = rep("no", length(lrPredProbs()))
    preds[lrPredProbs() > 0.5] = "yes"
    preds
  })
    
  
  dtPredLabels = reactive({
    preds = predict(dtTrain(), scoreDF(), type="class") 
    preds
  })
  
  rfPredLabels = reactive({
    predict(rfTrain(), scoreDF()) 
  })
  
    ## Display summary of the trained models
  
  output$lrConfMat <- renderPrint({
    if(input$trainButton==0)
    {
      return()
    }
    confusionMatrix(lrPredLabels(),trueLabels())
  })
  
  
  output$dtConfMat <- renderPrint({
    if(input$trainButton==0)
    {
      return()
    }
    confusionMatrix(dtPredLabels(),trueLabels())
  })
  
  
  output$rfConfMat <- renderPrint({
    if(input$trainButton==0)
    {
      return()
    }
    confusionMatrix(rfPredLabels(),trueLabels())  
    })
  
  
  output$metricsSummary = renderPrint({
    if(input$trainButton==0)
    {
      return()
    }
    ## AUC    
    
    pred.tmp <- prediction(predictions=lrPredProbs(),labels=trueLabels())
    auc.tmp <- performance(pred.tmp, "auc")
    aucLR <- as.numeric(auc.tmp@y.values)

    
    pred.tmp <- prediction(predictions=dtPredProbs(),labels=trueLabels())
    auc.tmp <- performance(pred.tmp, "auc")
    aucDT <- as.numeric(auc.tmp@y.values)

    pred.tmp <- prediction(predictions=rfPredProbs(),labels=trueLabels())
    auc.tmp <- performance(pred.tmp, "auc")
    aucRF <- as.numeric(auc.tmp@y.values)
    
    ## Gini index
    giniLR<-ineq(as.factor(lrPredLabels()),type="Gini")
    giniDT <- ineq(dtPredLabels(),type="Gini")
    giniRF <- ineq(rfPredLabels(),type="Gini")
    
    df<-data.frame(AUC=c(aucLR,aucDT,aucRF), Gini=c(giniLR,giniDT,giniRF))
    row.names(df)<-c("Logistic Regression", "Decision Trees", "Random Forest")
    df
    })

  scoreTrained = reactive({
    trainFormula = as.formula(isolate({regFormula()}))
    Label=isolate({trueLabels()})
    scores = cbind(scoreModels(splitData(),yColumn(), trainFormula),Label)
    scores
  })    
  
  
  ##################################################    
  # Model performance visualizations
  ##################################################

  output$rocPlots <- renderPlot({
    
    if(input$trainButton==0)
    {
      return()
    }
    
    scores = isolate({scoreTrained()})  
    
    meas="tpr"
    x.meas="fpr"

    lrData = dataFromROCR(as.numeric(scores$lrProb), scores$Label, "Logistic Regression", meas = meas, x.meas = x.meas)
    dtData = dataFromROCR(as.numeric(scores$dtProb), scores$Label, "Decision Tree", meas = meas, x.meas = x.meas)
    rfData = dataFromROCR(as.numeric(scores$rfProb), scores$Label, "Random Forest", meas = meas, x.meas = x.meas)
    rocrScores =as.data.frame(rbind(lrData, dtData, rfData)) 
    ggplot(rocrScores,aes(x,y, color=Model), ymax=1.0)+geom_line()+ylim(0,1) + xlab("False positive rate")+ylab("True positive rate")
    
    
  })
  
  
  output$liftPlots <- renderPlot({
    
    if(input$trainButton==0)
    {
      return()
    }
    
    scores = isolate({scoreTrained()})  
    
    meas="lift"
    x.meas="rpp"
    
    lrData = dataFromROCR(as.numeric(scores$lrProb), scores$Label, "Logistic Regression", meas = meas, x.meas = x.meas)
    dtData = dataFromROCR(as.numeric(scores$dtProb), scores$Label, "Decision Tree", meas = meas, x.meas = x.meas)
    rfData = dataFromROCR(as.numeric(scores$rfProb), scores$Label, "Random Forest", meas = meas, x.meas = x.meas)
    rocrScores =as.data.frame(rbind(lrData, dtData, rfData)) 
    ggplot(rocrScores,aes(x,y, color=Model))+geom_line()+ xlab("Rate of positive predictions")+ylab("Lift")+geom_abline(aes(slope=0,intercept=1))
    
    
  })
  

  
  
  
  
  output$precvsrecPlots <- renderPlot({
    
    if(input$trainButton==0)
    {
      return()
    }
    
    scores = isolate({scoreTrained()})  
    meas="prec"
    x.meas="rec"
    lrData = dataFromROCR(as.numeric(scores$lrProb), scores$Label, "Logistic Regression", meas = meas, x.meas = x.meas)
    dtData = dataFromROCR(as.numeric(scores$dtProb), scores$Label, "Decision Tree", meas = meas, x.meas = x.meas)
    rfData = dataFromROCR(as.numeric(scores$rfProb), scores$Label, "Random Forest", meas = meas, x.meas = x.meas)
    rocrScores =as.data.frame(rbind(lrData, dtData, rfData)) 
    ggplot(rocrScores,aes(x,y, color=Model), ymax=1.0)+geom_line()+ylim(0,1) + xlab("Recall")+ylab("Precision")
    
    
  })
  
  
  output$lorenzPlots <- renderPlot({
    if(input$trainButton==0)
    {
      return()
    }
    
    
    lrLorenz = Lc(as.factor(lrPredLabels()))
    dtLorenz = Lc(dtPredLabels())
    rfLorenz = Lc(rfPredLabels())
    
    lrLorenz$Model=rep("Logistic Regression",length(lrLorenz$p))
    dtLorenz$Model =rep("Decision Tree",length(dtLorenz$p))
    rfLorenz$Model =rep("Random Forest",length(rfLorenz$p))
    
    
    lorenzScores = data.frame(p=c(lrLorenz$p,dtLorenz$p,rfLorenz$p),
                              L=c(lrLorenz$L,dtLorenz$L,rfLorenz$L),
                              Model = c(lrLorenz$Model,dtLorenz$Model,rfLorenz$Model))
    
    ggplot(lorenzScores,aes(p,L, color=Model), ymax=1.0)+geom_line()+xlab("Percentage of observations")+ylab("Percentage of events")
  })
  
  
  
  output$glmPlots = renderTable({
    if(input$trainButton==0)
    {
      return()
    }
    summary(lrTrain())
  })
  
  
  output$dtPlots = renderPlot({
    if(input$trainButton==0)
    {
      return()
    }
    plot(dtTrain(), uniform=TRUE, 
         main="Classification Tree")
    text(dtTrain(), use.n=TRUE, all=TRUE, cex=.8)  })
  
  #########################    
  # Model Selection
  #########################  
  
  
  newData <-reactive({
    infile <- input$newfile
    if (is.null(infile))
      return(NULL)
    d<-read.csv(infile$datapath, header = input$header,
                sep = input$sep, quote = input$quote)
    d
  })
  
  
  scoreSelectedModel = reactive({
    
    originalData = isolate({theData()})
    chosenIndepVars = isolate({input$IndepVars})
    
    
    newDF = isolate({newData()})
    scoreDF = newDF[,names(originalData)%in%chosenIndepVars]
    

    if(is.null(input$selectModel)){
      return("Upload and score data first")
    }
    else if(input$selectModel=="lr"){
      lrPredProbs = predict(lrTrain(),scoreDF, type="response")
      preds = rep("no", length(lrPredProbs))
      preds[lrPredProbs > 0.5] = "yes"
      
    }
    else if(input$selectModel=="dt"){
      preds = predict(dtTrain(), scoreDF, type="class") 
    }
    else if(input$selectModel=="rf"){
      preds = predict(rfTrain(), scoreDF, type="class")
      
    }
    preds
  })    
  
  
  
  output$modelSummary<- renderPrint({
    
    originalData = isolate({theData()})
    chosenIndepVars = isolate({input$IndepVars})
    yColumn = isolate({labelColumn()})
    trainFormula = as.formula(isolate({regFormula()}))
    newDF = isolate({newData()})
    
    
    
    rawData = originalData[,names(originalData)%in%c(chosenIndepVars,yColumn)]
    scoreDF = newDF[,names(originalData)%in%chosenIndepVars]
    
    splitData = trainTestSplit(rawData,yColumn)
    
    trainDF = as.data.frame(splitData$trainData)
    if(is.null(input$selectModel)){
      return()
    }
    else if(input$selectModel=="lr"){
      lrTrain = glm(trainFormula, data = trainDF, family="binomial")
      sum = summary(lrTrain)
      
    }
    else if(input$selectModel=="dt"){
      dtTrain = rpart(trainFormula, data = trainDF, method="class")
      summary(dtTrain)
     }
    
    else if(input$selectModel=="rf"){
      rfTrain = randomForest(trainFormula, data = trainDF)
      sum = summary(rfTrain)
      
    }
    sum

  })
  
  output$previewScores<-renderPrint({
    if(input$scoreButton==0)
    {
      return()
    }
    head(scoreSelectedModel())
       })
  
  
  output$downloadData <- downloadHandler(
     filename = function() {
       paste('data-', Sys.Date(), '.csv', sep='')
     },
     content = function(con) {
       write.csv(scoreSelectedModel(), con)
     }
   )
  
  
  }

shinyApp(ui, server)