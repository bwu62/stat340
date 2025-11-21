library(shiny)
library(MASS)

ui = fluidPage(
  titlePanel("Pima logistic"),
  sidebarLayout(
    sidebarPanel(
      radioButtons("model","Which model",c("Null"="null","Glu"="glu","All"="all")),
      sliderInput("threshold","Prediction threshold",0,1,.5,.01),
      verbatimTextOutput("confusionMatrix"),
      verbatimTextOutput("statistics")
    ),
    mainPanel(plotOutput("rocPlot"))
  )
)

server = function(input, output) {
  
  library(MASS)
#  library(tidyverse)
#  library(pROC)
#  library(glue)
  
  if(is.factor(Pima.te$type)) Pima.te$type = ifelse(Pima.te$type=='Yes',1,0)
  model = list(null=glm(type~1, data=Pima.te, family="binomial"),
               glu=glm(type~glu, data=Pima.te, family="binomial"),
               all=glm(type~npreg+bp+skin+bmi+ped+age, data=Pima.te, family="binomial"))
  
  observeEvent(c(input$threshold,input$model),{
    predictions = predict.glm(model[[input$model]], newdata=Pima.te, type="response")
    conf.tab = table(
      Pima.te$type,
      factor(predictions >= input$threshold, levels=c(F,T)))
    colnames(conf.tab) = c("Predict 0","Predict 1")
    rownames(conf.tab) = c("No diabetes","Yes diabetes")
    
    output$confusionMatrix = renderPrint({
      conf.tab
    })
    
    output$statistics = renderPrint({
      TP = conf.tab[2,2]
      TN = conf.tab[1,1]
      FP = conf.tab[1,2]
      FN = conf.tab[2,1]
      cat("           Accuracy:",round((TP+TN)/(TP+TN+FP+FN),3))
      cat("\n        Sensitivity:",round(TP/(TP+FN),3))
      cat("\n        Specificity:",round(TN/(TN+FP),3))
      cat("\nFalse Positive Rate:", round(1- TN/(TN+FP),3))
      cat("\n     Precision(PPV):",round(TP/(TP+FP),3))
      cat("\n                NPV:", round(TN/(TN+FN),3))
    })
    
    output$rocPlot = renderPlot({
      predictions = predict.glm(model[[input$model]], newdata=Pima.te, type="response")
(      thresh=c(0, sort(unique(predictions)),1)
)      
      
      FPR <- thresh; TPR <- thresh
      for(i in 1:length(thresh)){
        conf.tab.thresh = table(
          Pima.te$type,
          factor(predictions >= thresh[i], levels=c(F,T)))
        FPR[i] <- conf.tab.thresh[1,2] / sum(conf.tab.thresh[1,])
        TPR[i] <- conf.tab.thresh[2,2] / sum(conf.tab.thresh[2,])
      }
      predAUC = sum(-diff(FPR) * (TPR[-1]+TPR[-length(TPR)])/2)
      pt = data.frame(x=(conf.tab[1,2]/sum(conf.tab[1,])),y=(conf.tab[2,2]/sum(conf.tab[2,])))
      par(pty="s")
      plot(FPR,TPR, type="l",
           xlab="false positive rate",
           ylab="true positive rate",
           main=paste0("Model: ",input$model,", AUC=",round(predAUC,4)))
      points(pt$x, pt$y, col="red", pch=16, cex=2)
    })
  })
}

shinyApp(ui = ui, server = server)
