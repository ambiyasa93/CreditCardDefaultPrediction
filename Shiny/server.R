library(shiny)
library(shinyWidgets)
library(plotly)
library(DT)
library(data.table)
library(caret)
library(e1071)
library(party)
source("ui.R")
source("chart.R")

#virtualenv_create(envname = "python_environment", python= "python3")
#virtualenv_install("python_environment", packages = c('keras', 'pandas','numpy','scipy','scikit-learn', 'tensorflow'))
#reticulate::use_virtualenv("python_environment", required = TRUE)


shinyServer(function(input, output) {
  observeEvent(input$navibar,{
    if(input$navibar == "Code"){
      js$browseURL("http://rpubs.com/ambiyasa/512394")
    }
  })
  
  #------------------------Var Imp
  output$plot1 <- renderPlotly(ggplotly(plot1(input$slct1[1]),tooltip = c("text")) %>% 
                                 config(displayModeBar = F))
  
  output$plot2 <- renderPlotly(ggplotly(plot2(input$slct2[1]),tooltip = c("text")) %>% 
                                 config(displayModeBar = F))
  
  #--------------------------------------New Chart Based on File
  ROCxgb2 <- reactive({
    cek <- input$slct100
    inFile <- input$file1
    if(is.null(inFile)||is.na(inFile)){
      cctest <- fread("dataset/test.csv", na.strings = c("", "NA"))
    }
    else{
      cctest <- fread(inFile$datapath, na.strings = c("", "NA"))
    }
      cctest <- data.frame(apply(cctest, MARGIN = 2, FUN = as.numeric))
      cctest$PAY_0 <- as.factor(cctest$PAY_0)
      cctest$PAY_2 <- as.factor(cctest$PAY_2)
      cctest$PAY_3 <- as.factor(cctest$PAY_3)
      cctest$PAY_4 <- as.factor(cctest$PAY_4)
      cctest$PAY_5 <- as.factor(cctest$PAY_5)
      cctest$PAY_6 <- as.factor(cctest$PAY_6)
      cctest$EDUCATION <- as.factor(cctest$EDUCATION)
      cctest$MARRIAGE <- as.factor(cctest$MARRIAGE)
      cctest$SEX <- as.factor(cctest$SEX)
      cc_test <- cctest[,-1]
      levels(cc_test$EDUCATION) <- factor(c("others1","graduate","university","high school","others2","others3","others4"))
      levels(cc_test$MARRIAGE) <- factor(c("married","single","divorce","others"))
      levels(cc_test$SEX) <- factor(c("male","female"))
      levels(cc_test$default.payment.next.month) <- factor(c("no","yes"))
      dummy <- dummyVars("~ EDUCATION+MARRIAGE+SEX+PAY_0+PAY_2+PAY_3+PAY_4+PAY_5+PAY_6", data = ccdef)
      dummy <- data.frame(predict(dummy, newdata = cc_test))
      cc_test <- cbind(cc_test,dummy)
      cc_test <- cc_test[,-c(2:4,6:11)]
      cctesty <- cc_test[,15]
      cctestx <- scale(cc_test[,-15])
      cctest2x <- cctestx[,-c(49,64,71,81)]
      if (cek==1) {
      xgbpredict <- predict(xgb, cctestx, type = "prob")
      l<-1
      for (i in seq(from=0.01, to=1, by=0.01)) {
        xgbpredict3 <-  ifelse(xgbpredict[,2]>i,1,0)
        xgbpredict3 <- as.factor(xgbpredict3)
        cctesty <- as.factor(cctesty)
        levels(xgbpredict3) <- factor(c("no","yes"))
        levels(cctesty) <- factor(c("no","yes"))
        confmatxgb <- confusionMatrix(xgbpredict3,cctesty,positive='yes')
        ROCxgb[l,1] <- confmatxgb$byClass[1]#sens
        ROCxgb[l,2] <- confmatxgb$byClass[2]#spec
        ROCxgb[l,3] <- confmatxgb$byClass[11]#bal acc
        ROCxgb[l,4] <- i #cutoff
        ROCxgb[l,5] <- "XGBtree"
        l<-l+1
      }
      ROCxgb2 <- ROCxgb
      }
      
      
      else if (cek == 2) {
        cc_test$AGE <- as.integer(cc_test$AGE)
        dctpredict <- predict(dct$finalModel, cc_test[,-15], type = "prob")
        dctpredict <- unlist(dctpredict)
        dctpredict2 <- matrix(0L, nrow = (length(dctpredict)/2), ncol = 2) 
        for (i in 1:length(dctpredict)){
          if (i %% 2 == 0){
            dctpredict2[(i/2),2] <- dctpredict[i]
          }
          else if (i %% 2 != 0){
            dctpredict2[(ceiling(i/2)),1] <- dctpredict[i]
          }
        }
        k<-1
        for (i in seq(from=0.01, to=1, by=0.01)) {
          dctpredict3 <-  ifelse(dctpredict2[,2]>i,1,0)
          dctpredict3 <- as.factor(dctpredict3)
          cctesty <- as.factor(cctesty)
          levels(dctpredict3) <- factor(c("no","yes"))
          levels(cctesty) <- factor(c("no","yes"))
          confmatdct<- confusionMatrix(dctpredict3,cctesty,positive='yes')
          ROCdct[k,1] <- confmatdct$byClass[1]#sens
          ROCdct[k,2] <- confmatdct$byClass[2]#spec
          ROCdct[k,3] <- confmatdct$byClass[11]#bal acc
          ROCdct[k,4] <- i #cutoff
          ROCdct[k,5] <- "Decision Tree"
          k<-k+1
        }
        
        ROCdct2 <- ROCdct
      }
      
      else if (cek == 3){
        predict <- model %>%  predict_proba(cctest2x)
        predict <- as.data.frame(predict)
        m<-1
        for (i in seq(from=0.01, to=1, by=0.01)) {
          predict2 <- ifelse(predict[,2]>i,1,0)
          predict2 <- as.factor(predict2)
          cctesty <- as.factor(cctesty)
          levels(predict2) <- factor(c("no","yes"))
          levels(cctesty) <- factor(c("no","yes"))
          confmatnn <- confusionMatrix(predict2,cctesty, positive = 'yes')
          ROCnn[m,1] <- confmatnn$byClass[1]#sens
          ROCnn[m,2] <- confmatnn$byClass[2]#spec
          ROCnn[m,3] <- confmatnn$byClass[11]#bal acc
          ROCnn[m,4] <- i #cutoff
          ROCnn[m,5] <- "Neural Network"
          m<-m+1
        }
        ROCnn2 <- ROCnn
        ROCnn2
      }
      
    })
    
  
  filteredData1 <- reactive({
    test <- as.numeric(input$sldr3)
    ROCxgb3 <- ROCxgb2()[(test*100),]
    ROCxgb3
  })
  
  output$plot3 <- renderPlotly(ggplotly(ggplot(ROCxgb2())+
                                 geom_line(aes(x=X2,y=X1,text=paste("Specitivity :",round(X2,digits = 2),"<br>",
                                                                    "Sensitivity : ",round(X1,digits = 2),"<br>",
                                                                    "Balanced Accuracy :",round(X3,digits = 2)), group=1),size=2,color="#F84D54")+
                                 xlab("Specitivity")+
                                 ylab("Sensitivity")+
                                 theme2+
                                 theme(strip.background =element_rect(fill="white"))+
                                 scale_color_manual(values=c("#00BFAC"), guide=FALSE)+
                                 geom_point(data = filteredData1(), 
                                                             aes(x = filteredData1()$X2[1],
                                                                 y = filteredData1()$X1[1]),
                                                             shape=21,
                                                             fill = "#F84D54",
                                                             size=6),
                                        tooltip = c("text")) %>% 
                                 config(displayModeBar = F))
  
  output$sensitivityxgb <- renderText({ as.character(round(filteredData1()$X1[1],digits=2))})
  output$specitivityxgb <- renderText({ as.character(round(filteredData1()$X2[1],digits=2))})
  output$balaccxgb <- renderText({ as.character(round(filteredData1()$X3[1],digits=2))})
  output$names <- renderText({ as.character(filteredData1()$X5[1])})

 
  #-----------------predict
  prediction <- reactive({
    req(input$file2)
    inFile <- input$file2
    cutoff <- input$num6
    cutoff <- as.numeric(cutoff)
    alg <- input$slct6
    cctest <- read.csv(inFile$datapath)
    cctest <- data.frame(apply(cctest, MARGIN = 2, FUN = as.numeric))
    cctest$PAY_0 <- as.factor(cctest$PAY_0)
    cctest$PAY_2 <- as.factor(cctest$PAY_2)
    cctest$PAY_3 <- as.factor(cctest$PAY_3)
    cctest$PAY_4 <- as.factor(cctest$PAY_4)
    cctest$PAY_5 <- as.factor(cctest$PAY_5)
    cctest$PAY_6 <- as.factor(cctest$PAY_6)
    cctest$EDUCATION <- as.factor(cctest$EDUCATION)
    cctest$MARRIAGE <- as.factor(cctest$MARRIAGE)
    cctest$SEX <- as.factor(cctest$SEX)
    cc_test <- cctest[,-1]
    levels(cc_test$EDUCATION) <- factor(c("others1","graduate","university","high school","others2","others3","others4"))
    levels(cc_test$MARRIAGE) <- factor(c("married","single","divorce","others"))
    levels(cc_test$SEX) <- factor(c("male","female"))
    dummy <- dummyVars("~ EDUCATION+MARRIAGE+SEX+PAY_0+PAY_2+PAY_3+PAY_4+PAY_5+PAY_6", data = ccdef)
    dummy <- data.frame(predict(dummy, newdata = cc_test))
    cc_test <- cbind(cc_test,dummy)
    cc_test <- cc_test[,-c(2:4,6:11)]
    cctest2x <- cc_test[,-c(49,64,71,81)]
    if (alg==1){
    cc_test$AGE <- as.integer(cc_test$AGE)
    dctpredict <- predict(dct$finalModel, cc_test, type = "prob")
    dctpredict <- unlist(dctpredict)
    dctpredict2 <- matrix(0L, nrow = (length(dctpredict)/2), ncol = 2) 
    for (i in 1:length(dctpredict)){
      if (i %% 2 == 0){
        dctpredict2[(i/2),2] <- dctpredict[i]
      }
      else if (i %% 2 != 0){
        dctpredict2[(ceiling(i/2)),1] <- dctpredict[i]
        }
    }
    predicts <-  ifelse(dctpredict2[,2]>cutoff,1,0)
    }
  
    else if (alg==2){
    xgbpredict <- predict(xgb, cc_test, type = "prob")
    predicts <-  ifelse(xgbpredict[,2]>cutoff,1,0)
    }
  
   else if (alg==3){
    cctest2x <- as.matrix(cctest2x)
    predict <- model %>%  predict_proba(cctest2x)
    predict <- as.data.frame(predict)
    predicts <- ifelse(predict[,2]>cutoff,1,0)
    }
  
    prediction <- cbind(cctest,predicts)

    return(prediction)
})
  
  output$contents <- renderDataTable(server = FALSE,{
    prediction()},extensions = c('Buttons'), options = list(dom = 'Btip',buttons = c('csv'), scrollX=TRUE, pageLength = 22))
#------------------------------------  
  
  output$plot10 <- renderPlotly(ggplotly(plot10(input$slct10[1]),tooltip = c("text")) %>% 
                                 config(displayModeBar = F))
  
  output$plot20 <- renderPlotly(ggplotly(plot20(input$slct20[1]),tooltip = c("text")) %>% 
                                  config(displayModeBar = F))
  
  output$plot30 <- renderPlotly(ggplotly(plot30(),tooltip = c("text")) %>% 
                                  config(displayModeBar = F))

 
  output$bubs1 <- renderBubbles(bub1())
  output$plotx <- renderPlotly(ggplotly(plot1(),tooltip = c("text")) %>% 
                                      config(displayModeBar = F))

  output$bubs2 <- renderBubbles(bub2())
  output$ploty <- renderPlotly(ggplotly(plot2(),tooltip = c("text")) %>% 
                                 config(displayModeBar = F))
  
  
})