library(dplyr)
library(reshape2)
library(ggplot2)
library(ggpubr)
library(ggthemes)
library(caret)
library(xgboost)
library(keras)
library(reticulate)
library(data.table)
library(e1071)
library(party)
library(shinyjs)
library(bubbles)
#---------------------
  #theme
  theme1 <- theme(panel.grid.major.y = element_blank(),
                  panel.grid.minor = element_blank(),
                  panel.grid.major.x = element_line(colour = "#F1F1F1"),
                  panel.background = element_rect(colour = "white",
                                                  fill = "white"),
                  text = element_text(size=12))
  
  theme2 <- theme(panel.grid.major.x = element_blank(),
                  panel.grid.minor = element_blank(),
                  panel.grid.major.y = element_line(colour = "#F1F1F1"),
                  panel.background = element_rect(colour = "white",
                                                  fill = "white"))
  
  theme3 <- theme(panel.grid.minor = element_blank(),
                  panel.grid.major.y = element_line(colour = "#F1F1F1"),
                  panel.grid.major.x = element_line(colour = "#F1F1F1"),
                  panel.background = element_rect(colour = "white",
                                                  fill = "white"))
#--------------------------------------------------------------------  
  
  plot1 <- function(){
  varxgb <- var1 %>% 
    top_n(n = as.integer(15))
  
  
  plot1 <- ggplot(varxgb)+
    geom_bar(aes(x=reorder(X1,X2), y=X2, 
                 text=paste("Variable : ",X1, "<br>","Importance Number: ",X2)),
             stat="identity", fill ="#00BFAC")+
    coord_flip()+
    xlab("")+
    ylab("Variable Importance")+
    theme(legend.title = element_blank())+
    theme1
  
  return(plot1)
  }
  
  plot2 <- function(){
  vardct <- var2 %>%
    arrange(desc(X2)) %>% 
    top_n(n = as.integer(15))
  

  plot2 <- ggplot(vardct)+
    geom_bar(aes(x=reorder(X1,X2), y=X2, 
                 text=paste("Variable : ",X1, "<br>","Importance Number: ",X2)),
             stat="identity", fill ="#F84D54")+
    coord_flip()+
    xlab("")+
    ylab("Variable Importance")+
    theme(legend.title = element_blank())+
    theme1
  return(plot2)
  }
  
#-------------------------------------------------------------------  
  
  #Read Model
  xgb <- readRDS(file = "model/xgb.rds")
  dct <- readRDS(file = "model/dct.rds")
  model <- load_model_hdf5(filepath = "model/nn.hdf5")
  
  xgbvar <- varImp(xgb)
  dctvar <- varImp(dct)
  var1 <- data.frame(matrix(0, nrow = nrow(xgbvar$importance), 2))
  var2 <- data.frame(matrix(0, nrow = nrow(dctvar$importance), 2))
  var1[,1] <- row.names(xgbvar$importance)
  var1[,2] <- xgbvar$importance$Overall
  var2[,1] <- row.names(dctvar$importance)
  var2[,2] <- dctvar$importance$yes
  
  
  #ROC Data
  
  ROCxgb <- data.frame(matrix(NA, 100, 5))
  ROCdct <- data.frame(matrix(NA, 100 , 5))
  ROCnn <- data.frame(matrix(NA, 100, 5))
  
  #process file
  ccdef <- fread("dataset/UCI_Credit_Card.csv", na.strings = c("", "NA"))
  ccdef$PAY_0 <- as.factor(ccdef$PAY_0)
  ccdef$PAY_2 <- as.factor(ccdef$PAY_2)
  ccdef$PAY_3 <- as.factor(ccdef$PAY_3)
  ccdef$PAY_4 <- as.factor(ccdef$PAY_4)
  ccdef$PAY_5 <- as.factor(ccdef$PAY_5)
  ccdef$PAY_6 <- as.factor(ccdef$PAY_6)
  ccdef$EDUCATION <- as.factor(ccdef$EDUCATION)
  ccdef$MARRIAGE <- as.factor(ccdef$MARRIAGE)
  ccdef$default.payment.next.month <- as.factor(ccdef$default.payment.next.month)
  ccdef$SEX <- as.factor(ccdef$SEX)
  ccdef <- ccdef[,-1]
  levels(ccdef$EDUCATION) <- factor(c("others1","graduate","university","high school","others2","others3","others4"))
  levels(ccdef$MARRIAGE) <- factor(c("married","single","divorce","others"))
  levels(ccdef$SEX) <- factor(c("male","female"))
  levels(ccdef$default.payment.next.month) <- factor(c("no","yes"))

#-----------------------------------
  
plot10 <- function(slider1){
  slider1 <- as.character(slider1)
  if (slider1=="1"){
  spprt1 <- ccdef %>% 
    select(SEX, default.payment.next.month) %>% 
    mutate(count =1) %>% 
    group_by(SEX,default.payment.next.month) %>% 
    summarise(defaultcount = sum(count)) 
  
  spprt1$SEX <- as.character(spprt1$SEX) 
  spprt1$default.payment.next.month <- as.character(spprt1$default.payment.next.month)
  
  
  plot10 <- ggplot(spprt1)+
                       geom_bar(aes(x=SEX, y=defaultcount, fill=default.payment.next.month,
                                text=paste("SEX : ",spprt1$SEX, "<br>","Default : ",spprt1$default.payment.next.month,"<br>","Total :",spprt1$defaultcount)),
                                stat="identity", position="stack")+
                       xlab("")+
                       ylab("Total Default")+
                       scale_fill_manual(values = c("#F84D54","#00BFAC"), name="Default")+
                       theme2
  return(plot10)
  }
  
  else if (slider1=="2"){
  spprt2 <- ccdef %>% 
    select(EDUCATION, default.payment.next.month) %>% 
    mutate(count =1) %>% 
    group_by(EDUCATION,default.payment.next.month) %>% 
    summarise(defaultcount = sum(count)) 
  
  spprt2$EDUCATION <- as.character(spprt2$EDUCATION) 
  spprt2$default.payment.next.month <- as.character(spprt2$default.payment.next.month)
  
  
  plot11 <- ggplot(spprt2)+
                       geom_bar(aes(x=reorder(EDUCATION,-defaultcount), y=defaultcount, fill=default.payment.next.month,
                                text=paste("SEX : ",spprt2$EDUCATION, "<br>","Default : ",spprt2$default.payment.next.month,"<br>","Total :",spprt2$defaultcount)),
                                stat="identity", position="stack")+
                       xlab("")+
                       ylab("Total Default")+
                       scale_fill_manual(values = c("#F84D54","#00BFAC"), name="Default")+
                       theme2
  return(plot11)
  }
  
  else if (slider1=="3"){
  spprt3 <- ccdef %>% 
    select(MARRIAGE, default.payment.next.month) %>% 
    mutate(count =1) %>% 
    group_by(MARRIAGE,default.payment.next.month) %>% 
    summarise(defaultcount = sum(count)) 
  
  spprt3$MARRIAGE <- as.character(spprt3$MARRIAGE) 
  spprt3$default.payment.next.month <- as.character(spprt3$default.payment.next.month)
  
  
  plot12 <- ggplot(spprt3)+
                       geom_bar(aes(x=reorder(MARRIAGE,-defaultcount), y=defaultcount, fill=default.payment.next.month,
                                text=paste("SEX : ",spprt3$MARRIAGE, "<br>","Default : ",spprt3$default.payment.next.month,"<br>","Total :",spprt3$defaultcount)),
                                stat="identity", position="stack")+
                       xlab("")+
                       ylab("Total Default")+
                       scale_fill_manual(values = c("#F84D54","#00BFAC"), name="Default")+
                       theme2
  return(plot12)
  }
  
  else if (slider1=="4"){
  spprt4 <- ccdef %>% 
    select(PAY_0, default.payment.next.month) %>% 
    mutate(count =1) %>% 
    group_by(PAY_0,default.payment.next.month) %>% 
    summarise(defaultcount = sum(count)) 
  
  spprt4$PAY_0 <- as.character(spprt4$PAY_0) 
  spprt4$default.payment.next.month <- as.character(spprt4$default.payment.next.month)
  
  
  plot13 <- ggplot(spprt4)+
                       geom_bar(aes(x=reorder(PAY_0,-defaultcount), y=defaultcount, fill=default.payment.next.month,
                                text=paste("SEX : ",spprt4$PAY_0, "<br>","Default : ",spprt4$default.payment.next.month,"<br>","Total :",spprt4$defaultcount)),
                                stat="identity", position="stack")+
                       xlab("")+
                       ylab("Total Default")+
                       scale_fill_manual(values = c("#00BFAC","#F84D54"), name="Default")+
                       theme2
  return(plot13)
  }
  
  else if (slider1=="5"){
  spprt5 <- ccdef %>% 
    select(PAY_2, default.payment.next.month) %>% 
    mutate(count =1) %>% 
    group_by(PAY_2,default.payment.next.month) %>% 
    summarise(defaultcount = sum(count)) 
  
  spprt5$PAY_2 <- as.character(spprt5$PAY_2) 
  spprt5$default.payment.next.month <- as.character(spprt5$default.payment.next.month)
  
  
  plot14 <- ggplot(spprt5)+
                       geom_bar(aes(x=reorder(PAY_2,-defaultcount), y=defaultcount, fill=default.payment.next.month,
                                text=paste("SEX : ",spprt5$PAY_2, "<br>","Default : ",spprt5$default.payment.next.month,"<br>","Total :",spprt5$defaultcount)),
                                stat="identity", position="stack")+
                       xlab("")+
                       ylab("Total Default")+
                       scale_fill_manual(values = c("#00BFAC","#F84D54"), name="Default")+
                       theme2
  return(plot14)
  }
  
  else if (slider1=="6"){
  spprt6 <- ccdef %>% 
    select(PAY_3, default.payment.next.month) %>% 
    mutate(count =1) %>% 
    group_by(PAY_3,default.payment.next.month) %>% 
    summarise(defaultcount = sum(count)) 
  
  spprt6$PAY_3 <- as.character(spprt6$PAY_3) 
  spprt6$default.payment.next.month <- as.character(spprt6$default.payment.next.month)
  
  
  plot15 <- ggplot(spprt6)+
                       geom_bar(aes(x=reorder(PAY_3,-defaultcount), y=defaultcount, fill=default.payment.next.month,
                                text=paste("SEX : ",spprt6$PAY_3, "<br>","Default : ",spprt6$default.payment.next.month,"<br>","Total :",spprt6$defaultcount)),
                                stat="identity", position="stack")+
                       xlab("")+
                       ylab("Total Default")+
                       scale_fill_manual(values = c("#00BFAC","#F84D54"), name="Default")+  
                       theme2
  return(plot15)
  }
  
  else if (slider1=="7"){
  spprt7 <- ccdef %>% 
    select(PAY_4, default.payment.next.month) %>% 
    mutate(count =1) %>% 
    group_by(PAY_4,default.payment.next.month) %>% 
    summarise(defaultcount = sum(count)) 
  
  spprt7$PAY_4 <- as.character(spprt7$PAY_4) 
  spprt7$default.payment.next.month <- as.character(spprt7$default.payment.next.month)
  
  
  plot16 <- ggplot(spprt7)+
                       geom_bar(aes(x=reorder(PAY_4,-defaultcount), y=defaultcount, fill=default.payment.next.month,
                                text=paste("SEX : ",spprt7$PAY_4, "<br>","Default : ",spprt7$default.payment.next.month,"<br>","Total :",spprt7$defaultcount)),
                                stat="identity", position="stack")+
                       xlab("")+
                       ylab("Total Default")+
                       scale_fill_manual(values = c("#00BFAC","#F84D54"), name="Default")+
                       theme2
  return(plot16)
  }
  
  
  else if (slider1=="8"){
  spprt8 <- ccdef %>% 
    select(PAY_5, default.payment.next.month) %>% 
    mutate(count =1) %>% 
    group_by(PAY_5,default.payment.next.month) %>% 
    summarise(defaultcount = sum(count)) 
  
  spprt8$PAY_5 <- as.character(spprt8$PAY_5) 
  spprt8$default.payment.next.month <- as.character(spprt8$default.payment.next.month)
  
  
  plot17 <- ggplot(spprt8)+
                       geom_bar(aes(x=reorder(PAY_5,-defaultcount), y=defaultcount, fill=default.payment.next.month,
                                text=paste("SEX : ",spprt8$PAY_5, "<br>","Default : ",spprt8$default.payment.next.month,"<br>","Total :",spprt8$defaultcount)),
                                stat="identity", position="stack")+
                       xlab("")+
                       ylab("Total Default")+
                       scale_fill_manual(values = c("#00BFAC","#F84D54"), name="Default")+
                       theme2
  return(plot17)
  }
  
  
  else if (slider1=="9"){
  spprt9 <- ccdef %>% 
    select(PAY_6, default.payment.next.month) %>% 
    mutate(count =1) %>% 
    group_by(PAY_6,default.payment.next.month) %>% 
    summarise(defaultcount = sum(count)) 
  
  spprt9$PAY_6 <- as.character(spprt9$PAY_6) 
  spprt9$default.payment.next.month <- as.character(spprt9$default.payment.next.month)
  
  
  plot18 <- ggplot(spprt9)+
                       geom_bar(aes(x=reorder(PAY_6,-defaultcount), y=defaultcount, fill=default.payment.next.month,
                                text=paste("SEX : ",spprt9$PAY_6, "<br>","Default : ",spprt9$default.payment.next.month,"<br>","Total :",spprt9$defaultcount)),
                                stat="identity", position="stack")+
                       xlab("")+
                       ylab("Total Default")+
                       scale_fill_manual(values = c("#00BFAC","#F84D54"), name="Default")+
                       theme2
  return(plot18)
  }
}
  
  
  plot20 <- function(slider2){
  slider2 <- as.character(slider2)
  if (slider2=="1"){
    spprt10 <- ccdef %>% 
    select(AGE,default.payment.next.month)
    spprt10$default.payment.next.month <- as.character(spprt10$default.payment.next.month)
    
    plot20 <- ggplot(spprt10)+
                         geom_boxplot(aes(x=default.payment.next.month, y=AGE, fill=default.payment.next.month),
                                      text=paste(spprt10$variable))+
                         xlab("Default")+
                         ylab("")+
                         scale_fill_manual(values = c("#F84D54","#00BFAC"), name="Default")+
                         scale_y_continuous(labels = scales::comma)+
                         theme2
    return(plot20)
  }
  else if (slider2=="2"){
    spprt11 <- ccdef %>% 
      select(LIMIT_BAL,default.payment.next.month)
    spprt11$default.payment.next.month <- as.character(spprt11$default.payment.next.month)
    
    plot21 <- ggplot(spprt11)+
                         geom_boxplot(aes(x=default.payment.next.month, y=LIMIT_BAL, fill=default.payment.next.month),
                                      text=paste(spprt11$variable))+
                         xlab("Default")+
                         ylab("")+
                         scale_fill_manual(values = c("#F84D54","#00BFAC"), name="Default")+
                         scale_y_continuous(labels = scales::comma)+
                         theme2
    return(plot21)
  }
  
  else if (slider2=="3"){
  spprt13 <- ccdef %>% 
    select(BILL_AMT1,default.payment.next.month) %>% 
    mutate(variable = "BILL_AMT1", value = BILL_AMT1)
  spprt13 <- spprt13[,-1]
  
  spprt23 <- ccdef %>% 
    select(PAY_AMT1,default.payment.next.month) %>% 
    mutate(variable = "PAY_AMT1", value = PAY_AMT1)
  spprt23 <- spprt23[,-1]
  
  sppt13 <- rbind(spprt13,spprt23)
  
  sppt13$default.payment.next.month <- as.character(spprt13$default.payment.next.month)
  
  
  plot22 <- ggplot(sppt13)+
                       geom_boxplot(aes(x=default.payment.next.month, y=value, fill=default.payment.next.month),
                                    text=paste(sppt13$variable))+
                       facet_wrap(~as.factor(sppt13$variable),ncol=2)+
                       xlab("Default")+
                       ylab("")+
                       scale_fill_manual(values = c("#F84D54","#00BFAC"), name="Default")+
                       scale_y_continuous(labels = scales::comma)+
                       theme2
  
  return(plot22)
  }
  
  else if (slider2=="4"){
    spprt14 <- ccdef %>% 
      select(BILL_AMT2,default.payment.next.month) %>% 
      mutate(variable = "BILL_AMT2", value = BILL_AMT2)
    spprt14 <- spprt14[,-1]
    
    spprt24 <- ccdef %>% 
      select(PAY_AMT2,default.payment.next.month) %>% 
      mutate(variable = "PAY_AMT2", value = PAY_AMT2)
    spprt24 <- spprt24[,-1]
    
    sppt14 <- rbind(spprt14,spprt24)
    
    sppt14$default.payment.next.month <- as.character(spprt14$default.payment.next.month)
    
    plot24 <- ggplot(sppt14)+
                         geom_boxplot(aes(x=default.payment.next.month, y=value, fill=default.payment.next.month),
                                      text=paste(sppt14$variable))+
                         facet_wrap(~as.factor(sppt14$variable),ncol=2)+
                         xlab("Default")+
                         ylab("")+
                         scale_fill_manual(values = c("#F84D54","#00BFAC"), name="Default")+
                         scale_y_continuous(labels = scales::comma)+
                         theme2
    
    return(plot24)
  }
  
  else if (slider2=="5"){
    spprt15 <- ccdef %>% 
      select(BILL_AMT3,default.payment.next.month) %>% 
      mutate(variable = "BILL_AMT3", value = BILL_AMT3)
    spprt15 <- spprt15[,-1]
    
    spprt25 <- ccdef %>% 
      select(PAY_AMT3,default.payment.next.month) %>% 
      mutate(variable = "PAY_AMT3", value = PAY_AMT3)
    spprt25 <- spprt25[,-1]
    
    sppt15 <- rbind(spprt15,spprt25)
    
    sppt15$default.payment.next.month <- as.character(spprt15$default.payment.next.month)
    
    
    plot25 <- ggplot(sppt15)+
                         geom_boxplot(aes(x=default.payment.next.month, y=value, fill=default.payment.next.month),
                                      text=paste(sppt15$variable))+
                         facet_wrap(~as.factor(sppt15$variable),ncol=2)+
                         xlab("Default")+
                         ylab("")+
                         scale_fill_manual(values = c("#F84D54","#00BFAC"), name="Default")+
                         scale_y_continuous(labels = scales::comma)+
                         theme2
    
    return(plot25)
  }
  
  else if (slider2=="6"){
    spprt16 <- ccdef %>% 
      select(BILL_AMT4,default.payment.next.month) %>% 
      mutate(variable = "BILL_AMT4", value = BILL_AMT4)
    spprt16 <- spprt16[,-1]
    
    spprt26 <- ccdef %>% 
      select(PAY_AMT4,default.payment.next.month) %>% 
      mutate(variable = "PAY_AMT4", value = PAY_AMT4)
    spprt26 <- spprt26[,-1]
    
    sppt16 <- rbind(spprt16,spprt26)
    
    sppt16$default.payment.next.month <- as.character(spprt16$default.payment.next.month)
    
    plot26 <- ggplot(sppt16)+
                         geom_boxplot(aes(x=default.payment.next.month, y=value, fill=default.payment.next.month),
                                      text=paste(sppt16$variable))+
                         facet_wrap(~as.factor(sppt16$variable),ncol=2)+
                         xlab("Default")+
                         ylab("")+
                         scale_fill_manual(values = c("#F84D54","#00BFAC"), name="Default")+
                         scale_y_continuous(labels = scales::comma)+
                         theme2
    
    return(plot26)
  }
  
  else if (slider2=="7"){
    spprt17 <- ccdef %>% 
      select(BILL_AMT5,default.payment.next.month) %>% 
      mutate(variable = "BILL_AMT5", value = BILL_AMT5)
    spprt17 <- spprt17[,-1]
    
    spprt27 <- ccdef %>% 
      select(PAY_AMT5,default.payment.next.month) %>% 
      mutate(variable = "PAY_AMT5", value = PAY_AMT5)
    spprt27 <- spprt27[,-1]
    
    sppt17 <- rbind(spprt17,spprt27)
    
    sppt17$default.payment.next.month <- as.character(spprt17$default.payment.next.month)
    
    plot27 <- ggplot(sppt17)+
                         geom_boxplot(aes(x=default.payment.next.month, y=value, fill=default.payment.next.month),
                                      text=paste(sppt17$variable))+
                         facet_wrap(~as.factor(sppt17$variable),ncol=2)+
                         xlab("Default")+
                         ylab("")+
                         scale_fill_manual(values = c("#F84D54","#00BFAC"), name="Default")+
                         scale_y_continuous(labels = scales::comma)+
                         theme2
    
    return(plot27)
  }
  
  else if (slider2=="8"){
    spprt18 <- ccdef %>% 
      select(BILL_AMT6,default.payment.next.month) %>% 
      mutate(variable = "BILL_AMT6", value = BILL_AMT6)
    spprt18 <- spprt18[,-1]
    
    spprt28 <- ccdef %>% 
      select(PAY_AMT6,default.payment.next.month) %>% 
      mutate(variable = "PAY_AMT6", value = PAY_AMT6)
    spprt28 <- spprt28[,-1]
    
    sppt18 <- rbind(spprt18,spprt28)
    
    sppt18$default.payment.next.month <- as.character(spprt18$default.payment.next.month)
    
    plot28 <- ggplot(sppt18)+
                         geom_boxplot(aes(x=default.payment.next.month, y=value, fill=default.payment.next.month),
                                      text=paste(sppt18$variable))+
                         facet_wrap(~as.factor(sppt18$variable),ncol=2)+
                         xlab("Default")+
                         ylab("")+
                         scale_fill_manual(values = c("#F84D54","#00BFAC"), name="Default")+
                         scale_y_continuous(labels = scales::comma)+
                         theme2
    
    return(plot28)
  }
  
  }
  
  ccrange1 <- data.frame(matrix(NA, 12, 5))
  ccrange2 <- data.frame(matrix(NA, 12, 5))

  id1 <- ccdef$default.payment.next.month=="yes"
  ccdef1 <- ccdef[id1,]
  ccrange1[,1] <- "yes"
  ccrange1[1,2] <- "1"
  ccrange1[2,2] <- "2"
  ccrange1[3,2] <- "3"
  ccrange1[4,2] <- "4"
  ccrange1[5,2] <- "5"
  ccrange1[6,2] <- "6"
  ccrange1[7,2] <- "1"
  ccrange1[8,2] <- "2"
  ccrange1[9,2] <- "3"
  ccrange1[10,2] <- "4"
  ccrange1[11,2] <- "5"
  ccrange1[12,2] <- "6"
  ccrange1[1,3:4] <- range(ccdef1$PAY_AMT1)
  ccrange1[2,3:4] <- range(ccdef1$PAY_AMT2)
  ccrange1[3,3:4] <- range(ccdef1$PAY_AMT3)
  ccrange1[4,3:4] <- range(ccdef1$PAY_AMT4)
  ccrange1[5,3:4] <- range(ccdef1$PAY_AMT5)
  ccrange1[6,3:4] <- range(ccdef1$PAY_AMT6)
  ccrange1[7,3:4] <- range(ccdef1$BILL_AMT1)
  ccrange1[8,3:4] <- range(ccdef1$BILL_AMT2)
  ccrange1[9,3:4] <- range(ccdef1$BILL_AMT3)
  ccrange1[10,3:4] <- range(ccdef1$BILL_AMT4)
  ccrange1[11,3:4] <- range(ccdef1$BILL_AMT5)
  ccrange1[12,3:4] <- range(ccdef1$BILL_AMT6)
  ccrange1[1:6,5] <- "PAY_AMT"
  ccrange1[7:12,5] <- "BILL_AMT"

  id2 <- ccdef$default.payment.next.month=="no"
  ccdef2 <- ccdef[id2,]
  ccrange2[,1] <- "no"
  ccrange2[1,2] <- "1"
  ccrange2[2,2] <- "2"
  ccrange2[3,2] <- "3"
  ccrange2[4,2] <- "4"
  ccrange2[5,2] <- "5"
  ccrange2[6,2] <- "6"
  ccrange2[7,2] <- "1"
  ccrange2[8,2] <- "2"
  ccrange2[9,2] <- "3"
  ccrange2[10,2] <- "4"
  ccrange2[11,2] <- "5"
  ccrange2[12,2] <- "6"
  ccrange2[1,3:4] <- range(ccdef2$PAY_AMT1)
  ccrange2[2,3:4] <- range(ccdef2$PAY_AMT2)
  ccrange2[3,3:4] <- range(ccdef2$PAY_AMT3)
  ccrange2[4,3:4] <- range(ccdef2$PAY_AMT4)
  ccrange2[5,3:4] <- range(ccdef2$PAY_AMT5)
  ccrange2[6,3:4] <- range(ccdef2$PAY_AMT6)
  ccrange2[7,3:4] <- range(ccdef2$BILL_AMT1)
  ccrange2[8,3:4] <- range(ccdef2$BILL_AMT2)
  ccrange2[9,3:4] <- range(ccdef2$BILL_AMT3)
  ccrange2[10,3:4] <- range(ccdef2$BILL_AMT4)
  ccrange2[11,3:4] <- range(ccdef2$BILL_AMT5)
  ccrange2[12,3:4] <- range(ccdef2$BILL_AMT6)
  ccrange2[1:6,5] <- "PAY_AMT"
  ccrange2[7:12,5] <- "BILL_AMT"

  ccrange <- rbind(ccrange1,ccrange2)
  ccrange$X2 <- as.numeric(ccrange$X2)
  
plot30 <- function(){
  plot30 <- ggplot(ccrange)+
    geom_ribbon(aes(x=ccrange$X2, ymin=ccrange$X3,ymax=ccrange$X4, group=ccrange$X5,fill=ccrange$X5,
                text=paste("Min :", ccrange$X3,"<br>","Max :",ccrange$X4)))+
    facet_wrap(~as.factor(ccrange$X1),ncol=1)+
    xlab("")+
    ylab("Range of Bill/Payment")+
    scale_fill_manual(values = c("#F84D54","#00BFAC"),labels = c("Bill Amount","Payment Amount"), name="")+
    theme2
  
  return(plot30)
}

bub1 <- function(){
varxgb1 <- var1 %>% 
  top_n(n = 15)

 bub1 <-bubbles(varxgb1$X2, varxgb1$X1, key = NULL, tooltip = paste("Var Imp :", varxgb1$X2), color = "#00BFAC",
        textColor = "#000000", width = 800, height = 900)
return(bub1)
  }
  

bub2 <- function(){
  varxgb2 <- var2 %>% 
    top_n(n = 15)
  
  bubbles(varxgb2$X2^3, varxgb2$X1, key = NULL, tooltip = paste("Var Imp :", varxgb2$X2), color = "#F84D54",
          textColor = "#000000", width = 800, height = 900)
}