rm(list=ls())
graphics.off()

library(tidyverse)                                              
library(caret)      # Classification and Regression Training
library(ggplot2)    # Data Visualization                                               
library(corrplot)   # Visualization of Correlation                                     
library(reshape2)   # Flexibily Reshape Data                                 

# Performance Measurement for training data (2005 to 2013)
NaiveBayesPerfMeas = data.frame(Method = character(), Sampling = character(), QB_TP = integer(), QB_TN = integer(), QB_FP = integer(), QB_FN = integer(),
                         WR_TP = integer(), WR_TN = integer(), WR_FP = integer(), WR_FN = integer(),
                         RB_TP = integer(), RB_TN = integer(), RB_FP = integer(), RB_FN = integer(),
                         Together_TP = integer(), Together_TN = integer(), Together_FP = integer(), Together_FN = integer(), stringsAsFactors = FALSE)

NaiveBayesPerfMeas[1,2] = "no_sampling"
NaiveBayesPerfMeas[2,2] = "oversampling"
NaiveBayesPerfMeas[3,2] = "undersampling"
NaiveBayesPerfMeas[4,2] = "Rose_both"
NaiveBayesPerfMeas[5,2] = "Smote"
NaiveBayesPerfMeas$Method = "NaiveBayes"

# Performance Measurement for testing data (2014)

NaiveBayesPerfMeasTest = data.frame(Method = character(), Sampling = character(), QB_TP = integer(), QB_TN = integer(), QB_FP = integer(), QB_FN = integer(),
                                WR_TP = integer(), WR_TN = integer(), WR_FP = integer(), WR_FN = integer(),
                                RB_TP = integer(), RB_TN = integer(), RB_FP = integer(), RB_FN = integer(),
                                Together_TP = integer(), Together_TN = integer(), Together_FP = integer(), Together_FN = integer(), stringsAsFactors = FALSE)

NaiveBayesPerfMeasTest[1,2] = "no_sampling"
NaiveBayesPerfMeasTest[2,2] = "oversampling"
NaiveBayesPerfMeasTest[3,2] = "undersampling"
NaiveBayesPerfMeasTest[4,2] = "Rose_both"
NaiveBayesPerfMeasTest[5,2] = "Smote"
NaiveBayesPerfMeasTest$Method = "NaiveBayes"

###################################################
# NOTICE
###################################################

# We will do the next steps 5 times (e.g. "1. No Splitting" does the same thing as "2. Oversampling"), but using different data for training the model
# In other words, this is the cross-validation of the sampling methods. The reason for doing it a couple of times instead of looping or functioning it
# is the easier availability of the steps in between in case of further processing them.

# 1. No Sampling ###################################################

load("../Data/CleanData/CleanClass2007to2014_3.Rdata")

# I. Naive Bayes Classifier - 07 to 13, together ----------

#1 - Preparations and data visualization  ----------

# Training data
CleanClass2007to2013_3<- CleanClass2007to2014_3[CleanClass2007to2014_3$Year != 2014,]
CleanClass2007to2013_3$Drafted <- as.factor(CleanClass2007to2013_3$Drafted)
Data2007to2013_tog <- CleanClass2007to2013_3 %>% select(-Position, -Class, -Name, -Player.Code, -Year, 
                                                        -Safety) #this variable has zero variance
# Testing data
CleanClass2014_3<- CleanClass2007to2014_3[CleanClass2007to2014_3$Year == 2014,]
CleanClass2014_3$Drafted <- as.factor(CleanClass2014_3$Drafted)
CleanClass2014_3_tog <- CleanClass2014_3 %>% select(-Position, -Class, -Name, -Player.Code, -Year, 
                                                    -Safety) #this variable has zero variance

# Density distributions
# (These observations are discussed in the ReadMe)
Data2007to2013_vis_tog <- Data2007to2013_tog

Df1_tog <- Data2007to2013_vis_tog[,c(1, 2:10)]
Long1_tog = melt(Df1_tog, id.vars= "Drafted")
ggplot(data = Long1_tog, aes(x = value, fill=Drafted)) + 
  geom_density(alpha=0.6) + 
  facet_wrap(~variable, scales = "free") 
ggplot(data = Long1_tog, aes(sample = value, color=Drafted)) + 
  geom_qq(alpha=0.6) + 
  geom_qq_line()+
  facet_wrap(~variable, scales = "free") 

Df2_tog <- Data2007to2013_vis_tog[,c(1, 11:19)]
Long2_tog = melt(Df2_tog, id.vars= "Drafted")
ggplot(data = Long2_tog, aes(x = value, fill=Drafted)) + 
  geom_density(alpha=0.6) + 
  facet_wrap(~variable, scales = "free")
ggplot(data = Long2_tog, aes(sample = value, color=Drafted)) + 
  geom_qq(alpha=0.6) + 
  geom_qq_line()+
  facet_wrap(~variable, scales = "free") 

Df3_tog <- Data2007to2013_vis_tog[,c(1, 20:24)]
Long3_tog = melt(Df3_tog, id.vars= "Drafted")
ggplot(data = Long3_tog, aes(x = value, fill=Drafted)) + 
  geom_density(alpha=0.6) + 
  facet_wrap(~variable, scales = "free")
ggplot(data = Long3_tog, aes(sample = value, color=Drafted)) + 
  geom_qq(alpha=0.6) + 
  geom_qq_line()+
  facet_wrap(~variable, scales = "free") 

# Correlation within variables
# The naive Bayes classifier makes a simplifying assumption (hence the name) to allow the computation to scale. 
# With naïve Bayes, we assume that the predictor variables are conditionally independent of one another given 
# the response value. This is an extremely strong assumption. In our context we can see quickly that our data 
# violates this as we have several moderately to strongly correlated variables. See the following correlation plots:
# (These observations are discussed in the ReadMe)

Data2007to2013_vis_tog %>%
  filter(Drafted =="1") %>%
  select_if(is.numeric) %>%
  cor() %>%
  corrplot::corrplot()
Data2007to2013_vis_tog %>%
  filter(Drafted =="0") %>%
  select_if(is.numeric) %>%
  cor() %>%
  corrplot::corrplot()

#2 - Naive Bayes ----------

# Define features (x) and target (y) 
features_tog <- setdiff(names(Data2007to2013_tog), "Drafted")
x_tog <- Data2007to2013_tog[,features_tog]
y_tog <- Data2007to2013_tog$Drafted

# Training a naive bayes model with 10-fold cross validation
set.seed(6969)
NB_tog <- train(x_tog,y_tog,method = "nb",trControl=trainControl(method='cv',number=10))

# For training data

# Predictions: 0.5 is used for probability cutoff value by default
predict_tog <- predict(NB_tog,Data2007to2013_tog) 
confusionMatrix(predict_tog,Data2007to2013_tog$Drafted)

# Comment on the warnings we get when running train() and predict(). (Is commented here once for the whole script).
# "Numerical 0 probability for all classes with observation...": This is not an error or indication that the code is 
# 'wrong', it is just information that some of the observations are producing some unusual probabilities: These observations
# are probably outliers.

CheckList_tog = cbind.data.frame(Data2007to2013_tog$Drafted,predict_tog)

names(CheckList_tog)[names(CheckList_tog)=="Data2007to2013_tog$Drafted"] <- "Y"
names(CheckList_tog)[names(CheckList_tog)=="predict_tog"] <- "Pred"

CheckList_tog = CheckList_tog %>%
  mutate(TP=ifelse(Y==Pred,ifelse(Pred==1,1,0),0)) %>%
  mutate(TN=ifelse(Y==Pred,ifelse(Pred==0,1,0),0)) %>%
  mutate(FP=ifelse(Y!=Pred,ifelse(Pred==1,1,0),0)) %>%
  mutate(FN=ifelse(Y!=Pred,ifelse(Pred==0,1,0),0))

# Performance Measurement
NaiveBayesPerfMeas[1,"Together_TP"] = sum(CheckList_tog$TP)
NaiveBayesPerfMeas[1,"Together_TN"] = sum(CheckList_tog$TN)
NaiveBayesPerfMeas[1,"Together_FP"] = sum(CheckList_tog$FP)
NaiveBayesPerfMeas[1,"Together_FN"] = sum(CheckList_tog$FN)

# For testing data
predict_togTest <- predict(NB_tog,CleanClass2014_3_tog)
CheckList_togTest = cbind.data.frame(CleanClass2014_3_tog$Drafted,predict_togTest)

names(CheckList_togTest)[names(CheckList_togTest)=="CleanClass2014_3_tog$Drafted"] <- "Y"
names(CheckList_togTest)[names(CheckList_togTest)=="predict_togTest"] <- "Pred"

CheckList_togTest = CheckList_togTest %>%
  mutate(TP=ifelse(Y==Pred,ifelse(Pred==1,1,0),0)) %>%
  mutate(TN=ifelse(Y==Pred,ifelse(Pred==0,1,0),0)) %>%
  mutate(FP=ifelse(Y!=Pred,ifelse(Pred==1,1,0),0)) %>%
  mutate(FN=ifelse(Y!=Pred,ifelse(Pred==0,1,0),0))

NaiveBayesPerfMeasTest[1,"Together_TP"] = sum(CheckList_togTest$TP)
NaiveBayesPerfMeasTest[1,"Together_TN"] = sum(CheckList_togTest$TN)
NaiveBayesPerfMeasTest[1,"Together_FP"] = sum(CheckList_togTest$FP)
NaiveBayesPerfMeasTest[1,"Together_FN"] = sum(CheckList_togTest$FN)

# II. Naive Bayes Classifier - 07 to 13, QB  ----------

#1 - Preparations and data visualization  ----------

# Training data
Data2007to2013_QB <- CleanClass2007to2013_3[CleanClass2007to2013_3$Position=="QB", ]
Data2007to2013_QB <- Data2007to2013_QB %>% select(-Class, -Position, -Name, -Player.Code, -Year, 
                                                  -Safety, -Kickoff.Ret.TD, -Punt.Ret.TD) #these variables have zero variance

# Testing data
CleanClass2014_3_QB<- CleanClass2014_3[CleanClass2014_3$Position=="QB", ]
CleanClass2014_3_QB <- CleanClass2014_3_QB %>% select(-Class, -Position, -Name, -Player.Code, -Year,
                                                      -Safety,-Kickoff.Ret.TD, -Punt.Ret.TD) #these variables have zero variance

# Density distributions
Data2007to2013_vis_QB <- Data2007to2013_QB

Df1_QB <- Data2007to2013_vis_QB[,c(1, 2:10)]
Long1_QB = melt(Df1_QB, id.vars= "Drafted")
ggplot(data = Long1_QB, aes(x = value, fill=Drafted)) + 
  geom_density(alpha=0.6) + 
  facet_wrap(~variable, scales = "free")
ggplot(data = Long1_QB, aes(sample = value, color=Drafted)) + 
  geom_qq(alpha=0.6) + 
  geom_qq_line()+
  facet_wrap(~variable, scales = "free") 

Df2_QB <- Data2007to2013_vis_QB[,c(1, 11:19)]
Long2_QB = melt(Df2_QB, id.vars= "Drafted")
ggplot(data = Long2_QB, aes(x = value, fill=Drafted)) + 
  geom_density(alpha=0.6) + 
  facet_wrap(~variable, scales = "free")
ggplot(data = Long2_QB, aes(sample = value, color=Drafted)) + 
  geom_qq(alpha=0.6) + 
  geom_qq_line()+
  facet_wrap(~variable, scales = "free") 

Df3_QB <- Data2007to2013_vis_QB[,c(1, 20:22)]
Long3_QB = melt(Df3_QB, id.vars= "Drafted")
ggplot(data = Long3_QB, aes(x = value, fill=Drafted)) + 
  geom_density(alpha=0.6) + 
  facet_wrap(~variable, scales = "free")
ggplot(data = Long3_QB, aes(sample = value, color=Drafted)) + 
  geom_qq(alpha=0.6) + 
  geom_qq_line()+
  facet_wrap(~variable, scales = "free") 

# Correlation within variables
Data2007to2013_vis_QB %>%
  filter(Drafted =="1") %>%
  select_if(is.numeric) %>%
  cor() %>%
  corrplot::corrplot()
Data2007to2013_vis_QB %>%
  filter(Drafted =="0") %>%
  select_if(is.numeric) %>%
  cor() %>%
  corrplot::corrplot()

#2 - Naive Bayes ----------

# Define features (x) and target (y) 
features_QB <- setdiff(names(Data2007to2013_QB), "Drafted")
x_QB <- Data2007to2013_QB[,features_QB]
y_QB <- Data2007to2013_QB$Drafted

# Training a naive bayes model with 10-fold cross validation
set.seed(6969)
NB_QB <- train(x_QB,y_QB,method = "nb",trControl=trainControl(method='cv',number=10))

# Predictions
predict_QB <- predict(NB_QB, newdata=Data2007to2013_QB)
confusionMatrix(predict_QB, Data2007to2013_QB$Drafted)

CheckList_QB = cbind.data.frame(Data2007to2013_QB$Drafted,predict_QB)

names(CheckList_QB)[names(CheckList_QB)=="Data2007to2013_QB$Drafted"] <- "Y"
names(CheckList_QB)[names(CheckList_QB)=="predict_QB"] <- "Pred"

CheckList_QB = CheckList_QB %>%
  mutate(TP=ifelse(Y==Pred,ifelse(Pred==1,1,0),0)) %>%
  mutate(TN=ifelse(Y==Pred,ifelse(Pred==0,1,0),0)) %>%
  mutate(FP=ifelse(Y!=Pred,ifelse(Pred==1,1,0),0)) %>%
  mutate(FN=ifelse(Y!=Pred,ifelse(Pred==0,1,0),0))

# Performance Measurement
NaiveBayesPerfMeas[1,"QB_TP"] = sum(CheckList_QB$TP)
NaiveBayesPerfMeas[1,"QB_TN"] = sum(CheckList_QB$TN)
NaiveBayesPerfMeas[1,"QB_FP"] = sum(CheckList_QB$FP)
NaiveBayesPerfMeas[1,"QB_FN"] = sum(CheckList_QB$FN)

# For testing data
predict_QBTest <- predict(NB_QB,CleanClass2014_3_QB)
CheckList_QBTest = cbind.data.frame(CleanClass2014_3_QB$Drafted,predict_QBTest)

names(CheckList_QBTest)[names(CheckList_QBTest)=="CleanClass2014_3_QB$Drafted"] <- "Y"
names(CheckList_QBTest)[names(CheckList_QBTest)=="predict_QBTest"] <- "Pred"

CheckList_QBTest = CheckList_QBTest %>%
  mutate(TP=ifelse(Y==Pred,ifelse(Pred==1,1,0),0)) %>%
  mutate(TN=ifelse(Y==Pred,ifelse(Pred==0,1,0),0)) %>%
  mutate(FP=ifelse(Y!=Pred,ifelse(Pred==1,1,0),0)) %>%
  mutate(FN=ifelse(Y!=Pred,ifelse(Pred==0,1,0),0))

NaiveBayesPerfMeasTest[1,"QB_TP"] = sum(CheckList_QBTest$TP)
NaiveBayesPerfMeasTest[1,"QB_TN"] = sum(CheckList_QBTest$TN)
NaiveBayesPerfMeasTest[1,"QB_FP"] = sum(CheckList_QBTest$FP)
NaiveBayesPerfMeasTest[1,"QB_FN"] = sum(CheckList_QBTest$FN)

# III. Naive Bayes Classifier - 07 to 13, WR ----------

#1 - Preparations and data visualization ----------

# Training data
Data2007to2013_WR <- CleanClass2007to2013_3[CleanClass2007to2013_3$Position=="WR", ]
Data2007to2013_WR <- Data2007to2013_WR %>% select(-Class, -Position, -Name, -Player.Code, -Year, 
                                                  -Safety) #these variables have zero variance

# Testing data
CleanClass2014_3_WR<- CleanClass2014_3[CleanClass2014_3$Position=="WR", ]
CleanClass2014_3_WR <- CleanClass2014_3_WR %>% select(-Class, -Position, -Name, -Player.Code, -Year,
                                                      -Safety) #these variables have zero variance

# Density distributions
Data2007to2013_vis_WR <- Data2007to2013_WR

Df1_WR <- Data2007to2013_vis_WR[,c(1, 2:10)]
Long1_WR = melt(Df1_WR, id.vars= "Drafted")
ggplot(data = Long1_WR, aes(x = value, fill=Drafted)) + 
  geom_density(alpha=0.6) + 
  facet_wrap(~variable, scales = "free")
ggplot(data = Long1_WR, aes(sample = value, color=Drafted)) + 
  geom_qq(alpha=0.6) + 
  geom_qq_line()+
  facet_wrap(~variable, scales = "free") 

Df2_WR <- Data2007to2013_vis_WR[,c(1, 11:19)]
Long2_WR = melt(Df2_WR, id.vars= "Drafted")
ggplot(data = Long2_WR, aes(x = value, fill=Drafted)) + 
  geom_density(alpha=0.6) + 
  facet_wrap(~variable, scales = "free")
ggplot(data = Long2_WR, aes(sample = value, color=Drafted)) + 
  geom_qq(alpha=0.6) + 
  geom_qq_line()+
  facet_wrap(~variable, scales = "free") 

Df3_WR <- Data2007to2013_vis_WR[,c(1, 20:24)]
Long3_WR = melt(Df3_WR, id.vars= "Drafted")
ggplot(data = Long3_WR, aes(x = value, fill=Drafted)) + 
  geom_density(alpha=0.6) + 
  facet_wrap(~variable, scales = "free")
ggplot(data = Long3_WR, aes(sample = value, color=Drafted)) + 
  geom_qq(alpha=0.6) + 
  geom_qq_line()+
  facet_wrap(~variable, scales = "free") 

# Correlation within variables
Data2007to2013_vis_WR %>%
  filter(Drafted =="1") %>%
  select_if(is.numeric) %>%
  cor() %>%
  corrplot::corrplot()
Data2007to2013_vis_WR %>%
  filter(Drafted =="0") %>%
  select_if(is.numeric) %>%
  cor() %>%
  corrplot::corrplot()

#2 - Naive Bayes ----------

# Define features (x) and target (y) 
features_WR <- setdiff(names(Data2007to2013_WR), "Drafted")
x_WR <- Data2007to2013_WR[,features_WR]
y_WR <- Data2007to2013_WR$Drafted

# Training a naive bayes model with 10-fold cross validation
set.seed(6969)
NB_WR <- train(x_WR,y_WR,method = "nb",trControl=trainControl(method='cv',number=10))

# Predictions: 0.5 is used for probability cutoff value by default
predict_WR <- predict(NB_WR, newdata = Data2007to2013_WR)
confusionMatrix(predict_WR, Data2007to2013_WR$Drafted)

# Predictions
predict_WR <- predict(NB_WR, newdata=Data2007to2013_WR)
confusionMatrix(predict_WR, Data2007to2013_WR$Drafted)

CheckList_WR = cbind.data.frame(Data2007to2013_WR$Drafted,predict_WR)

names(CheckList_WR)[names(CheckList_WR)=="Data2007to2013_WR$Drafted"] <- "Y"
names(CheckList_WR)[names(CheckList_WR)=="predict_WR"] <- "Pred"

CheckList_WR = CheckList_WR %>%
  mutate(TP=ifelse(Y==Pred,ifelse(Pred==1,1,0),0)) %>%
  mutate(TN=ifelse(Y==Pred,ifelse(Pred==0,1,0),0)) %>%
  mutate(FP=ifelse(Y!=Pred,ifelse(Pred==1,1,0),0)) %>%
  mutate(FN=ifelse(Y!=Pred,ifelse(Pred==0,1,0),0))

# Performance Measurement
NaiveBayesPerfMeas[1,"WR_TP"] = sum(CheckList_WR$TP)
NaiveBayesPerfMeas[1,"WR_TN"] = sum(CheckList_WR$TN)
NaiveBayesPerfMeas[1,"WR_FP"] = sum(CheckList_WR$FP)
NaiveBayesPerfMeas[1,"WR_FN"] = sum(CheckList_WR$FN)

# For testing data
predict_WRTest <- predict(NB_WR,CleanClass2014_3_WR)
CheckList_WRTest = cbind.data.frame(CleanClass2014_3_WR$Drafted,predict_WRTest)

names(CheckList_WRTest)[names(CheckList_WRTest)=="CleanClass2014_3_WR$Drafted"] <- "Y"
names(CheckList_WRTest)[names(CheckList_WRTest)=="predict_WRTest"] <- "Pred"

CheckList_WRTest = CheckList_WRTest %>%
  mutate(TP=ifelse(Y==Pred,ifelse(Pred==1,1,0),0)) %>%
  mutate(TN=ifelse(Y==Pred,ifelse(Pred==0,1,0),0)) %>%
  mutate(FP=ifelse(Y!=Pred,ifelse(Pred==1,1,0),0)) %>%
  mutate(FN=ifelse(Y!=Pred,ifelse(Pred==0,1,0),0))

NaiveBayesPerfMeasTest[1,"WR_TP"] = sum(CheckList_WRTest$TP)
NaiveBayesPerfMeasTest[1,"WR_TN"] = sum(CheckList_WRTest$TN)
NaiveBayesPerfMeasTest[1,"WR_FP"] = sum(CheckList_WRTest$FP)
NaiveBayesPerfMeasTest[1,"WR_FN"] = sum(CheckList_WRTest$FN)

# IV. Naive Bayes Classifier - 07 to 13, RB ----------

#1 - Preparations and data visualization ----------

# Training data
Data2007to2013_RB <- CleanClass2007to2013_3[CleanClass2007to2013_3$Position=="RB", ]
Data2007to2013_RB <- Data2007to2013_RB %>% select(-Class, -Position, -Name, -Player.Code, -Year, 
                                                  -Safety) #these variables have zero variance

# Testing data
CleanClass2014_3_RB <-  CleanClass2014_3[CleanClass2014_3$Position=="RB", ]
CleanClass2014_3_RB <- CleanClass2014_3_RB %>% select(-Class, -Position, -Name, -Player.Code, -Year,
                                                      -Safety) #these variables have zero variance

# Density distributions
Data2007to2013_vis_RB <- Data2007to2013_RB

Df1_RB <- Data2007to2013_vis_RB[,c(1, 2:10)]
Long1_RB = melt(Df1_RB, id.vars= "Drafted")
ggplot(data = Long1_RB, aes(x = value, fill=Drafted)) + 
  geom_density(alpha=0.6) + 
  facet_wrap(~variable, scales = "free")
ggplot(data = Long1_RB, aes(sample = value, color=Drafted)) + 
  geom_qq(alpha=0.6) + 
  geom_qq_line()+
  facet_wrap(~variable, scales = "free") 

Df2_RB <- Data2007to2013_vis_RB[,c(1, 11:19)]
Long2_RB = melt(Df2_RB, id.vars= "Drafted")
ggplot(data = Long2_RB, aes(x = value, fill=Drafted)) + 
  geom_density(alpha=0.6) + 
  facet_wrap(~variable, scales = "free")
ggplot(data = Long2_RB, aes(sample = value, color=Drafted)) + 
  geom_qq(alpha=0.6) + 
  geom_qq_line()+
  facet_wrap(~variable, scales = "free") 

Df3_RB <- Data2007to2013_vis_RB[,c(1, 20:24)]
Long3_RB = melt(Df3_RB, id.vars= "Drafted")
ggplot(data = Long3_RB, aes(x = value, fill=Drafted)) + 
  geom_density(alpha=0.6) + 
  facet_wrap(~variable, scales = "free")
ggplot(data = Long3_RB, aes(sample = value, color=Drafted)) + 
  geom_qq(alpha=0.6) + 
  geom_qq_line()+
  facet_wrap(~variable, scales = "free") 

# Correlation within variables
Data2007to2013_vis_RB %>%
  filter(Drafted =="1") %>%
  select_if(is.numeric) %>%
  cor() %>%
  corrplot::corrplot()
Data2007to2013_vis_RB %>%
  filter(Drafted =="0") %>%
  select_if(is.numeric) %>%
  cor() %>%
  corrplot::corrplot()

#2 - Naive Bayes ----------

# Define features (x) and target (y) 
features_RB <- setdiff(names(Data2007to2013_RB), "Drafted")
x_RB <- Data2007to2013_RB[,features_RB]
y_RB <- Data2007to2013_RB$Drafted

# Training a naive bayes model with 10-fold cross validation
set.seed(6969)
NB_RB <- train(x_RB,y_RB,method = "nb",trControl=trainControl(method='cv',number=10))

# Predictions: 0.5 is used for probability cutoff value by default
predict_RB <- predict(NB_RB, newdata = Data2007to2013_RB)
confusionMatrix(predict_RB, Data2007to2013_RB$Drafted)

CheckList_RB = cbind.data.frame(Data2007to2013_RB$Drafted,predict_RB)

names(CheckList_RB)[names(CheckList_RB)=="Data2007to2013_RB$Drafted"] <- "Y"
names(CheckList_RB)[names(CheckList_RB)=="predict_RB"] <- "Pred"

CheckList_RB = CheckList_RB %>%
  mutate(TP=ifelse(Y==Pred,ifelse(Pred==1,1,0),0)) %>%
  mutate(TN=ifelse(Y==Pred,ifelse(Pred==0,1,0),0)) %>%
  mutate(FP=ifelse(Y!=Pred,ifelse(Pred==1,1,0),0)) %>%
  mutate(FN=ifelse(Y!=Pred,ifelse(Pred==0,1,0),0))

# Performance Measurement
NaiveBayesPerfMeas[1,"RB_TP"] = sum(CheckList_RB$TP)
NaiveBayesPerfMeas[1,"RB_TN"] = sum(CheckList_RB$TN)
NaiveBayesPerfMeas[1,"RB_FP"] = sum(CheckList_RB$FP)
NaiveBayesPerfMeas[1,"RB_FN"] = sum(CheckList_RB$FN)

# For testing data
predict_RBTest <- predict(NB_RB,CleanClass2014_3_RB)
CheckList_RBTest = cbind.data.frame(CleanClass2014_3_RB$Drafted,predict_RBTest)

names(CheckList_RBTest)[names(CheckList_RBTest)=="CleanClass2014_3_RB$Drafted"] <- "Y"
names(CheckList_RBTest)[names(CheckList_RBTest)=="predict_RBTest"] <- "Pred"

CheckList_RBTest = CheckList_RBTest %>%
  mutate(TP=ifelse(Y==Pred,ifelse(Pred==1,1,0),0)) %>%
  mutate(TN=ifelse(Y==Pred,ifelse(Pred==0,1,0),0)) %>%
  mutate(FP=ifelse(Y!=Pred,ifelse(Pred==1,1,0),0)) %>%
  mutate(FN=ifelse(Y!=Pred,ifelse(Pred==0,1,0),0))

NaiveBayesPerfMeasTest[1,"RB_TP"] = sum(CheckList_RBTest$TP)
NaiveBayesPerfMeasTest[1,"RB_TN"] = sum(CheckList_RBTest$TN)
NaiveBayesPerfMeasTest[1,"RB_FP"] = sum(CheckList_RBTest$FP)
NaiveBayesPerfMeasTest[1,"RB_FN"] = sum(CheckList_RBTest$FN)

# 2. Oversampling ###################################################
# NOTICE: Data visualization will no longer be performed. If you are interested, the above code can be used analogously.

load("../Data/CleanData/CleanClass2007to2013_3_oversampling.Rdata")

# I. Naive Bayes Classifier - 07 to 13, together ----------

#1 - Preparations  ----------

# Training data
CleanClass2007to2014_3_oversampling$Drafted <- as.factor(CleanClass2007to2014_3_oversampling$Drafted)
Data2007to2013_togOS <- CleanClass2007to2014_3_oversampling %>% select(-Position, -Class, -Name, -Player.Code, -Year, 
                                                                       -Safety) #this variable has zero variance.
# Testing data
CleanClass2014_3_tog

#2 - Naive Bayes ----------

# Define features (x) and target (y) 
features_togOS <- setdiff(names(Data2007to2013_togOS), "Drafted")
x_togOS <- Data2007to2013_togOS[,features_togOS]
y_togOS <- Data2007to2013_togOS$Drafted

# Training a naive bayes model with 10-fold cross validation
set.seed(6969)
NB_togOS <- train(x_togOS,y_togOS,method = "nb",trControl=trainControl(method='cv',number=10))

# Predictions for training data (2007 to 2013): 0.5 is used for probability cutoff value by default
predict_togOS <- predict(NB_togOS, newdata = Data2007to2013_tog)
confusionMatrix(predict_togOS, Data2007to2013_tog$Drafted)

CheckList_togOS = cbind.data.frame(Data2007to2013_tog$Drafted,predict_togOS)

names(CheckList_togOS)[names(CheckList_togOS)=="Data2007to2013_tog$Drafted"] <- "Y"
names(CheckList_togOS)[names(CheckList_togOS)=="predict_togOS"] <- "Pred"

CheckList_togOS = CheckList_togOS %>%
  mutate(TP=ifelse(Y==Pred,ifelse(Pred==1,1,0),0)) %>%
  mutate(TN=ifelse(Y==Pred,ifelse(Pred==0,1,0),0)) %>%
  mutate(FP=ifelse(Y!=Pred,ifelse(Pred==1,1,0),0)) %>%
  mutate(FN=ifelse(Y!=Pred,ifelse(Pred==0,1,0),0))

# Performance Measurement
NaiveBayesPerfMeas[2,"Together_TP"] = sum(CheckList_togOS$TP)
NaiveBayesPerfMeas[2,"Together_TN"] = sum(CheckList_togOS$TN)
NaiveBayesPerfMeas[2,"Together_FP"] = sum(CheckList_togOS$FP)
NaiveBayesPerfMeas[2,"Together_FN"] = sum(CheckList_togOS$FN)

# For testing data
predict_togOSTest <- predict(NB_togOS,CleanClass2014_3_tog)
CheckList_togOSTest = cbind.data.frame(CleanClass2014_3_tog$Drafted,predict_togOSTest)

names(CheckList_togOSTest)[names(CheckList_togOSTest)=="CleanClass2014_3_tog$Drafted"] <- "Y"
names(CheckList_togOSTest)[names(CheckList_togOSTest)=="predict_togOSTest"] <- "Pred"

CheckList_togOSTest = CheckList_togOSTest %>%
  mutate(TP=ifelse(Y==Pred,ifelse(Pred==1,1,0),0)) %>%
  mutate(TN=ifelse(Y==Pred,ifelse(Pred==0,1,0),0)) %>%
  mutate(FP=ifelse(Y!=Pred,ifelse(Pred==1,1,0),0)) %>%
  mutate(FN=ifelse(Y!=Pred,ifelse(Pred==0,1,0),0))

NaiveBayesPerfMeasTest[2,"Together_TP"] = sum(CheckList_togOSTest$TP)
NaiveBayesPerfMeasTest[2,"Together_TN"] = sum(CheckList_togOSTest$TN)
NaiveBayesPerfMeasTest[2,"Together_FP"] = sum(CheckList_togOSTest$FP)
NaiveBayesPerfMeasTest[2,"Together_FN"] = sum(CheckList_togOSTest$FN)

# II. Naive Bayes Classifier - 07 to 13, QB  ----------

#1 - Preparations  ----------

# Training data
Data2007to2013_QBOS <- CleanClass2007to2014_3_oversampling[CleanClass2007to2014_3_oversampling$Position=="QB", ]
Data2007to2013_QBOS <- Data2007to2013_QBOS %>% select(-Class, -Position, -Name, -Player.Code, -Year,
                                                      -Safety, -Kickoff.Ret.TD, -Punt.Ret.TD) #these variables have zero variance

# Testing data
CleanClass2014_3_QB      

#2 - Naive Bayes ----------

# Define features (x) and target (y) 
features_QBOS <- setdiff(names(Data2007to2013_QBOS), "Drafted")
x_QBOS <- Data2007to2013_QBOS[,features_QBOS]
y_QBOS <- Data2007to2013_QBOS$Drafted

# Training a naive bayes model with 10-fold cross validation
set.seed(6969)
NB_QBOS <- train(x_QBOS,y_QBOS,method = "nb",trControl=trainControl(method='cv',number=10))

# Predictions for training data (2007 to 2013): 0.5 is used for probability cutoff value by default
predict_QBOS <- predict(NB_QBOS, newdata = Data2007to2013_QB)
confusionMatrix(predict_QBOS, Data2007to2013_QB$Drafted)

CheckList_QBOS = cbind.data.frame(Data2007to2013_QB$Drafted,predict_QBOS)

names(CheckList_QBOS)[names(CheckList_QBOS)=="Data2007to2013_QB$Drafted"] <- "Y"
names(CheckList_QBOS)[names(CheckList_QBOS)=="predict_QBOS"] <- "Pred"

CheckList_QBOS = CheckList_QBOS %>%
  mutate(TP=ifelse(Y==Pred,ifelse(Pred==1,1,0),0)) %>%
  mutate(TN=ifelse(Y==Pred,ifelse(Pred==0,1,0),0)) %>%
  mutate(FP=ifelse(Y!=Pred,ifelse(Pred==1,1,0),0)) %>%
  mutate(FN=ifelse(Y!=Pred,ifelse(Pred==0,1,0),0))

# Performance Measurement
NaiveBayesPerfMeas[2,"QB_TP"] = sum(CheckList_QBOS$TP)
NaiveBayesPerfMeas[2,"QB_TN"] = sum(CheckList_QBOS$TN)
NaiveBayesPerfMeas[2,"QB_FP"] = sum(CheckList_QBOS$FP)
NaiveBayesPerfMeas[2,"QB_FN"] = sum(CheckList_QBOS$FN)

# For testing data
predict_QBOSTest <- predict(NB_QBOS,CleanClass2014_3_QB)
CheckList_QBOSTest = cbind.data.frame(CleanClass2014_3_QB$Drafted,predict_QBOSTest)

names(CheckList_QBOSTest)[names(CheckList_QBOSTest)=="CleanClass2014_3_QB$Drafted"] <- "Y"
names(CheckList_QBOSTest)[names(CheckList_QBOSTest)=="predict_QBOSTest"] <- "Pred"

CheckList_QBOSTest = CheckList_QBOSTest %>%
  mutate(TP=ifelse(Y==Pred,ifelse(Pred==1,1,0),0)) %>%
  mutate(TN=ifelse(Y==Pred,ifelse(Pred==0,1,0),0)) %>%
  mutate(FP=ifelse(Y!=Pred,ifelse(Pred==1,1,0),0)) %>%
  mutate(FN=ifelse(Y!=Pred,ifelse(Pred==0,1,0),0))

NaiveBayesPerfMeasTest[2,"QB_TP"] = sum(CheckList_QBOSTest$TP)
NaiveBayesPerfMeasTest[2,"QB_TN"] = sum(CheckList_QBOSTest$TN)
NaiveBayesPerfMeasTest[2,"QB_FP"] = sum(CheckList_QBOSTest$FP)
NaiveBayesPerfMeasTest[2,"QB_FN"] = sum(CheckList_QBOSTest$FN)

# III. Naive Bayes Classifier - 07 to 13, WR ----------

#1 - Preparations ----------

# Training data
Data2007to2013_WROS <- CleanClass2007to2014_3_oversampling[CleanClass2007to2014_3_oversampling$Position=="WR", ]
Data2007to2013_WROS <- Data2007to2013_WROS %>% select(-Class, -Position, -Name, -Player.Code, -Year,
                                                      -Safety) #these variables have zero variance.

# Testing data
CleanClass2014_3_WR

#2 - Naive Bayes ----------

# Define features (x) and target (y) 
features_WROS <- setdiff(names(Data2007to2013_WROS), "Drafted")
x_WROS <- Data2007to2013_WROS[,features_WROS]
y_WROS <- Data2007to2013_WROS$Drafted

# Training a naive bayes model with 10-fold cross validation
set.seed(6969)
NB_WROS <- train(x_WROS,y_WROS,method = "nb",trControl=trainControl(method='cv',number=10))

# Predictions: 0.5 is used for probability cutoff value by default
predict_WROS <- predict(NB_WROS, newdata = Data2007to2013_WR)
confusionMatrix(predict_WROS, Data2007to2013_WR$Drafted)

CheckList_WROS = cbind.data.frame(Data2007to2013_WR$Drafted,predict_WROS)

names(CheckList_WROS)[names(CheckList_WROS)=="Data2007to2013_WR$Drafted"] <- "Y"
names(CheckList_WROS)[names(CheckList_WROS)=="predict_WROS"] <- "Pred"

CheckList_WROS = CheckList_WROS %>%
  mutate(TP=ifelse(Y==Pred,ifelse(Pred==1,1,0),0)) %>%
  mutate(TN=ifelse(Y==Pred,ifelse(Pred==0,1,0),0)) %>%
  mutate(FP=ifelse(Y!=Pred,ifelse(Pred==1,1,0),0)) %>%
  mutate(FN=ifelse(Y!=Pred,ifelse(Pred==0,1,0),0))

# Performance Measurement
NaiveBayesPerfMeas[2,"WR_TP"] = sum(CheckList_WROS$TP)
NaiveBayesPerfMeas[2,"WR_TN"] = sum(CheckList_WROS$TN)
NaiveBayesPerfMeas[2,"WR_FP"] = sum(CheckList_WROS$FP)
NaiveBayesPerfMeas[2,"WR_FN"] = sum(CheckList_WROS$FN)

# For testing data
predict_WROSTest <- predict(NB_WROS,CleanClass2014_3_WR)
CheckList_WROSTest = cbind.data.frame(CleanClass2014_3_WR$Drafted,predict_WROSTest)

names(CheckList_WROSTest)[names(CheckList_WROSTest)=="CleanClass2014_3_WR$Drafted"] <- "Y"
names(CheckList_WROSTest)[names(CheckList_WROSTest)=="predict_WROSTest"] <- "Pred"

CheckList_WROSTest = CheckList_WROSTest %>%
  mutate(TP=ifelse(Y==Pred,ifelse(Pred==1,1,0),0)) %>%
  mutate(TN=ifelse(Y==Pred,ifelse(Pred==0,1,0),0)) %>%
  mutate(FP=ifelse(Y!=Pred,ifelse(Pred==1,1,0),0)) %>%
  mutate(FN=ifelse(Y!=Pred,ifelse(Pred==0,1,0),0))

NaiveBayesPerfMeasTest[2,"WR_TP"] = sum(CheckList_WROSTest$TP)
NaiveBayesPerfMeasTest[2,"WR_TN"] = sum(CheckList_WROSTest$TN)
NaiveBayesPerfMeasTest[2,"WR_FP"] = sum(CheckList_WROSTest$FP)
NaiveBayesPerfMeasTest[2,"WR_FN"] = sum(CheckList_WROSTest$FN)

# IV. Naive Bayes Classifier - 07 to 13, RB ----------
#1 - Preparations ----------

# Training data
Data2007to2013_RBOS <- CleanClass2007to2014_3_oversampling[CleanClass2007to2014_3_oversampling$Position=="RB", ]
Data2007to2013_RBOS <- Data2007to2013_RBOS %>% select(-Class, -Position, -Name, -Player.Code, -Year, 
                                                      -Safety) #these variables have zero variance.

# Testing data
CleanClass2014_3_RB

#2 - Naive Bayes ----------

# Define features (x) and target (y) 
features_RBOS <- setdiff(names(Data2007to2013_RBOS), "Drafted")
x_RBOS <- Data2007to2013_RBOS[,features_RBOS]
y_RBOS <- Data2007to2013_RBOS$Drafted

# Training a naive bayes model with 10-fold cross validation
set.seed(6969)
NB_RBOS <- train(x_RBOS,y_RBOS,method = "nb",trControl=trainControl(method='cv',number=10))

# Predictions: 0.5 is used for probability cutoff value by default
predict_RBOS <- predict(NB_RBOS, newdata = Data2007to2013_RB)
confusionMatrix(predict_RBOS, Data2007to2013_RB$Drafted)

CheckList_RBOS = cbind.data.frame(Data2007to2013_RB$Drafted,predict_RBOS)

names(CheckList_RBOS)[names(CheckList_RBOS)=="Data2007to2013_RB$Drafted"] <- "Y"
names(CheckList_RBOS)[names(CheckList_RBOS)=="predict_RBOS"] <- "Pred"

CheckList_RBOS = CheckList_RBOS %>%
  mutate(TP=ifelse(Y==Pred,ifelse(Pred==1,1,0),0)) %>%
  mutate(TN=ifelse(Y==Pred,ifelse(Pred==0,1,0),0)) %>%
  mutate(FP=ifelse(Y!=Pred,ifelse(Pred==1,1,0),0)) %>%
  mutate(FN=ifelse(Y!=Pred,ifelse(Pred==0,1,0),0))

# Performance Measurement
NaiveBayesPerfMeas[2,"RB_TP"] = sum(CheckList_RBOS$TP)
NaiveBayesPerfMeas[2,"RB_TN"] = sum(CheckList_RBOS$TN)
NaiveBayesPerfMeas[2,"RB_FP"] = sum(CheckList_RBOS$FP)
NaiveBayesPerfMeas[2,"RB_FN"] = sum(CheckList_RBOS$FN)

# For testing data
predict_RBOSTest <- predict(NB_RBOS,CleanClass2014_3_RB)
CheckList_RBOSTest = cbind.data.frame(CleanClass2014_3_RB$Drafted,predict_RBOSTest)

names(CheckList_RBOSTest)[names(CheckList_RBOSTest)=="CleanClass2014_3_RB$Drafted"] <- "Y"
names(CheckList_RBOSTest)[names(CheckList_RBOSTest)=="predict_RBOSTest"] <- "Pred"

CheckList_RBOSTest = CheckList_RBOSTest %>%
  mutate(TP=ifelse(Y==Pred,ifelse(Pred==1,1,0),0)) %>%
  mutate(TN=ifelse(Y==Pred,ifelse(Pred==0,1,0),0)) %>%
  mutate(FP=ifelse(Y!=Pred,ifelse(Pred==1,1,0),0)) %>%
  mutate(FN=ifelse(Y!=Pred,ifelse(Pred==0,1,0),0))

NaiveBayesPerfMeasTest[2,"RB_TP"] = sum(CheckList_RBOSTest$TP)
NaiveBayesPerfMeasTest[2,"RB_TN"] = sum(CheckList_RBOSTest$TN)
NaiveBayesPerfMeasTest[2,"RB_FP"] = sum(CheckList_RBOSTest$FP)
NaiveBayesPerfMeasTest[2,"RB_FN"] = sum(CheckList_RBOSTest$FN)

# 3. Undersampling ###################################################

load("../Data/CleanData/CleanClass2007to2013_3_undersampling.Rdata")

# I. Naive Bayes Classifier - 07 to 13, together ----------

#1 - Preparations  ----------

# Training data
CleanClass2007to2014_3_undersampling$Drafted <- as.factor(CleanClass2007to2014_3_undersampling$Drafted)
Data2007to2013_togUS <- CleanClass2007to2014_3_undersampling %>% select(-Position, -Class, -Name, -Player.Code, -Year, 
                                                                        -Safety) #this variable has zero variance.
# Testing data
CleanClass2014_3_tog

#2 - Naive Bayes ----------

# Define features (x) and target (y) 
features_togUS <- setdiff(names(Data2007to2013_togUS), "Drafted")
x_togUS <- Data2007to2013_togUS[,features_togUS]
y_togUS <- Data2007to2013_togUS$Drafted

# Training a naive bayes model with 10-fold cross validation
set.seed(6969)
NB_togUS <- train(x_togUS,y_togUS,method = "nb",trControl=trainControl(method='cv',number=10))

# Predictions for training data (2007 to 2013): 0.5 is used for probability cutoff value by default
predict_togUS <- predict(NB_togUS, newdata = Data2007to2013_tog)
confusionMatrix(predict_togUS, Data2007to2013_tog$Drafted)

CheckList_togUS = cbind.data.frame(Data2007to2013_tog$Drafted,predict_togUS)

names(CheckList_togUS)[names(CheckList_togUS)=="Data2007to2013_tog$Drafted"] <- "Y"
names(CheckList_togUS)[names(CheckList_togUS)=="predict_togUS"] <- "Pred"

CheckList_togUS = CheckList_togUS %>%
  mutate(TP=ifelse(Y==Pred,ifelse(Pred==1,1,0),0)) %>%
  mutate(TN=ifelse(Y==Pred,ifelse(Pred==0,1,0),0)) %>%
  mutate(FP=ifelse(Y!=Pred,ifelse(Pred==1,1,0),0)) %>%
  mutate(FN=ifelse(Y!=Pred,ifelse(Pred==0,1,0),0))

# Performance Measurement
NaiveBayesPerfMeas[3,"Together_TP"] = sum(CheckList_togUS$TP)
NaiveBayesPerfMeas[3,"Together_TN"] = sum(CheckList_togUS$TN)
NaiveBayesPerfMeas[3,"Together_FP"] = sum(CheckList_togUS$FP)
NaiveBayesPerfMeas[3,"Together_FN"] = sum(CheckList_togUS$FN)

# For testing data
predict_togUSTest <- predict(NB_togUS,CleanClass2014_3_tog)
CheckList_togUSTest = cbind.data.frame(CleanClass2014_3_tog$Drafted,predict_togUSTest)

names(CheckList_togUSTest)[names(CheckList_togUSTest)=="CleanClass2014_3_tog$Drafted"] <- "Y"
names(CheckList_togUSTest)[names(CheckList_togUSTest)=="predict_togUSTest"] <- "Pred"

CheckList_togUSTest = CheckList_togUSTest %>%
  mutate(TP=ifelse(Y==Pred,ifelse(Pred==1,1,0),0)) %>%
  mutate(TN=ifelse(Y==Pred,ifelse(Pred==0,1,0),0)) %>%
  mutate(FP=ifelse(Y!=Pred,ifelse(Pred==1,1,0),0)) %>%
  mutate(FN=ifelse(Y!=Pred,ifelse(Pred==0,1,0),0))

NaiveBayesPerfMeasTest[3,"Together_TP"] = sum(CheckList_togUSTest$TP)
NaiveBayesPerfMeasTest[3,"Together_TN"] = sum(CheckList_togUSTest$TN)
NaiveBayesPerfMeasTest[3,"Together_FP"] = sum(CheckList_togUSTest$FP)
NaiveBayesPerfMeasTest[3,"Together_FN"] = sum(CheckList_togUSTest$FN)

# II. Naive Bayes Classifier - 07 to 13, QB  ----------

#1 - Preparations  ----------

# Training data
Data2007to2013_QBUS <- CleanClass2007to2014_3_undersampling[CleanClass2007to2014_3_undersampling$Position=="QB", ]
Data2007to2013_QBUS <- Data2007to2013_QBUS %>% select(-Class, -Position, -Name, -Player.Code, -Year,
                                                      -Safety, -Kickoff.Ret.TD, -Punt.Ret.TD) #these variables have zero variance.

# Testing data
CleanClass2014_3_QB      

#2 - Naive Bayes ----------

# Define features (x) and target (y) 
features_QBUS <- setdiff(names(Data2007to2013_QBUS), "Drafted")
x_QBUS <- Data2007to2013_QBUS[,features_QBUS]
y_QBUS <- Data2007to2013_QBUS$Drafted

# Training a naive bayes model with 10-fold cross validation
set.seed(6969)
NB_QBUS <- train(x_QBUS,y_QBUS,method = "nb",trControl=trainControl(method='cv',number=10))

# Predictions for training data (2007 to 2013): 0.5 is used for probability cutoff value by default
predict_QBUS <- predict(NB_QBUS, newdata = Data2007to2013_QB)
confusionMatrix(predict_QBUS, Data2007to2013_QB$Drafted)

CheckList_QBUS = cbind.data.frame(Data2007to2013_QB$Drafted,predict_QBUS)

names(CheckList_QBUS)[names(CheckList_QBUS)=="Data2007to2013_QB$Drafted"] <- "Y"
names(CheckList_QBUS)[names(CheckList_QBUS)=="predict_QBUS"] <- "Pred"

CheckList_QBUS = CheckList_QBUS %>%
  mutate(TP=ifelse(Y==Pred,ifelse(Pred==1,1,0),0)) %>%
  mutate(TN=ifelse(Y==Pred,ifelse(Pred==0,1,0),0)) %>%
  mutate(FP=ifelse(Y!=Pred,ifelse(Pred==1,1,0),0)) %>%
  mutate(FN=ifelse(Y!=Pred,ifelse(Pred==0,1,0),0))

# Performance Measurement
NaiveBayesPerfMeas[3,"QB_TP"] = sum(CheckList_QBUS$TP)
NaiveBayesPerfMeas[3,"QB_TN"] = sum(CheckList_QBUS$TN)
NaiveBayesPerfMeas[3,"QB_FP"] = sum(CheckList_QBUS$FP)
NaiveBayesPerfMeas[3,"QB_FN"] = sum(CheckList_QBUS$FN)

# For testing data
predict_QBUSTest <- predict(NB_QBUS,CleanClass2014_3_QB)
CheckList_QBUSTest = cbind.data.frame(CleanClass2014_3_QB$Drafted,predict_QBUSTest)

names(CheckList_QBUSTest)[names(CheckList_QBUSTest)=="CleanClass2014_3_QB$Drafted"] <- "Y"
names(CheckList_QBUSTest)[names(CheckList_QBUSTest)=="predict_QBUSTest"] <- "Pred"

CheckList_QBUSTest = CheckList_QBUSTest %>%
  mutate(TP=ifelse(Y==Pred,ifelse(Pred==1,1,0),0)) %>%
  mutate(TN=ifelse(Y==Pred,ifelse(Pred==0,1,0),0)) %>%
  mutate(FP=ifelse(Y!=Pred,ifelse(Pred==1,1,0),0)) %>%
  mutate(FN=ifelse(Y!=Pred,ifelse(Pred==0,1,0),0))

NaiveBayesPerfMeasTest[3,"QB_TP"] = sum(CheckList_QBUSTest$TP)
NaiveBayesPerfMeasTest[3,"QB_TN"] = sum(CheckList_QBUSTest$TN)
NaiveBayesPerfMeasTest[3,"QB_FP"] = sum(CheckList_QBUSTest$FP)
NaiveBayesPerfMeasTest[3,"QB_FN"] = sum(CheckList_QBUSTest$FN)


# III. Naive Bayes Classifier - 07 to 13, WR ----------

#1 - Preparations ----------

# Training data
Data2007to2013_WRUS <- CleanClass2007to2014_3_undersampling[CleanClass2007to2014_3_undersampling$Position=="WR", ]
Data2007to2013_WRUS <- Data2007to2013_WRUS %>% select(-Class, -Position, -Name, -Player.Code, -Year,
                                                      -Safety) #these variables have zero variance

# Testing data
CleanClass2014_3_WR

#2 - Naive Bayes ----------

# Define features (x) and target (y) 
features_WRUS <- setdiff(names(Data2007to2013_WRUS), "Drafted")
x_WRUS <- Data2007to2013_WRUS[,features_WRUS]
y_WRUS <- Data2007to2013_WRUS$Drafted

# Training a naive bayes model with 10-fold cross validation
set.seed(6969)
NB_WRUS <- train(x_WRUS,y_WRUS,method = "nb",trControl=trainControl(method='cv',number=10))

# Predictions: 0.5 is used for probability cutoff value by default
predict_WRUS <- predict(NB_WRUS, newdata = Data2007to2013_WR)
confusionMatrix(predict_WRUS, Data2007to2013_WR$Drafted)

CheckList_WRUS = cbind.data.frame(Data2007to2013_WR$Drafted,predict_WRUS)

names(CheckList_WRUS)[names(CheckList_WRUS)=="Data2007to2013_WR$Drafted"] <- "Y"
names(CheckList_WRUS)[names(CheckList_WRUS)=="predict_WRUS"] <- "Pred"

CheckList_WRUS = CheckList_WRUS %>%
  mutate(TP=ifelse(Y==Pred,ifelse(Pred==1,1,0),0)) %>%
  mutate(TN=ifelse(Y==Pred,ifelse(Pred==0,1,0),0)) %>%
  mutate(FP=ifelse(Y!=Pred,ifelse(Pred==1,1,0),0)) %>%
  mutate(FN=ifelse(Y!=Pred,ifelse(Pred==0,1,0),0))

# Performance Measurement
NaiveBayesPerfMeas[3,"WR_TP"] = sum(CheckList_WRUS$TP)
NaiveBayesPerfMeas[3,"WR_TN"] = sum(CheckList_WRUS$TN)
NaiveBayesPerfMeas[3,"WR_FP"] = sum(CheckList_WRUS$FP)
NaiveBayesPerfMeas[3,"WR_FN"] = sum(CheckList_WRUS$FN)

# For testing data
predict_WRUSTest <- predict(NB_WRUS,CleanClass2014_3_WR)
CheckList_WRUSTest = cbind.data.frame(CleanClass2014_3_WR$Drafted,predict_WRUSTest)

names(CheckList_WRUSTest)[names(CheckList_WRUSTest)=="CleanClass2014_3_WR$Drafted"] <- "Y"
names(CheckList_WRUSTest)[names(CheckList_WRUSTest)=="predict_WRUSTest"] <- "Pred"

CheckList_WRUSTest = CheckList_WRUSTest %>%
  mutate(TP=ifelse(Y==Pred,ifelse(Pred==1,1,0),0)) %>%
  mutate(TN=ifelse(Y==Pred,ifelse(Pred==0,1,0),0)) %>%
  mutate(FP=ifelse(Y!=Pred,ifelse(Pred==1,1,0),0)) %>%
  mutate(FN=ifelse(Y!=Pred,ifelse(Pred==0,1,0),0))

NaiveBayesPerfMeasTest[3,"WR_TP"] = sum(CheckList_WRUSTest$TP)
NaiveBayesPerfMeasTest[3,"WR_TN"] = sum(CheckList_WRUSTest$TN)
NaiveBayesPerfMeasTest[3,"WR_FP"] = sum(CheckList_WRUSTest$FP)
NaiveBayesPerfMeasTest[3,"WR_FN"] = sum(CheckList_WRUSTest$FN)

# IV. Naive Bayes Classifier - 07 to 13, RB ----------

#1 - Preparations ----------

# Training data
Data2007to2013_RBUS <- CleanClass2007to2014_3_undersampling[CleanClass2007to2014_3_undersampling$Position=="RB", ]
Data2007to2013_RBUS <- Data2007to2013_RBUS %>% select(-Class, -Position, -Name, -Player.Code, -Year,
                                                      -Safety) #these variables have zero variance

# Testing data
CleanClass2014_3_RB

#2 - Naive Bayes ----------

# Define features (x) and target (y) 
features_RBUS <- setdiff(names(Data2007to2013_RBUS), "Drafted")
x_RBUS <- Data2007to2013_RBUS[,features_RBUS]
y_RBUS <- Data2007to2013_RBUS$Drafted

# Training a naive bayes model with 10-fold cross validation
set.seed(6969)
NB_RBUS <- train(x_RBUS,y_RBUS,method = "nb",trControl=trainControl(method='cv',number=10))

# Predictions: 0.5 is used for probability cutoff value by default
predict_RBUS <- predict(NB_RBUS, newdata = Data2007to2013_RB)
confusionMatrix(predict_RBUS, Data2007to2013_RB$Drafted)

CheckList_RBUS = cbind.data.frame(Data2007to2013_RB$Drafted,predict_RBUS)

names(CheckList_RBUS)[names(CheckList_RBUS)=="Data2007to2013_RB$Drafted"] <- "Y"
names(CheckList_RBUS)[names(CheckList_RBUS)=="predict_RBUS"] <- "Pred"

CheckList_RBUS = CheckList_RBUS %>%
  mutate(TP=ifelse(Y==Pred,ifelse(Pred==1,1,0),0)) %>%
  mutate(TN=ifelse(Y==Pred,ifelse(Pred==0,1,0),0)) %>%
  mutate(FP=ifelse(Y!=Pred,ifelse(Pred==1,1,0),0)) %>%
  mutate(FN=ifelse(Y!=Pred,ifelse(Pred==0,1,0),0))

# Performance Measurement
NaiveBayesPerfMeas[3,"RB_TP"] = sum(CheckList_RBUS$TP)
NaiveBayesPerfMeas[3,"RB_TN"] = sum(CheckList_RBUS$TN)
NaiveBayesPerfMeas[3,"RB_FP"] = sum(CheckList_RBUS$FP)
NaiveBayesPerfMeas[3,"RB_FN"] = sum(CheckList_RBUS$FN)

# For testing data
predict_RBUSTest <- predict(NB_RBUS,CleanClass2014_3_RB)
CheckList_RBUSTest = cbind.data.frame(CleanClass2014_3_RB$Drafted,predict_RBUSTest)

names(CheckList_RBUSTest)[names(CheckList_RBUSTest)=="CleanClass2014_3_RB$Drafted"] <- "Y"
names(CheckList_RBUSTest)[names(CheckList_RBUSTest)=="predict_RBUSTest"] <- "Pred"

CheckList_RBUSTest = CheckList_RBUSTest %>%
  mutate(TP=ifelse(Y==Pred,ifelse(Pred==1,1,0),0)) %>%
  mutate(TN=ifelse(Y==Pred,ifelse(Pred==0,1,0),0)) %>%
  mutate(FP=ifelse(Y!=Pred,ifelse(Pred==1,1,0),0)) %>%
  mutate(FN=ifelse(Y!=Pred,ifelse(Pred==0,1,0),0))

NaiveBayesPerfMeasTest[3,"RB_TP"] = sum(CheckList_RBUSTest$TP)
NaiveBayesPerfMeasTest[3,"RB_TN"] = sum(CheckList_RBUSTest$TN)
NaiveBayesPerfMeasTest[3,"RB_FP"] = sum(CheckList_RBUSTest$FP)
NaiveBayesPerfMeasTest[3,"RB_FN"] = sum(CheckList_RBUSTest$FN)

# 4. Rose_both ###################################################

load("../Data/CleanData/CleanClass2007to2013_3_Rose.both.Rdata")

# I. Naive Bayes Classifier - 07 to 13, together ----------

#1 - Preparations  ----------

# Training data
CleanClass2007to2014_3_Rose.both$Drafted <- as.factor(CleanClass2007to2014_3_Rose.both$Drafted)
Data2007to2013_togBO <- CleanClass2007to2014_3_Rose.both %>% select(-Position, -Class, -Name, -Player.Code, -Year, 
                                                                    -Safety) #this variable has zero variance
# Testing data
CleanClass2014_3_tog

#2 - Naive Bayes ----------

# Define features (x) and target (y) 
features_togBO <- setdiff(names(Data2007to2013_togBO), "Drafted")
x_togBO <- Data2007to2013_togBO[,features_togBO]
y_togBO <- Data2007to2013_togBO$Drafted

# Training a naive bayes model with 10-fold cross validation
set.seed(6969)
NB_togBO <- train(x_togBO,y_togBO,method = "nb",trControl=trainControl(method='cv',number=10))

# Predictions for training data (2007 to 2013): 0.5 is used for probability cutoff value by default
predict_togBO <- predict(NB_togBO, newdata = Data2007to2013_tog)
confusionMatrix(predict_togBO, Data2007to2013_tog$Drafted)


CheckList_togBO = cbind.data.frame(Data2007to2013_tog$Drafted,predict_togBO)

names(CheckList_togBO)[names(CheckList_togBO)=="Data2007to2013_tog$Drafted"] <- "Y"
names(CheckList_togBO)[names(CheckList_togBO)=="predict_togBO"] <- "Pred"

CheckList_togBO = CheckList_togBO %>%
  mutate(TP=ifelse(Y==Pred,ifelse(Pred==1,1,0),0)) %>%
  mutate(TN=ifelse(Y==Pred,ifelse(Pred==0,1,0),0)) %>%
  mutate(FP=ifelse(Y!=Pred,ifelse(Pred==1,1,0),0)) %>%
  mutate(FN=ifelse(Y!=Pred,ifelse(Pred==0,1,0),0))

# Performance Measurement
NaiveBayesPerfMeas[4,"Together_TP"] = sum(CheckList_togBO$TP)
NaiveBayesPerfMeas[4,"Together_TN"] = sum(CheckList_togBO$TN)
NaiveBayesPerfMeas[4,"Together_FP"] = sum(CheckList_togBO$FP)
NaiveBayesPerfMeas[4,"Together_FN"] = sum(CheckList_togBO$FN)

# For testing data
predict_togBOTest <- predict(NB_togBO,CleanClass2014_3_tog)
CheckList_togBOTest = cbind.data.frame(CleanClass2014_3_tog$Drafted,predict_togBOTest)

names(CheckList_togBOTest)[names(CheckList_togBOTest)=="CleanClass2014_3_tog$Drafted"] <- "Y"
names(CheckList_togBOTest)[names(CheckList_togBOTest)=="predict_togBOTest"] <- "Pred"

CheckList_togBOTest = CheckList_togBOTest %>%
  mutate(TP=ifelse(Y==Pred,ifelse(Pred==1,1,0),0)) %>%
  mutate(TN=ifelse(Y==Pred,ifelse(Pred==0,1,0),0)) %>%
  mutate(FP=ifelse(Y!=Pred,ifelse(Pred==1,1,0),0)) %>%
  mutate(FN=ifelse(Y!=Pred,ifelse(Pred==0,1,0),0))

NaiveBayesPerfMeasTest[4,"Together_TP"] = sum(CheckList_togBOTest$TP)
NaiveBayesPerfMeasTest[4,"Together_TN"] = sum(CheckList_togBOTest$TN)
NaiveBayesPerfMeasTest[4,"Together_FP"] = sum(CheckList_togBOTest$FP)
NaiveBayesPerfMeasTest[4,"Together_FN"] = sum(CheckList_togBOTest$FN)

# II. Naive Bayes Classifier - 07 to 13, QB  ----------

#1 - Preparations  ----------

# Training data
Data2007to2013_QBBO <- CleanClass2007to2014_3_Rose.both[CleanClass2007to2014_3_Rose.both$Position=="QB", ]
Data2007to2013_QBBO <- Data2007to2013_QBBO %>% select(-Class, -Position, -Name, -Player.Code, -Year,
                                                      -Safety, -Kickoff.Ret.TD, -Punt.Ret.TD) #these variables have zero variance

# Testing data
CleanClass2014_3_QB      

#2 - Naive Bayes ----------

# Define features (x) and target (y) 
features_QBBO <- setdiff(names(Data2007to2013_QBBO), "Drafted")
x_QBBO <- Data2007to2013_QBBO[,features_QBBO]
y_QBBO <- Data2007to2013_QBBO$Drafted

# Training a naive bayes model with 10-fold cross validation
set.seed(6969)
NB_QBBO <- train(x_QBBO,y_QBBO,method = "nb",trControl=trainControl(method='cv',number=10))

# Predictions for training data (2007 to 2013): 0.5 is used for probability cutoff value by default
predict_QBBO <- predict(NB_QBBO, newdata = Data2007to2013_QB)
confusionMatrix(predict_QBBO, Data2007to2013_QB$Drafted)

CheckList_QBBO = cbind.data.frame(Data2007to2013_QB$Drafted,predict_QBBO)

names(CheckList_QBBO)[names(CheckList_QBBO)=="Data2007to2013_QB$Drafted"] <- "Y"
names(CheckList_QBBO)[names(CheckList_QBBO)=="predict_QBBO"] <- "Pred"

CheckList_QBBO = CheckList_QBBO %>%
  mutate(TP=ifelse(Y==Pred,ifelse(Pred==1,1,0),0)) %>%
  mutate(TN=ifelse(Y==Pred,ifelse(Pred==0,1,0),0)) %>%
  mutate(FP=ifelse(Y!=Pred,ifelse(Pred==1,1,0),0)) %>%
  mutate(FN=ifelse(Y!=Pred,ifelse(Pred==0,1,0),0))

# Performance Measurement
NaiveBayesPerfMeas[4,"QB_TP"] = sum(CheckList_QBBO$TP)
NaiveBayesPerfMeas[4,"QB_TN"] = sum(CheckList_QBBO$TN)
NaiveBayesPerfMeas[4,"QB_FP"] = sum(CheckList_QBBO$FP)
NaiveBayesPerfMeas[4,"QB_FN"] = sum(CheckList_QBBO$FN)

# For testing data
predict_QBBOTest <- predict(NB_QBBO,CleanClass2014_3_QB)
CheckList_QBBOTest = cbind.data.frame(CleanClass2014_3_QB$Drafted,predict_QBBOTest)

names(CheckList_QBBOTest)[names(CheckList_QBBOTest)=="CleanClass2014_3_QB$Drafted"] <- "Y"
names(CheckList_QBBOTest)[names(CheckList_QBBOTest)=="predict_QBBOTest"] <- "Pred"

CheckList_QBBOTest = CheckList_QBBOTest %>%
  mutate(TP=ifelse(Y==Pred,ifelse(Pred==1,1,0),0)) %>%
  mutate(TN=ifelse(Y==Pred,ifelse(Pred==0,1,0),0)) %>%
  mutate(FP=ifelse(Y!=Pred,ifelse(Pred==1,1,0),0)) %>%
  mutate(FN=ifelse(Y!=Pred,ifelse(Pred==0,1,0),0))

NaiveBayesPerfMeasTest[4,"QB_TP"] = sum(CheckList_QBBOTest$TP)
NaiveBayesPerfMeasTest[4,"QB_TN"] = sum(CheckList_QBBOTest$TN)
NaiveBayesPerfMeasTest[4,"QB_FP"] = sum(CheckList_QBBOTest$FP)
NaiveBayesPerfMeasTest[4,"QB_FN"] = sum(CheckList_QBBOTest$FN)

# III. Naive Bayes Classifier - 07 to 13, WR ----------

#1 - Preparations ----------

# Training data
Data2007to2013_WRBO <- CleanClass2007to2014_3_Rose.both[CleanClass2007to2014_3_Rose.both$Position=="WR", ]
Data2007to2013_WRBO <- Data2007to2013_WRBO %>% select(-Class, -Position, -Name, -Player.Code, -Year, 
                                                      -Safety) #these variables have zero variance

# Testing data
CleanClass2014_3_WR

#2 - Naive Bayes ----------

# Define features (x) and target (y) 
features_WRBO <- setdiff(names(Data2007to2013_WRBO), "Drafted")
x_WRBO <- Data2007to2013_WRBO[,features_WRBO]
y_WRBO <- Data2007to2013_WRBO$Drafted

# Training a naive bayes model with 10-fold cross validation
set.seed(6969)
NB_WRBO <- train(x_WRBO,y_WRBO,method = "nb",trControl=trainControl(method='cv',number=10))

# Predictions: 0.5 is used for probability cutoff value by default
predict_WRBO <- predict(NB_WRBO, newdata = Data2007to2013_WR)
confusionMatrix(predict_WRBO, Data2007to2013_WR$Drafted)

CheckList_WRBO = cbind.data.frame(Data2007to2013_WR$Drafted,predict_WRBO)

names(CheckList_WRBO)[names(CheckList_WRBO)=="Data2007to2013_WR$Drafted"] <- "Y"
names(CheckList_WRBO)[names(CheckList_WRBO)=="predict_WRBO"] <- "Pred"

CheckList_WRBO = CheckList_WRBO %>%
  mutate(TP=ifelse(Y==Pred,ifelse(Pred==1,1,0),0)) %>%
  mutate(TN=ifelse(Y==Pred,ifelse(Pred==0,1,0),0)) %>%
  mutate(FP=ifelse(Y!=Pred,ifelse(Pred==1,1,0),0)) %>%
  mutate(FN=ifelse(Y!=Pred,ifelse(Pred==0,1,0),0))

# Performance Measurement
NaiveBayesPerfMeas[4,"WR_TP"] = sum(CheckList_WRBO$TP)
NaiveBayesPerfMeas[4,"WR_TN"] = sum(CheckList_WRBO$TN)
NaiveBayesPerfMeas[4,"WR_FP"] = sum(CheckList_WRBO$FP)
NaiveBayesPerfMeas[4,"WR_FN"] = sum(CheckList_WRBO$FN)

# For testing data
predict_WRBOTest <- predict(NB_WRBO,CleanClass2014_3_WR)
CheckList_WRBOTest = cbind.data.frame(CleanClass2014_3_WR$Drafted,predict_WRBOTest)

names(CheckList_WRBOTest)[names(CheckList_WRBOTest)=="CleanClass2014_3_WR$Drafted"] <- "Y"
names(CheckList_WRBOTest)[names(CheckList_WRBOTest)=="predict_WRBOTest"] <- "Pred"

CheckList_WRBOTest = CheckList_WRBOTest %>%
  mutate(TP=ifelse(Y==Pred,ifelse(Pred==1,1,0),0)) %>%
  mutate(TN=ifelse(Y==Pred,ifelse(Pred==0,1,0),0)) %>%
  mutate(FP=ifelse(Y!=Pred,ifelse(Pred==1,1,0),0)) %>%
  mutate(FN=ifelse(Y!=Pred,ifelse(Pred==0,1,0),0))

NaiveBayesPerfMeasTest[4,"WR_TP"] = sum(CheckList_WRBOTest$TP)
NaiveBayesPerfMeasTest[4,"WR_TN"] = sum(CheckList_WRBOTest$TN)
NaiveBayesPerfMeasTest[4,"WR_FP"] = sum(CheckList_WRBOTest$FP)
NaiveBayesPerfMeasTest[4,"WR_FN"] = sum(CheckList_WRBOTest$FN)


# IV. Naive Bayes Classifier - 07 to 13, RB ----------

#1 - Preparations ----------

# Training data
Data2007to2013_RBBO <- CleanClass2007to2014_3_Rose.both[CleanClass2007to2014_3_Rose.both$Position=="RB", ]
Data2007to2013_RBBO <- Data2007to2013_RBBO %>% select(-Class, -Position, -Name, -Player.Code, -Year,
                                                      -Safety) #these variables have zero variance.

# Testing data
CleanClass2014_3_RB

#2 - Naive Bayes ----------

# Define features (x) and target (y) 
features_RBBO <- setdiff(names(Data2007to2013_RBBO), "Drafted")
x_RBBO <- Data2007to2013_RBBO[,features_RBBO]
y_RBBO <- Data2007to2013_RBBO$Drafted

# Training a naive bayes model with 10-fold cross validation
set.seed(6969)
NB_RBBO <- train(x_RBBO,y_RBBO,method = "nb",trControl=trainControl(method='cv',number=10))

# Predictions: 0.5 is used for probability cutoff value by default
predict_RBBO <- predict(NB_RBBO, newdata = Data2007to2013_RB)
confusionMatrix(predict_RBBO, Data2007to2013_RB$Drafted)

CheckList_RBBO = cbind.data.frame(Data2007to2013_RB$Drafted,predict_RBBO)

names(CheckList_RBBO)[names(CheckList_RBBO)=="Data2007to2013_RB$Drafted"] <- "Y"
names(CheckList_RBBO)[names(CheckList_RBBO)=="predict_RBBO"] <- "Pred"

CheckList_RBBO = CheckList_RBBO %>%
  mutate(TP=ifelse(Y==Pred,ifelse(Pred==1,1,0),0)) %>%
  mutate(TN=ifelse(Y==Pred,ifelse(Pred==0,1,0),0)) %>%
  mutate(FP=ifelse(Y!=Pred,ifelse(Pred==1,1,0),0)) %>%
  mutate(FN=ifelse(Y!=Pred,ifelse(Pred==0,1,0),0))

# Performance Measurement
NaiveBayesPerfMeas[4,"RB_TP"] = sum(CheckList_RBBO$TP)
NaiveBayesPerfMeas[4,"RB_TN"] = sum(CheckList_RBBO$TN)
NaiveBayesPerfMeas[4,"RB_FP"] = sum(CheckList_RBBO$FP)
NaiveBayesPerfMeas[4,"RB_FN"] = sum(CheckList_RBBO$FN)

# For testing data
predict_RBBOTest <- predict(NB_RBBO,CleanClass2014_3_RB)
CheckList_RBBOTest = cbind.data.frame(CleanClass2014_3_RB$Drafted,predict_RBBOTest)

names(CheckList_RBBOTest)[names(CheckList_RBBOTest)=="CleanClass2014_3_RB$Drafted"] <- "Y"
names(CheckList_RBBOTest)[names(CheckList_RBBOTest)=="predict_RBBOTest"] <- "Pred"

CheckList_RBBOTest = CheckList_RBBOTest %>%
  mutate(TP=ifelse(Y==Pred,ifelse(Pred==1,1,0),0)) %>%
  mutate(TN=ifelse(Y==Pred,ifelse(Pred==0,1,0),0)) %>%
  mutate(FP=ifelse(Y!=Pred,ifelse(Pred==1,1,0),0)) %>%
  mutate(FN=ifelse(Y!=Pred,ifelse(Pred==0,1,0),0))

NaiveBayesPerfMeasTest[4,"RB_TP"] = sum(CheckList_RBBOTest$TP)
NaiveBayesPerfMeasTest[4,"RB_TN"] = sum(CheckList_RBBOTest$TN)
NaiveBayesPerfMeasTest[4,"RB_FP"] = sum(CheckList_RBBOTest$FP)
NaiveBayesPerfMeasTest[4,"RB_FN"] = sum(CheckList_RBBOTest$FN)

# 5. Smote ###################################################

load("../Data/CleanData/CleanClass2007to2013_3_smote.Rdata")

# I. Naive Bayes Classifier - 07 to 13, together ----------

#1 - Preparations  ----------

# Training data
cleanData_smote$Drafted <- as.factor(cleanData_smote$Drafted)
Data2007to2013_togSM <- cleanData_smote %>% select(-Position, -Name, -Player.Code, -Year, 
                                                   -Safety) #this variable has zero variance
# Testing data
CleanClass2014_3_tog

#2 - Naive Bayes ----------

# Define features (x) and target (y) 
features_togSM <- setdiff(names(Data2007to2013_togSM), "Drafted")
x_togSM <- Data2007to2013_togSM[,features_togSM]
y_togSM <- Data2007to2013_togSM$Drafted

# Training a naive bayes model with 10-fold cross validation
set.seed(6969)
NB_togSM <- train(x_togSM,y_togSM,method = "nb",trControl=trainControl(method='cv',number=10))

# Predictions for training data (2007 to 2013): 0.5 is used for probability cutoff value by default
predict_togSM <- predict(NB_togSM, newdata = Data2007to2013_tog)
confusionMatrix(predict_togSM, Data2007to2013_tog$Drafted)

CheckList_togSM = cbind.data.frame(Data2007to2013_tog$Drafted,predict_togSM)

names(CheckList_togSM)[names(CheckList_togSM)=="Data2007to2013_tog$Drafted"] <- "Y"
names(CheckList_togSM)[names(CheckList_togSM)=="predict_togSM"] <- "Pred"

CheckList_togSM = CheckList_togSM %>%
  mutate(TP=ifelse(Y==Pred,ifelse(Pred==1,1,0),0)) %>%
  mutate(TN=ifelse(Y==Pred,ifelse(Pred==0,1,0),0)) %>%
  mutate(FP=ifelse(Y!=Pred,ifelse(Pred==1,1,0),0)) %>%
  mutate(FN=ifelse(Y!=Pred,ifelse(Pred==0,1,0),0))

# Performance Measurement
NaiveBayesPerfMeas[5,"Together_TP"] = sum(CheckList_togSM$TP)
NaiveBayesPerfMeas[5,"Together_TN"] = sum(CheckList_togSM$TN)
NaiveBayesPerfMeas[5,"Together_FP"] = sum(CheckList_togSM$FP)
NaiveBayesPerfMeas[5,"Together_FN"] = sum(CheckList_togSM$FN)

# For testing data
predict_togSMTest <- predict(NB_togSM,CleanClass2014_3_tog)
CheckList_togSMTest = cbind.data.frame(CleanClass2014_3_tog$Drafted,predict_togSMTest)

names(CheckList_togSMTest)[names(CheckList_togSMTest)=="CleanClass2014_3_tog$Drafted"] <- "Y"
names(CheckList_togSMTest)[names(CheckList_togSMTest)=="predict_togSMTest"] <- "Pred"

CheckList_togSMTest = CheckList_togSMTest %>%
  mutate(TP=ifelse(Y==Pred,ifelse(Pred==1,1,0),0)) %>%
  mutate(TN=ifelse(Y==Pred,ifelse(Pred==0,1,0),0)) %>%
  mutate(FP=ifelse(Y!=Pred,ifelse(Pred==1,1,0),0)) %>%
  mutate(FN=ifelse(Y!=Pred,ifelse(Pred==0,1,0),0))

NaiveBayesPerfMeasTest[5,"Together_TP"] = sum(CheckList_togSMTest$TP)
NaiveBayesPerfMeasTest[5,"Together_TN"] = sum(CheckList_togSMTest$TN)
NaiveBayesPerfMeasTest[5,"Together_FP"] = sum(CheckList_togSMTest$FP)
NaiveBayesPerfMeasTest[5,"Together_FN"] = sum(CheckList_togSMTest$FN)

# II. Naive Bayes Classifier - 07 to 13, QB  ----------

#1 - Preparations  ----------

# Training data
Data2007to2013_QBSM <- cleanData_smote[cleanData_smote$Position=="QB", ]
Data2007to2013_QBSM <- Data2007to2013_QBSM %>% select(-Position, -Name, -Player.Code, -Year,
                                                      -Safety, -Kickoff.Ret.TD, -Punt.Ret.TD) #these variables have zero variance

# Testing data
CleanClass2014_3_QB      

#2 - Naive Bayes ----------

# Define features (x) and target (y) 
features_QBSM <- setdiff(names(Data2007to2013_QBSM), "Drafted")
x_QBSM <- Data2007to2013_QBSM[,features_QBSM]
y_QBSM <- Data2007to2013_QBSM$Drafted

# Training a naive bayes model with 10-fold cross validation
set.seed(6969)
NB_QBSM <- train(x_QBSM,y_QBSM,method = "nb",trControl=trainControl(method='cv',number=10))

# Predictions for training data (2007 to 2013): 0.5 is used for probability cutoff value by default
predict_QBSM <- predict(NB_QBSM, newdata = Data2007to2013_QB)
confusionMatrix(predict_QBSM, Data2007to2013_QB$Drafted)

CheckList_QBSM = cbind.data.frame(Data2007to2013_QB$Drafted,predict_QBSM)

names(CheckList_QBSM)[names(CheckList_QBSM)=="Data2007to2013_QB$Drafted"] <- "Y"
names(CheckList_QBSM)[names(CheckList_QBSM)=="predict_QBSM"] <- "Pred"

CheckList_QBSM = CheckList_QBSM %>%
  mutate(TP=ifelse(Y==Pred,ifelse(Pred==1,1,0),0)) %>%
  mutate(TN=ifelse(Y==Pred,ifelse(Pred==0,1,0),0)) %>%
  mutate(FP=ifelse(Y!=Pred,ifelse(Pred==1,1,0),0)) %>%
  mutate(FN=ifelse(Y!=Pred,ifelse(Pred==0,1,0),0))

# Performance Measurement
NaiveBayesPerfMeas[5,"QB_TP"] = sum(CheckList_QBSM$TP)
NaiveBayesPerfMeas[5,"QB_TN"] = sum(CheckList_QBSM$TN)
NaiveBayesPerfMeas[5,"QB_FP"] = sum(CheckList_QBSM$FP)
NaiveBayesPerfMeas[5,"QB_FN"] = sum(CheckList_QBSM$FN)

# For testing data
predict_QBSMTest <- predict(NB_QBSM,CleanClass2014_3_QB)
CheckList_QBSMTest = cbind.data.frame(CleanClass2014_3_QB$Drafted,predict_QBSMTest)

names(CheckList_QBSMTest)[names(CheckList_QBSMTest)=="CleanClass2014_3_QB$Drafted"] <- "Y"
names(CheckList_QBSMTest)[names(CheckList_QBSMTest)=="predict_QBSMTest"] <- "Pred"

CheckList_QBSMTest = CheckList_QBSMTest %>%
  mutate(TP=ifelse(Y==Pred,ifelse(Pred==1,1,0),0)) %>%
  mutate(TN=ifelse(Y==Pred,ifelse(Pred==0,1,0),0)) %>%
  mutate(FP=ifelse(Y!=Pred,ifelse(Pred==1,1,0),0)) %>%
  mutate(FN=ifelse(Y!=Pred,ifelse(Pred==0,1,0),0))

NaiveBayesPerfMeasTest[5,"QB_TP"] = sum(CheckList_QBSMTest$TP)
NaiveBayesPerfMeasTest[5,"QB_TN"] = sum(CheckList_QBSMTest$TN)
NaiveBayesPerfMeasTest[5,"QB_FP"] = sum(CheckList_QBSMTest$FP)
NaiveBayesPerfMeasTest[5,"QB_FN"] = sum(CheckList_QBSMTest$FN)

# III. Naive Bayes Classifier - 07 to 13, WR ----------

#1 - Preparations ----------

# Training data
Data2007to2013_WRSM <- cleanData_smote[cleanData_smote$Position=="WR", ]
Data2007to2013_WRSM <- Data2007to2013_WRSM %>% select(-Position, -Name, -Player.Code, -Year,
                                                      -Safety) #these variables have zero variance

# Testing data
CleanClass2014_3_WR

#2 - Naive Bayes ----------

# Define features (x) and target (y) 
features_WRSM <- setdiff(names(Data2007to2013_WRSM), "Drafted")
x_WRSM <- Data2007to2013_WRSM[,features_WRSM]
y_WRSM <- Data2007to2013_WRSM$Drafted

# Training a naive bayes model with 10-fold cross validation
set.seed(6969)
NB_WRSM <- train(x_WRSM,y_WRSM,method = "nb",trControl=trainControl(method='cv',number=10))

# Predictions: 0.5 is used for probability cutoff value by default
predict_WRSM <- predict(NB_WRSM, newdata = Data2007to2013_WR)
confusionMatrix(predict_WRSM, Data2007to2013_WR$Drafted)

CheckList_WRSM = cbind.data.frame(Data2007to2013_WR$Drafted,predict_WRSM)

names(CheckList_WRSM)[names(CheckList_WRSM)=="Data2007to2013_WR$Drafted"] <- "Y"
names(CheckList_WRSM)[names(CheckList_WRSM)=="predict_WRSM"] <- "Pred"

CheckList_WRSM = CheckList_WRSM %>%
  mutate(TP=ifelse(Y==Pred,ifelse(Pred==1,1,0),0)) %>%
  mutate(TN=ifelse(Y==Pred,ifelse(Pred==0,1,0),0)) %>%
  mutate(FP=ifelse(Y!=Pred,ifelse(Pred==1,1,0),0)) %>%
  mutate(FN=ifelse(Y!=Pred,ifelse(Pred==0,1,0),0))

# Performance Measurement
NaiveBayesPerfMeas[5,"WR_TP"] = sum(CheckList_WRSM$TP)
NaiveBayesPerfMeas[5,"WR_TN"] = sum(CheckList_WRSM$TN)
NaiveBayesPerfMeas[5,"WR_FP"] = sum(CheckList_WRSM$FP)
NaiveBayesPerfMeas[5,"WR_FN"] = sum(CheckList_WRSM$FN)

# For testing data
predict_WRSMTest <- predict(NB_WRSM,CleanClass2014_3_WR)
CheckList_WRSMTest = cbind.data.frame(CleanClass2014_3_WR$Drafted,predict_WRSMTest)

names(CheckList_WRSMTest)[names(CheckList_WRSMTest)=="CleanClass2014_3_WR$Drafted"] <- "Y"
names(CheckList_WRSMTest)[names(CheckList_WRSMTest)=="predict_WRSMTest"] <- "Pred"

CheckList_WRSMTest = CheckList_WRSMTest %>%
  mutate(TP=ifelse(Y==Pred,ifelse(Pred==1,1,0),0)) %>%
  mutate(TN=ifelse(Y==Pred,ifelse(Pred==0,1,0),0)) %>%
  mutate(FP=ifelse(Y!=Pred,ifelse(Pred==1,1,0),0)) %>%
  mutate(FN=ifelse(Y!=Pred,ifelse(Pred==0,1,0),0))

NaiveBayesPerfMeasTest[5,"WR_TP"] = sum(CheckList_WRSMTest$TP)
NaiveBayesPerfMeasTest[5,"WR_TN"] = sum(CheckList_WRSMTest$TN)
NaiveBayesPerfMeasTest[5,"WR_FP"] = sum(CheckList_WRSMTest$FP)
NaiveBayesPerfMeasTest[5,"WR_FN"] = sum(CheckList_WRSMTest$FN)

# IV. Naive Bayes Classifier - 07 to 13, RB ----------

#1 - Preparations ----------

# Training data
Data2007to2013_RBSM <- cleanData_smote[cleanData_smote$Position=="RB", ]
Data2007to2013_RBSM <- Data2007to2013_RBSM %>% select(-Position, -Name, -Player.Code,-Year, 
                                                      -Safety) #these variables have zero variance

# Testing data
CleanClass2014_3_RB

#2 - Naive Bayes ----------

# Define features (x) and target (y) 
features_RBSM <- setdiff(names(Data2007to2013_RBSM), "Drafted")
x_RBSM <- Data2007to2013_RBSM[,features_RBSM]
y_RBSM <- Data2007to2013_RBSM$Drafted

# Training a naive bayes model with 10-fold cross validation
set.seed(6969)
NB_RBSM <- train(x_RBSM,y_RBSM,method = "nb",trControl=trainControl(method='cv',number=10))

# Predictions: 0.5 is used for probability cutoff value by default
predict_RBSM <- predict(NB_RBSM, newdata = Data2007to2013_RB)
confusionMatrix(predict_RBSM, Data2007to2013_RB$Drafted)

CheckList_RBSM = cbind.data.frame(Data2007to2013_RB$Drafted,predict_RBSM)

names(CheckList_RBSM)[names(CheckList_RBSM)=="Data2007to2013_RB$Drafted"] <- "Y"
names(CheckList_RBSM)[names(CheckList_RBSM)=="predict_RBSM"] <- "Pred"

CheckList_RBSM = CheckList_RBSM %>%
  mutate(TP=ifelse(Y==Pred,ifelse(Pred==1,1,0),0)) %>%
  mutate(TN=ifelse(Y==Pred,ifelse(Pred==0,1,0),0)) %>%
  mutate(FP=ifelse(Y!=Pred,ifelse(Pred==1,1,0),0)) %>%
  mutate(FN=ifelse(Y!=Pred,ifelse(Pred==0,1,0),0))

# Performance Measurement
NaiveBayesPerfMeas[5,"RB_TP"] = sum(CheckList_RBSM$TP)
NaiveBayesPerfMeas[5,"RB_TN"] = sum(CheckList_RBSM$TN)
NaiveBayesPerfMeas[5,"RB_FP"] = sum(CheckList_RBSM$FP)
NaiveBayesPerfMeas[5,"RB_FN"] = sum(CheckList_RBSM$FN)

# For testing data
predict_RBSMTest <- predict(NB_RBSM,CleanClass2014_3_RB)
CheckList_RBSMTest = cbind.data.frame(CleanClass2014_3_RB$Drafted,predict_RBSMTest)

names(CheckList_RBSMTest)[names(CheckList_RBSMTest)=="CleanClass2014_3_RB$Drafted"] <- "Y"
names(CheckList_RBSMTest)[names(CheckList_RBSMTest)=="predict_RBSMTest"] <- "Pred"

CheckList_RBSMTest = CheckList_RBSMTest %>%
  mutate(TP=ifelse(Y==Pred,ifelse(Pred==1,1,0),0)) %>%
  mutate(TN=ifelse(Y==Pred,ifelse(Pred==0,1,0),0)) %>%
  mutate(FP=ifelse(Y!=Pred,ifelse(Pred==1,1,0),0)) %>%
  mutate(FN=ifelse(Y!=Pred,ifelse(Pred==0,1,0),0))

NaiveBayesPerfMeasTest[5,"RB_TP"] = sum(CheckList_RBSMTest$TP)
NaiveBayesPerfMeasTest[5,"RB_TN"] = sum(CheckList_RBSMTest$TN)
NaiveBayesPerfMeasTest[5,"RB_FP"] = sum(CheckList_RBSMTest$FP)
NaiveBayesPerfMeasTest[5,"RB_FN"] = sum(CheckList_RBSMTest$FN)

# 5. Save NaiveBayesPerfMeas as a new dataset ###################################################

# Training
save(NaiveBayesPerfMeas, file="../Data/PerformanceMeasurement/NaiveBayesPerfMeas.Rdata")

# Testing
save(NaiveBayesPerfMeasTest, file="../Data/PerformanceMeasurement/NaiveBayesPerfMeasTest.Rdata")



