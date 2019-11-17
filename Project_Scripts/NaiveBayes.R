rm(list=ls())
graphics.off()

library(tidyverse)                                              
library(e1071) 
library(caret)
library(ggplot2)                                                
library(corrplot)                                     
library(reshape2)                                  
library(ggloop)
library(pROC)
library(ROCR)

Overview_Missclassification <- matrix(NA, ncol=4, nrow = 1)
colnames(Overview_Missclassification) <- c("Together", "QB","WR","RB")

# Data for Classes 2007 to 2014

load("../Data/CleanData/CleanClass2007to2014_2.Rdata")
sum(is.na(CleanClass2007to2014_2))

# I. Naive Bayes Classifier - 07 to 14, together ----------

#1 - Preparations  ----------

Data2007to2014_tog <- CleanClass2007to2014_2 %>% select(-Class, -Position, -Name, -Player.Code, -Safety)

# Convert target variable to a factor

Data2007to2014_tog$Drafted <- as.factor(Data2007to2014_tog$Drafted)
str(Data2007to2014_tog)

#2 - Data Visualization  ----------

# Density distributions

Data2007to2014_vis_tog <- Data2007to2014_tog %>% select(-Year)

Df1_tog <- Data2007to2014_vis_tog[,c(1, 2:10)]
Long1_tog = melt(Df1_tog, id.vars= "Drafted")
ggplot(data = Long1_tog, aes(x = value, fill=Drafted)) + 
  geom_density(alpha=0.6) + 
  facet_wrap(~variable, scales = "free")

Df2_tog <- Data2007to2014_vis_tog[,c(1, 11:19)]
Long2_tog = melt(Df2_tog, id.vars= "Drafted")
ggplot(data = Long2_tog, aes(x = value, fill=Drafted)) + 
  geom_density(alpha=0.6) + 
  facet_wrap(~variable, scales = "free")

Df3_tog <- Data2007to2014_vis_tog[,c(1, 20:24)]
Long3_tog = melt(Df3_tog, id.vars= "Drafted")
ggplot(data = Long3_tog, aes(x = value, fill=Drafted)) + 
  geom_density(alpha=0.6) + 
  facet_wrap(~variable, scales = "free")

# Correlation within variables

# The naive Bayes classifier makes a simplifying assumption (hence the name) to allow the computation to scale. 
# With naïve Bayes, we assume that the predictor variables are conditionally independent of one another given 
# the response value. This is an extremely strong assumption. 

# In our context we can see quickly that our data violates this as we have several moderately to strongly correlated variables.
# See the following correlation plot:

Data2007to2014_vis_tog %>%
  filter(Drafted =="1") %>%
  select_if(is.numeric) %>%
  cor() %>%
  corrplot::corrplot()

#3 - Data Partition ----------

# extract training and testing data

dtrain_tog <- Data2007to2014_tog[Data2007to2014_tog$Year != 2014,]
dtest_tog <- Data2007to2014_tog[Data2007to2014_tog$Year == 2014,]

dtrain_tog <-dtrain_tog %>% select(-Year)
dtest_tog <- dtest_tog %>% select(-Year)

# distribution of draft rates

table(Data2007to2014_tog$Drafted) %>% prop.table()
table(dtrain_tog$Drafted) %>% prop.table()
table(dtest_tog$Drafted) %>% prop.table()

#4 - Naive Bayes ----------

# Define features (x) and target (y) 

features_tog <- setdiff(names(dtrain_tog), "Drafted")
x_tog <- dtrain_tog[,features_tog]
y_tog <- dtrain_tog$Drafted

# Training a naive bayes model with 10-fold cross validation

NB_tog <- train(x_tog,y_tog,method = "nb",trControl=trainControl(method='cv',number=10))

# Model performance

NB_tog
plot(NB_tog)

varImp(NB_tog)
plot(varImp(NB_tog),main="Variable Importance Together")
varImp_tog <- plot(varImp(NB_tog), main="Variable Importance Together")

# Predictions
# 0.5 is used for probability cutoff value by default

predict_tog <- predict(NB_tog, newdata = dtest_tog)
confusionMatrix(predict_tog, dtest_tog$Drafted)

CheckList_tog = cbind.data.frame(dtest_tog$Drafted,predict_tog)

names(CheckList_tog)[names(CheckList_tog)=="dtest_tog$Drafted"] <- "Y"
names(CheckList_tog)[names(CheckList_tog)=="predict_tog"] <- "Pred"

CheckList_tog = CheckList_tog %>%
  mutate(TP=ifelse(Y==Pred,ifelse(Pred==1,1,0),0)) %>%
  mutate(TN=ifelse(Y==Pred,ifelse(Pred==0,1,0),0)) %>%
  mutate(FP=ifelse(Y!=Pred,ifelse(Pred==1,1,0),0)) %>%
  mutate(FN=ifelse(Y!=Pred,ifelse(Pred==0,1,0),0))

Overview_Missclassification[1,1] = ((sum(CheckList_tog$FP)+sum(CheckList_tog$FN))/nrow(CheckList_tog))

# ROC

predict_tog_ROC <- predict(NB_tog,dtest_tog, type = 'prob')
predict_tog_ROC <- prediction(predict_tog_ROC[,2], dtest_tog$Drafted)
roc_tog <- performance(predict_tog_ROC, "tpr", "fpr")
plot(roc_tog, colorize=T, main="ROC Curve Together")
abline(a=0, b=1)

# Area under curve (AUC)

auc_tog <- performance(predict_tog_ROC, "auc")
auc_tog <- unlist(slot(auc_tog, "y.values"))
auc_tog <- round(auc_tog,4)
legend(.7,.4, auc_tog, title="AUC", cex=0.8)

# II. Naive Bayes Classifier - 07 to 14, QB ----------

#1 - Preparations  ----------

Data2007to2014_QB <- CleanClass2007to2014_2[CleanClass2007to2014_2$Position=="QB", ]
Data2007to2014_QB <- Data2007to2014_QB %>% select(-Class, -Position, -Name, -Player.Code, 
                                                  -Safety, -Kickoff.Ret.TD, -Punt.Ret.TD, -Kickoff.Ret, -Kickoff.Ret.Yard)

# Convert target variable to a factor

Data2007to2014_QB$Drafted <- as.factor(Data2007to2014_QB$Drafted)
str(Data2007to2014_QB)

#2 - Data Visualization  ----------

# Density distributions

Data2007to2014_vis_QB <- Data2007to2014_QB %>% select(-Year)

Df1_QB <- Data2007to2014_vis_QB[,c(1, 2:7)]
Long1_QB = melt(Df1_QB, id.vars= "Drafted")
ggplot(data = Long1_QB, aes(x = value, fill=Drafted)) + 
  geom_density(alpha=0.6) + 
  facet_wrap(~variable, scales = "free")

Df2_QB <- Data2007to2014_vis_QB[,c(1, 8:13)]
Long2_QB = melt(Df2_QB, id.vars= "Drafted")
ggplot(data = Long2_QB, aes(x = value, fill=Drafted)) + 
  geom_density(alpha=0.6) + 
  facet_wrap(~variable, scales = "free")

Df3_QB <- Data2007to2014_vis_QB[,c(1, 14:20)]
Long3_QB = melt(Df3_QB, id.vars= "Drafted")
ggplot(data = Long3_QB, aes(x = value, fill=Drafted)) + 
  geom_density(alpha=0.6) + 
  facet_wrap(~variable, scales = "free")

# Correlation within variables

# The naive Bayes classifier makes a simplifying assumption (hence the name) to allow the computation to scale. 
# With naïve Bayes, we assume that the predictor variables are conditionally independent of one another given 
# the response value. This is an extremely strong assumption. 

# In our context we can see quickly that our data violates this as we have several moderately to strongly correlated variables.
# See the following correlation plot:

Data2007to2014_vis_QB %>%
  filter(Drafted =="1") %>%
  select_if(is.numeric) %>%
  cor() %>%
  corrplot::corrplot()

#3 - Data Partition ----------

# extract training and testing data

dtrain_QB <- Data2007to2014_QB[Data2007to2014_QB$Year != 2014,]
dtest_QB <- Data2007to2014_QB[Data2007to2014_QB$Year == 2014,]

dtrain_QB <-dtrain_QB %>% select(-Year)
dtest_QB <- dtest_QB %>% select(-Year)

# distribution of draft rates

table(Data2007to2014_QB$Drafted) %>% prop.table()
table(dtrain_QB$Drafted) %>% prop.table()
table(dtest_QB$Drafted) %>% prop.table()

#4 - Naive Bayes ----------

# Define features (x) and target (y) 

features_QB <- setdiff(names(dtrain_QB), "Drafted")
x_QB <- dtrain_QB[,features_QB]
y_QB <- dtrain_QB$Drafted

# Training a naive bayes model with 10-fold cross validation

NB_QB <- train(x_QB,y_QB,method = "nb",trControl=trainControl(method='cv',number=10))

# Model performance

NB_QB
plot(NB_QB)

varImp(NB_QB)
plot(varImp(NB_QB),main="Variable Importance QB")
varImp_QB <- plot(varImp(NB_QB), main="Variable Importance QB")

# Predictions

predict_QB <- predict(NB_QB, newdata = dtest_QB)
confusionMatrix(predict_QB, dtest_QB$Drafted)

CheckList_QB = cbind.data.frame(dtest_QB$Drafted, predict_QB)

names(CheckList_QB)[names(CheckList_QB)=="dtest_QB$Drafted"] <- "Y"
names(CheckList_QB)[names(CheckList_QB)=="predict_QB"] <- "Pred"

CheckList_QB = CheckList_QB %>%
  mutate(TP=ifelse(Y==Pred,ifelse(Pred==1,1,0),0)) %>%
  mutate(TN=ifelse(Y==Pred,ifelse(Pred==0,1,0),0)) %>%
  mutate(FP=ifelse(Y!=Pred,ifelse(Pred==1,1,0),0)) %>%
  mutate(FN=ifelse(Y!=Pred,ifelse(Pred==0,1,0),0))

Overview_Missclassification[1,2] = ((sum(CheckList_QB$FP)+sum(CheckList_QB$FN))/nrow(CheckList_QB))

# ROC

predict_QB_ROC <- predict(NB_QB,dtest_QB, type = 'prob')
predict_QB_ROC <- prediction(predict_QB_ROC[,2], dtest_QB$Drafted)
roc_QB <- performance(predict_QB_ROC, "tpr", "fpr")
plot(roc_QB, colorize=T, main="ROC Curve QB")
abline(a=0, b=1)

# Area under curve (AUC)

auc_QB <- performance(predict_QB_ROC, "auc")
auc_QB <- unlist(slot(auc_QB, "y.values"))
auc_QB <- round(auc_QB,4)
legend(.7,.4, auc_QB, title="AUC", cex=0.8)


# III. Naive Bayes Classifier - 07 to 14, RB ----------

#1 - Preparations  ----------

Data2007to2014_RB <- CleanClass2007to2014_2[CleanClass2007to2014_2$Position=="RB", ]
Data2007to2014_RB <- Data2007to2014_RB %>% select(-Class, -Position, -Name, -Player.Code, 
                                                  -Safety)

# Convert target variable to a factor

Data2007to2014_RB$Drafted <- as.factor(Data2007to2014_RB$Drafted)
str(Data2007to2014_RB)

#2 - Data Visualization  ----------

# Density distributions

Data2007to2014_vis_RB <- Data2007to2014_RB %>% select(-Year)

Df1_RB <- Data2007to2014_vis_RB[,c(1, 2:10)]
Long1_RB = melt(Df1_RB, id.vars= "Drafted")
ggplot(data = Long1_RB, aes(x = value, fill=Drafted)) + 
  geom_density(alpha=0.6) + 
  facet_wrap(~variable, scales = "free")

Df2_RB <- Data2007to2014_vis_RB[,c(1, 11:19)]
Long2_RB = melt(Df2_RB, id.vars= "Drafted")
ggplot(data = Long2_RB, aes(x = value, fill=Drafted)) + 
  geom_density(alpha=0.6) + 
  facet_wrap(~variable, scales = "free")

Df3_RB <- Data2007to2014_vis_RB[,c(1, 20:24)]
Long3_RB = melt(Df3_RB, id.vars= "Drafted")
ggplot(data = Long3_RB, aes(x = value, fill=Drafted)) + 
  geom_density(alpha=0.6) + 
  facet_wrap(~variable, scales = "free")

# Correlation within variables

# The naive Bayes classifier makes a simplifying assumption (hence the name) to allow the computation to scale. 
# With naïve Bayes, we assume that the predictor variables are conditionally independent of one another given 
# the response value. This is an extremely strong assumption. 

# In our context we can see quickly that our data violates this as we have several moderately to strongly correlated variables.
# See the following correlation plot:

Data2007to2014_vis_RB %>%
  filter(Drafted =="1") %>%
  select_if(is.numeric) %>%
  cor() %>%
  corrplot::corrplot()

#3 - Data Partition ----------

# extract training and testing data

dtrain_RB <- Data2007to2014_RB[Data2007to2014_RB$Year != 2014,]
dtest_RB <- Data2007to2014_RB[Data2007to2014_RB$Year == 2014,]

dtrain_RB <-dtrain_RB %>% select(-Year)
dtest_RB <- dtest_RB %>% select(-Year)

# distribution of draft rates

table(Data2007to2014_RB$Drafted) %>% prop.table()
table(dtrain_RB$Drafted) %>% prop.table()
table(dtest_RB$Drafted) %>% prop.table()

#4 - Naive Bayes ----------

# Define features (x) and target (y) 

features_RB <- setdiff(names(dtrain_RB), "Drafted")
x_RB <- dtrain_RB[,features_RB]
y_RB <- dtrain_RB$Drafted

# Training a naive bayes model with 10-fold cross validation

NB_RB <- train(x_RB,y_RB,method = "nb",trControl=trainControl(method='cv',number=10))

# Model performance

NB_RB
plot(NB_RB)

varImp(NB_RB)
plot(varImp(NB_RB),main="Variable Importance RB")
varImp_RB <- plot(varImp(NB_RB), main="Variable Importance RB")

# Predictions

predict_RB <- predict(NB_RB, newdata = dtest_RB)
confusionMatrix(predict_RB, dtest_RB$Drafted)

CheckList_RB = cbind.data.frame(dtest_RB$Drafted, predict_RB)

names(CheckList_RB)[names(CheckList_RB)=="dtest_RB$Drafted"] <- "Y"
names(CheckList_RB)[names(CheckList_RB)=="predict_RB"] <- "Pred"

CheckList_RB = CheckList_RB %>%
  mutate(TP=ifelse(Y==Pred,ifelse(Pred==1,1,0),0)) %>%
  mutate(TN=ifelse(Y==Pred,ifelse(Pred==0,1,0),0)) %>%
  mutate(FP=ifelse(Y!=Pred,ifelse(Pred==1,1,0),0)) %>%
  mutate(FN=ifelse(Y!=Pred,ifelse(Pred==0,1,0),0))

Overview_Missclassification[1,3] = ((sum(CheckList_RB$FP)+sum(CheckList_RB$FN))/nrow(CheckList_RB))

# ROC

predict_RB_ROC <- predict(NB_RB,dtest_RB, type = 'prob')
predict_RB_ROC <- prediction(predict_RB_ROC[,2], dtest_RB$Drafted)
roc_RB <- performance(predict_RB_ROC, "tpr", "fpr")
plot(roc_RB, colorize=T, main="ROC Curve RB")
abline(a=0, b=1)

# Area under curve (AUC)

auc_RB <- performance(predict_RB_ROC, "auc")
auc_RB <- unlist(slot(auc_RB, "y.values"))
auc_RB <- round(auc_RB,4)
legend(.7,.4, auc_RB, title="AUC", cex=0.8)

# IV. Naive Bayes Classifier - 07 to 14, WR ----------

#1 - Preparations  ----------

Data2007to2014_WR <- CleanClass2007to2014_2[CleanClass2007to2014_2$Position=="WR", ]
Data2007to2014_WR <- Data2007to2014_WR %>% select(-Class, -Position, -Name, -Player.Code, 
                                                  -Safety)

# Convert target variable to a factor

Data2007to2014_WR$Drafted <- as.factor(Data2007to2014_WR$Drafted)
str(Data2007to2014_WR)

#2 - Data Visualization  ----------

# Density distributions

Data2007to2014_vis_WR <- Data2007to2014_WR %>% select(-Year)

Df1_WR <- Data2007to2014_vis_WR[,c(1, 2:10)]
Long1_WR = melt(Df1_WR, id.vars= "Drafted")
ggplot(data = Long1_WR, aes(x = value, fill=Drafted)) + 
  geom_density(alpha=0.6) + 
  facet_wrap(~variable, scales = "free")

Df2_WR <- Data2007to2014_vis_WR[,c(1, 11:19)]
Long2_WR = melt(Df2_WR, id.vars= "Drafted")
ggplot(data = Long2_WR, aes(x = value, fill=Drafted)) + 
  geom_density(alpha=0.6) + 
  facet_wrap(~variable, scales = "free")

Df3_WR <- Data2007to2014_vis_WR[,c(1, 20:24)]
Long3_WR = melt(Df3_WR, id.vars= "Drafted")
ggplot(data = Long3_WR, aes(x = value, fill=Drafted)) + 
  geom_density(alpha=0.6) + 
  facet_wrap(~variable, scales = "free")

# Correlation within variables

# The naive Bayes classifier makes a simplifying assumption (hence the name) to allow the computation to scale. 
# With naïve Bayes, we assume that the predictor variables are conditionally independent of one another given 
# the response value. This is an extremely strong assumption. 

# In our context we can see quickly that our data violates this as we have several moderately to strongly correlated variables.
# See the following correlation plot:

Data2007to2014_vis_WR %>%
  filter(Drafted =="1") %>%
  select_if(is.numeric) %>%
  cor() %>%
  corrplot::corrplot()

#3 - Data Partition ----------

# extract training and testing data

dtrain_WR <- Data2007to2014_WR[Data2007to2014_WR$Year != 2014,]
dtest_WR <- Data2007to2014_WR[Data2007to2014_WR$Year == 2014,]

dtrain_WR <-dtrain_WR %>% select(-Year)
dtest_WR <- dtest_WR %>% select(-Year)

# distribution of draft rates

table(Data2007to2014_WR$Drafted) %>% prop.table()
table(dtrain_WR$Drafted) %>% prop.table()
table(dtest_WR$Drafted) %>% prop.table()

#4 - Naive Bayes ----------

# Define features (x) and target (y) 

features_WR <- setdiff(names(dtrain_WR), "Drafted")
x_WR <- dtrain_WR[,features_WR]
y_WR <- dtrain_WR$Drafted

# Training a naive bayes model with 10-fold cross validation

NB_WR <- train(x_WR,y_WR,method = "nb",trControl=trainControl(method='cv',number=10))

# Model performance

NB_WR
plot(NB_WR)

varImp(NB_WR)
plot(varImp(NB_WR),main="Variable Importance WR")
varImp_WR <- plot(varImp(NB_WR), main="Variable Importance WR")

# Predictions

predict_WR <- predict(NB_WR, newdata = dtest_WR)
confusionMatrix(predict_WR, dtest_WR$Drafted)

CheckList_WR = cbind.data.frame(dtest_WR$Drafted, predict_WR)

names(CheckList_WR)[names(CheckList_WR)=="dtest_WR$Drafted"] <- "Y"
names(CheckList_WR)[names(CheckList_WR)=="predict_WR"] <- "Pred"

CheckList_WR = CheckList_WR %>%
  mutate(TP=ifelse(Y==Pred,ifelse(Pred==1,1,0),0)) %>%
  mutate(TN=ifelse(Y==Pred,ifelse(Pred==0,1,0),0)) %>%
  mutate(FP=ifelse(Y!=Pred,ifelse(Pred==1,1,0),0)) %>%
  mutate(FN=ifelse(Y!=Pred,ifelse(Pred==0,1,0),0))

Overview_Missclassification[1,4] = ((sum(CheckList_WR$FP)+sum(CheckList_WR$FN))/nrow(CheckList_WR))

# ROC

predict_WR_ROC <- predict(NB_WR,dtest_WR, type = 'prob')
predict_WR_ROC <- prediction(predict_WR_ROC[,2], dtest_WR$Drafted)
roc_WR <- performance(predict_WR_ROC, "tpr", "fpr")
plot(roc_WR, colorize=T, main="ROC Curve WR")
abline(a=0, b=1)

# Area under curve (AUC)

auc_WR <- performance(predict_WR_ROC, "auc")
auc_WR <- unlist(slot(auc_WR, "y.values"))
auc_WR <- round(auc_WR,4)
legend(.7,.4, auc_WR, title="AUC", cex=0.8)

# V. Naive Bayes Classifier - Overview ----------

Overview_Missclassification

varImp_tog
varImp_QB
varImp_WR
varImp_RB

