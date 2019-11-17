rm(list=ls())
graphics.off()

library(caret)
library(pROC)
library(ROCR)
library(mlbench)
library(tidyverse)

Overview_Missclassification <- matrix(NA, ncol=4, nrow = 1)
colnames(Overview_Missclassification) <- c("Together", "QB","WR","RB")

# Data for Classes 2007 to 2014

load("../Data/CleanData/CleanClass2007to2014_2.Rdata")
sum(is.na(CleanClass2007to2014_2))

# I. KNN Classifier - 07 to 14, together ----------

#1 - Preparations  ----------

Data2007to2014_tog <- CleanClass2007to2014_2 %>% select(-Class, -Position, -Name, -Player.Code, 
-Safety) #this variable has zero variance hence it can not be normalized.

# Convert target variable to a factor

Data2007to2014_tog$Drafted <- as.factor(Data2007to2014_tog$Drafted)
str(Data2007to2014_tog)

#2 - Data Partition ----------

# extract training and testing data

dtrain_tog <- Data2007to2014_tog[Data2007to2014_tog$Year != 2014,]
dtest_tog <- Data2007to2014_tog[Data2007to2014_tog$Year == 2014,]

dtrain_tog <-dtrain_tog %>% select(-Year)
dtest_tog <- dtest_tog %>% select(-Year)

#3 - KNN ----------

tr_control <- trainControl(method="repeatedcv", number=10, repeats = 3)

set.seed(6969)
KNN_tog <- train(Drafted~., 
             data=dtrain_tog, 
             method="knn",
             trControl=tr_control,
             preProcess=c("center", "scale")) 

# Substract mean (="center") from each value and then divide this result by standard deviation (="scale").
# The normalized result ist the z-value.

# Model performance

KNN_tog
plot(KNN_tog)

varImp(KNN_tog)
plot(varImp(KNN_tog), main="Variable Importance Together")
varImp_tog <- plot(varImp(KNN_tog), main="Variable Importance Together")

# Predictions
# 0.5 is used for probability cutoff value by default

predict_tog <- predict(KNN_tog,dtest_tog)
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

predict_tog_ROC <- predict(KNN_tog,dtest_tog, type = 'prob')
predict_tog_ROC <- prediction(predict_tog_ROC[,2], dtest_tog$Drafted)
roc_tog <- performance(predict_tog_ROC, "tpr", "fpr")
plot(roc_tog, colorize=T, main="ROC Curve Together")
abline(a=0, b=1)

# Area under curve (AUC)

auc_tog <- performance(predict_tog_ROC, "auc")
auc_tog <- unlist(slot(auc_tog, "y.values"))
auc_tog <- round(auc_tog,4)
legend(.7,.4, auc_tog, title="AUC", cex=0.8)


# II. KNN Classifier - 07 to 14, QB  ----------

#1 - Preparations  ----------

Data2007to2014_QB <- CleanClass2007to2014_2[CleanClass2007to2014_2$Position=="QB", ]
Data2007to2014_QB <- Data2007to2014_QB %>% select(-Class, -Position, -Name, -Player.Code, 
-Safety, -Kickoff.Ret.TD, -Punt.Ret.TD) #these variables have zero variance hence they can not be normalized.

# Convert target variable to a factor

Data2007to2014_QB$Drafted <- as.factor(Data2007to2014_QB$Drafted)
str(Data2007to2014_tog)

#2 - Data Partition ----------

# extract training and testing data

dtrain_QB <- Data2007to2014_QB[Data2007to2014_QB$Year != 2014,]
dtest_QB <- Data2007to2014_QB[Data2007to2014_QB$Year == 2014,]

dtrain_QB <-dtrain_QB %>% select(-Year)
dtest_QB <- dtest_QB %>% select(-Year)

#3 - KNN ----------

tr_control <- trainControl(method="repeatedcv", number=10, repeats = 3)

set.seed(6969)
KNN_QB <- train(Drafted~., 
                 data=dtrain_QB, 
                 method="knn",
                 trControl=tr_control,
                 preProcess=c("center", "scale")) 

# Substract mean (="center") from each value and then divide this result by standard deviation (="scale").
# The normalized result ist the z-value.

# Model performance

KNN_QB
plot(KNN_QB)

varImp(KNN_QB)
plot(varImp(KNN_QB),main="Variable Importance QB")
varImp_QB <- plot(varImp(KNN_QB), main="Variable Importance QB")

# Predictions

predict_QB <- predict(KNN_QB, newdata=dtest_QB)
confusionMatrix(predict_QB, dtest_QB$Drafted)

CheckList_QB = cbind.data.frame(dtest_QB$Drafted,predict_QB)

names(CheckList_QB)[names(CheckList_QB)=="dtest_QB$Drafted"] <- "Y"
names(CheckList_QB)[names(CheckList_QB)=="predict_QB"] <- "Pred"

CheckList_QB = CheckList_QB %>%
  mutate(TP=ifelse(Y==Pred,ifelse(Pred==1,1,0),0)) %>%
  mutate(TN=ifelse(Y==Pred,ifelse(Pred==0,1,0),0)) %>%
  mutate(FP=ifelse(Y!=Pred,ifelse(Pred==1,1,0),0)) %>%
  mutate(FN=ifelse(Y!=Pred,ifelse(Pred==0,1,0),0))

Overview_Missclassification[1,2] = ((sum(CheckList_QB$FP)+sum(CheckList_QB$FN))/nrow(CheckList_QB))

# ROC

predict_QB_ROC <- predict(KNN_QB,dtest_QB, type = 'prob')
predict_QB_ROC <- prediction(predict_QB_ROC[,2], dtest_QB$Drafted)
roc_QB <- performance(predict_tog_ROC, "tpr", "fpr")
plot(roc_QB, colorize=T, main="ROC Curve QB")
abline(a=0, b=1)

# Area under curve (AUC)

auc_QB <- performance(predict_QB_ROC, "auc")
auc_QB <- unlist(slot(auc_QB, "y.values"))
auc_QB <- round(auc_QB,4)
legend(.7,.4, auc_QB, title="AUC", cex=0.8)

# III. KNN Classifier - 07 to 14, WR ----------

#1 - Preparations ----------

Data2007to2014_WR <- CleanClass2007to2014_2[CleanClass2007to2014_2$Position=="WR", ]
Data2007to2014_WR <- Data2007to2014_WR %>% select(-Class, -Position, -Name, -Player.Code, 
-Safety) #this variable has zero variance hence it can not be normalized.

# Convert target variable to a factor

Data2007to2014_WR$Drafted <- as.factor(Data2007to2014_WR$Drafted)
str(Data2007to2014_WR)

#2 - Data Partition ----------

# extract training and testing data

dtrain_WR <- Data2007to2014_WR[Data2007to2014_WR$Year != 2014,]
dtest_WR <- Data2007to2014_WR[Data2007to2014_WR$Year == 2014,]

dtrain_WR <-dtrain_WR %>% select(-Year)
dtest_WR <- dtest_WR %>% select(-Year)

#3 - KNN ----------

tr_control <- trainControl(method="repeatedcv", number=10, repeats = 3)

set.seed(6969)
KNN_WR <- train(Drafted~., 
                data=dtrain_WR, 
                method="knn",
                trControl=tr_control,
                preProcess=c("center", "scale")) 

# Substract mean (="center") from each value and then divide this result by standard deviation (="scale").
# The normalized result ist the z-value.

# Model performance

KNN_WR
plot(KNN_WR)

varImp(KNN_WR)
plot(varImp(KNN_WR), main="Variable Importance WR")
varImp_WR <- plot(varImp(KNN_WR), main="Variable Importance WR")

# Predictions

predict_WR <- predict(KNN_WR, newdata=dtest_WR)
confusionMatrix(predict_WR, dtest_WR$Drafted)

CheckList_WR = cbind.data.frame(dtest_WR$Drafted,predict_WR)

names(CheckList_WR)[names(CheckList_WR)=="dtest_WR$Drafted"] <- "Y"
names(CheckList_WR)[names(CheckList_WR)=="predict_WR"] <- "Pred"

CheckList_WR = CheckList_WR %>%
  mutate(TP=ifelse(Y==Pred,ifelse(Pred==1,1,0),0)) %>%
  mutate(TN=ifelse(Y==Pred,ifelse(Pred==0,1,0),0)) %>%
  mutate(FP=ifelse(Y!=Pred,ifelse(Pred==1,1,0),0)) %>%
  mutate(FN=ifelse(Y!=Pred,ifelse(Pred==0,1,0),0))

Overview_Missclassification[1,3] = ((sum(CheckList_WR$FP)+sum(CheckList_WR$FN))/nrow(CheckList_WR))

# ROC

predict_WR_ROC <- predict(KNN_WR,dtest_WR, type = 'prob')
predict_WR_ROC <- prediction(predict_WR_ROC[,2], dtest_WR$Drafted)
roc_WR <- performance(predict_WR_ROC, "tpr", "fpr")
plot(roc_WR, colorize=T, main="ROC Curve WR")
abline(a=0, b=1)

# Area under curve (AUC)

auc_WR <- performance(predict_WR_ROC, "auc")
auc_WR <- unlist(slot(auc_WR, "y.values"))
auc_WR <- round(auc_WR,4)
legend(.7,.4, auc_WR, title="AUC", cex=0.8)

# IV. KNN Classifier - 07 to 14, RB ----------

#1 - Preparations ----------

Data2007to2014_RB <- CleanClass2007to2014_2[CleanClass2007to2014_2$Position=="RB", ]
Data2007to2014_RB <- Data2007to2014_RB %>% select(-Class, -Position, -Name, -Player.Code, 
-Safety) #this variable has zero variance hence it can not be normalized.

# Convert target variable to a factor

Data2007to2014_RB$Drafted <- as.factor(Data2007to2014_RB$Drafted)
str(Data2007to2014_RB)

#2 - Data Partition ----------

# extract training and testing data

dtrain_RB <- Data2007to2014_RB[Data2007to2014_RB$Year != 2014,]
dtest_RB <- Data2007to2014_RB[Data2007to2014_RB$Year == 2014,]

dtrain_RB <-dtrain_RB %>% select(-Year)
dtest_RB <- dtest_RB %>% select(-Year)

#3 - KNN ----------

tr_control <- trainControl(method="repeatedcv", number=10, repeats = 3)

set.seed(6969)
KNN_RB <- train(Drafted~., 
                data=dtrain_RB, 
                method="knn",
                trControl=tr_control,
                preProcess=c("center", "scale")) 

# Substract mean (="center") from each value and then divide this result by standard deviation (="scale").
# The normalized result ist the z-value.

# Model performance

KNN_RB
plot(KNN_RB)

varImp(KNN_RB)
plot(varImp(KNN_RB), main="Variable Importance RB")
varImp_RB <- plot(varImp(KNN_RB), main="Variable Importance RB")

# Predictions

predict_RB <- predict(KNN_RB, newdata=dtest_RB)
confusionMatrix(predict_RB, dtest_RB$Drafted)

CheckList_RB = cbind.data.frame(dtest_RB$Drafted,predict_RB)

names(CheckList_RB)[names(CheckList_RB)=="dtest_RB$Drafted"] <- "Y"
names(CheckList_RB)[names(CheckList_RB)=="predict_RB"] <- "Pred"

CheckList_RB = CheckList_RB %>%
  mutate(TP=ifelse(Y==Pred,ifelse(Pred==1,1,0),0)) %>%
  mutate(TN=ifelse(Y==Pred,ifelse(Pred==0,1,0),0)) %>%
  mutate(FP=ifelse(Y!=Pred,ifelse(Pred==1,1,0),0)) %>%
  mutate(FN=ifelse(Y!=Pred,ifelse(Pred==0,1,0),0))

Overview_Missclassification[1,4] = ((sum(CheckList_RB$FP)+sum(CheckList_RB$FN))/nrow(CheckList_RB))

# ROC

predict_RB_ROC <- predict(KNN_RB,dtest_RB, type = 'prob')
predict_RB_ROC <- prediction(predict_RB_ROC[,2], dtest_RB$Drafted)
roc_RB <- performance(predict_RB_ROC, "tpr", "fpr")
plot(roc_RB, colorize=T, main="ROC Curve RB")
abline(a=0, b=1)

# Area under curve (AUC)

auc_RB <- performance(predict_RB_ROC, "auc")
auc_RB <- unlist(slot(auc_RB, "y.values"))
auc_RB <- round(auc_RB,4)
legend(.7,.4, auc_RB, title="AUC", cex=0.8)

# V. KNN Classifier - Overview ----------

Overview_Missclassification

varImp_tog
varImp_QB
varImp_WR
varImp_RB

