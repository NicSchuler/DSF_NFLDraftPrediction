rm(list=ls())
graphics.off()

library(tidyverse)
library(caret)      # Classification and Regression Training
library(pROC)       # Display and analyze ROC Curves
library(ROCR)       # Visualizing the Performance of Scoring Classifiers
library(ROSE)
library(DMwR)

KNNPerfMeas <- data.frame(Method = character(), QB_TP = integer(), QB_TN = integer(), QB_FP = integer(), QB_FN = integer(),
                                 WR_TP = integer(), WR_TN = integer(), WR_FP = integer(), WR_FN = integer(),
                                 RB_TP = integer(), RB_TN = integer(), RB_FP = integer(), RB_FN = integer(),
                                 Together_TP = integer(), Together_TN = integer(), Together_FP = integer(), Together_FN = integer(), stringsAsFactors = FALSE)
KNNPerfMeas[1,1] <- "KNN"

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

#3 - KNN - Training  ----------

# 3.1 Initial Model  ----------

tr_control <- trainControl(method="repeatedcv", number=10, repeats = 3)

set.seed(6969)
KNN_tog <- train(Drafted~., 
             data=dtrain_tog, 
             method="knn",
             trControl=tr_control,
             preProcess=c("center", "scale")) 

# Substract mean (="center") from each value and then divide this result by standard deviation (="scale").
# The normalized result is the z-value.

# Model performance
KNN_tog
plot(KNN_tog)

varImp(KNN_tog)
plot(varImp(KNN_tog), main="Variable Importance Together")

# Predictions
# 0.5 is used for probability cutoff value by default
predict_tog <- predict(KNN_tog,dtrain_tog)
cm_original <- confusionMatrix(predict_tog, dtrain_tog$Drafted)

# accuracy = 1- missclassification rate
# 95% CI (confidence interval): Accuracy value is likely to lie between 0.9361 and 0.9492.
# No information rate: Largest poportion of the observed class: (Here: (4646/4992))
# Sensitivity: How often are we able to predict correctly class 0: (4598/(4598+48))
# Specificity: How often are we able to predict correctly class 1: (109/(109+237))

# We obtain high Sensitivity and very low specificity, because if we take a look at the distribution of our classes 
# 0 and 1 we see that it is imbalanced, respectively we have few datapoints for class 1 and much more for class 0. 
# This prediction is dominated by class 0. Therefore the accuracy for the overall model is missleading. An unbalanced 
# dataset will bias the prediction model towards the more common class In fact, we are more intrested in predicting 
# class 1 - a cfp beeing drafted - correctly.

(table(dtrain_tog$Drafted))

# Different approaches to overcome this problem:

# 3.2 Under sampling  ----------

# We randomly select a subset of samples from the class with more instances, here class 0, to match the number of 
# samples coming from each class. In our context, we randomly pick 346 out of the 4646 not drafted cases. 
# The main disadvantage of under-sampling is that we loose potentially relevant information from the left-out samples.

tr_control_under <- trainControl(method="repeatedcv", number=10, repeats = 3, sampling = "down")

set.seed(6969)
KNN_tog_under <- train(Drafted~., 
                 data=dtrain_tog, 
                 method="knn",
                 trControl=tr_control_under,
                 preProcess=c("center", "scale")) 

predict_tog_under <- predict(KNN_tog_under,dtrain_tog)
cm_over <- confusionMatrix(predict_tog_under, dtrain_tog$Drafted)

# 3.2 Over sampling  ----------

# We randomly duplicate samples from the class with fewer instances, here class 1, or we generate additional instances 
# based on the data that we have, so as to match the number of samples in each class. While we avoid loosing information 
# with this approach, we also run the risk of overfitting our model as we are more likely to get the same samples in the 
# training and in the test data, i.e. the test data is no longer independent from training data. This would lead to an 
# overestimation of our model’s performance and generalizability.

tr_control_over <- trainControl(method="repeatedcv", number=10, repeats = 3, sampling = "up")

set.seed(6969)
KNN_tog_over <- train(Drafted~., 
                       data=dtrain_tog, 
                       method="knn",
                       trControl=tr_control_over,
                       preProcess=c("center", "scale")) 

predict_tog_over <- predict(KNN_tog_over,dtrain_tog)
cm_under <- confusionMatrix(predict_tog_over, dtrain_tog$Drafted)

# 3.3 Using ROSE  ----------

# A hybrid method that combine under-sampling with the generation of additional data.

tr_control_rose <- trainControl(method="repeatedcv", number=10, repeats = 3, sampling = "rose")

set.seed(6969)
KNN_tog_rose <- train(Drafted~., 
                      data=dtrain_tog, 
                      method="knn",
                      trControl=tr_control_rose,
                      preProcess=c("center", "scale")) 

predict_tog_rose <- predict(KNN_tog_rose,dtrain_tog)
cm_rose <- confusionMatrix(predict_tog_rose, dtrain_tog$Drafted)

# 3.4 Using SMOTE  ----------

# A hybrid method that combine under-sampling with the generation of additional data.

tr_control_smote <- trainControl(method="repeatedcv", number=10, repeats = 3, sampling = "smote")

set.seed(6969)
KNN_tog_smote <- train(Drafted~., 
                      data=dtrain_tog, 
                      method="knn",
                      trControl=tr_control_smote,
                      preProcess=c("center", "scale")) 

predict_tog_smote <- predict(KNN_tog_smote,dtrain_tog)
cm_smote <- confusionMatrix(predict_tog_smote, dtrain_tog$Drafted)

# 3.5 Selection of the final model  ----------

# Compare predictions of all the models

models <- list(original = KNN_tog,
               under = KNN_tog_under,
               over = KNN_tog_over,
               smote = KNN_tog_smote,
               rose = KNN_tog_rose)

resampling <- resamples(models)
bwplot(resampling)

library(dplyr)
comparison <- data.frame(model = names(models),
                         Sensitivity = rep(NA, length(models)),
                         Specificity = rep(NA, length(models)),
                         Accuracy = rep(NA, length(models)),
                         Performance = rep(NA, length(models)))

for (name in names(models)) {
  model <- get(paste0("cm_", name))
  comparison[comparison$model == name, 2:5] <- c(model$byClass["Sensitivity"],
                                                 model$byClass["Specificity"],
                                                 model$byClass["Precision"],
                                                 model$byClass["Recall"])
}


# Fill comparison data frame
# original
comparison[1,2] = 0.9897
comparison[1,3] = 0.3150
comparison[1,4] = 0.9429
comparison[1,5] = (109) / (sum(1,237,48)*sum(109,237))
# under
comparison[2,2] = 0.8728
comparison[2,3] = 0.8324
comparison[2,4] = 0.87 
comparison[2,5] = (288) / (sum(1,58,591)*sum(288,58))
# over
comparison[3,2] = 0.8851
comparison[3,3] = 1.0000 
comparison[3,4] = 0.893
comparison[3,5] = (346) / (sum(1,0,534)*sum(346,0))
# smote
comparison[4,2] = 0.8592
comparison[4,3] = 0.9075 
comparison[4,4] = 0.8626
comparison[4,5] = (314) / (sum(1,32,654)*sum(314,32))
# rose
comparison[5,2] = 0.9950
comparison[5,3] = 0.1821
comparison[5,4] = 0.9387
comparison[5,5] = (63) / (sum(1,283,23)*sum(63,283))

library(tidyr)
comparison %>%
  gather(x, y, Sensitivity:Accuracy) %>%
  ggplot(aes(x = x, y = y, color = model)) +
  geom_jitter(width = 0.06, alpha = 0.5, size = 2.5)+
  ggtitle("Comparison of Models for Training - Together") +
  labs(y="Value", x="Performance Measurement")+
  geom_segment(x = 0.75, y = 0.9307, xend = 1.25, yend = 0.9307, linetype="dashed", color = "red", alpha=0.5)

comparison %>%
  gather(x, y, Performance) %>%
  ggplot(aes(x = x, y = y, color = model)) +
  geom_point(alpha = 0.5, size = 2.5)+
  ggtitle("Performance Measurement for Training - Together") +
  labs(y="Value", x="Performance Measurement")

#4 - KNN - Testing  ----------

# 4.1 Initial Model  ----------

predict_tog_test <- predict(KNN_tog,dtest_tog)
confusionMatrix(predict_tog_test, dtest_tog$Drafted)

# 4.2 Under Sampling  ----------

predict_tog_under_test <- predict(KNN_tog_under,dtest_tog)
confusionMatrix(predict_tog_under_test, dtest_tog$Drafted)

# 4.3 Over Sampling  ----------

predict_tog_over_test <- predict(KNN_tog_over,dtest_tog)
confusionMatrix(predict_tog_over_test, dtest_tog$Drafted)

# 4.4 Using ROSE  ----------

predict_tog_rose_test <- predict(KNN_tog_rose,dtest_tog)
confusionMatrix(predict_tog_rose_test, dtest_tog$Drafted)

# 4.5 Using SMOTE  ----------

predict_tog_smote_test <- predict(KNN_tog_smote,dtest_tog)
confusionMatrix(predict_tog_smote_test, dtest_tog$Drafted)

# 4.6 Model Comparison  ----------

comparison_testing <- data.frame(model = names(models),
                         Sensitivity = rep(NA, length(models)),
                         Specificity = rep(NA, length(models)),
                         Accuracy = rep(NA, length(models)),
                         Performance = rep(NA, length(models)))

# Fill comparison data frame
# original
comparison_testing[1,2] = 0.9767
comparison_testing[1,3] = 0.2885
comparison_testing[1,4] = 0.9507
comparison_testing[1,5] = (15) / (sum(1,37,31)*sum(15,37))
# under
comparison_testing[2,2] = 0.8404
comparison_testing[2,3] = 0.8269
comparison_testing[2,4] = 0.8399 
comparison_testing[2,5] = (43) / (sum(1,9,212)*sum(43,9))
# over
comparison_testing[3,2] = 0.8366
comparison_testing[3,3] = 0.7885 
comparison_testing[3,4] = 0.8348
comparison_testing[3,5] = (41) / (sum(1,11,217)*sum(41,11))
# smote
comparison_testing[4,2] = 0.8261
comparison_testing[4,3] = 0.8077 
comparison_testing[4,4] = 0.8254
comparison_testing[4,5] = (42) / (sum(1,10,231)*sum(42,10))
# rose
comparison_testing[5,2] = 0.98720
comparison_testing[5,3] = 0.09615
comparison_testing[5,4] = 0.9536
comparison_testing[5,5] = (5) / (sum(1,47,17)*sum(5,47))


comparison_testing %>%
  gather(x, y, Sensitivity:Accuracy) %>%
  ggplot(aes(x = x, y = y, color = model)) +
  geom_jitter(width = 0.1, alpha = 0.5, size = 2.5)+
  ggtitle("Comparison of Models for Testing - Together") +
  labs(y="Value", x="Performance Measurement")+
  geom_segment(x = 0.75, y = 0.9307, xend = 1.25, yend = 0.9307, linetype="dashed", color = "red", alpha=0.5)

comparison_testing %>%
  gather(x, y, Performance) %>%
  ggplot(aes(x = x, y = y, color = model)) +
  geom_point(alpha = 0.5, size = 2.5)+
  ggtitle("Performance Measurement for Testing - Together") +
  labs(y="Value", x="Performance Measurement")



# ABLAGE -----------.
CheckList_tog = cbind.data.frame(dtrain_tog$Drafted,predict_tog)

names(CheckList_tog)[names(CheckList_tog)=="dtrain_tog$Drafted"] <- "Y"
names(CheckList_tog)[names(CheckList_tog)=="predict_tog"] <- "Pred"

CheckList_tog = CheckList_tog %>%
  mutate(TP=ifelse(Y==Pred,ifelse(Pred==1,1,0),0)) %>%
  mutate(TN=ifelse(Y==Pred,ifelse(Pred==0,1,0),0)) %>%
  mutate(FP=ifelse(Y!=Pred,ifelse(Pred==1,1,0),0)) %>%
  mutate(FN=ifelse(Y!=Pred,ifelse(Pred==0,1,0),0))

# Performance Measurement
KNNPerfMeas[1,"Together_TP"] = sum(CheckList_tog$TP)
KNNPerfMeas[1,"Together_TN"] = sum(CheckList_tog$TN)
KNNPerfMeas[1,"Together_FP"] = sum(CheckList_tog$FP)
KNNPerfMeas[1,"Together_FN"] = sum(CheckList_tog$FN)

# ROC
predict_tog_ROC <- predict(KNN_tog,dtrain_tog, type = 'prob')
predict_tog_ROC <- prediction(predict_tog_ROC[,2], dtrain_tog$Drafted)
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

# Predictions
predict_QB <- predict(KNN_QB, newdata=dtrain_QB)
confusionMatrix(predict_QB, dtrain_QB$Drafted)

CheckList_QB = cbind.data.frame(dtrain_QB$Drafted,predict_QB)

names(CheckList_QB)[names(CheckList_QB)=="dtrain_QB$Drafted"] <- "Y"
names(CheckList_QB)[names(CheckList_QB)=="predict_QB"] <- "Pred"

CheckList_QB = CheckList_QB %>%
  mutate(TP=ifelse(Y==Pred,ifelse(Pred==1,1,0),0)) %>%
  mutate(TN=ifelse(Y==Pred,ifelse(Pred==0,1,0),0)) %>%
  mutate(FP=ifelse(Y!=Pred,ifelse(Pred==1,1,0),0)) %>%
  mutate(FN=ifelse(Y!=Pred,ifelse(Pred==0,1,0),0))

# Performance Measurement
KNNPerfMeas[1,"QB_TP"] = sum(CheckList_QB$TP)
KNNPerfMeas[1,"QB_TN"] = sum(CheckList_QB$TN)
KNNPerfMeas[1,"QB_FP"] = sum(CheckList_QB$FP)
KNNPerfMeas[1,"QB_FN"] = sum(CheckList_QB$FN)

# ROC
predict_QB_ROC <- predict(KNN_QB,dtrain_QB, type = 'prob')
predict_QB_ROC <- prediction(predict_QB_ROC[,2], dtrain_QB$Drafted)
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

# Predictions
predict_WR <- predict(KNN_WR, newdata=dtrain_WR)
confusionMatrix(predict_WR, dtrain_WR$Drafted)

CheckList_WR = cbind.data.frame(dtrain_WR$Drafted,predict_WR)

names(CheckList_WR)[names(CheckList_WR)=="dtrain_WR$Drafted"] <- "Y"
names(CheckList_WR)[names(CheckList_WR)=="predict_WR"] <- "Pred"

CheckList_WR = CheckList_WR %>%
  mutate(TP=ifelse(Y==Pred,ifelse(Pred==1,1,0),0)) %>%
  mutate(TN=ifelse(Y==Pred,ifelse(Pred==0,1,0),0)) %>%
  mutate(FP=ifelse(Y!=Pred,ifelse(Pred==1,1,0),0)) %>%
  mutate(FN=ifelse(Y!=Pred,ifelse(Pred==0,1,0),0))

# Performance Measurement
KNNPerfMeas[1,"WR_TP"] = sum(CheckList_WR$TP)
KNNPerfMeas[1,"WR_TN"] = sum(CheckList_WR$TN)
KNNPerfMeas[1,"WR_FP"] = sum(CheckList_WR$FP)
KNNPerfMeas[1,"WR_FN"] = sum(CheckList_WR$FN)

# ROC
predict_WR_ROC <- predict(KNN_WR,dtrain_WR, type = 'prob')
predict_WR_ROC <- prediction(predict_WR_ROC[,2], dtrain_WR$Drafted)
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

# Predictions
predict_RB <- predict(KNN_RB, newdata=dtrain_RB)
confusionMatrix(predict_RB, dtrain_RB$Drafted)

CheckList_RB = cbind.data.frame(dtrain_RB$Drafted,predict_RB)

names(CheckList_RB)[names(CheckList_RB)=="dtrain_RB$Drafted"] <- "Y"
names(CheckList_RB)[names(CheckList_RB)=="predict_RB"] <- "Pred"

CheckList_RB = CheckList_RB %>%
  mutate(TP=ifelse(Y==Pred,ifelse(Pred==1,1,0),0)) %>%
  mutate(TN=ifelse(Y==Pred,ifelse(Pred==0,1,0),0)) %>%
  mutate(FP=ifelse(Y!=Pred,ifelse(Pred==1,1,0),0)) %>%
  mutate(FN=ifelse(Y!=Pred,ifelse(Pred==0,1,0),0))

# Performance Measurement
KNNPerfMeas[1,"RB_TP"] = sum(CheckList_RB$TP)
KNNPerfMeas[1,"RB_TN"] = sum(CheckList_RB$TN)
KNNPerfMeas[1,"RB_FP"] = sum(CheckList_RB$FP)
KNNPerfMeas[1,"RB_FN"] = sum(CheckList_RB$FN)

# ROC
predict_RB_ROC <- predict(KNN_RB,dtrain_RB, type = 'prob')
predict_RB_ROC <- prediction(predict_RB_ROC[,2], dtrain_RB$Drafted)
roc_RB <- performance(predict_RB_ROC, "tpr", "fpr")
plot(roc_RB, colorize=T, main="ROC Curve RB")
abline(a=0, b=1)

# Area under curve (AUC)
auc_RB <- performance(predict_RB_ROC, "auc")
auc_RB <- unlist(slot(auc_RB, "y.values"))
auc_RB <- round(auc_RB,4)
legend(.7,.4, auc_RB, title="AUC", cex=0.8)

# V. KNN Classifier - Overview ----------

KNNPerfMeas

