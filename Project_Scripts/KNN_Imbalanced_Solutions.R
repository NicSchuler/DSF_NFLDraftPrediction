rm(list=ls())
graphics.off()

library(tidyverse)
library(dplyr)      # for data manipulation
library(caret)      # for model-building
library(DMwR)       # for smote implementation
library(purrr)      # for functional programming (map)
library(pROC)       # for AUC calculations
library(ROCR)       # Visualizing the Performance of Scoring Classifiers
library(ggplot2)    # Data Visualization                                               
library(corrplot)   # Visualization of Correlation                                     
library(reshape2)   # Flexibily Reshape Data   

load("../Data/CleanClass2007to2014_new.Rdata")
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

#3.1 Initial Model

ctrl <- trainControl(method="repeatedcv", number=10, repeats = 3, summaryFunction = twoClassSummary,
                           classProbs = TRUE,)

set.seed(6969)
orig_fit <- train(make.names(Drafted)~., 
                 data=dtrain_tog, 
                 method="knn",
                 trControl=ctrl,
                 metric = "ROC",
                 preProcess=c("center", "scale")) 

# ROC
pred_orig <- predict(orig_fit,dtrain_tog, type = 'prob')
pred_orig <- prediction(pred_orig[,2], dtrain_tog$Drafted)
ROC_orig <- performance(pred_orig, "tpr", "fpr")
plot(ROC_orig, colorize=T, main="ROC Curve Together")
abline(a=0, b=1)

#3.2 Weighted

# Create model weights (they sum to one)

model_weights <- ifelse(dtrain_tog$Drafted == "0",
                        (1/table(dtrain_tog$Drafted)[1]) * 0.5,
                        (1/table(dtrain_tog$Drafted)[2]) * 0.5)

# Use the same seed to ensure same cross-validation splits

ctrl$seeds <- orig_fit$control$seeds

# Build weighted model

weighted_fit <- train(make.names(Drafted) ~ .,
                      data = dtrain_tog,
                      method = "knn",
                      weights = model_weights,
                      metric = "ROC",
                      trControl = ctrl,
                      preProcess=c("center", "scale"))

# ROC
pred_weighted <- predict(weighted_fit,dtest_tog, type = 'prob')
pred_weighted <- prediction(pred_weighted[,2], dtest_tog$Drafted)
ROC_weighted <- performance(pred_weighted, "tpr", "fpr")
plot(ROC_weighted, colorize=T, main="ROC Curve Weighted")
abline(a=0, b=1)



#3.3  Build down-sampled model

ctrl$sampling <- "down"

down_fit <- train(make.names(Drafted) ~ .,
                  data = dtrain_tog,
                  method = "knn",
                  metric = "ROC",
                  trControl = ctrl,
                  preProcess=c("center", "scale"))

# ROC
pred_down <- predict(down_fit,dtest_tog, type = 'prob')
pred_down <- prediction(pred_down[,2], dtest_tog$Drafted)
ROC_down <- performance(pred_down, "tpr", "fpr")
plot(ROC_down, colorize=T, main="ROC Curve Down-sampled")
abline(a=0, b=1)



#3.4 Build up-sampled model

ctrl$sampling <- "up"

up_fit <- train(make.names(Drafted) ~ .,
                data = dtrain_tog,
                method = "knn",
                metric = "ROC",
                trControl = ctrl,
                preProcess=c("center", "scale"))

#3.5 Build smote model

ctrl$sampling <- "smote"

smote_fit <- train(make.names(Drafted) ~ .,
                   data = dtrain_tog,
                   method = "knn",
                   metric = "ROC",
                   trControl = ctrl,
                   preProcess=c("center", "scale"))


#4. Examine results for train set

model_list <- list(original = orig_fit,
                   weighted = weighted_fit,
                   down = down_fit,
                   up = up_fit,
                   SMOTE = smote_fit)

model_list_roc <- model_list %>%
  map(test_roc, data = dtest_tog)

model_list_roc %>%
  map(auc)







