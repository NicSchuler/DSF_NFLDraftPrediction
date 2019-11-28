
rm(list=ls())
graphics.off()

library(tidyverse)
library(boot)
library(caret)


### Preparations ---------

## Data for 2007 to 2014

load("../Data/CleanData/CleanClass2007to2014_2.Rdata")

cleanData = as_tibble(CleanClass2007to2014_2)

## matrix for performance measurement

LogisticRegressionPerfMeas = data.frame(Method = character(), Sampling = character(), QB_TP = integer(), QB_TN = integer(), QB_FP = integer(), QB_FN = integer(),
                                        
                                        WR_TP = integer(), WR_TN = integer(), WR_FP = integer(), WR_FN = integer(),
                                        
                                        RB_TP = integer(), RB_TN = integer(), RB_FP = integer(), RB_FN = integer(),
                                        
                                        Together_TP = integer(), Together_TN = integer(), Together_FP = integer(), Together_FN = integer(), stringsAsFactors = FALSE)



LogisticRegressionPerfMeas[1:5,1] = "LogisticRegression"





### Logistic Regression for all players together --------

## training and testing data
# We use years 2007 to 2013 for training and the year 2014 for testing.

Data_tog_train = cleanData %>%
  filter(Year != 2014) %>%
  select(-Class, -Position, -Name, -Player.Code, -Year)

Data_tog_test = cleanData %>%
  filter(Year == 2014) %>%
  select(-Class, -Position, -Name, -Player.Code, -Year)

## train the model

# define training control
train_control <- trainControl(method = "cv", number = 10)

# train the model on training set
model_logit_tog <- train(Drafted ~ .,
               data = Data_tog_train,
               trControl = train_control,
               method = "glm",
               family=binomial())

## Performance Measurement
# training error

Together_Pred = ifelse(predict(model_logit_tog)>0.5, 1, 0)

CheckList_train_tog = tibble("Together_Pred" = Together_Pred,
                 "Together_TP" = ifelse(Together_Pred == 1 & Data_tog_train == 1, 1, 0),
                 "Together_FP" = ifelse(Together_Pred == 1 & Data_tog_train == 0, 1, 0),
                 "Together_TN" = ifelse(Together_Pred == 0 & Data_tog_train == 0, 1, 0),
                 "Together_FN" = ifelse(Together_Pred == 0 & Data_tog_train == 1, 1, 0))

# testing error 

Together_Pred = ifelse(predict(model_logit_tog, newdata = Data_tog_test)>0.5, 1, 0)

CheckList_test_tog = tibble("Together_Pred" = Together_Pred,
                 "Together_TP" = ifelse(Together_Pred == 1 & Data_tog_test == 1, 1, 0),
                 "Together_FP" = ifelse(Together_Pred == 1 & Data_tog_test == 0, 1, 0),
                 "Together_TN" = ifelse(Together_Pred == 0 & Data_tog_test == 0, 1, 0),
                 "Together_FN" = ifelse(Together_Pred == 0 & Data_tog_test == 1, 1, 0))

# Fill the Performance Measurement Matrix

LogisticRegressionPerfMeas[1,"Together_TP"] = sum(CheckList_test_tog$Together_TP)

LogisticRegressionPerfMeas[1,"Together_TN"] = sum(CheckList_test_tog$Together_TN)

LogisticRegressionPerfMeas[1,"Together_FP"] = sum(CheckList_test_tog$Together_FP)

LogisticRegressionPerfMeas[1,"Together_FN"] = sum(CheckList_test_tog$Together_FN)


### Logistic Regression for QBs --------

## training and testing data
# We use years 2007 to 2013 for training and the year 2014 for testing.

Data_QB_train = cleanData %>%
  filter(Year != 2014) %>%
  filter(Position == "QB") %>%
  select(-Class, -Position, -Name, -Player.Code, -Year)

Data_QB_test = cleanData %>%
  filter(Year == 2014) %>%
  filter(Position == "QB") %>%
  select(-Class, -Position, -Name, -Player.Code, -Year)

## train the model

# define training control
train_control <- trainControl(method = "cv", number = 10)

# train the model on training set
model_logit_QB <- train(Drafted ~ .,
                         data = Data_QB_train,
                         trControl = train_control,
                         method = "glm",
                         family=binomial())

## Performance Measurement
# training error

QB_Pred = ifelse(predict(model_logit_QB)>0.5, 1, 0)

CheckList_train_QB = tibble("QB_Pred" = QB_Pred,
                             "QB_TP" = ifelse(QB_Pred == 1 &Data_QB_train == 1, 1, 0),
                             "QB_FP" = ifelse(QB_Pred == 1 &Data_QB_train == 0, 1, 0),
                             "QB_TN" = ifelse(QB_Pred == 0 &Data_QB_train == 0, 1, 0),
                             "QB_FN" = ifelse(QB_Pred == 0 &Data_QB_train == 1, 1, 0))

# testing error 

QB_Pred = ifelse(predict(model_logit_QB, newdata = Data_QB_test)>0.5, 1, 0)

CheckList_test_QB = tibble("QB_Pred" = QB_Pred,
                            "QB_TP" = ifelse(QB_Pred == 1 & Data_QB_test == 1, 1, 0),
                            "QB_FP" = ifelse(QB_Pred == 1 & Data_QB_test == 0, 1, 0),
                            "QB_TN" = ifelse(QB_Pred == 0 & Data_QB_test == 0, 1, 0),
                            "QB_FN" = ifelse(QB_Pred == 0 & Data_QB_test == 1, 1, 0))

# Fill the Performance Measurement Matrix

LogisticRegressionPerfMeas[1,"QB_TP"] = sum(CheckList_test_QB$QB_TP)

LogisticRegressionPerfMeas[1,"QB_TN"] = sum(CheckList_test_QB$QB_TN)

LogisticRegressionPerfMeas[1,"QB_FP"] = sum(CheckList_test_QB$QB_FP)

LogisticRegressionPerfMeas[1,"QB_FN"] = sum(CheckList_test_QB$QB_FN)


### Logistic Regression for WRs --------

## training and testing data
# We use years 2007 to 2013 for training and the year 2014 for testing.

Data_WR_train = cleanData %>%
  filter(Year != 2014) %>%
  filter(Position == "WR") %>%
  select(-Class, -Position, -Name, -Player.Code, -Year)

Data_WR_test = cleanData %>%
  filter(Year == 2014) %>%
  filter(Position == "WR") %>%
  select(-Class, -Position, -Name, -Player.Code, -Year)

## train the model

# define training control
train_control <- trainControl(method = "cv", number = 10)

# train the model on training set
model_logit_WR <- train(Drafted ~ .,
                        data = Data_WR_train,
                        trControl = train_control,
                        method = "glm",
                        family=binomial())

## Performance Measurement
# training error

WR_Pred = ifelse(predict(model_logit_WR)>0.5, 1, 0)

CheckList_train_WR = tibble("WR_Pred" = WR_Pred,
                            "WR_TP" = ifelse(WR_Pred == 1 &Data_WR_train == 1, 1, 0),
                            "WR_FP" = ifelse(WR_Pred == 1 &Data_WR_train == 0, 1, 0),
                            "WR_TN" = ifelse(WR_Pred == 0 &Data_WR_train == 0, 1, 0),
                            "WR_FN" = ifelse(WR_Pred == 0 &Data_WR_train == 1, 1, 0))

# testing error 

WR_Pred = ifelse(predict(model_logit_WR, newdata = Data_WR_test)>0.5, 1, 0)

CheckList_test_WR = tibble("WR_Pred" = WR_Pred,
                           "WR_TP" = ifelse(WR_Pred == 1 & Data_WR_test == 1, 1, 0),
                           "WR_FP" = ifelse(WR_Pred == 1 & Data_WR_test == 0, 1, 0),
                           "WR_TN" = ifelse(WR_Pred == 0 & Data_WR_test == 0, 1, 0),
                           "WR_FN" = ifelse(WR_Pred == 0 & Data_WR_test == 1, 1, 0))

# Fill the Performance Measurement Matrix

LogisticRegressionPerfMeas[1,"WR_TP"] = sum(CheckList_test_WR$WR_TP)

LogisticRegressionPerfMeas[1,"WR_TN"] = sum(CheckList_test_WR$WR_TN)

LogisticRegressionPerfMeas[1,"WR_FP"] = sum(CheckList_test_WR$WR_FP)

LogisticRegressionPerfMeas[1,"WR_FN"] = sum(CheckList_test_WR$WR_FN)


### Logistic Regression for RBs --------

## training and testing data
# We use years 2007 to 2013 for training and the year 2014 for testing.

Data_RB_train = cleanData %>%
  filter(Year != 2014) %>%
  filter(Position == "RB") %>%
  select(-Class, -Position, -Name, -Player.Code, -Year)

Data_RB_test = cleanData %>%
  filter(Year == 2014) %>%
  filter(Position == "RB") %>%
  select(-Class, -Position, -Name, -Player.Code, -Year)

## train the model

# define training control
train_control <- trainControl(method = "cv", number = 10)

# train the model on training set
model_logit_RB <- train(Drafted ~ .,
                        data = Data_RB_train,
                        trControl = train_control,
                        method = "glm",
                        family=binomial())

## Performance Measurement
# training error

RB_Pred = ifelse(predict(model_logit_RB)>0.5, 1, 0)

CheckList_train_RB = tibble("RB_Pred" = RB_Pred,
                            "RB_TP" = ifelse(RB_Pred == 1 &Data_RB_train == 1, 1, 0),
                            "RB_FP" = ifelse(RB_Pred == 1 &Data_RB_train == 0, 1, 0),
                            "RB_TN" = ifelse(RB_Pred == 0 &Data_RB_train == 0, 1, 0),
                            "RB_FN" = ifelse(RB_Pred == 0 &Data_RB_train == 1, 1, 0))

# testing error 

RB_Pred = ifelse(predict(model_logit_RB, newdata = Data_RB_test)>0.5, 1, 0)

CheckList_test_RB = tibble("RB_Pred" = RB_Pred,
                           "RB_TP" = ifelse(RB_Pred == 1 & Data_RB_test == 1, 1, 0),
                           "RB_FP" = ifelse(RB_Pred == 1 & Data_RB_test == 0, 1, 0),
                           "RB_TN" = ifelse(RB_Pred == 0 & Data_RB_test == 0, 1, 0),
                           "RB_FN" = ifelse(RB_Pred == 0 & Data_RB_test == 1, 1, 0))

# Fill the Performance Measurement Matrix

LogisticRegressionPerfMeas[1,"RB_TP"] = sum(CheckList_test_RB$RB_TP)

LogisticRegressionPerfMeas[1,"RB_TN"] = sum(CheckList_test_RB$RB_TN)

LogisticRegressionPerfMeas[1,"RB_FP"] = sum(CheckList_test_RB$RB_FP)

LogisticRegressionPerfMeas[1,"RB_FN"] = sum(CheckList_test_RB$RB_FN)

### Performance Measurement ----------

# Save Performance Measurement data frame

save(LogisticRegressionPerfMeas, file = "../Data/PerformanceMeasurement/LogisticRegressionPerfMeas.Rdata")

### Combined method ----------






save(LogisticRegressionCombinedMethod, file = "../Data/CombinedMethod/LogisticRegressionCombinedMethod.Rdata")




