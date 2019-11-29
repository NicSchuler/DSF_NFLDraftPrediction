


rm(list=ls())
graphics.off()

library(tidyverse)
library(boot)
library(caret)


### Preparations ---------

## Data for 2007 to 2014

load("../Data/CleanData/CleanClass2007to2014_2.Rdata")
no_samplingData = as_tibble(CleanClass2007to2014_2)

# oversampling

load("../Data/CleanData/CleanClass2007to2013_3_oversampling.Rdata")
oversamplingData = as_tibble(CleanClass2007to2014_3_oversampling)

# undersampling

load("../Data/CleanData/CleanClass2007to2013_3_undersampling.Rdata")
undersamplingData = CleanClass2007to2014_3_undersampling

# Rose_both

load("../Data/CleanData/CleanClass2007to2013_3_Rose.both.Rdata")
Rose_bothData = CleanClass2007to2014_3_Rose.both

# Smote

load("../Data/CleanData/CleanClass2007to2013_3_smote.Rdata")
SmoteData = cleanData_smote


## matrix for performance measurement

LogisticRegressionPerfMeas = data.frame(Method = character(), Sampling = character(), QB_TP = integer(), QB_TN = integer(), QB_FP = integer(), QB_FN = integer(),
                                        
                                        WR_TP = integer(), WR_TN = integer(), WR_FP = integer(), WR_FN = integer(),
                                        
                                        RB_TP = integer(), RB_TN = integer(), RB_FP = integer(), RB_FN = integer(),
                                        
                                        Together_TP = integer(), Together_TN = integer(), Together_FP = integer(), Together_FN = integer(), stringsAsFactors = FALSE)



LogisticRegressionPerfMeas[1:5,1] = "LogisticRegression"
LogisticRegressionPerfMeas[1:5,2] = c("no_sampling", "oversampling", "undersampling", "Rose_both", "Smote")


# 1. No Sampling ---------

### Logistic Regression for all players together --------

## training and testing data
# We use years 2007 to 2013 for training and the year 2014 for testing.

Data_tog_train = no_samplingData %>%
  filter(Year != 2014) %>%
  select(-Class, -Position, -Name, -Player.Code, -Year)

Data_tog_test = no_samplingData %>%
  filter(Year == 2014) %>%
  select(-Class, -Position, -Name, -Player.Code, -Year)

## training the model on the training data, including a 10-fold cross-validation
model_logit_tog = train(Drafted ~ .,
                        data = Data_tog_train,
                        trControl = trainControl(method = "cv", number = 10),
                        method = "glm",
                        family=binomial())

## Performance Measurement
# training error

Together_Pred = ifelse(predict(model_logit_tog)>0.5, 1, 0)

CheckList_train_tog_1 = tibble("Together_Pred" = Together_Pred,
                               "Together_TP" = ifelse(Together_Pred == 1 & Data_tog_train == 1, 1, 0),
                               "Together_FP" = ifelse(Together_Pred == 1 & Data_tog_train == 0, 1, 0),
                               "Together_TN" = ifelse(Together_Pred == 0 & Data_tog_train == 0, 1, 0),
                               "Together_FN" = ifelse(Together_Pred == 0 & Data_tog_train == 1, 1, 0))

# testing error 

Together_Pred = ifelse(predict(model_logit_tog, newdata = Data_tog_test)>0.5, 1, 0)

CheckList_test_tog_1 = tibble("Together_Pred" = Together_Pred,
                              "Together_TP" = ifelse(Together_Pred == 1 & Data_tog_test == 1, 1, 0),
                              "Together_FP" = ifelse(Together_Pred == 1 & Data_tog_test == 0, 1, 0),
                              "Together_TN" = ifelse(Together_Pred == 0 & Data_tog_test == 0, 1, 0),
                              "Together_FN" = ifelse(Together_Pred == 0 & Data_tog_test == 1, 1, 0))

# Fill the Performance Measurement Matrix

LogisticRegressionPerfMeas[1,"Together_TP"] = sum(CheckList_test_tog_1$Together_TP)
LogisticRegressionPerfMeas[1,"Together_TN"] = sum(CheckList_test_tog_1$Together_TN)
LogisticRegressionPerfMeas[1,"Together_FP"] = sum(CheckList_test_tog_1$Together_FP)
LogisticRegressionPerfMeas[1,"Together_FN"] = sum(CheckList_test_tog_1$Together_FN)


### Logistic Regression for QBs --------

## training and testing data
# We use years 2007 to 2013 for training and the year 2014 for testing.

Data_QB_train = no_samplingData %>%
  filter(Year != 2014) %>%
  filter(Position == "QB") %>%
  select(-Class, -Position, -Name, -Player.Code, -Year)

Data_QB_test = no_samplingData %>%
  filter(Year == 2014) %>%
  filter(Position == "QB") %>%
  select(-Class, -Position, -Name, -Player.Code, -Year)

## training the model on the training set, including a 10-fold cross-validation
model_logit_QB <- train(Drafted ~ .,
                        data = Data_QB_train,
                        trControl = trainControl(method = "cv", number = 10),
                        method = "glm",
                        family=binomial())

## Performance Measurement
# training error

QB_Pred = ifelse(predict(model_logit_QB)>0.5, 1, 0)

CheckList_train_QB_1 = tibble("QB_Pred" = QB_Pred,
                              "QB_TP" = ifelse(QB_Pred == 1 &Data_QB_train == 1, 1, 0),
                              "QB_FP" = ifelse(QB_Pred == 1 &Data_QB_train == 0, 1, 0),
                              "QB_TN" = ifelse(QB_Pred == 0 &Data_QB_train == 0, 1, 0),
                              "QB_FN" = ifelse(QB_Pred == 0 &Data_QB_train == 1, 1, 0))

# testing error 

QB_Pred = ifelse(predict(model_logit_QB, newdata = Data_QB_test)>0.5, 1, 0)

CheckList_test_QB_1 = tibble("QB_Pred" = QB_Pred,
                             "QB_TP" = ifelse(QB_Pred == 1 & Data_QB_test == 1, 1, 0),
                             "QB_FP" = ifelse(QB_Pred == 1 & Data_QB_test == 0, 1, 0),
                             "QB_TN" = ifelse(QB_Pred == 0 & Data_QB_test == 0, 1, 0),
                             "QB_FN" = ifelse(QB_Pred == 0 & Data_QB_test == 1, 1, 0))

# Fill the Performance Measurement Matrix

LogisticRegressionPerfMeas[1,"QB_TP"] = sum(CheckList_test_QB_1$QB_TP)
LogisticRegressionPerfMeas[1,"QB_TN"] = sum(CheckList_test_QB_1$QB_TN)
LogisticRegressionPerfMeas[1,"QB_FP"] = sum(CheckList_test_QB_1$QB_FP)
LogisticRegressionPerfMeas[1,"QB_FN"] = sum(CheckList_test_QB_1$QB_FN)


### Logistic Regression for WRs --------

## training and testing data
# We use years 2007 to 2013 for training and the year 2014 for testing.

Data_WR_train = no_samplingData %>%
  filter(Year != 2014) %>%
  filter(Position == "WR") %>%
  select(-Class, -Position, -Name, -Player.Code, -Year)

Data_WR_test = no_samplingData %>%
  filter(Year == 2014) %>%
  filter(Position == "WR") %>%
  select(-Class, -Position, -Name, -Player.Code, -Year)

## train the model on the training data, including a 10-fold cross-validation
model_logit_WR <- train(Drafted ~ .,
                        data = Data_WR_train,
                        trControl = trainControl(method = "cv", number = 10),
                        method = "glm",
                        family=binomial())

## Performance Measurement
# training error

WR_Pred = ifelse(predict(model_logit_WR)>0.5, 1, 0)

CheckList_train_WR_1 = tibble("WR_Pred" = WR_Pred,
                              "WR_TP" = ifelse(WR_Pred == 1 &Data_WR_train == 1, 1, 0),
                              "WR_FP" = ifelse(WR_Pred == 1 &Data_WR_train == 0, 1, 0),
                              "WR_TN" = ifelse(WR_Pred == 0 &Data_WR_train == 0, 1, 0),
                              "WR_FN" = ifelse(WR_Pred == 0 &Data_WR_train == 1, 1, 0))

# testing error 

WR_Pred = ifelse(predict(model_logit_WR, newdata = Data_WR_test)>0.5, 1, 0)

CheckList_test_WR_1 = tibble("WR_Pred" = WR_Pred,
                             "WR_TP" = ifelse(WR_Pred == 1 & Data_WR_test == 1, 1, 0),
                             "WR_FP" = ifelse(WR_Pred == 1 & Data_WR_test == 0, 1, 0),
                             "WR_TN" = ifelse(WR_Pred == 0 & Data_WR_test == 0, 1, 0),
                             "WR_FN" = ifelse(WR_Pred == 0 & Data_WR_test == 1, 1, 0))

# Fill the Performance Measurement Matrix

LogisticRegressionPerfMeas[1,"WR_TP"] = sum(CheckList_test_WR_1$WR_TP)
LogisticRegressionPerfMeas[1,"WR_TN"] = sum(CheckList_test_WR_1$WR_TN)
LogisticRegressionPerfMeas[1,"WR_FP"] = sum(CheckList_test_WR_1$WR_FP)
LogisticRegressionPerfMeas[1,"WR_FN"] = sum(CheckList_test_WR_1$WR_FN)


### Logistic Regression for RBs --------

## training and testing data
# We use years 2007 to 2013 for training and the year 2014 for testing.

Data_RB_train = no_samplingData %>%
  filter(Year != 2014) %>%
  filter(Position == "RB") %>%
  select(-Class, -Position, -Name, -Player.Code, -Year)

Data_RB_test = no_samplingData %>%
  filter(Year == 2014) %>%
  filter(Position == "RB") %>%
  select(-Class, -Position, -Name, -Player.Code, -Year)

## train the model on training data, inclding a 10-fold cross-validation
model_logit_RB <- train(Drafted ~ .,
                        data = Data_RB_train,
                        trControl = trainControl(method = "cv", number = 10),
                        method = "glm",
                        family=binomial())

## Performance Measurement
# training error

RB_Pred = ifelse(predict(model_logit_RB)>0.5, 1, 0)

CheckList_train_RB_1 = tibble("RB_Pred" = RB_Pred,
                              "RB_TP" = ifelse(RB_Pred == 1 &Data_RB_train == 1, 1, 0),
                              "RB_FP" = ifelse(RB_Pred == 1 &Data_RB_train == 0, 1, 0),
                              "RB_TN" = ifelse(RB_Pred == 0 &Data_RB_train == 0, 1, 0),
                              "RB_FN" = ifelse(RB_Pred == 0 &Data_RB_train == 1, 1, 0))

# testing error 

RB_Pred = ifelse(predict(model_logit_RB, newdata = Data_RB_test)>0.5, 1, 0)

CheckList_test_RB_1 = tibble("RB_Pred" = RB_Pred,
                             "RB_TP" = ifelse(RB_Pred == 1 & Data_RB_test == 1, 1, 0),
                             "RB_FP" = ifelse(RB_Pred == 1 & Data_RB_test == 0, 1, 0),
                             "RB_TN" = ifelse(RB_Pred == 0 & Data_RB_test == 0, 1, 0),
                             "RB_FN" = ifelse(RB_Pred == 0 & Data_RB_test == 1, 1, 0))

# Fill the Performance Measurement Matrix

LogisticRegressionPerfMeas[1,"RB_TP"] = sum(CheckList_test_RB_1$RB_TP)
LogisticRegressionPerfMeas[1,"RB_TN"] = sum(CheckList_test_RB_1$RB_TN)
LogisticRegressionPerfMeas[1,"RB_FP"] = sum(CheckList_test_RB_1$RB_FP)
LogisticRegressionPerfMeas[1,"RB_FN"] = sum(CheckList_test_RB_1$RB_FN)


# 2. Oversampling ---------

### Logistic Regression for all players together --------

## training and testing data
# We use years 2007 to 2013 for training and the year 2014 for testing.

Data_tog_train = oversamplingData %>%
  filter(Year != 2014) %>%
  select(-Class, -Position, -Name, -Player.Code, -Year)

Data_tog_test = no_samplingData %>%
  filter(Year == 2014) %>%
  select(-Class, -Position, -Name, -Player.Code, -Year)

## training the model on the training data, including a 10-fold cross-validation
model_logit_tog = train(Drafted ~ .,
                        data = Data_tog_train,
                        trControl = trainControl(method = "cv", number = 10),
                        method = "glm",
                        family=binomial())

## Performance Measurement
# training error

Together_Pred = ifelse(predict(model_logit_tog)>0.5, 1, 0)

CheckList_train_tog_2 = tibble("Together_Pred" = Together_Pred,
                               "Together_TP" = ifelse(Together_Pred == 1 & Data_tog_train == 1, 1, 0),
                               "Together_FP" = ifelse(Together_Pred == 1 & Data_tog_train == 0, 1, 0),
                               "Together_TN" = ifelse(Together_Pred == 0 & Data_tog_train == 0, 1, 0),
                               "Together_FN" = ifelse(Together_Pred == 0 & Data_tog_train == 1, 1, 0))

# testing error 

Together_Pred = ifelse(predict(model_logit_tog, newdata = Data_tog_test)>0.5, 1, 0)

CheckList_test_tog_2 = tibble("Together_Pred" = Together_Pred,
                              "Together_TP" = ifelse(Together_Pred == 1 & Data_tog_test == 1, 1, 0),
                              "Together_FP" = ifelse(Together_Pred == 1 & Data_tog_test == 0, 1, 0),
                              "Together_TN" = ifelse(Together_Pred == 0 & Data_tog_test == 0, 1, 0),
                              "Together_FN" = ifelse(Together_Pred == 0 & Data_tog_test == 1, 1, 0))

# Fill the Performance Measurement Matrix

LogisticRegressionPerfMeas[2,"Together_TP"] = sum(CheckList_test_tog_2$Together_TP)
LogisticRegressionPerfMeas[2,"Together_TN"] = sum(CheckList_test_tog_2$Together_TN)
LogisticRegressionPerfMeas[2,"Together_FP"] = sum(CheckList_test_tog_2$Together_FP)
LogisticRegressionPerfMeas[2,"Together_FN"] = sum(CheckList_test_tog_2$Together_FN)


### Logistic Regression for QBs --------

## training and testing data
# We use years 2007 to 2013 for training and the year 2014 for testing.

Data_QB_train = oversamplingData %>%
  filter(Year != 2014) %>%
  filter(Position == "QB") %>%
  select(-Class, -Position, -Name, -Player.Code, -Year)

Data_QB_test = no_samplingData %>%
  filter(Year == 2014) %>%
  filter(Position == "QB") %>%
  select(-Class, -Position, -Name, -Player.Code, -Year)

## training the model on the training set, including a 10-fold cross-validation
model_logit_QB <- train(Drafted ~ .,
                        data = Data_QB_train,
                        trControl = trainControl(method = "cv", number = 10),
                        method = "glm",
                        family=binomial())

## Performance Measurement
# training error

QB_Pred = ifelse(predict(model_logit_QB)>0.5, 1, 0)

CheckList_train_QB_2 = tibble("QB_Pred" = QB_Pred,
                              "QB_TP" = ifelse(QB_Pred == 1 &Data_QB_train == 1, 1, 0),
                              "QB_FP" = ifelse(QB_Pred == 1 &Data_QB_train == 0, 1, 0),
                              "QB_TN" = ifelse(QB_Pred == 0 &Data_QB_train == 0, 1, 0),
                              "QB_FN" = ifelse(QB_Pred == 0 &Data_QB_train == 1, 1, 0))

# testing error 

QB_Pred = ifelse(predict(model_logit_QB, newdata = Data_QB_test)>0.5, 1, 0)

CheckList_test_QB_2 = tibble("QB_Pred" = QB_Pred,
                             "QB_TP" = ifelse(QB_Pred == 1 & Data_QB_test == 1, 1, 0),
                             "QB_FP" = ifelse(QB_Pred == 1 & Data_QB_test == 0, 1, 0),
                             "QB_TN" = ifelse(QB_Pred == 0 & Data_QB_test == 0, 1, 0),
                             "QB_FN" = ifelse(QB_Pred == 0 & Data_QB_test == 1, 1, 0))

# Fill the Performance Measurement Matrix

LogisticRegressionPerfMeas[2,"QB_TP"] = sum(CheckList_test_QB_2$QB_TP)
LogisticRegressionPerfMeas[2,"QB_TN"] = sum(CheckList_test_QB_2$QB_TN)
LogisticRegressionPerfMeas[2,"QB_FP"] = sum(CheckList_test_QB_2$QB_FP)
LogisticRegressionPerfMeas[2,"QB_FN"] = sum(CheckList_test_QB_2$QB_FN)


### Logistic Regression for WRs --------

## training and testing data
# We use years 2007 to 2013 for training and the year 2014 for testing.

Data_WR_train = oversamplingData %>%
  filter(Year != 2014) %>%
  filter(Position == "WR") %>%
  select(-Class, -Position, -Name, -Player.Code, -Year)

Data_WR_test = no_samplingData %>%
  filter(Year == 2014) %>%
  filter(Position == "WR") %>%
  select(-Class, -Position, -Name, -Player.Code, -Year)

## train the model on the training data, including a 10-fold cross-validation
model_logit_WR <- train(Drafted ~ .,
                        data = Data_WR_train,
                        trControl = trainControl(method = "cv", number = 10),
                        method = "glm",
                        family=binomial())

## Performance Measurement
# training error

WR_Pred = ifelse(predict(model_logit_WR)>0.5, 1, 0)

CheckList_train_WR_2 = tibble("WR_Pred" = WR_Pred,
                              "WR_TP" = ifelse(WR_Pred == 1 &Data_WR_train == 1, 1, 0),
                              "WR_FP" = ifelse(WR_Pred == 1 &Data_WR_train == 0, 1, 0),
                              "WR_TN" = ifelse(WR_Pred == 0 &Data_WR_train == 0, 1, 0),
                              "WR_FN" = ifelse(WR_Pred == 0 &Data_WR_train == 1, 1, 0))

# testing error 

WR_Pred = ifelse(predict(model_logit_WR, newdata = Data_WR_test)>0.5, 1, 0)

CheckList_test_WR_2 = tibble("WR_Pred" = WR_Pred,
                             "WR_TP" = ifelse(WR_Pred == 1 & Data_WR_test == 1, 1, 0),
                             "WR_FP" = ifelse(WR_Pred == 1 & Data_WR_test == 0, 1, 0),
                             "WR_TN" = ifelse(WR_Pred == 0 & Data_WR_test == 0, 1, 0),
                             "WR_FN" = ifelse(WR_Pred == 0 & Data_WR_test == 1, 1, 0))

# Fill the Performance Measurement Matrix

LogisticRegressionPerfMeas[2,"WR_TP"] = sum(CheckList_test_WR_2$WR_TP)
LogisticRegressionPerfMeas[2,"WR_TN"] = sum(CheckList_test_WR_2$WR_TN)
LogisticRegressionPerfMeas[2,"WR_FP"] = sum(CheckList_test_WR_2$WR_FP)
LogisticRegressionPerfMeas[2,"WR_FN"] = sum(CheckList_test_WR_2$WR_FN)


### Logistic Regression for RBs --------

## training and testing data
# We use years 2007 to 2013 for training and the year 2014 for testing.

Data_RB_train = oversamplingData %>%
  filter(Year != 2014) %>%
  filter(Position == "RB") %>%
  select(-Class, -Position, -Name, -Player.Code, -Year)

Data_RB_test = no_samplingData %>%
  filter(Year == 2014) %>%
  filter(Position == "RB") %>%
  select(-Class, -Position, -Name, -Player.Code, -Year)

## train the model on training data, inclding a 10-fold cross-validation
model_logit_RB <- train(Drafted ~ .,
                        data = Data_RB_train,
                        trControl = trainControl(method = "cv", number = 10),
                        method = "glm",
                        family=binomial())

## Performance Measurement
# training error

RB_Pred = ifelse(predict(model_logit_RB)>0.5, 1, 0)

CheckList_train_RB_2 = tibble("RB_Pred" = RB_Pred,
                            "RB_TP" = ifelse(RB_Pred == 1 &Data_RB_train == 1, 1, 0),
                            "RB_FP" = ifelse(RB_Pred == 1 &Data_RB_train == 0, 1, 0),
                            "RB_TN" = ifelse(RB_Pred == 0 &Data_RB_train == 0, 1, 0),
                            "RB_FN" = ifelse(RB_Pred == 0 &Data_RB_train == 1, 1, 0))

# testing error 

RB_Pred = ifelse(predict(model_logit_RB, newdata = Data_RB_test)>0.5, 1, 0)

CheckList_test_RB_2 = tibble("RB_Pred" = RB_Pred,
                           "RB_TP" = ifelse(RB_Pred == 1 & Data_RB_test == 1, 1, 0),
                           "RB_FP" = ifelse(RB_Pred == 1 & Data_RB_test == 0, 1, 0),
                           "RB_TN" = ifelse(RB_Pred == 0 & Data_RB_test == 0, 1, 0),
                           "RB_FN" = ifelse(RB_Pred == 0 & Data_RB_test == 1, 1, 0))

# Fill the Performance Measurement Matrix

LogisticRegressionPerfMeas[2,"RB_TP"] = sum(CheckList_test_RB_2$RB_TP)
LogisticRegressionPerfMeas[2,"RB_TN"] = sum(CheckList_test_RB_2$RB_TN)
LogisticRegressionPerfMeas[2,"RB_FP"] = sum(CheckList_test_RB_2$RB_FP)
LogisticRegressionPerfMeas[2,"RB_FN"] = sum(CheckList_test_RB_2$RB_FN)


# 3. Undersampling ---------

### Logistic Regression for all players together --------

## training and testing data
# We use years 2007 to 2013 for training and the year 2014 for testing.

Data_tog_train = undersamplingData %>%
  filter(Year != 2014) %>%
  select(-Class, -Position, -Name, -Player.Code, -Year)

Data_tog_test = no_samplingData %>%
  filter(Year == 2014) %>%
  select(-Class, -Position, -Name, -Player.Code, -Year)

## training the model on the training data, including a 10-fold cross-validation
model_logit_tog = train(Drafted ~ .,
               data = Data_tog_train,
               trControl = trainControl(method = "cv", number = 10),
               method = "glm",
               family=binomial())

## Performance Measurement
# training error

Together_Pred = ifelse(predict(model_logit_tog)>0.5, 1, 0)

CheckList_train_tog_3 = tibble("Together_Pred" = Together_Pred,
                 "Together_TP" = ifelse(Together_Pred == 1 & Data_tog_train == 1, 1, 0),
                 "Together_FP" = ifelse(Together_Pred == 1 & Data_tog_train == 0, 1, 0),
                 "Together_TN" = ifelse(Together_Pred == 0 & Data_tog_train == 0, 1, 0),
                 "Together_FN" = ifelse(Together_Pred == 0 & Data_tog_train == 1, 1, 0))

# testing error 

Together_Pred = ifelse(predict(model_logit_tog, newdata = Data_tog_test)>0.5, 1, 0)

CheckList_test_tog_3 = tibble("Together_Pred" = Together_Pred,
                 "Together_TP" = ifelse(Together_Pred == 1 & Data_tog_test == 1, 1, 0),
                 "Together_FP" = ifelse(Together_Pred == 1 & Data_tog_test == 0, 1, 0),
                 "Together_TN" = ifelse(Together_Pred == 0 & Data_tog_test == 0, 1, 0),
                 "Together_FN" = ifelse(Together_Pred == 0 & Data_tog_test == 1, 1, 0))

# Fill the Performance Measurement Matrix

LogisticRegressionPerfMeas[3,"Together_TP"] = sum(CheckList_test_tog_3$Together_TP)
LogisticRegressionPerfMeas[3,"Together_TN"] = sum(CheckList_test_tog_3$Together_TN)
LogisticRegressionPerfMeas[3,"Together_FP"] = sum(CheckList_test_tog_3$Together_FP)
LogisticRegressionPerfMeas[3,"Together_FN"] = sum(CheckList_test_tog_3$Together_FN)


### Logistic Regression for QBs --------

## training and testing data
# We use years 2007 to 2013 for training and the year 2014 for testing.

Data_QB_train = undersamplingData %>%
  filter(Year != 2014) %>%
  filter(Position == "QB") %>%
  select(-Class, -Position, -Name, -Player.Code, -Year)

Data_QB_test = no_samplingData %>%
  filter(Year == 2014) %>%
  filter(Position == "QB") %>%
  select(-Class, -Position, -Name, -Player.Code, -Year)

## training the model on the training set, including a 10-fold cross-validation
model_logit_QB <- train(Drafted ~ .,
                         data = Data_QB_train,
                         trControl = trainControl(method = "cv", number = 10),
                         method = "glm",
                         family=binomial())

## Performance Measurement
# training error

QB_Pred = ifelse(predict(model_logit_QB)>0.5, 1, 0)

CheckList_train_QB_3 = tibble("QB_Pred" = QB_Pred,
                             "QB_TP" = ifelse(QB_Pred == 1 &Data_QB_train == 1, 1, 0),
                             "QB_FP" = ifelse(QB_Pred == 1 &Data_QB_train == 0, 1, 0),
                             "QB_TN" = ifelse(QB_Pred == 0 &Data_QB_train == 0, 1, 0),
                             "QB_FN" = ifelse(QB_Pred == 0 &Data_QB_train == 1, 1, 0))

# testing error 

QB_Pred = ifelse(predict(model_logit_QB, newdata = Data_QB_test)>0.5, 1, 0)

CheckList_test_QB_3 = tibble("QB_Pred" = QB_Pred,
                            "QB_TP" = ifelse(QB_Pred == 1 & Data_QB_test == 1, 1, 0),
                            "QB_FP" = ifelse(QB_Pred == 1 & Data_QB_test == 0, 1, 0),
                            "QB_TN" = ifelse(QB_Pred == 0 & Data_QB_test == 0, 1, 0),
                            "QB_FN" = ifelse(QB_Pred == 0 & Data_QB_test == 1, 1, 0))

# Fill the Performance Measurement Matrix

LogisticRegressionPerfMeas[3,"QB_TP"] = sum(CheckList_test_QB_3$QB_TP)
LogisticRegressionPerfMeas[3,"QB_TN"] = sum(CheckList_test_QB_3$QB_TN)
LogisticRegressionPerfMeas[3,"QB_FP"] = sum(CheckList_test_QB_3$QB_FP)
LogisticRegressionPerfMeas[3,"QB_FN"] = sum(CheckList_test_QB_3$QB_FN)


### Logistic Regression for WRs --------

## training and testing data
# We use years 2007 to 2013 for training and the year 2014 for testing.

Data_WR_train = undersamplingData %>%
  filter(Year != 2014) %>%
  filter(Position == "WR") %>%
  select(-Class, -Position, -Name, -Player.Code, -Year)

Data_WR_test = no_samplingData %>%
  filter(Year == 2014) %>%
  filter(Position == "WR") %>%
  select(-Class, -Position, -Name, -Player.Code, -Year)

## train the model on the training data, including a 10-fold cross-validation
model_logit_WR <- train(Drafted ~ .,
                        data = Data_WR_train,
                        trControl = trainControl(method = "cv", number = 10),
                        method = "glm",
                        family=binomial())

## Performance Measurement
# training error

WR_Pred = ifelse(predict(model_logit_WR)>0.5, 1, 0)

CheckList_train_WR_3 = tibble("WR_Pred" = WR_Pred,
                            "WR_TP" = ifelse(WR_Pred == 1 &Data_WR_train == 1, 1, 0),
                            "WR_FP" = ifelse(WR_Pred == 1 &Data_WR_train == 0, 1, 0),
                            "WR_TN" = ifelse(WR_Pred == 0 &Data_WR_train == 0, 1, 0),
                            "WR_FN" = ifelse(WR_Pred == 0 &Data_WR_train == 1, 1, 0))

# testing error 

WR_Pred = ifelse(predict(model_logit_WR, newdata = Data_WR_test)>0.5, 1, 0)

CheckList_test_WR_3 = tibble("WR_Pred" = WR_Pred,
                           "WR_TP" = ifelse(WR_Pred == 1 & Data_WR_test == 1, 1, 0),
                           "WR_FP" = ifelse(WR_Pred == 1 & Data_WR_test == 0, 1, 0),
                           "WR_TN" = ifelse(WR_Pred == 0 & Data_WR_test == 0, 1, 0),
                           "WR_FN" = ifelse(WR_Pred == 0 & Data_WR_test == 1, 1, 0))

# Fill the Performance Measurement Matrix

LogisticRegressionPerfMeas[3,"WR_TP"] = sum(CheckList_test_WR_3$WR_TP)
LogisticRegressionPerfMeas[3,"WR_TN"] = sum(CheckList_test_WR_3$WR_TN)
LogisticRegressionPerfMeas[3,"WR_FP"] = sum(CheckList_test_WR_3$WR_FP)
LogisticRegressionPerfMeas[3,"WR_FN"] = sum(CheckList_test_WR_3$WR_FN)


### Logistic Regression for RBs --------

## training and testing data
# We use years 2007 to 2013 for training and the year 2014 for testing.

Data_RB_train = undersamplingData %>%
  filter(Year != 2014) %>%
  filter(Position == "RB") %>%
  select(-Class, -Position, -Name, -Player.Code, -Year)

Data_RB_test = no_samplingData %>%
  filter(Year == 2014) %>%
  filter(Position == "RB") %>%
  select(-Class, -Position, -Name, -Player.Code, -Year)

## train the model on training data, inclding a 10-fold cross-validation
model_logit_RB <- train(Drafted ~ .,
                        data = Data_RB_train,
                        trControl = trainControl(method = "cv", number = 10),
                        method = "glm",
                        family=binomial())

## Performance Measurement
# training error

RB_Pred = ifelse(predict(model_logit_RB)>0.5, 1, 0)

CheckList_train_RB_3 = tibble("RB_Pred" = RB_Pred,
                            "RB_TP" = ifelse(RB_Pred == 1 &Data_RB_train == 1, 1, 0),
                            "RB_FP" = ifelse(RB_Pred == 1 &Data_RB_train == 0, 1, 0),
                            "RB_TN" = ifelse(RB_Pred == 0 &Data_RB_train == 0, 1, 0),
                            "RB_FN" = ifelse(RB_Pred == 0 &Data_RB_train == 1, 1, 0))

# testing error 

RB_Pred = ifelse(predict(model_logit_RB, newdata = Data_RB_test)>0.5, 1, 0)

CheckList_test_RB_3 = tibble("RB_Pred" = RB_Pred,
                           "RB_TP" = ifelse(RB_Pred == 1 & Data_RB_test == 1, 1, 0),
                           "RB_FP" = ifelse(RB_Pred == 1 & Data_RB_test == 0, 1, 0),
                           "RB_TN" = ifelse(RB_Pred == 0 & Data_RB_test == 0, 1, 0),
                           "RB_FN" = ifelse(RB_Pred == 0 & Data_RB_test == 1, 1, 0))

# Fill the Performance Measurement Matrix

LogisticRegressionPerfMeas[3,"RB_TP"] = sum(CheckList_test_RB_3$RB_TP)
LogisticRegressionPerfMeas[3,"RB_TN"] = sum(CheckList_test_RB_3$RB_TN)
LogisticRegressionPerfMeas[3,"RB_FP"] = sum(CheckList_test_RB_3$RB_FP)
LogisticRegressionPerfMeas[3,"RB_FN"] = sum(CheckList_test_RB_3$RB_FN)



# 5. Smote ---------

### Logistic Regression for all players together --------

## training and testing data
# We use years 2007 to 2013 for training and the year 2014 for testing.

Data_tog_train = Rose_bothData %>%
  filter(Year != 2014) %>%
  select(-Class, -Position, -Name, -Player.Code, -Year)

Data_tog_test = no_samplingData %>%
  filter(Year == 2014) %>%
  select(-Class, -Position, -Name, -Player.Code, -Year)

## training the model on the training data, including a 10-fold cross-validation
model_logit_tog = train(Drafted ~ .,
               data = Data_tog_train,
               trControl = trainControl(method = "cv", number = 10),
               method = "glm",
               family=binomial())

## Performance Measurement
# training error

Together_Pred = ifelse(predict(model_logit_tog)>0.5, 1, 0)

CheckList_train_tog_4 = tibble("Together_Pred" = Together_Pred,
                 "Together_TP" = ifelse(Together_Pred == 1 & Data_tog_train == 1, 1, 0),
                 "Together_FP" = ifelse(Together_Pred == 1 & Data_tog_train == 0, 1, 0),
                 "Together_TN" = ifelse(Together_Pred == 0 & Data_tog_train == 0, 1, 0),
                 "Together_FN" = ifelse(Together_Pred == 0 & Data_tog_train == 1, 1, 0))

# testing error 

Together_Pred = ifelse(predict(model_logit_tog, newdata = Data_tog_test)>0.5, 1, 0)

CheckList_test_tog_4 = tibble("Together_Pred" = Together_Pred,
                 "Together_TP" = ifelse(Together_Pred == 1 & Data_tog_test == 1, 1, 0),
                 "Together_FP" = ifelse(Together_Pred == 1 & Data_tog_test == 0, 1, 0),
                 "Together_TN" = ifelse(Together_Pred == 0 & Data_tog_test == 0, 1, 0),
                 "Together_FN" = ifelse(Together_Pred == 0 & Data_tog_test == 1, 1, 0))

# Fill the Performance Measurement Matrix

LogisticRegressionPerfMeas[4,"Together_TP"] = sum(CheckList_test_tog_4$Together_TP)
LogisticRegressionPerfMeas[4,"Together_TN"] = sum(CheckList_test_tog_4$Together_TN)
LogisticRegressionPerfMeas[4,"Together_FP"] = sum(CheckList_test_tog_4$Together_FP)
LogisticRegressionPerfMeas[4,"Together_FN"] = sum(CheckList_test_tog_4$Together_FN)


### Logistic Regression for QBs --------

## training and testing data
# We use years 2007 to 2013 for training and the year 2014 for testing.

Data_QB_train = Rose_bothData %>%
  filter(Year != 2014) %>%
  filter(Position == "QB") %>%
  select(-Class, -Position, -Name, -Player.Code, -Year)

Data_QB_test = no_samplingData %>%
  filter(Year == 2014) %>%
  filter(Position == "QB") %>%
  select(-Class, -Position, -Name, -Player.Code, -Year)

## training the model on the training set, including a 10-fold cross-validation
model_logit_QB <- train(Drafted ~ .,
                         data = Data_QB_train,
                         trControl = trainControl(method = "cv", number = 10),
                         method = "glm",
                         family=binomial())

## Performance Measurement
# training error

QB_Pred = ifelse(predict(model_logit_QB)>0.5, 1, 0)

CheckList_train_QB_4 = tibble("QB_Pred" = QB_Pred,
                             "QB_TP" = ifelse(QB_Pred == 1 &Data_QB_train == 1, 1, 0),
                             "QB_FP" = ifelse(QB_Pred == 1 &Data_QB_train == 0, 1, 0),
                             "QB_TN" = ifelse(QB_Pred == 0 &Data_QB_train == 0, 1, 0),
                             "QB_FN" = ifelse(QB_Pred == 0 &Data_QB_train == 1, 1, 0))

# testing error 

QB_Pred = ifelse(predict(model_logit_QB, newdata = Data_QB_test)>0.5, 1, 0)

CheckList_test_QB_4 = tibble("QB_Pred" = QB_Pred,
                            "QB_TP" = ifelse(QB_Pred == 1 & Data_QB_test == 1, 1, 0),
                            "QB_FP" = ifelse(QB_Pred == 1 & Data_QB_test == 0, 1, 0),
                            "QB_TN" = ifelse(QB_Pred == 0 & Data_QB_test == 0, 1, 0),
                            "QB_FN" = ifelse(QB_Pred == 0 & Data_QB_test == 1, 1, 0))

# Fill the Performance Measurement Matrix

LogisticRegressionPerfMeas[4,"QB_TP"] = sum(CheckList_test_QB_4$QB_TP)
LogisticRegressionPerfMeas[4,"QB_TN"] = sum(CheckList_test_QB_4$QB_TN)
LogisticRegressionPerfMeas[4,"QB_FP"] = sum(CheckList_test_QB_4$QB_FP)
LogisticRegressionPerfMeas[4,"QB_FN"] = sum(CheckList_test_QB_4$QB_FN)


### Logistic Regression for WRs --------

## training and testing data
# We use years 2007 to 2013 for training and the year 2014 for testing.

Data_WR_train = Rose_bothData %>%
  filter(Year != 2014) %>%
  filter(Position == "WR") %>%
  select(-Class, -Position, -Name, -Player.Code, -Year)

Data_WR_test = no_samplingData %>%
  filter(Year == 2014) %>%
  filter(Position == "WR") %>%
  select(-Class, -Position, -Name, -Player.Code, -Year)

## train the model on the training data, including a 10-fold cross-validation
model_logit_WR <- train(Drafted ~ .,
                        data = Data_WR_train,
                        trControl = trainControl(method = "cv", number = 10),
                        method = "glm",
                        family=binomial())

## Performance Measurement
# training error

WR_Pred = ifelse(predict(model_logit_WR)>0.5, 1, 0)

CheckList_train_WR_4 = tibble("WR_Pred" = WR_Pred,
                            "WR_TP" = ifelse(WR_Pred == 1 &Data_WR_train == 1, 1, 0),
                            "WR_FP" = ifelse(WR_Pred == 1 &Data_WR_train == 0, 1, 0),
                            "WR_TN" = ifelse(WR_Pred == 0 &Data_WR_train == 0, 1, 0),
                            "WR_FN" = ifelse(WR_Pred == 0 &Data_WR_train == 1, 1, 0))

# testing error 

WR_Pred = ifelse(predict(model_logit_WR, newdata = Data_WR_test)>0.5, 1, 0)

CheckList_test_WR_4 = tibble("WR_Pred" = WR_Pred,
                           "WR_TP" = ifelse(WR_Pred == 1 & Data_WR_test == 1, 1, 0),
                           "WR_FP" = ifelse(WR_Pred == 1 & Data_WR_test == 0, 1, 0),
                           "WR_TN" = ifelse(WR_Pred == 0 & Data_WR_test == 0, 1, 0),
                           "WR_FN" = ifelse(WR_Pred == 0 & Data_WR_test == 1, 1, 0))

# Fill the Performance Measurement Matrix

LogisticRegressionPerfMeas[4,"WR_TP"] = sum(CheckList_test_WR_4$WR_TP)
LogisticRegressionPerfMeas[4,"WR_TN"] = sum(CheckList_test_WR_4$WR_TN)
LogisticRegressionPerfMeas[4,"WR_FP"] = sum(CheckList_test_WR_4$WR_FP)
LogisticRegressionPerfMeas[4,"WR_FN"] = sum(CheckList_test_WR_4$WR_FN)


### Logistic Regression for RBs --------

## training and testing data
# We use years 2007 to 2013 for training and the year 2014 for testing.

Data_RB_train = Rose_bothData %>%
  filter(Year != 2014) %>%
  filter(Position == "RB") %>%
  select(-Class, -Position, -Name, -Player.Code, -Year)

Data_RB_test = no_samplingData %>%
  filter(Year == 2014) %>%
  filter(Position == "RB") %>%
  select(-Class, -Position, -Name, -Player.Code, -Year)

## train the model on training data, inclding a 10-fold cross-validation
model_logit_RB <- train(Drafted ~ .,
                        data = Data_RB_train,
                        trControl = trainControl(method = "cv", number = 10),
                        method = "glm",
                        family=binomial())

## Performance Measurement
# training error

RB_Pred = ifelse(predict(model_logit_RB)>0.5, 1, 0)

CheckList_train_RB_4 = tibble("RB_Pred" = RB_Pred,
                            "RB_TP" = ifelse(RB_Pred == 1 &Data_RB_train == 1, 1, 0),
                            "RB_FP" = ifelse(RB_Pred == 1 &Data_RB_train == 0, 1, 0),
                            "RB_TN" = ifelse(RB_Pred == 0 &Data_RB_train == 0, 1, 0),
                            "RB_FN" = ifelse(RB_Pred == 0 &Data_RB_train == 1, 1, 0))

# testing error 

RB_Pred = ifelse(predict(model_logit_RB, newdata = Data_RB_test)>0.5, 1, 0)

CheckList_test_RB_4 = tibble("RB_Pred" = RB_Pred,
                           "RB_TP" = ifelse(RB_Pred == 1 & Data_RB_test == 1, 1, 0),
                           "RB_FP" = ifelse(RB_Pred == 1 & Data_RB_test == 0, 1, 0),
                           "RB_TN" = ifelse(RB_Pred == 0 & Data_RB_test == 0, 1, 0),
                           "RB_FN" = ifelse(RB_Pred == 0 & Data_RB_test == 1, 1, 0))

# Fill the Performance Measurement Matrix

LogisticRegressionPerfMeas[4,"RB_TP"] = sum(CheckList_test_RB_4$RB_TP)
LogisticRegressionPerfMeas[4,"RB_TN"] = sum(CheckList_test_RB_4$RB_TN)
LogisticRegressionPerfMeas[4,"RB_FP"] = sum(CheckList_test_RB_4$RB_FP)
LogisticRegressionPerfMeas[4,"RB_FN"] = sum(CheckList_test_RB_4$RB_FN)



# 5. Smote ---------

### Logistic Regression for all players together --------

## training and testing data
# We use years 2007 to 2013 for training and the year 2014 for testing.

Data_tog_train = SmoteData %>%
  filter(Year != 2014) %>%
  select(-Position, -Name, -Player.Code, -Year)

Data_tog_test = no_samplingData %>%
  filter(Year == 2014) %>%
  select(-Class, -Position, -Name, -Player.Code, -Year)

## training the model on the training data, including a 10-fold cross-validation
model_logit_tog = train(Drafted ~ .,
               data = Data_tog_train,
               trControl = trainControl(method = "cv", number = 10),
               method = "glm",
               family=binomial())

## Performance Measurement
# training error

Together_Pred = ifelse(predict(model_logit_tog)>0.5, 1, 0)

CheckList_train_tog_5 = tibble("Together_Pred" = Together_Pred,
                 "Together_TP" = ifelse(Together_Pred == 1 & Data_tog_train == 1, 1, 0),
                 "Together_FP" = ifelse(Together_Pred == 1 & Data_tog_train == 0, 1, 0),
                 "Together_TN" = ifelse(Together_Pred == 0 & Data_tog_train == 0, 1, 0),
                 "Together_FN" = ifelse(Together_Pred == 0 & Data_tog_train == 1, 1, 0))

# testing error 

Together_Pred = ifelse(predict(model_logit_tog, newdata = Data_tog_test)>0.5, 1, 0)

CheckList_test_tog_5 = tibble("Together_Pred" = Together_Pred,
                 "Together_TP" = ifelse(Together_Pred == 1 & Data_tog_test == 1, 1, 0),
                 "Together_FP" = ifelse(Together_Pred == 1 & Data_tog_test == 0, 1, 0),
                 "Together_TN" = ifelse(Together_Pred == 0 & Data_tog_test == 0, 1, 0),
                 "Together_FN" = ifelse(Together_Pred == 0 & Data_tog_test == 1, 1, 0))

# Fill the Performance Measurement Matrix

LogisticRegressionPerfMeas[5,"Together_TP"] = sum(CheckList_test_tog_5$Together_TP)
LogisticRegressionPerfMeas[5,"Together_TN"] = sum(CheckList_test_tog_5$Together_TN)
LogisticRegressionPerfMeas[5,"Together_FP"] = sum(CheckList_test_tog_5$Together_FP)
LogisticRegressionPerfMeas[5,"Together_FN"] = sum(CheckList_test_tog_5$Together_FN)


### Logistic Regression for QBs --------

## training and testing data
# We use years 2007 to 2013 for training and the year 2014 for testing.

Data_QB_train = SmoteData %>%
  filter(Year != 2014) %>%
  filter(Position == "QB") %>%
  select(-Position, -Name, -Player.Code, -Year)

Data_QB_test = no_samplingData %>%
  filter(Year == 2014) %>%
  filter(Position == "QB") %>%
  select(-Class, -Position, -Name, -Player.Code, -Year)

## training the model on the training set, including a 10-fold cross-validation
model_logit_QB <- train(Drafted ~ .,
                         data = Data_QB_train,
                         trControl = trainControl(method = "cv", number = 10),
                         method = "glm",
                         family=binomial())

## Performance Measurement
# training error

QB_Pred = ifelse(predict(model_logit_QB)>0.5, 1, 0)

CheckList_train_QB_5 = tibble("QB_Pred" = QB_Pred,
                             "QB_TP" = ifelse(QB_Pred == 1 &Data_QB_train == 1, 1, 0),
                             "QB_FP" = ifelse(QB_Pred == 1 &Data_QB_train == 0, 1, 0),
                             "QB_TN" = ifelse(QB_Pred == 0 &Data_QB_train == 0, 1, 0),
                             "QB_FN" = ifelse(QB_Pred == 0 &Data_QB_train == 1, 1, 0))

# testing error 

QB_Pred = ifelse(predict(model_logit_QB, newdata = Data_QB_test)>0.5, 1, 0)

CheckList_test_QB_5 = tibble("QB_Pred" = QB_Pred,
                            "QB_TP" = ifelse(QB_Pred == 1 & Data_QB_test == 1, 1, 0),
                            "QB_FP" = ifelse(QB_Pred == 1 & Data_QB_test == 0, 1, 0),
                            "QB_TN" = ifelse(QB_Pred == 0 & Data_QB_test == 0, 1, 0),
                            "QB_FN" = ifelse(QB_Pred == 0 & Data_QB_test == 1, 1, 0))

# Fill the Performance Measurement Matrix

LogisticRegressionPerfMeas[5,"QB_TP"] = sum(CheckList_test_QB_5$QB_TP)
LogisticRegressionPerfMeas[5,"QB_TN"] = sum(CheckList_test_QB_5$QB_TN)
LogisticRegressionPerfMeas[5,"QB_FP"] = sum(CheckList_test_QB_5$QB_FP)
LogisticRegressionPerfMeas[5,"QB_FN"] = sum(CheckList_test_QB_5$QB_FN)


### Logistic Regression for WRs --------

## training and testing data
# We use years 2007 to 2013 for training and the year 2014 for testing.

Data_WR_train = SmoteData %>%
  filter(Year != 2014) %>%
  filter(Position == "WR") %>%
  select(-Position, -Name, -Player.Code, -Year)

Data_WR_test = no_samplingData %>%
  filter(Year == 2014) %>%
  filter(Position == "WR") %>%
  select(-Class, -Position, -Name, -Player.Code, -Year)

## train the model on the training data, including a 10-fold cross-validation
model_logit_WR <- train(Drafted ~ .,
                        data = Data_WR_train,
                        trControl = trainControl(method = "cv", number = 10),
                        method = "glm",
                        family=binomial())

## Performance Measurement
# training error

WR_Pred = ifelse(predict(model_logit_WR)>0.5, 1, 0)

CheckList_train_WR_5 = tibble("WR_Pred" = WR_Pred,
                            "WR_TP" = ifelse(WR_Pred == 1 &Data_WR_train == 1, 1, 0),
                            "WR_FP" = ifelse(WR_Pred == 1 &Data_WR_train == 0, 1, 0),
                            "WR_TN" = ifelse(WR_Pred == 0 &Data_WR_train == 0, 1, 0),
                            "WR_FN" = ifelse(WR_Pred == 0 &Data_WR_train == 1, 1, 0))

# testing error 

WR_Pred = ifelse(predict(model_logit_WR, newdata = Data_WR_test)>0.5, 1, 0)

CheckList_test_WR_5 = tibble("WR_Pred" = WR_Pred,
                           "WR_TP" = ifelse(WR_Pred == 1 & Data_WR_test == 1, 1, 0),
                           "WR_FP" = ifelse(WR_Pred == 1 & Data_WR_test == 0, 1, 0),
                           "WR_TN" = ifelse(WR_Pred == 0 & Data_WR_test == 0, 1, 0),
                           "WR_FN" = ifelse(WR_Pred == 0 & Data_WR_test == 1, 1, 0))

# Fill the Performance Measurement Matrix

LogisticRegressionPerfMeas[5,"WR_TP"] = sum(CheckList_test_WR_5$WR_TP)
LogisticRegressionPerfMeas[5,"WR_TN"] = sum(CheckList_test_WR_5$WR_TN)
LogisticRegressionPerfMeas[5,"WR_FP"] = sum(CheckList_test_WR_5$WR_FP)
LogisticRegressionPerfMeas[5,"WR_FN"] = sum(CheckList_test_WR_5$WR_FN)


### Logistic Regression for RBs --------

## training and testing data
# We use years 2007 to 2013 for training and the year 2014 for testing.

Data_RB_train = SmoteData %>%
  filter(Year != 2014) %>%
  filter(Position == "RB") %>%
  select(-Position, -Name, -Player.Code, -Year)

Data_RB_test = no_samplingData %>%
  filter(Year == 2014) %>%
  filter(Position == "RB") %>%
  select(-Class, -Position, -Name, -Player.Code, -Year)

## train the model on training data, inclding a 10-fold cross-validation
model_logit_RB <- train(Drafted ~ .,
                        data = Data_RB_train,
                        trControl = trainControl(method = "cv", number = 10),
                        method = "glm",
                        family=binomial())

## Performance Measurement
# training error

RB_Pred = ifelse(predict(model_logit_RB)>0.5, 1, 0)

CheckList_train_RB_5 = tibble("RB_Pred" = RB_Pred,
                            "RB_TP" = ifelse(RB_Pred == 1 &Data_RB_train == 1, 1, 0),
                            "RB_FP" = ifelse(RB_Pred == 1 &Data_RB_train == 0, 1, 0),
                            "RB_TN" = ifelse(RB_Pred == 0 &Data_RB_train == 0, 1, 0),
                            "RB_FN" = ifelse(RB_Pred == 0 &Data_RB_train == 1, 1, 0))

# testing error 

RB_Pred = ifelse(predict(model_logit_RB, newdata = Data_RB_test)>0.5, 1, 0)

CheckList_test_RB_5 = tibble("RB_Pred" = RB_Pred,
                           "RB_TP" = ifelse(RB_Pred == 1 & Data_RB_test == 1, 1, 0),
                           "RB_FP" = ifelse(RB_Pred == 1 & Data_RB_test == 0, 1, 0),
                           "RB_TN" = ifelse(RB_Pred == 0 & Data_RB_test == 0, 1, 0),
                           "RB_FN" = ifelse(RB_Pred == 0 & Data_RB_test == 1, 1, 0))

# Fill the Performance Measurement Matrix

LogisticRegressionPerfMeas[5,"RB_TP"] = sum(CheckList_test_RB_5$RB_TP)
LogisticRegressionPerfMeas[5,"RB_TN"] = sum(CheckList_test_RB_5$RB_TN)
LogisticRegressionPerfMeas[5,"RB_FP"] = sum(CheckList_test_RB_5$RB_FP)
LogisticRegressionPerfMeas[5,"RB_FN"] = sum(CheckList_test_RB_5$RB_FN)

### Performance Measurement ----------

# Save Performance Measurement data frame

save(LogisticRegressionPerfMeas, file = "../Data/PerformanceMeasurement/LogisticRegressionPerfMeas.Rdata")
