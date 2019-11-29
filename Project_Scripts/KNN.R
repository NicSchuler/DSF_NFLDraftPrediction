
rm(list=ls())
graphics.off()

library(tidyverse)
library(caret)      # Classification and Regression Training

KNNPerfMeas = data.frame(Method = character(), Sampling = character(), QB_TP = integer(), QB_TN = integer(), QB_FP = integer(), QB_FN = integer(),
                                        WR_TP = integer(), WR_TN = integer(), WR_FP = integer(), WR_FN = integer(),
                                        RB_TP = integer(), RB_TN = integer(), RB_FP = integer(), RB_FN = integer(),
                                        Together_TP = integer(), Together_TN = integer(), Together_FP = integer(), Together_FN = integer(), stringsAsFactors = FALSE)

KNNPerfMeas[1,2] = "no_sampling"
KNNPerfMeas[2,2] = "oversampling"
KNNPerfMeas[3,2] = "undersampling"
KNNPerfMeas[4,2] = "Rose_both"
KNNPerfMeas[5,2] = "Smote"
KNNPerfMeas$Method = "KNN"

###################################################
# NOTICE
###################################################

# We will do the next steps 5 times (e.g. "1. No Sampling" does the same thing as "2. Oversampling"), but using different data for training the model
# In other words, this is the cross-validation of the sampling methods. The reason for doing it a couple of times instead of looping or functioning it
# is the easier availability of the steps in between in case of further processing them.

# 1. No Sampling ###################################################

load("../Data/CleanData/CleanClass2007to2014_3.Rdata")

# I. KNN Classifier - 07 to 13, together ----------

#1 - Preparations  ----------

# Training data
CleanClass2007to2013_3<- CleanClass2007to2014_3[CleanClass2007to2014_3$Year != 2014,]
CleanClass2007to2013_3$Drafted <- as.factor(CleanClass2007to2013_3$Drafted)
Data2007to2013_tog <- CleanClass2007to2013_3 %>% select(-Position, -Class, -Name, -Player.Code, -Year, 
                                                      -Safety) #this variable has zero variance hence it can not be normalized.
# Testing data
CleanClass2014_3<- CleanClass2007to2014_3[CleanClass2007to2014_3$Year == 2014,]
CleanClass2014_3$Drafted <- as.factor(CleanClass2014_3$Drafted)
CleanClass2014_3_tog <- CleanClass2014_3 %>% select(-Position, -Class, -Name, -Player.Code, -Year, 
                                                        -Safety) #this variable has zero variance hence it can not be normalized.

#2 - KNN ----------

tr_control <- trainControl(method="repeatedcv", number=10, repeats = 3)

set.seed(6969)
KNN_tog <- train(Drafted~., 
             data=Data2007to2013_tog,
             method="knn",
             trControl=tr_control,
             preProcess=c("center", "scale")) 

# Predictions: 0.5 is used for probability cutoff value by default
predict_tog <- predict(KNN_tog,Data2007to2013_tog) 
confusionMatrix(predict_tog,Data2007to2013_tog$Drafted)

CheckList_tog = cbind.data.frame(Data2007to2013_tog$Drafted,predict_tog)

names(CheckList_tog)[names(CheckList_tog)=="Data2007to2013_tog$Drafted"] <- "Y"
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

# II. KNN Classifier - 07 to 13, QB  ----------

#1 - Preparations  ----------

# Training data
Data2007to2013_QB <- CleanClass2007to2013_3[CleanClass2007to2013_3$Position=="QB", ]
Data2007to2013_QB <- Data2007to2013_QB %>% select(-Class, -Position, -Name, -Player.Code, -Year, 
-Safety, -Kickoff.Ret.TD, -Punt.Ret.TD) #these variables have zero variance hence they can not be normalized.

# Testing data
CleanClass2014_3_QB<- CleanClass2014_3[CleanClass2014_3$Position=="QB", ]
CleanClass2014_3_QB <- CleanClass2014_3_QB %>% select(-Class, -Position, -Name, -Player.Code, -Year,
-Safety, -Kickoff.Ret.TD, -Punt.Ret.TD) #these variables have zero variance hence they can not be normalized.

#2 - KNN ----------

tr_control <- trainControl(method="repeatedcv", number=10, repeats = 3)

set.seed(6969)
KNN_QB <- train(Drafted~., 
                 data=Data2007to2013_QB, 
                 method="knn",
                 trControl=tr_control,
                 preProcess=c("center", "scale")) 

# Substract mean (="center") from each value and then divide this result by standard deviation (="scale").
# The normalized result ist the z-value.

# Predictions
predict_QB <- predict(KNN_QB, newdata=Data2007to2013_QB)
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
KNNPerfMeas[1,"QB_TP"] = sum(CheckList_QB$TP)
KNNPerfMeas[1,"QB_TN"] = sum(CheckList_QB$TN)
KNNPerfMeas[1,"QB_FP"] = sum(CheckList_QB$FP)
KNNPerfMeas[1,"QB_FN"] = sum(CheckList_QB$FN)

# III. KNN Classifier - 07 to 13, WR ----------

#1 - Preparations ----------

# Training data
Data2007to2013_WR <- CleanClass2007to2013_3[CleanClass2007to2013_3$Position=="WR", ]
Data2007to2013_WR <- Data2007to2013_WR %>% select(-Class, -Position, -Name, -Player.Code, -Year, 
                                                  -Safety) #these variables have zero variance hence they can not be normalized.

# Testing data
CleanClass2014_3_WR<- CleanClass2014_3[CleanClass2014_3$Position=="WR", ]
CleanClass2014_3_WR <- CleanClass2014_3_WR %>% select(-Class, -Position, -Name, -Player.Code, -Year, 
                                                      -Safety) #these variables have zero variance hence they can not be normalized.

#2 - KNN ----------

tr_control <- trainControl(method="repeatedcv", number=10, repeats = 3)

set.seed(6969)
KNN_WR <- train(Drafted~., 
                data=Data2007to2013_WR, 
                method="knn",
                trControl=tr_control,
                preProcess=c("center", "scale")) 

# Substract mean (="center") from each value and then divide this result by standard deviation (="scale").
# The normalized result ist the z-value.

# Predictions
predict_WR <- predict(KNN_WR, newdata=Data2007to2013_WR)
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
KNNPerfMeas[1,"WR_TP"] = sum(CheckList_WR$TP)
KNNPerfMeas[1,"WR_TN"] = sum(CheckList_WR$TN)
KNNPerfMeas[1,"WR_FP"] = sum(CheckList_WR$FP)
KNNPerfMeas[1,"WR_FN"] = sum(CheckList_WR$FN)

# IV. KNN Classifier - 07 to 13, RB ----------
#1 - Preparations ----------

# Training data
Data2007to2013_RB <- CleanClass2007to2013_3[CleanClass2007to2013_3$Position=="RB", ]
Data2007to2013_RB <- Data2007to2013_RB %>% select(-Class, -Position, -Name, -Player.Code, -Year,
                                                  -Safety) #these variables have zero variance hence they can not be normalized.

# Testing data
CleanClass2014_3_RB <-  CleanClass2014_3[CleanClass2014_3$Position=="RB", ]
CleanClass2014_3_RB <- CleanClass2014_3_RB %>% select(-Class, -Position, -Name, -Player.Code, -Year,
                                                      -Safety) #these variables have zero variance hence they can not be normalized.

#2 - KNN ----------

tr_control <- trainControl(method="repeatedcv", number=10, repeats = 3)

set.seed(6969)
KNN_RB <- train(Drafted~., 
                data=Data2007to2013_RB, 
                method="knn",
                trControl=tr_control,
                preProcess=c("center", "scale")) 

# Substract mean (="center") from each value and then divide this result by standard deviation (="scale").
# The normalized result ist the z-value.

# Predictions
predict_RB <- predict(KNN_RB, newdata=Data2007to2013_RB)
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
KNNPerfMeas[1,"RB_TP"] = sum(CheckList_RB$TP)
KNNPerfMeas[1,"RB_TN"] = sum(CheckList_RB$TN)
KNNPerfMeas[1,"RB_FP"] = sum(CheckList_RB$FP)
KNNPerfMeas[1,"RB_FN"] = sum(CheckList_RB$FN)

# 2. Oversampling ###################################################

load("../Data/CleanData/CleanClass2007to2013_3_oversampling.Rdata")

# I. KNN Classifier - 07 to 13, together ----------

#1 - Preparations  ----------

# Training data
CleanClass2007to2014_3_oversampling$Drafted <- as.factor(CleanClass2007to2014_3_oversampling$Drafted)
Data2007to2013_togOS <- CleanClass2007to2014_3_oversampling %>% select(-Position, -Class, -Name, -Player.Code, -Year, 
                                                        -Safety) #this variable has zero variance hence it can not be normalized.
# Testing data
CleanClass2014_3_tog

#2 - KNN ----------

tr_control <- trainControl(method="repeatedcv", number=10, repeats = 3)

set.seed(6969)
KNN_togOS <- train(Drafted~., 
                 data=Data2007to2013_togOS,
                 method="knn",
                 trControl=tr_control,
                 preProcess=c("center", "scale")) 

# Predictions: 0.5 is used for probability cutoff value by default
predict_togOS <- predict(KNN_togOS,Data2007to2013_tog) 
confusionMatrix(predict_togOS,Data2007to2013_tog$Drafted)

CheckList_togOS = cbind.data.frame(Data2007to2013_tog$Drafted,predict_togOS)

names(CheckList_togOS)[names(CheckList_togOS)=="Data2007to2013_tog$Drafted"] <- "Y"
names(CheckList_togOS)[names(CheckList_togOS)=="predict_togOS"] <- "Pred"

CheckList_togOS = CheckList_togOS %>%
  mutate(TP=ifelse(Y==Pred,ifelse(Pred==1,1,0),0)) %>%
  mutate(TN=ifelse(Y==Pred,ifelse(Pred==0,1,0),0)) %>%
  mutate(FP=ifelse(Y!=Pred,ifelse(Pred==1,1,0),0)) %>%
  mutate(FN=ifelse(Y!=Pred,ifelse(Pred==0,1,0),0))

# Performance Measurement
KNNPerfMeas[2,"Together_TP"] = sum(CheckList_togOS$TP)
KNNPerfMeas[2,"Together_TN"] = sum(CheckList_togOS$TN)
KNNPerfMeas[2,"Together_FP"] = sum(CheckList_togOS$FP)
KNNPerfMeas[2,"Together_FN"] = sum(CheckList_togOS$FN)

# II. KNN Classifier - 07 to 13, QB  ----------

#1 - Preparations  ----------

# Training data
Data2007to2013_QBOS <- CleanClass2007to2014_3_oversampling[CleanClass2007to2014_3_oversampling$Position=="QB", ]
Data2007to2013_QBOS <- Data2007to2013_QBOS %>% select(-Class, -Position, -Name, -Player.Code, -Year,
                                                  -Safety, -Kickoff.Ret.TD, -Punt.Ret.TD) #these variables have zero variance hence they can not be normalized.

# Testing data
CleanClass2014_3_QB      
                                              
#2 - KNN ----------

tr_control <- trainControl(method="repeatedcv", number=10, repeats = 3)

set.seed(6969)
KNN_QBOS <- train(Drafted~., 
                data=Data2007to2013_QBOS, 
                method="knn",
                trControl=tr_control,
                preProcess=c("center", "scale")) 

# Substract mean (="center") from each value and then divide this result by standard deviation (="scale").
# The normalized result ist the z-value.

# Predictions
predict_QBOS <- predict(KNN_QBOS, newdata=Data2007to2013_QB)
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
KNNPerfMeas[2,"QB_TP"] = sum(CheckList_QBOS$TP)
KNNPerfMeas[2,"QB_TN"] = sum(CheckList_QBOS$TN)
KNNPerfMeas[2,"QB_FP"] = sum(CheckList_QBOS$FP)
KNNPerfMeas[2,"QB_FN"] = sum(CheckList_QBOS$FN)

# III. KNN Classifier - 07 to 13, WR ----------

#1 - Preparations ----------

# Training data
Data2007to2013_WROS <- CleanClass2007to2014_3_oversampling[CleanClass2007to2014_3_oversampling$Position=="WR", ]
Data2007to2013_WROS <- Data2007to2013_WROS %>% select(-Class, -Position, -Name, -Player.Code, -Year,
                                                  -Safety) #these variables have zero variance hence they can not be normalized.

# Testing data
CleanClass2014_3_WR

#2 - KNN ----------

tr_control <- trainControl(method="repeatedcv", number=10, repeats = 3)

set.seed(6969)
KNN_WROS <- train(Drafted~., 
                data=Data2007to2013_WROS, 
                method="knn",
                trControl=tr_control,
                preProcess=c("center", "scale")) 

# Substract mean (="center") from each value and then divide this result by standard deviation (="scale").
# The normalized result ist the z-value.

# Predictions
predict_WROS <- predict(KNN_WROS, newdata=Data2007to2013_WR)
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
KNNPerfMeas[2,"WR_TP"] = sum(CheckList_WROS$TP)
KNNPerfMeas[2,"WR_TN"] = sum(CheckList_WROS$TN)
KNNPerfMeas[2,"WR_FP"] = sum(CheckList_WROS$FP)
KNNPerfMeas[2,"WR_FN"] = sum(CheckList_WROS$FN)

# IV. KNN Classifier - 07 to 13, RB ----------
#1 - Preparations ----------

# Training data
Data2007to2013_RBOS <- CleanClass2007to2014_3_oversampling[CleanClass2007to2014_3_oversampling$Position=="RB", ]
Data2007to2013_RBOS <- Data2007to2013_RBOS %>% select(-Class, -Position, -Name, -Player.Code, -Year, 
                                                  -Safety) #these variables have zero variance hence they can not be normalized.

# Testing data
CleanClass2014_3_RB

#2 - KNN ----------

tr_control <- trainControl(method="repeatedcv", number=10, repeats = 3)

set.seed(6969)
KNN_RBOS <- train(Drafted~., 
                data=Data2007to2013_RBOS, 
                method="knn",
                trControl=tr_control,
                preProcess=c("center", "scale")) 

# Substract mean (="center") from each value and then divide this result by standard deviation (="scale").
# The normalized result ist the z-value.

# Predictions
predict_RBOS <- predict(KNN_RBOS, newdata=Data2007to2013_RB)
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
KNNPerfMeas[2,"RB_TP"] = sum(CheckList_RBOS$TP)
KNNPerfMeas[2,"RB_TN"] = sum(CheckList_RBOS$TN)
KNNPerfMeas[2,"RB_FP"] = sum(CheckList_RBOS$FP)
KNNPerfMeas[2,"RB_FN"] = sum(CheckList_RBOS$FN)

# 3. Undersampling ###################################################

load("../Data/CleanData/CleanClass2007to2013_3_undersampling.Rdata")

# I. KNN Classifier - 07 to 13, together ----------

#1 - Preparations  ----------

# Training data
CleanClass2007to2014_3_undersampling$Drafted <- as.factor(CleanClass2007to2014_3_undersampling$Drafted)
Data2007to2013_togUS <- CleanClass2007to2014_3_undersampling %>% select(-Position, -Class, -Name, -Player.Code, -Year, 
                                                                       -Safety) #this variable has zero variance hence it can not be normalized.
# Testing data
CleanClass2014_3_tog

#2 - KNN ----------

tr_control <- trainControl(method="repeatedcv", number=10, repeats = 3)

set.seed(6969)
KNN_togUS <- train(Drafted~., 
                   data=Data2007to2013_togUS,
                   method="knn",
                   trControl=tr_control,
                   preProcess=c("center", "scale")) 

# Predictions: 0.5 is used for probability cutoff value by default
predict_togUS <- predict(KNN_togUS,Data2007to2013_tog) 
confusionMatrix(predict_togUS,Data2007to2013_tog$Drafted)

CheckList_togUS = cbind.data.frame(Data2007to2013_tog$Drafted,predict_togUS)

names(CheckList_togUS)[names(CheckList_togUS)=="Data2007to2013_tog$Drafted"] <- "Y"
names(CheckList_togUS)[names(CheckList_togUS)=="predict_togUS"] <- "Pred"

CheckList_togUS = CheckList_togUS %>%
  mutate(TP=ifelse(Y==Pred,ifelse(Pred==1,1,0),0)) %>%
  mutate(TN=ifelse(Y==Pred,ifelse(Pred==0,1,0),0)) %>%
  mutate(FP=ifelse(Y!=Pred,ifelse(Pred==1,1,0),0)) %>%
  mutate(FN=ifelse(Y!=Pred,ifelse(Pred==0,1,0),0))

# Performance Measurement
KNNPerfMeas[3,"Together_TP"] = sum(CheckList_togUS$TP)
KNNPerfMeas[3,"Together_TN"] = sum(CheckList_togUS$TN)
KNNPerfMeas[3,"Together_FP"] = sum(CheckList_togUS$FP)
KNNPerfMeas[3,"Together_FN"] = sum(CheckList_togUS$FN)

# II. KNN Classifier - 07 to 13, QB  ----------

#1 - Preparations  ----------

# Training data
Data2007to2013_QBUS <- CleanClass2007to2014_3_undersampling[CleanClass2007to2014_3_undersampling$Position=="QB", ]
Data2007to2013_QBUS <- Data2007to2013_QBUS %>% select(-Class, -Position, -Name, -Player.Code, -Year,
                                                      -Safety, -Kickoff.Ret.TD, -Punt.Ret.TD, -Kickoff.Ret, -Kickoff.Ret.Yard, -Punt.Ret, -Punt.Ret.Yard) #these variables have zero variance hence they can not be normalized.

# Testing data
CleanClass2014_3_QB      

#2 - KNN ----------

tr_control <- trainControl(method="repeatedcv", number=10, repeats = 3)

set.seed(6969)
KNN_QBUS <- train(Drafted~., 
                  data=Data2007to2013_QBUS, 
                  method="knn",
                  trControl=tr_control,
                  preProcess=c("center", "scale")) 

# Substract mean (="center") from each value and then divide this result by standard deviation (="scale").
# The normalized result ist the z-value.

# Predictions
predict_QBUS <- predict(KNN_QBUS, newdata=Data2007to2013_QB)
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
KNNPerfMeas[3,"QB_TP"] = sum(CheckList_QBUS$TP)
KNNPerfMeas[3,"QB_TN"] = sum(CheckList_QBUS$TN)
KNNPerfMeas[3,"QB_FP"] = sum(CheckList_QBUS$FP)
KNNPerfMeas[3,"QB_FN"] = sum(CheckList_QBUS$FN)

# III. KNN Classifier - 07 to 13, WR ----------

#1 - Preparations ----------

# Training data
Data2007to2013_WRUS <- CleanClass2007to2014_3_undersampling[CleanClass2007to2014_3_undersampling$Position=="WR", ]
Data2007to2013_WRUS <- Data2007to2013_WRUS %>% select(-Class, -Position, -Name, -Player.Code, -Year,
                                                      -Safety) #these variables have zero variance hence they can not be normalized.

# Testing data
CleanClass2014_3_WR

#2 - KNN ----------

tr_control <- trainControl(method="repeatedcv", number=10, repeats = 3)

set.seed(6969)
KNN_WRUS <- train(Drafted~., 
                  data=Data2007to2013_WRUS, 
                  method="knn",
                  trControl=tr_control,
                  preProcess=c("center", "scale")) 

# Substract mean (="center") from each value and then divide this result by standard deviation (="scale").
# The normalized result ist the z-value.

# Predictions
predict_WRUS <- predict(KNN_WRUS, newdata=Data2007to2013_WR)
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
KNNPerfMeas[3,"WR_TP"] = sum(CheckList_WRUS$TP)
KNNPerfMeas[3,"WR_TN"] = sum(CheckList_WRUS$TN)
KNNPerfMeas[3,"WR_FP"] = sum(CheckList_WRUS$FP)
KNNPerfMeas[3,"WR_FN"] = sum(CheckList_WRUS$FN)

# IV. KNN Classifier - 07 to 13, RB ----------

#1 - Preparations ----------

# Training data
Data2007to2013_RBUS <- CleanClass2007to2014_3_undersampling[CleanClass2007to2014_3_undersampling$Position=="RB", ]
Data2007to2013_RBUS <- Data2007to2013_RBUS %>% select(-Class, -Position, -Name, -Player.Code, -Year,
                                                      -Safety, -Pass.Conv) #these variables have zero variance hence they can not be normalized.

# Testing data
CleanClass2014_3_RB

#2 - KNN ----------

tr_control <- trainControl(method="repeatedcv", number=10, repeats = 3)

set.seed(6969)
KNN_RBUS <- train(Drafted~., 
                  data=Data2007to2013_RBUS, 
                  method="knn",
                  trControl=tr_control,
                  preProcess=c("center", "scale")) 

# Substract mean (="center") from each value and then divide this result by standard deviation (="scale").
# The normalized result ist the z-value.

# Predictions
predict_RBUS <- predict(KNN_RBUS, newdata=Data2007to2013_RB)
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
KNNPerfMeas[3,"RB_TP"] = sum(CheckList_RBUS$TP)
KNNPerfMeas[3,"RB_TN"] = sum(CheckList_RBUS$TN)
KNNPerfMeas[3,"RB_FP"] = sum(CheckList_RBUS$FP)
KNNPerfMeas[3,"RB_FN"] = sum(CheckList_RBUS$FN)

# 4. Rose_both ###################################################

load("../Data/CleanData/CleanClass2007to2013_3_Rose.both.Rdata")

# I. KNN Classifier - 07 to 13, together ----------

#1 - Preparations  ----------

# Training data
CleanClass2007to2014_3_Rose.both$Drafted <- as.factor(CleanClass2007to2014_3_Rose.both$Drafted)
Data2007to2013_togBO <- CleanClass2007to2014_3_Rose.both %>% select(-Position, -Class, -Name, -Player.Code, -Year, 
                                                                        -Safety) #this variable has zero variance hence it can not be normalized.
# Testing data
CleanClass2014_3_tog

#2 - KNN ----------

tr_control <- trainControl(method="repeatedcv", number=10, repeats = 3)

set.seed(6969)
KNN_togBO <- train(Drafted~., 
                   data=Data2007to2013_togBO,
                   method="knn",
                   trControl=tr_control,
                   preProcess=c("center", "scale")) 

# Predictions: 0.5 is used for probability cutoff value by default
predict_togBO <- predict(KNN_togBO,Data2007to2013_tog) 
confusionMatrix(predict_togBO,Data2007to2013_tog$Drafted)

CheckList_togBO = cbind.data.frame(Data2007to2013_tog$Drafted,predict_togBO)

names(CheckList_togBO)[names(CheckList_togBO)=="Data2007to2013_tog$Drafted"] <- "Y"
names(CheckList_togBO)[names(CheckList_togBO)=="predict_togBO"] <- "Pred"

CheckList_togBO = CheckList_togBO %>%
  mutate(TP=ifelse(Y==Pred,ifelse(Pred==1,1,0),0)) %>%
  mutate(TN=ifelse(Y==Pred,ifelse(Pred==0,1,0),0)) %>%
  mutate(FP=ifelse(Y!=Pred,ifelse(Pred==1,1,0),0)) %>%
  mutate(FN=ifelse(Y!=Pred,ifelse(Pred==0,1,0),0))

# Performance Measurement
KNNPerfMeas[4,"Together_TP"] = sum(CheckList_togBO$TP)
KNNPerfMeas[4,"Together_TN"] = sum(CheckList_togBO$TN)
KNNPerfMeas[4,"Together_FP"] = sum(CheckList_togBO$FP)
KNNPerfMeas[4,"Together_FN"] = sum(CheckList_togBO$FN)

# II. KNN Classifier - 07 to 13, QB  ----------

#1 - Preparations  ----------

# Training data
Data2007to2013_QBBO <- CleanClass2007to2014_3_Rose.both[CleanClass2007to2014_3_Rose.both$Position=="QB", ]
Data2007to2013_QBBO <- Data2007to2013_QBBO %>% select(-Class, -Position, -Name, -Player.Code, -Year,
                                                      -Safety, -Kickoff.Ret.TD, -Punt.Ret.TD) #these variables have zero variance hence they can not be normalized.

# Testing data
CleanClass2014_3_QB      

#2 - KNN ----------

tr_control <- trainControl(method="repeatedcv", number=10, repeats = 3)

set.seed(6969)
KNN_QBBO <- train(Drafted~., 
                  data=Data2007to2013_QBBO, 
                  method="knn",
                  trControl=tr_control,
                  preProcess=c("center", "scale")) 

# Substract mean (="center") from each value and then divide this result by standard deviation (="scale").
# The normalized result ist the z-value.

# Predictions
predict_QBBO <- predict(KNN_QBBO, newdata=Data2007to2013_QB)
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
KNNPerfMeas[4,"QB_TP"] = sum(CheckList_QBBO$TP)
KNNPerfMeas[4,"QB_TN"] = sum(CheckList_QBBO$TN)
KNNPerfMeas[4,"QB_FP"] = sum(CheckList_QBBO$FP)
KNNPerfMeas[4,"QB_FN"] = sum(CheckList_QBBO$FN)

# III. KNN Classifier - 07 to 13, WR ----------

#1 - Preparations ----------

# Training data
Data2007to2013_WRBO <- CleanClass2007to2014_3_Rose.both[CleanClass2007to2014_3_Rose.both$Position=="WR", ]
Data2007to2013_WRBO <- Data2007to2013_WRBO %>% select(-Class, -Position, -Name, -Player.Code, -Year, 
                                                      -Safety) #these variables have zero variance hence they can not be normalized.

# Testing data
CleanClass2014_3_WR

#2 - KNN ----------

tr_control <- trainControl(method="repeatedcv", number=10, repeats = 3)

set.seed(6969)
KNN_WRBO <- train(Drafted~., 
                  data=Data2007to2013_WRBO, 
                  method="knn",
                  trControl=tr_control,
                  preProcess=c("center", "scale")) 

# Substract mean (="center") from each value and then divide this result by standard deviation (="scale").
# The normalized result ist the z-value.

# Predictions
predict_WRBO <- predict(KNN_WRBO, newdata=Data2007to2013_WR)
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
KNNPerfMeas[4,"WR_TP"] = sum(CheckList_WRBO$TP)
KNNPerfMeas[4,"WR_TN"] = sum(CheckList_WRBO$TN)
KNNPerfMeas[4,"WR_FP"] = sum(CheckList_WRBO$FP)
KNNPerfMeas[4,"WR_FN"] = sum(CheckList_WRBO$FN)

# IV. KNN Classifier - 07 to 13, RB ----------

#1 - Preparations ----------

# Training data
Data2007to2013_RBBO <- CleanClass2007to2014_3_Rose.both[CleanClass2007to2014_3_Rose.both$Position=="RB", ]
Data2007to2013_RBBO <- Data2007to2013_RBBO %>% select(-Class, -Position, -Name, -Player.Code, -Year,
                                                      -Safety) #these variables have zero variance hence they can not be normalized.

# Testing data
CleanClass2014_3_RB

#2 - KNN ----------

tr_control <- trainControl(method="repeatedcv", number=10, repeats = 3)

set.seed(6969)
KNN_RBBO <- train(Drafted~., 
                  data=Data2007to2013_RBBO, 
                  method="knn",
                  trControl=tr_control,
                  preProcess=c("center", "scale")) 

# Substract mean (="center") from each value and then divide this result by standard deviation (="scale").
# The normalized result ist the z-value.

# Predictions
predict_RBBO <- predict(KNN_RBBO, newdata=Data2007to2013_RB)
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
KNNPerfMeas[4,"RB_TP"] = sum(CheckList_RBBO$TP)
KNNPerfMeas[4,"RB_TN"] = sum(CheckList_RBBO$TN)
KNNPerfMeas[4,"RB_FP"] = sum(CheckList_RBBO$FP)
KNNPerfMeas[4,"RB_FN"] = sum(CheckList_RBBO$FN)

# 5. Smote ###################################################

load("../Data/CleanData/CleanClass2007to2013_3_smote.Rdata")

# I. KNN Classifier - 07 to 13, together ----------

#1 - Preparations  ----------

# Training data
cleanData_smote$Drafted <- as.factor(cleanData_smote$Drafted)
Data2007to2013_togSM <- cleanData_smote %>% select(-Position, -Name, -Player.Code, -Year, 
                                                                    -Safety) #this variable has zero variance hence it can not be normalized.
# Testing data
CleanClass2014_3_tog

#2 - KNN ----------

tr_control <- trainControl(method="repeatedcv", number=10, repeats = 3)

set.seed(6969)
KNN_togSM <- train(Drafted~., 
                   data=Data2007to2013_togSM,
                   method="knn",
                   trControl=tr_control,
                   preProcess=c("center", "scale")) 

# Predictions: 0.5 is used for probability cutoff value by default
predict_togSM <- predict(KNN_togSM,Data2007to2013_tog) 
confusionMatrix(predict_togSM,Data2007to2013_tog$Drafted)

CheckList_togSM = cbind.data.frame(Data2007to2013_tog$Drafted,predict_togSM)

names(CheckList_togSM)[names(CheckList_togSM)=="Data2007to2013_tog$Drafted"] <- "Y"
names(CheckList_togSM)[names(CheckList_togSM)=="predict_togSM"] <- "Pred"

CheckList_togSM = CheckList_togSM %>%
  mutate(TP=ifelse(Y==Pred,ifelse(Pred==1,1,0),0)) %>%
  mutate(TN=ifelse(Y==Pred,ifelse(Pred==0,1,0),0)) %>%
  mutate(FP=ifelse(Y!=Pred,ifelse(Pred==1,1,0),0)) %>%
  mutate(FN=ifelse(Y!=Pred,ifelse(Pred==0,1,0),0))

# Performance Measurement
KNNPerfMeas[5,"Together_TP"] = sum(CheckList_togSM$TP)
KNNPerfMeas[5,"Together_TN"] = sum(CheckList_togSM$TN)
KNNPerfMeas[5,"Together_FP"] = sum(CheckList_togSM$FP)
KNNPerfMeas[5,"Together_FN"] = sum(CheckList_togSM$FN)

# II. KNN Classifier - 07 to 13, QB  ----------

#1 - Preparations  ----------

# Training data
Data2007to2013_QBSM <- cleanData_smote[cleanData_smote$Position=="QB", ]
Data2007to2013_QBSM <- Data2007to2013_QBSM %>% select(-Position, -Name, -Player.Code, -Year,
                                                      -Safety, -Kickoff.Ret.TD, -Punt.Ret.TD) #these variables have zero variance hence they can not be normalized.

# Testing data
CleanClass2014_3_QB      

#2 - KNN ----------

tr_control <- trainControl(method="repeatedcv", number=10, repeats = 3)

set.seed(6969)
KNN_QBSM <- train(Drafted~., 
                  data=Data2007to2013_QBSM, 
                  method="knn",
                  trControl=tr_control,
                  preProcess=c("center", "scale")) 

# Substract mean (="center") from each value and then divide this result by standard deviation (="scale").
# The normalized result ist the z-value.

# Predictions
predict_QBSM <- predict(KNN_QBSM, newdata=Data2007to2013_QB)
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
KNNPerfMeas[5,"QB_TP"] = sum(CheckList_QBSM$TP)
KNNPerfMeas[5,"QB_TN"] = sum(CheckList_QBSM$TN)
KNNPerfMeas[5,"QB_FP"] = sum(CheckList_QBSM$FP)
KNNPerfMeas[5,"QB_FN"] = sum(CheckList_QBSM$FN)

# III. KNN Classifier - 07 to 13, WR ----------

#1 - Preparations ----------

# Training data
Data2007to2013_WRSM <- cleanData_smote[cleanData_smote$Position=="WR", ]
Data2007to2013_WRSM <- Data2007to2013_WRSM %>% select(-Position, -Name, -Player.Code, -Year,
                                                      -Safety) #these variables have zero variance hence they can not be normalized.

# Testing data
CleanClass2014_3_WR

#2 - KNN ----------

tr_control <- trainControl(method="repeatedcv", number=10, repeats = 3)

set.seed(6969)
KNN_WRSM <- train(Drafted~., 
                  data=Data2007to2013_WRSM, 
                  method="knn",
                  trControl=tr_control,
                  preProcess=c("center", "scale")) 

# Substract mean (="center") from each value and then divide this result by standard deviation (="scale").
# The normalized result ist the z-value.

# Predictions
predict_WRSM <- predict(KNN_WRSM, newdata=Data2007to2013_WR)
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
KNNPerfMeas[5,"WR_TP"] = sum(CheckList_WRSM$TP)
KNNPerfMeas[5,"WR_TN"] = sum(CheckList_WRSM$TN)
KNNPerfMeas[5,"WR_FP"] = sum(CheckList_WRSM$FP)
KNNPerfMeas[5,"WR_FN"] = sum(CheckList_WRSM$FN)

# IV. KNN Classifier - 07 to 13, RB ----------

#1 - Preparations ----------

# Training data
Data2007to2013_RBSM <- cleanData_smote[cleanData_smote$Position=="RB", ]
Data2007to2013_RBSM <- Data2007to2013_RBSM %>% select(-Position, -Name, -Player.Code,-Year, 
                                                      -Safety) #these variables have zero variance hence they can not be normalized.

# Testing data
CleanClass2014_3_RB

#2 - KNN ----------

tr_control <- trainControl(method="repeatedcv", number=10, repeats = 3)

set.seed(6969)
KNN_RBSM <- train(Drafted~., 
                  data=Data2007to2013_RBSM, 
                  method="knn",
                  trControl=tr_control,
                  preProcess=c("center", "scale")) 

# Substract mean (="center") from each value and then divide this result by standard deviation (="scale").
# The normalized result ist the z-value.

# Predictions
predict_RBSM <- predict(KNN_RBSM, newdata=Data2007to2013_RB)
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
KNNPerfMeas[5,"RB_TP"] = sum(CheckList_RBSM$TP)
KNNPerfMeas[5,"RB_TN"] = sum(CheckList_RBSM$TN)
KNNPerfMeas[5,"RB_FP"] = sum(CheckList_RBSM$FP)
KNNPerfMeas[5,"RB_FN"] = sum(CheckList_RBSM$FN)

# 5. Save KNNPerfMeas as a new dataset ###################################################

save(KNNPerfMeas, file="../Data/PerformanceMeasurement/KNNPerfMeas.Rdata")
