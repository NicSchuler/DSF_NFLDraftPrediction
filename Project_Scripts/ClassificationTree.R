# Load required packages

library(dplyr)       # data wrangling
library(rpart)       # performing regression trees
library(rpart.plot)  # plotting regression trees
library(tidyverse)
library(rattle)				    	# Fancy tree plot
library(RColorBrewer)				# Color selection for fancy tree plot

load("../Data/CleanData/CleanClass2007to2014_3.Rdata")
load("../Data/CleanData/CleanClass2007to2013_3_oversampling.Rdata")
load("../Data/CleanData/CleanClass2007to2013_3_undersampling.Rdata")
load("../Data/CleanData/CleanClass2007to2013_3_rose.both.Rdata")
load("../Data/CleanData/CleanClass2007to2013_3_smote.Rdata")

ClassificationTreePerfMeas = data.frame(Method = character(), Sampling = character(), QB_TP = integer(), QB_TN = integer(), QB_FP = integer(), QB_FN = integer(),
                                        WR_TP = integer(), WR_TN = integer(), WR_FP = integer(), WR_FN = integer(),
                                        RB_TP = integer(), RB_TN = integer(), RB_FP = integer(), RB_FN = integer(),
                                        Together_TP = integer(), Together_TN = integer(), Together_FP = integer(), Together_FN = integer(), stringsAsFactors = FALSE)

ClassificationTreePerfMeas[1,2] = "no_sampling"
ClassificationTreePerfMeas[2,2] = "oversampling"
ClassificationTreePerfMeas[3,2] = "undersampling"
ClassificationTreePerfMeas[4,2] = "Rose_both"
ClassificationTreePerfMeas[5,2] = "Smote"
ClassificationTreePerfMeas$Method = "ClassificationTree"

# We will do the next steps 5 times (e.g. "1. No Splitting" does the same thing as "2. Oversampling"), but using different data for training the model
# In other words, this is the cross-validation of the sampling methods. The reason for doing it a couple of times instead of looping or functioning it
# is the easier availability of the steps in between in case of further processing them.

## 1. No Sampling ------------------------
# Splitting the data
# We use all the available information just before the 2014 NFL-Draft, in order to train the model and then apply it on the data for 2014.
# In other words we act as if it was the end of April 2014 (which is one week before the draft). Therefore we will do the performance measurement
# on the training data and apply the best model to the 2014 data to see if we were right.
DtrainNS = CleanClass2007to2014_3 %>%
  filter(Year != 2014)

DtestNS = CleanClass2007to2014_3 %>%
  filter(Year == 2014)


# QB ---------------------------
# Predicting the likelyhood of a QB being picked in the draft
DtrainQBNS = DtrainNS %>%
  filter(Position == "QB") %>%
  select(-c(Player.Code, Name, Class, Position, Year))

DtestQBNS = DtestNS %>%
  filter(Position == "QB") %>%
  select(-c(Player.Code, Name, Class, Position, Year))

# Run a classification tree. We use the whole data for training, since the rpart-function has a built in cross-validation. For the evaluation of the 
# best model we also use the whole training set for this cross-validation.
ClassTreeQBNS = rpart(
  formula = Drafted ~ .,
  data    = DtrainQBNS,
  method  = "class")

CheckList = as.data.frame(cbind(DtrainQBNS$Drafted, predict(ClassTreeQBNS, DtrainQBNS)))

CheckListQBNS = CheckList %>%
  mutate(Y=V1) %>%
  select(-V1) %>%
  mutate(QB_Pred=ifelse(CheckList[,3]>0.5, 1,0)) %>%
  mutate(QB_TP=ifelse(Y==QB_Pred,ifelse(QB_Pred==1,1,0),0)) %>%
  mutate(QB_TN=ifelse(Y==QB_Pred,ifelse(QB_Pred==0,1,0),0)) %>%
  mutate(QB_FP=ifelse(Y!=QB_Pred,ifelse(QB_Pred==1,1,0),0)) %>%
  mutate(QB_FN=ifelse(Y!=QB_Pred,ifelse(QB_Pred==0,1,0),0))

# Fill the Performance Measurement Matrix
ClassificationTreePerfMeas[1,"QB_TP"] = sum(CheckListQBNS$QB_TP)
ClassificationTreePerfMeas[1,"QB_TN"] = sum(CheckListQBNS$QB_TN)
ClassificationTreePerfMeas[1,"QB_FP"] = sum(CheckListQBNS$QB_FP)
ClassificationTreePerfMeas[1,"QB_FN"] = sum(CheckListQBNS$QB_FN)

# Plotting the Tree
fancyRpartPlot(ClassTreeQBNS)

# WR ---------------------------
# Predicting the likelyhood of a WR being picked in the draft
DtrainWRNS = DtrainNS %>%
  filter(Position == "WR") %>%
  select(-c(Player.Code, Name, Class, Position, Year))

DtestWRNS = DtestNS %>%
  filter(Position == "WR") %>%
  select(-c(Player.Code, Name, Class, Position, Year))

# Run a classification tree. We use the whole data for training, since the rpart-function has a built in cross-validation. For the evaluation of the 
# best model we also use the whole training set for this cross-validation.
ClassTreeWRNS = rpart(
  formula = Drafted ~ .,
  data    = DtrainWRNS,
  method  = "class")

CheckList = as.data.frame(cbind(DtrainWRNS$Drafted, predict(ClassTreeWRNS, DtrainWRNS)))

CheckListWRNS = CheckList %>%
  mutate(Y=V1) %>%
  select(-V1) %>%
  mutate(WR_Pred=ifelse(CheckList[,3]>0.5, 1,0)) %>%
  mutate(WR_TP=ifelse(Y==WR_Pred,ifelse(WR_Pred==1,1,0),0)) %>%
  mutate(WR_TN=ifelse(Y==WR_Pred,ifelse(WR_Pred==0,1,0),0)) %>%
  mutate(WR_FP=ifelse(Y!=WR_Pred,ifelse(WR_Pred==1,1,0),0)) %>%
  mutate(WR_FN=ifelse(Y!=WR_Pred,ifelse(WR_Pred==0,1,0),0))

# Fill the Performance Measurement Matrix
ClassificationTreePerfMeas[1,"WR_TP"] = sum(CheckListWRNS$WR_TP)
ClassificationTreePerfMeas[1,"WR_TN"] = sum(CheckListWRNS$WR_TN)
ClassificationTreePerfMeas[1,"WR_FP"] = sum(CheckListWRNS$WR_FP)
ClassificationTreePerfMeas[1,"WR_FN"] = sum(CheckListWRNS$WR_FN)

# Plotting the Tree
fancyRpartPlot(ClassTreeWRNS)

# RB ---------------------------
# Predicting the likelyhood of a RB being picked in the draft
DtrainRBNS = DtrainNS %>%
  filter(Position == "RB") %>%
  select(-c(Player.Code, Name, Class, Position, Year))

DtestRBNS = DtestNS %>%
  filter(Position == "RB") %>%
  select(-c(Player.Code, Name, Class, Position, Year))

# Run a classification tree. We use the whole data for training, since the rpart-function has a built in cross-validation. For the evaluation of the 
# best model we also use the whole training set for this cross-validation.
ClassTreeRBNS = rpart(
  formula = Drafted ~ .,
  data    = DtrainRBNS,
  method  = "class")

CheckList = as.data.frame(cbind(DtrainRBNS$Drafted, predict(ClassTreeRBNS, DtrainRBNS)))

CheckListRBNS = CheckList %>%
  mutate(Y=V1) %>%
  select(-V1) %>%
  mutate(RB_Pred=ifelse(CheckList[,3]>0.5, 1,0)) %>%
  mutate(RB_TP=ifelse(Y==RB_Pred,ifelse(RB_Pred==1,1,0),0)) %>%
  mutate(RB_TN=ifelse(Y==RB_Pred,ifelse(RB_Pred==0,1,0),0)) %>%
  mutate(RB_FP=ifelse(Y!=RB_Pred,ifelse(RB_Pred==1,1,0),0)) %>%
  mutate(RB_FN=ifelse(Y!=RB_Pred,ifelse(RB_Pred==0,1,0),0))

# Fill the Performance Measurement Matrix
ClassificationTreePerfMeas[1,"RB_TP"] = sum(CheckListRBNS$RB_TP)
ClassificationTreePerfMeas[1,"RB_TN"] = sum(CheckListRBNS$RB_TN)
ClassificationTreePerfMeas[1,"RB_FP"] = sum(CheckListRBNS$RB_FP)
ClassificationTreePerfMeas[1,"RB_FN"] = sum(CheckListRBNS$RB_FN)

# Plotting the Tree
fancyRpartPlot(ClassTreeRBNS)

# Together ---------------------------
# Predicting the likelyhood of QB/RB/WR together for being picked in the draft
DtrainTogetherNS = DtrainNS %>%
  select(-c(Player.Code, Name, Class, Position, Year))

DtestTogetherNS = DtestNS %>%
  select(-c(Player.Code, Name, Class, Position, Year))

# Run a classification tree. We use the whole data for training, since the rpart-function has a built in cross-validation. For the evaluation of the 
# best model we also use the whole training set for this cross-validation.
ClassTreeTogetherNS = rpart(
  formula = Drafted ~ .,
  data    = DtrainTogetherNS,
  method  = "class")

CheckList = as.data.frame(cbind(DtrainTogetherNS$Drafted, predict(ClassTreeTogetherNS, DtrainTogetherNS)))

CheckListTogetherNS = CheckList %>%
  mutate(Drafted=V1) %>%
  select(-V1) %>%
  mutate(Together_Pred=ifelse(CheckList[,3]>0.5, 1,0)) %>%
  mutate(Together_TP=ifelse(Drafted==Together_Pred,ifelse(Together_Pred==1,1,0),0)) %>%
  mutate(Together_TN=ifelse(Drafted==Together_Pred,ifelse(Together_Pred==0,1,0),0)) %>%
  mutate(Together_FP=ifelse(Drafted!=Together_Pred,ifelse(Together_Pred==1,1,0),0)) %>%
  mutate(Together_FN=ifelse(Drafted!=Together_Pred,ifelse(Together_Pred==0,1,0),0))

# Fill the Performance Measurement Matrix
ClassificationTreePerfMeas[1,"Together_TP"] = sum(CheckListTogetherNS$Together_TP)
ClassificationTreePerfMeas[1,"Together_TN"] = sum(CheckListTogetherNS$Together_TN)
ClassificationTreePerfMeas[1,"Together_FP"] = sum(CheckListTogetherNS$Together_FP)
ClassificationTreePerfMeas[1,"Together_FN"] = sum(CheckListTogetherNS$Together_FN)

# Plotting the Tree
fancyRpartPlot(ClassTreeTogetherNS)


## 2. Oversampling ------------------------
# Splitting the data
# We use all the available information just before the 2014 NFL-Draft, in order to train the model and then apply it on the data for 2014.
# In other words we act as if it was the end of April 2014 (which is one week before the draft). Therefore we will do the performance measurement
# on the training data and apply the best model to the 2014 data to see if we were right.
DtrainOS = CleanClass2007to2014_3_oversampling %>%
  filter(Year != 2014)

DtestOS = CleanClass2007to2014_3_oversampling %>%
  filter(Year == 2014)


# QB ---------------------------
# Predicting the likelyhood of a QB being picked in the draft
DtrainQBOS = DtrainOS %>%
  filter(Position == "QB") %>%
  select(-c(Player.Code, Name, Class, Position, Year))

DtestQBOS = DtestOS %>%
  filter(Position == "QB") %>%
  select(-c(Player.Code, Name, Class, Position, Year))

# Run a classification tree. We use the whole data for training, since the rpart-function has a built in cross-validation. For the evaluation of the 
# best model we also use the whole training set for this cross-validation.
ClassTreeQBOS = rpart(
  formula = Drafted ~ .,
  data    = DtrainQBOS,
  method  = "class")

CheckList = as.data.frame(cbind(DtrainQBNS$Drafted, predict(ClassTreeQBOS, DtrainQBNS)))

CheckListQBOS = CheckList %>%
  mutate(Y=V1) %>%
  select(-V1) %>%
  mutate(QB_Pred=ifelse(CheckList[,3]>0.5, 1,0)) %>%
  mutate(QB_TP=ifelse(Y==QB_Pred,ifelse(QB_Pred==1,1,0),0)) %>%
  mutate(QB_TN=ifelse(Y==QB_Pred,ifelse(QB_Pred==0,1,0),0)) %>%
  mutate(QB_FP=ifelse(Y!=QB_Pred,ifelse(QB_Pred==1,1,0),0)) %>%
  mutate(QB_FN=ifelse(Y!=QB_Pred,ifelse(QB_Pred==0,1,0),0))

# Fill the Performance Measurement Matrix
ClassificationTreePerfMeas[2,"QB_TP"] = sum(CheckListQBOS$QB_TP)
ClassificationTreePerfMeas[2,"QB_TN"] = sum(CheckListQBOS$QB_TN)
ClassificationTreePerfMeas[2,"QB_FP"] = sum(CheckListQBOS$QB_FP)
ClassificationTreePerfMeas[2,"QB_FN"] = sum(CheckListQBOS$QB_FN)

# Plotting the Tree
fancyRpartPlot(ClassTreeQBOS)

# WR ---------------------------
# Predicting the likelyhood of a WR being picked in the draft
DtrainWROS = DtrainOS %>%
  filter(Position == "WR") %>%
  select(-c(Player.Code, Name, Class, Position, Year))

DtestWROS = DtestOS %>%
  filter(Position == "WR") %>%
  select(-c(Player.Code, Name, Class, Position, Year))

# Run a classification tree. We use the whole data for training, since the rpart-function has a built in cross-validation. For the evaluation of the 
# best model we also use the whole training set for this cross-validation.
ClassTreeWROS = rpart(
  formula = Drafted ~ .,
  data    = DtrainWROS,
  method  = "class")

CheckList = as.data.frame(cbind(DtrainWRNS$Drafted, predict(ClassTreeWROS, DtrainWRNS)))

CheckListWROS = CheckList %>%
  mutate(Y=V1) %>%
  select(-V1) %>%
  mutate(WR_Pred=ifelse(CheckList[,3]>0.5, 1,0)) %>%
  mutate(WR_TP=ifelse(Y==WR_Pred,ifelse(WR_Pred==1,1,0),0)) %>%
  mutate(WR_TN=ifelse(Y==WR_Pred,ifelse(WR_Pred==0,1,0),0)) %>%
  mutate(WR_FP=ifelse(Y!=WR_Pred,ifelse(WR_Pred==1,1,0),0)) %>%
  mutate(WR_FN=ifelse(Y!=WR_Pred,ifelse(WR_Pred==0,1,0),0))

# Fill the Performance Measurement Matrix
ClassificationTreePerfMeas[2,"WR_TP"] = sum(CheckListWROS$WR_TP)
ClassificationTreePerfMeas[2,"WR_TN"] = sum(CheckListWROS$WR_TN)
ClassificationTreePerfMeas[2,"WR_FP"] = sum(CheckListWROS$WR_FP)
ClassificationTreePerfMeas[2,"WR_FN"] = sum(CheckListWROS$WR_FN)

# Plotting the Tree
fancyRpartPlot(ClassTreeWROS)

# RB ---------------------------
# Predicting the likelyhood of a RB being picked in the draft
DtrainRBOS = DtrainOS %>%
  filter(Position == "RB") %>%
  select(-c(Player.Code, Name, Class, Position, Year))

DtestRBOS = DtestOS %>%
  filter(Position == "RB") %>%
  select(-c(Player.Code, Name, Class, Position, Year))

# Run a classification tree. We use the whole data for training, since the rpart-function has a built in cross-validation. For the evaluation of the 
# best model we also use the whole training set for this cross-validation.
ClassTreeRBOS = rpart(
  formula = Drafted ~ .,
  data    = DtrainRBOS,
  method  = "class")

CheckList = as.data.frame(cbind(DtrainRBNS$Drafted, predict(ClassTreeRBOS, DtrainRBNS)))

CheckListRBOS = CheckList %>%
  mutate(Y=V1) %>%
  select(-V1) %>%
  mutate(RB_Pred=ifelse(CheckList[,3]>0.5, 1,0)) %>%
  mutate(RB_TP=ifelse(Y==RB_Pred,ifelse(RB_Pred==1,1,0),0)) %>%
  mutate(RB_TN=ifelse(Y==RB_Pred,ifelse(RB_Pred==0,1,0),0)) %>%
  mutate(RB_FP=ifelse(Y!=RB_Pred,ifelse(RB_Pred==1,1,0),0)) %>%
  mutate(RB_FN=ifelse(Y!=RB_Pred,ifelse(RB_Pred==0,1,0),0))

# Fill the Performance Measurement Matrix
ClassificationTreePerfMeas[2,"RB_TP"] = sum(CheckListRBOS$RB_TP)
ClassificationTreePerfMeas[2,"RB_TN"] = sum(CheckListRBOS$RB_TN)
ClassificationTreePerfMeas[2,"RB_FP"] = sum(CheckListRBOS$RB_FP)
ClassificationTreePerfMeas[2,"RB_FN"] = sum(CheckListRBOS$RB_FN)

# Plotting the Tree
fancyRpartPlot(ClassTreeRBOS)

# Together ---------------------------
# Predicting the likelyhood of QB/RB/WR together for being picked in the draft
DtrainTogetherOS = DtrainOS %>%
  select(-c(Player.Code, Name, Class, Position, Year))

DtestTogetherOS = DtestOS %>%
  select(-c(Player.Code, Name, Class, Position, Year))

# Run a classification tree. We use the whole data for training, since the rpart-function has a built in cross-validation. For the evaluation of the 
# best model we also use the whole training set for this cross-validation.
ClassTreeTogetherOS = rpart(
  formula = Drafted ~ .,
  data    = DtrainTogetherOS,
  method  = "class")

CheckList = as.data.frame(cbind(DtrainTogetherNS$Drafted, predict(ClassTreeTogetherOS, DtrainTogetherNS)))

CheckListTogetherOS = CheckList %>%
  mutate(Drafted=V1) %>%
  select(-V1) %>%
  mutate(Together_Pred=ifelse(CheckList[,3]>0.5, 1,0)) %>%
  mutate(Together_TP=ifelse(Drafted==Together_Pred,ifelse(Together_Pred==1,1,0),0)) %>%
  mutate(Together_TN=ifelse(Drafted==Together_Pred,ifelse(Together_Pred==0,1,0),0)) %>%
  mutate(Together_FP=ifelse(Drafted!=Together_Pred,ifelse(Together_Pred==1,1,0),0)) %>%
  mutate(Together_FN=ifelse(Drafted!=Together_Pred,ifelse(Together_Pred==0,1,0),0))

# Fill the Performance Measurement Matrix
ClassificationTreePerfMeas[2,"Together_TP"] = sum(CheckListTogetherOS$Together_TP)
ClassificationTreePerfMeas[2,"Together_TN"] = sum(CheckListTogetherOS$Together_TN)
ClassificationTreePerfMeas[2,"Together_FP"] = sum(CheckListTogetherOS$Together_FP)
ClassificationTreePerfMeas[2,"Together_FN"] = sum(CheckListTogetherOS$Together_FN)

# Plotting the Tree
fancyRpartPlot(ClassTreeTogetherOS)



## 3. Undersampling ------------------------
# Splitting the data
# We use all the available information just before the 2014 NFL-Draft, in order to train the model and then apply it on the data for 2014.
# In other words we act as if it was the end of April 2014 (which is one week before the draft). Therefore we will do the performance measurement
# on the training data and apply the best model to the 2014 data to see if we were right.
DtrainUS = CleanClass2007to2014_3_undersampling %>%
  filter(Year != 2014)

DtestUS = CleanClass2007to2014_3_undersampling %>%
  filter(Year == 2014)


# QB ---------------------------
# Predicting the likelyhood of a QB being picked in the draft
DtrainQBUS = DtrainUS %>%
  filter(Position == "QB") %>%
  select(-c(Player.Code, Name, Class, Position, Year))

DtestQBUS = DtestUS %>%
  filter(Position == "QB") %>%
  select(-c(Player.Code, Name, Class, Position, Year))

# Run a classification tree. We use the whole data for training, since the rpart-function has a built in cross-validation. For the evaluation of the 
# best model we also use the whole training set for this cross-validation.
ClassTreeQBUS = rpart(
  formula = Drafted ~ .,
  data    = DtrainQBUS,
  method  = "class")

CheckList = as.data.frame(cbind(DtrainQBNS$Drafted, predict(ClassTreeQBUS, DtrainQBNS)))

CheckListQBUS = CheckList %>%
  mutate(Y=V1) %>%
  select(-V1) %>%
  mutate(QB_Pred=ifelse(CheckList[,3]>0.5, 1,0)) %>%
  mutate(QB_TP=ifelse(Y==QB_Pred,ifelse(QB_Pred==1,1,0),0)) %>%
  mutate(QB_TN=ifelse(Y==QB_Pred,ifelse(QB_Pred==0,1,0),0)) %>%
  mutate(QB_FP=ifelse(Y!=QB_Pred,ifelse(QB_Pred==1,1,0),0)) %>%
  mutate(QB_FN=ifelse(Y!=QB_Pred,ifelse(QB_Pred==0,1,0),0))

# Fill the Performance Measurement Matrix
ClassificationTreePerfMeas[3,"QB_TP"] = sum(CheckListQBUS$QB_TP)
ClassificationTreePerfMeas[3,"QB_TN"] = sum(CheckListQBUS$QB_TN)
ClassificationTreePerfMeas[3,"QB_FP"] = sum(CheckListQBUS$QB_FP)
ClassificationTreePerfMeas[3,"QB_FN"] = sum(CheckListQBUS$QB_FN)

# Plotting the Tree
fancyRpartPlot(ClassTreeQBUS)

# WR ---------------------------
# Predicting the likelyhood of a WR being picked in the draft
DtrainWRUS = DtrainUS %>%
  filter(Position == "WR") %>%
  select(-c(Player.Code, Name, Class, Position, Year))

DtestWRUS = DtestUS %>%
  filter(Position == "WR") %>%
  select(-c(Player.Code, Name, Class, Position, Year))

# Run a classification tree. We use the whole data for training, since the rpart-function has a built in cross-validation. For the evaluation of the 
# best model we also use the whole training set for this cross-validation.
ClassTreeWRUS = rpart(
  formula = Drafted ~ .,
  data    = DtrainWRUS,
  method  = "class")

CheckList = as.data.frame(cbind(DtrainWRNS$Drafted, predict(ClassTreeWRUS, DtrainWRNS)))

CheckListWRUS = CheckList %>%
  mutate(Y=V1) %>%
  select(-V1) %>%
  mutate(WR_Pred=ifelse(CheckList[,3]>0.5, 1,0)) %>%
  mutate(WR_TP=ifelse(Y==WR_Pred,ifelse(WR_Pred==1,1,0),0)) %>%
  mutate(WR_TN=ifelse(Y==WR_Pred,ifelse(WR_Pred==0,1,0),0)) %>%
  mutate(WR_FP=ifelse(Y!=WR_Pred,ifelse(WR_Pred==1,1,0),0)) %>%
  mutate(WR_FN=ifelse(Y!=WR_Pred,ifelse(WR_Pred==0,1,0),0))

# Fill the Performance Measurement Matrix
ClassificationTreePerfMeas[3,"WR_TP"] = sum(CheckListWRUS$WR_TP)
ClassificationTreePerfMeas[3,"WR_TN"] = sum(CheckListWRUS$WR_TN)
ClassificationTreePerfMeas[3,"WR_FP"] = sum(CheckListWRUS$WR_FP)
ClassificationTreePerfMeas[3,"WR_FN"] = sum(CheckListWRUS$WR_FN)

# Plotting the Tree
fancyRpartPlot(ClassTreeWRUS)

# RB ---------------------------
# Predicting the likelyhood of a RB being picked in the draft
DtrainRBUS = DtrainUS %>%
  filter(Position == "RB") %>%
  select(-c(Player.Code, Name, Class, Position, Year))

DtestRBUS = DtestUS %>%
  filter(Position == "RB") %>%
  select(-c(Player.Code, Name, Class, Position, Year))

# Run a classification tree. We use the whole data for training, since the rpart-function has a built in cross-validation. For the evaluation of the 
# best model we also use the whole training set for this cross-validation.
ClassTreeRBUS = rpart(
  formula = Drafted ~ .,
  data    = DtrainRBUS,
  method  = "class")

CheckList = as.data.frame(cbind(DtrainRBNS$Drafted, predict(ClassTreeRBUS, DtrainRBNS)))

CheckListRBUS = CheckList %>%
  mutate(Y=V1) %>%
  select(-V1) %>%
  mutate(RB_Pred=ifelse(CheckList[,3]>0.5, 1,0)) %>%
  mutate(RB_TP=ifelse(Y==RB_Pred,ifelse(RB_Pred==1,1,0),0)) %>%
  mutate(RB_TN=ifelse(Y==RB_Pred,ifelse(RB_Pred==0,1,0),0)) %>%
  mutate(RB_FP=ifelse(Y!=RB_Pred,ifelse(RB_Pred==1,1,0),0)) %>%
  mutate(RB_FN=ifelse(Y!=RB_Pred,ifelse(RB_Pred==0,1,0),0))

# Fill the Performance Measurement Matrix
ClassificationTreePerfMeas[3,"RB_TP"] = sum(CheckListRBUS$RB_TP)
ClassificationTreePerfMeas[3,"RB_TN"] = sum(CheckListRBUS$RB_TN)
ClassificationTreePerfMeas[3,"RB_FP"] = sum(CheckListRBUS$RB_FP)
ClassificationTreePerfMeas[3,"RB_FN"] = sum(CheckListRBUS$RB_FN)

# Plotting the Tree
fancyRpartPlot(ClassTreeRBUS)

# Together ---------------------------
# Predicting the likelyhood of QB/RB/WR together for being picked in the draft
DtrainTogetherUS = DtrainUS %>%
  select(-c(Player.Code, Name, Class, Position, Year))

DtestTogetherUS = DtestUS %>%
  select(-c(Player.Code, Name, Class, Position, Year))

# Run a classification tree. We use the whole data for training, since the rpart-function has a built in cross-validation. For the evaluation of the 
# best model we also use the whole training set for this cross-validation.
ClassTreeTogetherUS = rpart(
  formula = Drafted ~ .,
  data    = DtrainTogetherUS,
  method  = "class")

CheckList = as.data.frame(cbind(DtrainTogetherNS$Drafted, predict(ClassTreeTogetherUS, DtrainTogetherNS)))

CheckListTogetherUS = CheckList %>%
  mutate(Drafted=V1) %>%
  select(-V1) %>%
  mutate(Together_Pred=ifelse(CheckList[,3]>0.5, 1,0)) %>%
  mutate(Together_TP=ifelse(Drafted==Together_Pred,ifelse(Together_Pred==1,1,0),0)) %>%
  mutate(Together_TN=ifelse(Drafted==Together_Pred,ifelse(Together_Pred==0,1,0),0)) %>%
  mutate(Together_FP=ifelse(Drafted!=Together_Pred,ifelse(Together_Pred==1,1,0),0)) %>%
  mutate(Together_FN=ifelse(Drafted!=Together_Pred,ifelse(Together_Pred==0,1,0),0))

# Fill the Performance Measurement Matrix
ClassificationTreePerfMeas[3,"Together_TP"] = sum(CheckListTogetherUS$Together_TP)
ClassificationTreePerfMeas[3,"Together_TN"] = sum(CheckListTogetherUS$Together_TN)
ClassificationTreePerfMeas[3,"Together_FP"] = sum(CheckListTogetherUS$Together_FP)
ClassificationTreePerfMeas[3,"Together_FN"] = sum(CheckListTogetherUS$Together_FN)

# Plotting the Tree
fancyRpartPlot(ClassTreeTogetherUS)


## 4. Rose_Both------------------------
# Splitting the data
# We use all the available information just before the 2014 NFL-Draft, in order to train the model and then apply it on the data for 2014.
# In other words we act as if it was the end of April 2014 (which is one week before the draft). Therefore we will do the performance measurement
# on the training data and apply the best model to the 2014 data to see if we were right.
DtrainRO = CleanClass2007to2014_3_Rose.both %>%
  filter(Year != 2014)

DtestRO = CleanClass2007to2014_3_Rose.both %>%
  filter(Year == 2014)


# QB ---------------------------
# Predicting the likelyhood of a QB being picked in the draft
DtrainQBRO = DtrainRO %>%
  filter(Position == "QB") %>%
  select(-c(Player.Code, Name, Class, Position, Year))

DtestQBRO = DtestRO %>%
  filter(Position == "QB") %>%
  select(-c(Player.Code, Name, Class, Position, Year))

# Run a classification tree. We use the whole data for training, since the rpart-function has a built in cross-validation. For the evaluation of the 
# best model we also use the whole training set for this cross-validation.
ClassTreeQBRO = rpart(
  formula = Drafted ~ .,
  data    = DtrainQBRO,
  method  = "class")

CheckList = as.data.frame(cbind(DtrainQBNS$Drafted, predict(ClassTreeQBRO, DtrainQBNS)))

CheckListQBRO = CheckList %>%
  mutate(Y=V1) %>%
  select(-V1) %>%
  mutate(QB_Pred=ifelse(CheckList[,3]>0.5, 1,0)) %>%
  mutate(QB_TP=ifelse(Y==QB_Pred,ifelse(QB_Pred==1,1,0),0)) %>%
  mutate(QB_TN=ifelse(Y==QB_Pred,ifelse(QB_Pred==0,1,0),0)) %>%
  mutate(QB_FP=ifelse(Y!=QB_Pred,ifelse(QB_Pred==1,1,0),0)) %>%
  mutate(QB_FN=ifelse(Y!=QB_Pred,ifelse(QB_Pred==0,1,0),0))

# Fill the Performance Measurement Matrix
ClassificationTreePerfMeas[4,"QB_TP"] = sum(CheckListQBRO$QB_TP)
ClassificationTreePerfMeas[4,"QB_TN"] = sum(CheckListQBRO$QB_TN)
ClassificationTreePerfMeas[4,"QB_FP"] = sum(CheckListQBRO$QB_FP)
ClassificationTreePerfMeas[4,"QB_FN"] = sum(CheckListQBRO$QB_FN)

# Plotting the Tree
fancyRpartPlot(ClassTreeQBRO)

# WR ---------------------------
# Predicting the likelyhood of a WR being picked in the draft
DtrainWRRO = DtrainRO %>%
  filter(Position == "WR") %>%
  select(-c(Player.Code, Name, Class, Position, Year))

DtestWRRO = DtestRO %>%
  filter(Position == "WR") %>%
  select(-c(Player.Code, Name, Class, Position, Year))

# Run a classification tree. We use the whole data for training, since the rpart-function has a built in cross-validation. For the evaluation of the 
# best model we also use the whole training set for this cross-validation.
ClassTreeWRRO = rpart(
  formula = Drafted ~ .,
  data    = DtrainWRRO,
  method  = "class")

CheckList = as.data.frame(cbind(DtrainWRNS$Drafted, predict(ClassTreeWRRO, DtrainWRNS)))

CheckListWRRO = CheckList %>%
  mutate(Y=V1) %>%
  select(-V1) %>%
  mutate(WR_Pred=ifelse(CheckList[,3]>0.5, 1,0)) %>%
  mutate(WR_TP=ifelse(Y==WR_Pred,ifelse(WR_Pred==1,1,0),0)) %>%
  mutate(WR_TN=ifelse(Y==WR_Pred,ifelse(WR_Pred==0,1,0),0)) %>%
  mutate(WR_FP=ifelse(Y!=WR_Pred,ifelse(WR_Pred==1,1,0),0)) %>%
  mutate(WR_FN=ifelse(Y!=WR_Pred,ifelse(WR_Pred==0,1,0),0))

# Fill the Performance Measurement Matrix
ClassificationTreePerfMeas[4,"WR_TP"] = sum(CheckListWRRO$WR_TP)
ClassificationTreePerfMeas[4,"WR_TN"] = sum(CheckListWRRO$WR_TN)
ClassificationTreePerfMeas[4,"WR_FP"] = sum(CheckListWRRO$WR_FP)
ClassificationTreePerfMeas[4,"WR_FN"] = sum(CheckListWRRO$WR_FN)

# Plotting the Tree
fancyRpartPlot(ClassTreeWRRO)

# RB ---------------------------
# Predicting the likelyhood of a RB being picked in the draft
DtrainRBRO = DtrainRO %>%
  filter(Position == "RB") %>%
  select(-c(Player.Code, Name, Class, Position, Year))

DtestRBRO = DtestRO %>%
  filter(Position == "RB") %>%
  select(-c(Player.Code, Name, Class, Position, Year))

# Run a classification tree. We use the whole data for training, since the rpart-function has a built in cross-validation. For the evaluation of the 
# best model we also use the whole training set for this cross-validation.
ClassTreeRBRO = rpart(
  formula = Drafted ~ .,
  data    = DtrainRBRO,
  method  = "class")

CheckList = as.data.frame(cbind(DtrainRBNS$Drafted, predict(ClassTreeRBRO, DtrainRBNS)))

CheckListRBRO = CheckList %>%
  mutate(Y=V1) %>%
  select(-V1) %>%
  mutate(RB_Pred=ifelse(CheckList[,3]>0.5, 1,0)) %>%
  mutate(RB_TP=ifelse(Y==RB_Pred,ifelse(RB_Pred==1,1,0),0)) %>%
  mutate(RB_TN=ifelse(Y==RB_Pred,ifelse(RB_Pred==0,1,0),0)) %>%
  mutate(RB_FP=ifelse(Y!=RB_Pred,ifelse(RB_Pred==1,1,0),0)) %>%
  mutate(RB_FN=ifelse(Y!=RB_Pred,ifelse(RB_Pred==0,1,0),0))

# Fill the Performance Measurement Matrix
ClassificationTreePerfMeas[4,"RB_TP"] = sum(CheckListRBRO$RB_TP)
ClassificationTreePerfMeas[4,"RB_TN"] = sum(CheckListRBRO$RB_TN)
ClassificationTreePerfMeas[4,"RB_FP"] = sum(CheckListRBRO$RB_FP)
ClassificationTreePerfMeas[4,"RB_FN"] = sum(CheckListRBRO$RB_FN)

# Plotting the Tree
fancyRpartPlot(ClassTreeRBRO)

# Together ---------------------------
# Predicting the likelyhood of QB/RB/WR together for being picked in the draft
DtrainTogetherRO = DtrainRO %>%
  select(-c(Player.Code, Name, Class, Position, Year))

DtestTogetherRO = DtestRO %>%
  select(-c(Player.Code, Name, Class, Position, Year))

# Run a classification tree. We use the whole data for training, since the rpart-function has a built in cross-validation. For the evaluation of the 
# best model we also use the whole training set for this cross-validation.
ClassTreeTogetherRO = rpart(
  formula = Drafted ~ .,
  data    = DtrainTogetherRO,
  method  = "class")

CheckList = as.data.frame(cbind(DtrainTogetherNS$Drafted, predict(ClassTreeTogetherRO, DtrainTogetherNS)))

CheckListTogetherRO = CheckList %>%
  mutate(Drafted=V1) %>%
  select(-V1) %>%
  mutate(Together_Pred=ifelse(CheckList[,3]>0.5, 1,0)) %>%
  mutate(Together_TP=ifelse(Drafted==Together_Pred,ifelse(Together_Pred==1,1,0),0)) %>%
  mutate(Together_TN=ifelse(Drafted==Together_Pred,ifelse(Together_Pred==0,1,0),0)) %>%
  mutate(Together_FP=ifelse(Drafted!=Together_Pred,ifelse(Together_Pred==1,1,0),0)) %>%
  mutate(Together_FN=ifelse(Drafted!=Together_Pred,ifelse(Together_Pred==0,1,0),0))

# Fill the Performance Measurement Matrix
ClassificationTreePerfMeas[4,"Together_TP"] = sum(CheckListTogetherRO$Together_TP)
ClassificationTreePerfMeas[4,"Together_TN"] = sum(CheckListTogetherRO$Together_TN)
ClassificationTreePerfMeas[4,"Together_FP"] = sum(CheckListTogetherRO$Together_FP)
ClassificationTreePerfMeas[4,"Together_FN"] = sum(CheckListTogetherRO$Together_FN)

# Plotting the Tree
fancyRpartPlot(ClassTreeTogetherRO)



## 5. Smote------------------------
# Splitting the data
# We use all the available information just before the 2014 NFL-Draft, in order to train the model and then apply it on the data for 2014.
# In other words we act as if it was the end of April 2014 (which is one week before the draft). Therefore we will do the performance measurement
# on the training data and apply the best model to the 2014 data to see if we were right.
DtrainSM = cleanData_smote %>%
  filter(Year != 2014)

DtestSM = cleanData_smote %>%
  filter(Year == 2014)


# QB ---------------------------
# Predicting the likelyhood of a QB being picked in the draft
DtrainQBSM = DtrainSM %>%
  filter(Position == "QB") %>%
  select(-c(Player.Code, Name, Position, Year))

DtestQBSM = DtestSM %>%
  filter(Position == "QB") %>%
  select(-c(Player.Code, Name, Position, Year))

# Run a classification tree. We use the whole data for training, since the rpart-function has a built in cross-validation. For the evaluation of the 
# best model we also use the whole training set for this cross-validation.
ClassTreeQBSM = rpart(
  formula = Drafted ~ .,
  data    = DtrainQBSM,
  method  = "class")

CheckList = as.data.frame(cbind(DtrainQBNS$Drafted, predict(ClassTreeQBSM, DtrainQBNS)))

CheckListQBSM = CheckList %>%
  mutate(Y=V1) %>%
  select(-V1) %>%
  mutate(QB_Pred=ifelse(CheckList[,3]>0.5, 1,0)) %>%
  mutate(QB_TP=ifelse(Y==QB_Pred,ifelse(QB_Pred==1,1,0),0)) %>%
  mutate(QB_TN=ifelse(Y==QB_Pred,ifelse(QB_Pred==0,1,0),0)) %>%
  mutate(QB_FP=ifelse(Y!=QB_Pred,ifelse(QB_Pred==1,1,0),0)) %>%
  mutate(QB_FN=ifelse(Y!=QB_Pred,ifelse(QB_Pred==0,1,0),0))

# Fill the Performance Measurement Matrix
ClassificationTreePerfMeas[5,"QB_TP"] = sum(CheckListQBSM$QB_TP)
ClassificationTreePerfMeas[5,"QB_TN"] = sum(CheckListQBSM$QB_TN)
ClassificationTreePerfMeas[5,"QB_FP"] = sum(CheckListQBSM$QB_FP)
ClassificationTreePerfMeas[5,"QB_FN"] = sum(CheckListQBSM$QB_FN)

# Plotting the Tree
fancyRpartPlot(ClassTreeQBSM)

# WR ---------------------------
# Predicting the likelyhood of a WR being picked in the draft
DtrainWRSM = DtrainSM %>%
  filter(Position == "WR") %>%
  select(-c(Player.Code, Name, Position, Year))

DtestWRSM = DtestSM %>%
  filter(Position == "WR") %>%
  select(-c(Player.Code, Name, Position, Year))

# Run a classification tree. We use the whole data for training, since the rpart-function has a built in cross-validation. For the evaluation of the 
# best model we also use the whole training set for this cross-validation.
ClassTreeWRSM = rpart(
  formula = Drafted ~ .,
  data    = DtrainWRSM,
  method  = "class")

CheckList = as.data.frame(cbind(DtrainWRNS$Drafted, predict(ClassTreeWRSM, DtrainWRNS)))

CheckListWRSM = CheckList %>%
  mutate(Y=V1) %>%
  select(-V1) %>%
  mutate(WR_Pred=ifelse(CheckList[,3]>0.5, 1,0)) %>%
  mutate(WR_TP=ifelse(Y==WR_Pred,ifelse(WR_Pred==1,1,0),0)) %>%
  mutate(WR_TN=ifelse(Y==WR_Pred,ifelse(WR_Pred==0,1,0),0)) %>%
  mutate(WR_FP=ifelse(Y!=WR_Pred,ifelse(WR_Pred==1,1,0),0)) %>%
  mutate(WR_FN=ifelse(Y!=WR_Pred,ifelse(WR_Pred==0,1,0),0))

# Fill the Performance Measurement Matrix
ClassificationTreePerfMeas[5,"WR_TP"] = sum(CheckListWRSM$WR_TP)
ClassificationTreePerfMeas[5,"WR_TN"] = sum(CheckListWRSM$WR_TN)
ClassificationTreePerfMeas[5,"WR_FP"] = sum(CheckListWRSM$WR_FP)
ClassificationTreePerfMeas[5,"WR_FN"] = sum(CheckListWRSM$WR_FN)

# Plotting the Tree
fancyRpartPlot(ClassTreeWRSM)

# RB ---------------------------
# Predicting the likelyhood of a RB being picked in the draft
DtrainRBSM = DtrainSM %>%
  filter(Position == "RB") %>%
  select(-c(Player.Code, Name, Position, Year))

DtestRBSM = DtestSM %>%
  filter(Position == "RB") %>%
  select(-c(Player.Code, Name, Position, Year))

# Run a classification tree. We use the whole data for training, since the rpart-function has a built in cross-validation. For the evaluation of the 
# best model we also use the whole training set for this cross-validation.
ClassTreeRBSM = rpart(
  formula = Drafted ~ .,
  data    = DtrainRBSM,
  method  = "class")

CheckList = as.data.frame(cbind(DtrainRBNS$Drafted, predict(ClassTreeRBSM, DtrainRBNS)))

CheckListRBSM = CheckList %>%
  mutate(Y=V1) %>%
  select(-V1) %>%
  mutate(RB_Pred=ifelse(CheckList[,3]>0.5, 1,0)) %>%
  mutate(RB_TP=ifelse(Y==RB_Pred,ifelse(RB_Pred==1,1,0),0)) %>%
  mutate(RB_TN=ifelse(Y==RB_Pred,ifelse(RB_Pred==0,1,0),0)) %>%
  mutate(RB_FP=ifelse(Y!=RB_Pred,ifelse(RB_Pred==1,1,0),0)) %>%
  mutate(RB_FN=ifelse(Y!=RB_Pred,ifelse(RB_Pred==0,1,0),0))

# Fill the Performance Measurement Matrix
ClassificationTreePerfMeas[5,"RB_TP"] = sum(CheckListRBSM$RB_TP)
ClassificationTreePerfMeas[5,"RB_TN"] = sum(CheckListRBSM$RB_TN)
ClassificationTreePerfMeas[5,"RB_FP"] = sum(CheckListRBSM$RB_FP)
ClassificationTreePerfMeas[5,"RB_FN"] = sum(CheckListRBSM$RB_FN)

# Plotting the Tree
fancyRpartPlot(ClassTreeRBSM)

# Together ---------------------------
# Predicting the likelyhood of QB/RB/WR together for being picked in the draft
DtrainTogetherSM = DtrainSM %>%
  select(-c(Player.Code, Name, Position, Year))

DtestTogetherSM = DtestSM %>%
  select(-c(Player.Code, Name, Position, Year))

# Run a classification tree. We use the whole data for training, since the rpart-function has a built in cross-validation. For the evaluation of the 
# best model we also use the whole training set for this cross-validation.
ClassTreeTogetherSM = rpart(
  formula = Drafted ~ .,
  data    = DtrainTogetherSM,
  method  = "class")

CheckList = as.data.frame(cbind(DtrainTogetherNS$Drafted, predict(ClassTreeTogetherSM, DtrainTogetherNS)))

CheckListTogetherSM = CheckList %>%
  mutate(Drafted=V1) %>%
  select(-V1) %>%
  mutate(Together_Pred=ifelse(CheckList[,3]>0.5, 1,0)) %>%
  mutate(Together_TP=ifelse(Drafted==Together_Pred,ifelse(Together_Pred==1,1,0),0)) %>%
  mutate(Together_TN=ifelse(Drafted==Together_Pred,ifelse(Together_Pred==0,1,0),0)) %>%
  mutate(Together_FP=ifelse(Drafted!=Together_Pred,ifelse(Together_Pred==1,1,0),0)) %>%
  mutate(Together_FN=ifelse(Drafted!=Together_Pred,ifelse(Together_Pred==0,1,0),0))

# Fill the Performance Measurement Matrix
ClassificationTreePerfMeas[5,"Together_TP"] = sum(CheckListTogetherSM$Together_TP)
ClassificationTreePerfMeas[5,"Together_TN"] = sum(CheckListTogetherSM$Together_TN)
ClassificationTreePerfMeas[5,"Together_FP"] = sum(CheckListTogetherSM$Together_FP)
ClassificationTreePerfMeas[5,"Together_FN"] = sum(CheckListTogetherSM$Together_FN)

# Plotting the Tree
fancyRpartPlot(ClassTreeTogetherSM)


# Save the tibble for the Performance Measurement separately
save(ClassificationTreePerfMeas, file = "../Data/PerformanceMeasurement/ClassificationTreePerfMeas.Rdata")


# savePlotToFile(file.name = "QBtreeNS.jpg")





# # Tibble for Combined method--------------------------
# # Construct the tibble for the Combined Method
# ClassificationTreeCombinedMethod_tog = cbind.data.frame(Player.Code=Dtrain$Player.Code, Position=Dtrain$Position, CheckListTogether[,3:8])
# ClassificationTreeCombinedMethod_QB = cbind.data.frame(Player.Code=Dtrain$Player.Code[Dtrain$Position=="QB"], CheckListQB[,4:8])
# ClassificationTreeCombinedMethod_RB = cbind.data.frame(Player.Code=Dtrain$Player.Code[Dtrain$Position=="RB"], CheckListRB[,4:8])
# ClassificationTreeCombinedMethod_WR = cbind.data.frame(Player.Code=Dtrain$Player.Code[Dtrain$Position=="WR"], CheckListWR[,4:8])
# 
# ClassificationTreeCombinedMethod = merge(x = ClassificationTreeCombinedMethod_tog, y = ClassificationTreeCombinedMethod_QB, by = "Player.Code", all.x = TRUE)
# ClassificationTreeCombinedMethod = merge(x = ClassificationTreeCombinedMethod, y = ClassificationTreeCombinedMethod_RB, by = "Player.Code", all.x = TRUE)
# ClassificationTreeCombinedMethod = merge(x = ClassificationTreeCombinedMethod, y = ClassificationTreeCombinedMethod_WR, by = "Player.Code", all.x = TRUE)
# 
# save(ClassificationTreeCombinedMethod, file = "../Data/CombinedMethod/ClassificationTreeCombinedMethod.Rdata")
# 
# # Pro Memoria-----------------------
# # PerfMeas = ((sum(CheckListtog$TP))/((1+sum(CheckListtog$FN)+sum(CheckListtog$FP))*sum(CheckListtog$Y)))
