# Load required packages

library(dplyr)       # data wrangling
library(rpart)       # performing regression trees
library(rpart.plot)  # plotting regression trees
library(tidyverse)
library(rattle)				    	# Fancy tree plot
library(RColorBrewer)				# Color selection for fancy tree plot

load("../Data/CleanData/CleanClass2007to2014_2.Rdata")

ClassificationTreePerfMeas = data.frame(Method = character(), QB_TP = integer(), QB_TN = integer(), QB_FP = integer(), QB_FN = integer(),
                                        WR_TP = integer(), WR_TN = integer(), WR_FP = integer(), WR_FN = integer(),
                                        RB_TP = integer(), RB_TN = integer(), RB_FP = integer(), RB_FN = integer(),
                                        Together_TP = integer(), Together_TN = integer(), Together_FP = integer(), Together_FN = integer(), stringsAsFactors = FALSE)

ClassificationTreePerfMeas[1,1] = "ClassificationTree"

# Splitting the data
# We use all the available information just before the 2014 NFL-Draft, in order to train the model and then apply it on the data for 2014.
# In other words we act as if it was the end of April 2014 (which is one week before the draft). Therefore we will do the performance measurement
# on the training data and apply the best model to the 2014 data to see if we were right.
Dtrain = CleanClass2007to2014_2 %>%
  filter(Year != 2014)

Dtest = CleanClass2007to2014_2 %>%
  filter(Year == 2014)


# QB ---------------------------
# Predicting the likelyhood of a QB being picked in the draft
DtrainQB = Dtrain %>%
  filter(Position == "QB") %>%
  select(-c(Player.Code, Name, Class, Position, Year))

DtestQB = Dtest %>%
  filter(Position == "QB") %>%
  select(-c(Player.Code, Name, Class, Position, Year))

# Run a classification tree. We use the whole data for training, since the rpart-function has a built in cross-validation. For the evaluation of the 
# best model we also use the whole training set for this cross-validation.
ClassTreeQB = rpart(
  formula = Drafted ~ .,
  data    = DtrainQB,
  method  = "class")

CheckList = as.data.frame(cbind(DtrainQB$Drafted, predict(ClassTreeQB, DtrainQB)))

CheckListQB = CheckList %>%
  mutate(Y=V1) %>%
  select(-V1) %>%
  mutate(QB_Pred=ifelse(CheckList[,3]>0.5, 1,0)) %>%
  mutate(QB_TP=ifelse(Y==QB_Pred,ifelse(QB_Pred==1,1,0),0)) %>%
  mutate(QB_TN=ifelse(Y==QB_Pred,ifelse(QB_Pred==0,1,0),0)) %>%
  mutate(QB_FP=ifelse(Y!=QB_Pred,ifelse(QB_Pred==1,1,0),0)) %>%
  mutate(QB_FN=ifelse(Y!=QB_Pred,ifelse(QB_Pred==0,1,0),0))

# Fill the Performance Measurement Matrix
ClassificationTreePerfMeas[1,"QB_TP"] = sum(CheckListQB$QB_TP)
ClassificationTreePerfMeas[1,"QB_TN"] = sum(CheckListQB$QB_TN)
ClassificationTreePerfMeas[1,"QB_FP"] = sum(CheckListQB$QB_FP)
ClassificationTreePerfMeas[1,"QB_FN"] = sum(CheckListQB$QB_FN)

# Plotting the Tree
fancyRpartPlot(ClassTreeQB)

savePlotToFile(file.name = "QBtree.jpg")

# WR ---------------------------
# Predicting the likelyhood of a WR being picked in the draft
DtrainWR = Dtrain %>%
  filter(Position == "WR") %>%
  select(-c(Player.Code, Name, Class, Position, Year))

DtestWR = Dtest %>%
  filter(Position == "WR") %>%
  select(-c(Player.Code, Name, Class, Position, Year))

# Run a classification tree. We use the whole data for training, since the rpart-function has a built in cross-validation. For the evaluation of the 
# best model we also use the whole training set for this cross-validation.
ClassTreeWR = rpart(
  formula = Drafted ~ .,
  data    = DtrainWR,
  method  = "class")

CheckList = as.data.frame(cbind(DtrainWR$Drafted, predict(ClassTreeWR, DtrainWR)))

CheckListWR = CheckList %>%
  mutate(Y=V1) %>%
  select(-V1) %>%
  mutate(WR_Pred=ifelse(CheckList[,3]>0.5, 1,0)) %>%
  mutate(WR_TP=ifelse(Y==WR_Pred,ifelse(WR_Pred==1,1,0),0)) %>%
  mutate(WR_TN=ifelse(Y==WR_Pred,ifelse(WR_Pred==0,1,0),0)) %>%
  mutate(WR_FP=ifelse(Y!=WR_Pred,ifelse(WR_Pred==1,1,0),0)) %>%
  mutate(WR_FN=ifelse(Y!=WR_Pred,ifelse(WR_Pred==0,1,0),0))

# Fill the Performance Measurement Matrix
ClassificationTreePerfMeas[1,"WR_TP"] = sum(CheckListWR$WR_TP)
ClassificationTreePerfMeas[1,"WR_TN"] = sum(CheckListWR$WR_TN)
ClassificationTreePerfMeas[1,"WR_FP"] = sum(CheckListWR$WR_FP)
ClassificationTreePerfMeas[1,"WR_FN"] = sum(CheckListWR$WR_FN)

# Plotting the Tree
fancyRpartPlot(ClassTreeWR)

savePlotToFile(file.name = "WRtree.jpg")

# RB ---------------------------
# Predicting the likelyhood of a RB being picked in the draft
DtrainRB = Dtrain %>%
  filter(Position == "RB") %>%
  select(-c(Player.Code, Name, Class, Position, Year))

DtestRB = Dtest %>%
  filter(Position == "RB") %>%
  select(-c(Player.Code, Name, Class, Position, Year))

# Run a classification tree. We use the whole data for training, since the rpart-function has a built in cross-validation. For the evaluation of the 
# best model we also use the whole training set for this cross-validation.
ClassTreeRB = rpart(
  formula = Drafted ~ .,
  data    = DtrainRB,
  method  = "class")

CheckList = as.data.frame(cbind(DtrainRB$Drafted, predict(ClassTreeRB, DtrainRB)))

CheckListRB = CheckList %>%
  mutate(Y=V1) %>%
  select(-V1) %>%
  mutate(RB_Pred=ifelse(CheckList[,3]>0.5, 1,0)) %>%
  mutate(RB_TP=ifelse(Y==RB_Pred,ifelse(RB_Pred==1,1,0),0)) %>%
  mutate(RB_TN=ifelse(Y==RB_Pred,ifelse(RB_Pred==0,1,0),0)) %>%
  mutate(RB_FP=ifelse(Y!=RB_Pred,ifelse(RB_Pred==1,1,0),0)) %>%
  mutate(RB_FN=ifelse(Y!=RB_Pred,ifelse(RB_Pred==0,1,0),0))

# Fill the Performance Measurement Matrix
ClassificationTreePerfMeas[1,"RB_TP"] = sum(CheckListRB$RB_TP)
ClassificationTreePerfMeas[1,"RB_TN"] = sum(CheckListRB$RB_TN)
ClassificationTreePerfMeas[1,"RB_FP"] = sum(CheckListRB$RB_FP)
ClassificationTreePerfMeas[1,"RB_FN"] = sum(CheckListRB$RB_FN)

# Plotting the Tree
fancyRpartPlot(ClassTreeRB)

savePlotToFile(file.name = "RBtree.jpg")

# Together ---------------------------
# Predicting the likelyhood of QB/RB/WR together for being picked in the draft
DtrainTogether = Dtrain %>%
  select(-c(Player.Code, Name, Class, Position, Year))

DtestTogether = Dtest %>%
  select(-c(Player.Code, Name, Class, Position, Year))

# Run a classification tree. We use the whole data for training, since the rpart-function has a built in cross-validation. For the evaluation of the 
# best model we also use the whole training set for this cross-validation.
ClassTreeTogether = rpart(
  formula = Drafted ~ .,
  data    = DtrainTogether,
  method  = "class")

CheckList = as.data.frame(cbind(DtrainTogether$Drafted, predict(ClassTreeTogether, DtrainTogether)))

CheckListTogether = CheckList %>%
  mutate(Drafted=V1) %>%
  select(-V1) %>%
  mutate(Together_Pred=ifelse(CheckList[,3]>0.5, 1,0)) %>%
  mutate(Together_TP=ifelse(Drafted==Together_Pred,ifelse(Together_Pred==1,1,0),0)) %>%
  mutate(Together_TN=ifelse(Drafted==Together_Pred,ifelse(Together_Pred==0,1,0),0)) %>%
  mutate(Together_FP=ifelse(Drafted!=Together_Pred,ifelse(Together_Pred==1,1,0),0)) %>%
  mutate(Together_FN=ifelse(Drafted!=Together_Pred,ifelse(Together_Pred==0,1,0),0))

# Fill the Performance Measurement Matrix
ClassificationTreePerfMeas[1,"Together_TP"] = sum(CheckListTogether$Together_TP)
ClassificationTreePerfMeas[1,"Together_TN"] = sum(CheckListTogether$Together_TN)
ClassificationTreePerfMeas[1,"Together_FP"] = sum(CheckListTogether$Together_FP)
ClassificationTreePerfMeas[1,"Together_FN"] = sum(CheckListTogether$Together_FN)

# Plotting the Tree
fancyRpartPlot(ClassTreeTogether)

savePlotToFile(file.name = "Togethertree.jpg")

# Save the tibble for the Performance Measurement separately
save(ClassificationTreePerfMeas, file = "../Data/PerformanceMeasurement/ClassificationTreePerfMeas.Rdata")

# Tibble for Combined method--------------------------
# Construct the tibble for the Combined Method
ClassificationTreeCombinedMethod_tog = cbind.data.frame(Player.Code=Dtrain$Player.Code, Position=Dtrain$Position, CheckListTogether[,3:8])
ClassificationTreeCombinedMethod_QB = cbind.data.frame(Player.Code=Dtrain$Player.Code[Dtrain$Position=="QB"], CheckListQB[,4:8])
ClassificationTreeCombinedMethod_RB = cbind.data.frame(Player.Code=Dtrain$Player.Code[Dtrain$Position=="RB"], CheckListRB[,4:8])
ClassificationTreeCombinedMethod_WR = cbind.data.frame(Player.Code=Dtrain$Player.Code[Dtrain$Position=="WR"], CheckListWR[,4:8])

ClassificationTreeCombinedMethod = merge(x = ClassificationTreeCombinedMethod_tog, y = ClassificationTreeCombinedMethod_QB, by = "Player.Code", all.x = TRUE)
ClassificationTreeCombinedMethod = merge(x = ClassificationTreeCombinedMethod, y = ClassificationTreeCombinedMethod_RB, by = "Player.Code", all.x = TRUE)
ClassificationTreeCombinedMethod = merge(x = ClassificationTreeCombinedMethod, y = ClassificationTreeCombinedMethod_WR, by = "Player.Code", all.x = TRUE)

save(ClassificationTreeCombinedMethod, file = "../Data/CombinedMethod/ClassificationTreeCombinedMethod.Rdata")

# Pro Memoria-----------------------
# PerfMeas = ((sum(CheckListtog$TP))/((1+sum(CheckListtog$FN)+sum(CheckListtog$FP))*sum(CheckListtog$Y)))
