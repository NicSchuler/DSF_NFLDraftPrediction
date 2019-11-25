# Load required packages

library(dplyr)       # data wrangling
library(rpart)       # performing regression trees
library(rpart.plot)  # plotting regression trees
library(tidyverse)
library(rattle)				    	# Fancy tree plot
library(RColorBrewer)				# Color selection for fancy tree plot

# RB and Together still needs to be updated

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

# Run a classification tree
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
# Predicting the likelyhood of a QB being picked in the draft
DtrainWR = Dtrain %>%
  filter(Position == "WR") %>%
  select(-c(Player.Code, Name, Class, Position, Year))

DtestWR = Dtest %>%
  filter(Position == "WR") %>%
  select(-c(Player.Code, Name, Class, Position, Year))

# Run a classification tree
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
CleanClass2007to2014_RB = CleanClass2007to2014_2 %>%
  filter(Position == "RB") %>%
  select(-c(Player.Code, Name, Class, Position))

DtrainRB = CleanClass2007to2014_RB %>%
  filter(Year != 2014) %>%
  select(-Year)

DtestRB = CleanClass2007to2014_RB %>%
  filter(Year == 2014) %>%
  select(-Year)

# Run a classification tree
ClassTreeRB = rpart(
  formula = Drafted ~ .,
  data    = DtrainRB,
  method  = "class")

CheckList = as.data.frame(cbind(DtestRB$Drafted, predict(ClassTreeRB, DtestRB)))

CheckListRB = CheckList %>%
  mutate(Y=V1) %>%
  select(-V1) %>%
  mutate(Pred=ifelse(CheckList[,3]>0.5, 1,0)) %>%
  mutate(TP=ifelse(Y==Pred,ifelse(Pred==1,1,0),0)) %>%
  mutate(TN=ifelse(Y==Pred,ifelse(Pred==0,1,0),0)) %>%
  mutate(FP=ifelse(Y!=Pred,ifelse(Pred==1,1,0),0)) %>%
  mutate(FN=ifelse(Y!=Pred,ifelse(Pred==0,1,0),0))

# Fill the Performance Measurement Matrix
ClassificationTreePerfMeas[1,"RB_TP"] = sum(CheckListRB$TP)
ClassificationTreePerfMeas[1,"RB_TN"] = sum(CheckListRB$TN)
ClassificationTreePerfMeas[1,"RB_FP"] = sum(CheckListRB$FP)
ClassificationTreePerfMeas[1,"RB_FN"] = sum(CheckListRB$FN)

# Together ---------------------------
# Predicting the likelyhood of a QB/WR/RB being picked in the draft (without filter)
CleanClass2007to2014_tog = CleanClass2007to2014_2 %>%
  select(-c(Player.Code, Name, Class, Position))

Dtraintog = CleanClass2007to2014_tog %>%
  filter(Year != 2014) %>%
  select(-Year)

Dtesttog = CleanClass2007to2014_tog %>%
  filter(Year == 2014) %>%
  select(-Year)

# Run a classification tree
ClassTreetog = rpart(
  formula = Drafted ~ .,
  data    = Dtraintog,
  method  = "class")

CheckList = as.data.frame(cbind(Dtesttog$Drafted, predict(ClassTreetog, Dtesttog)))

CheckListTogether = CheckList %>%
  mutate(Y=V1) %>%
  select(-V1) %>%
  mutate(Pred=ifelse(CheckList[,3]>0.5, 1,0)) %>%
  mutate(TP=ifelse(Y==Pred,ifelse(Pred==1,1,0),0)) %>%
  mutate(TN=ifelse(Y==Pred,ifelse(Pred==0,1,0),0)) %>%
  mutate(FP=ifelse(Y!=Pred,ifelse(Pred==1,1,0),0)) %>%
  mutate(FN=ifelse(Y!=Pred,ifelse(Pred==0,1,0),0))

# Fill the Performance Measurement Matrix
ClassificationTreePerfMeas[1,"Together_TP"] = sum(CheckListTogether$TP)
ClassificationTreePerfMeas[1,"Together_TN"] = sum(CheckListTogether$TN)
ClassificationTreePerfMeas[1,"Together_FP"] = sum(CheckListTogether$FP)
ClassificationTreePerfMeas[1,"Together_FN"] = sum(CheckListTogether$FN)

save(ClassificationTreePerfMeas, file="../Data/PerformanceMeasurement/ClassificationTreePerfMeas.Rdata")


fancyRpartPlot(ClassTreetog)

savePlotToFile(file.name = "Togtree.jpg")


# Pro Memoria
# PerfMeas = ((sum(CheckListtog$TP))/((1+sum(CheckListtog$FN)+sum(CheckListtog$FP))*sum(CheckListtog$Y)))

# Boosted Tree -----------------------------

# https://www.datacamp.com/community/tutorials/decision-trees-R
# GMB - Package

# need to CV the Boosted Trees!!!!
# library(gbm)
# ClassificationTreePerfMeasBoost = data.frame(Method = character(), QB_TP = integer(), QB_TN = integer(), QB_FP = integer(), QB_FN = integer(),
#                                         WR_TP = integer(), WR_TN = integer(), WR_FP = integer(), WR_FN = integer(),
#                                         RB_TP = integer(), RB_TN = integer(), RB_FP = integer(), RB_FN = integer(),
#                                         Together_TP = integer(), Together_TN = integer(), Together_FP = integer(), Together_FN = integer(), Together_PM = integer(),
#                                         stringsAsFactors = FALSE)
# 
# ClassificationTreePerfMeasBoost[1,1] = "ClassificationTreeBoost"
# n.trees = 1000
# set.seed(1)
# ClassTreetogBoost = gbm(
#   formula = Drafted ~ .,
#   distribution = "adaboost",
#   n.trees = 2000,
#   data = Dtraintog,
#   shrinkage = 0.01)
# 
# CheckListBoost = as.data.frame(cbind(Dtesttog$Drafted, predict(ClassTreetogBoost, newdata = Dtesttog, n.trees = 2000)))
# 
# CheckListtogBoost = CheckListBoost %>%
#   mutate(Y=V1) %>%
#   select(-V1) %>%
#   mutate(Pred=ifelse(CheckListBoost[,2]>0.5, 1,0)) %>%
#   mutate(TP=ifelse(Y==Pred,ifelse(Pred==1,1,0),0)) %>%
#   mutate(TN=ifelse(Y==Pred,ifelse(Pred==0,1,0),0)) %>%
#   mutate(FP=ifelse(Y!=Pred,ifelse(Pred==1,1,0),0)) %>%
#   mutate(FN=ifelse(Y!=Pred,ifelse(Pred==0,1,0),0))
# 
# # Fill the Performance Measurement Matrix
# ClassificationTreePerfMeasBoost[1,"Together_TP"] = sum(CheckListtogBoost$TP)
# ClassificationTreePerfMeasBoost[1,"Together_TN"] = sum(CheckListtogBoost$TN)
# ClassificationTreePerfMeasBoost[1,"Together_FP"] = sum(CheckListtogBoost$FP)
# ClassificationTreePerfMeasBoost[1,"Together_FN"] = sum(CheckListtogBoost$FN)
# ClassificationTreePerfMeasBoost[1,"Together_PM"] = ((sum(CheckListtogBoost$TP))/((1+sum(CheckListtogBoost$FN)+sum(CheckListtogBoost$FP))*sum(CheckListtogBoost$Y)))
# 
# asdf = rbind(ClassificationTreePerfMeas,ClassificationTreePerfMeasBoost)
