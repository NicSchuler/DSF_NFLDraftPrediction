# Load required packages

library(rsample)     # data splitting 
library(dplyr)       # data wrangling
library(rpart)       # performing regression trees
library(rpart.plot)  # plotting regression trees
library(ipred)       # bagging
library(caret)       # bagging
library(e1071)
library(tidyverse)
library(rattle)				    	# Fancy tree plot
library(RColorBrewer)				# Color selection for fancy tree plot
library(gbm)
# 

load("../Data/CleanData/CleanClass2007to2014_2.Rdata")

ClassificationTreePerfMeas = data.frame(Method = character(), QB_TP = integer(), QB_TN = integer(), QB_FP = integer(), QB_FN = integer(),
                                        WR_TP = integer(), WR_TN = integer(), WR_FP = integer(), WR_FN = integer(),
                                        RB_TP = integer(), RB_TN = integer(), RB_FP = integer(), RB_FN = integer(),
                                        Together_TP = integer(), Together_TN = integer(), Together_FP = integer(), Together_FN = integer(), stringsAsFactors = FALSE)

ClassificationTreePerfMeas[1,1] = "ClassificationTree"



# QB ---------------------------
# Predicting the likelyhood of a QB being picked in the draft
CleanClass2007to2014_QB = CleanClass2007to2014_2 %>%
  filter(Position == "QB") %>%
  select(-c(Player.Code, Name, Class, Position))

DtrainQB = CleanClass2007to2014_QB %>%
  filter(Year != 2014) %>%
  select(-Year)

DtestQB = CleanClass2007to2014_QB %>%
  filter(Year == 2014) %>%
  select(-Year)

# Run a classification tree
ClassTreeQB = rpart(
  formula = Drafted ~ .,
  data    = DtrainQB,
  method  = "class")

CheckList = as.data.frame(cbind(DtestQB$Drafted, predict(ClassTreeQB, DtestQB)))

CheckListQB = CheckList %>%
  mutate(Y=V1) %>%
  select(-V1) %>%
  mutate(Pred=ifelse(CheckList[,3]>0.5, 1,0)) %>%
  mutate(TP=ifelse(Y==Pred,ifelse(Pred==1,1,0),0)) %>%
  mutate(TN=ifelse(Y==Pred,ifelse(Pred==0,1,0),0)) %>%
  mutate(FP=ifelse(Y!=Pred,ifelse(Pred==1,1,0),0)) %>%
  mutate(FN=ifelse(Y!=Pred,ifelse(Pred==0,1,0),0))

# Fill the Performance Measurement Matrix
ClassificationTreePerfMeas[1,"QB_TP"] = sum(CheckListQB$TP)
ClassificationTreePerfMeas[1,"QB_TN"] = sum(CheckListQB$TN)
ClassificationTreePerfMeas[1,"QB_FP"] = sum(CheckListQB$FP)
ClassificationTreePerfMeas[1,"QB_FN"] = sum(CheckListQB$FN)

# WR ---------------------------
# Predicting the likelyhood of a WR being picked in the draft
CleanClass2007to2014_WR = CleanClass2007to2014_2 %>%
  filter(Position == "WR") %>%
  select(-c(Player.Code, Name, Class, Position))

DtrainWR = CleanClass2007to2014_WR %>%
  filter(Year != 2014) %>%
  select(-Year)

DtestWR = CleanClass2007to2014_WR %>%
  filter(Year == 2014) %>%
  select(-Year)

# Run a classification tree
ClassTreeWR = rpart(
  formula = Drafted ~ .,
  data    = DtrainWR,
  method  = "class")

CheckList = as.data.frame(cbind(DtestWR$Drafted, predict(ClassTreeWR, DtestWR)))

CheckListWR = CheckList %>%
  mutate(Y=V1) %>%
  select(-V1) %>%
  mutate(Pred=ifelse(CheckList[,3]>0.5, 1,0)) %>%
  mutate(TP=ifelse(Y==Pred,ifelse(Pred==1,1,0),0)) %>%
  mutate(TN=ifelse(Y==Pred,ifelse(Pred==0,1,0),0)) %>%
  mutate(FP=ifelse(Y!=Pred,ifelse(Pred==1,1,0),0)) %>%
  mutate(FN=ifelse(Y!=Pred,ifelse(Pred==0,1,0),0))

# Fill the Performance Measurement Matrix
ClassificationTreePerfMeas[1,"WR_TP"] = sum(CheckListWR$TP)
ClassificationTreePerfMeas[1,"WR_TN"] = sum(CheckListWR$TN)
ClassificationTreePerfMeas[1,"WR_FP"] = sum(CheckListWR$FP)
ClassificationTreePerfMeas[1,"WR_FN"] = sum(CheckListWR$FN)

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

ClassificationTreePerfMeasBoost = data.frame(Method = character(), QB_TP = integer(), QB_TN = integer(), QB_FP = integer(), QB_FN = integer(),
                                        WR_TP = integer(), WR_TN = integer(), WR_FP = integer(), WR_FN = integer(),
                                        RB_TP = integer(), RB_TN = integer(), RB_FP = integer(), RB_FN = integer(),
                                        Together_TP = integer(), Together_TN = integer(), Together_FP = integer(), Together_FN = integer(), Together_PM = integer(),
                                        stringsAsFactors = FALSE)

ClassificationTreePerfMeasBoost[1,1] = "ClassificationTreeBoost"
n.trees = 1000
set.seed(1)
ClassTreetogBoost = gbm(
  formula = Drafted ~ .,
  distribution = "adaboost",
  n.trees = 2000,
  data = Dtraintog,
  shrinkage = 0.01)

CheckListBoost = as.data.frame(cbind(Dtesttog$Drafted, predict(ClassTreetogBoost, newdata = Dtesttog, n.trees = 2000)))

CheckListtogBoost = CheckListBoost %>%
  mutate(Y=V1) %>%
  select(-V1) %>%
  mutate(Pred=ifelse(CheckListBoost[,2]>0.5, 1,0)) %>%
  mutate(TP=ifelse(Y==Pred,ifelse(Pred==1,1,0),0)) %>%
  mutate(TN=ifelse(Y==Pred,ifelse(Pred==0,1,0),0)) %>%
  mutate(FP=ifelse(Y!=Pred,ifelse(Pred==1,1,0),0)) %>%
  mutate(FN=ifelse(Y!=Pred,ifelse(Pred==0,1,0),0))

# Fill the Performance Measurement Matrix
ClassificationTreePerfMeasBoost[1,"Together_TP"] = sum(CheckListtogBoost$TP)
ClassificationTreePerfMeasBoost[1,"Together_TN"] = sum(CheckListtogBoost$TN)
ClassificationTreePerfMeasBoost[1,"Together_FP"] = sum(CheckListtogBoost$FP)
ClassificationTreePerfMeasBoost[1,"Together_FN"] = sum(CheckListtogBoost$FN)
ClassificationTreePerfMeasBoost[1,"Together_PM"] = ((sum(CheckListtogBoost$TP))/((1+sum(CheckListtogBoost$FN)+sum(CheckListtogBoost$FP))*sum(CheckListtogBoost$Y)))

asdf = rbind(ClassificationTreePerfMeas,ClassificationTreePerfMeasBoost)
