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
# 

load("../Data/CleanData/CleanClass2007to2014_2.Rdata")

PerfMeasTibble = matrix(NA, ncol=4, nrow = 1)
colnames(PerfMeasTibble) = c("QB","WR","RB","together")

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
  method  = "class",
  control = list(cp = 0, minsplit = 1, maxdepth = 30, xval = 10))

printcp(ClassTreeQB)  
plotcp(ClassTreeQB) 

CheckList = as.data.frame(cbind(DtestQB$Drafted, predict(ClassTreeQB, DtestQB)))

CheckListQB = CheckList %>%
  mutate(Y=V1) %>%
  select(-V1) %>%
  mutate(Pred=ifelse(CheckList[,3]>0.5, 1,0)) %>%
  mutate(TP=ifelse(Y==Pred,ifelse(Pred==1,1,0),0)) %>%
  mutate(TN=ifelse(Y==Pred,ifelse(Pred==0,1,0),0)) %>%
  mutate(FP=ifelse(Y!=Pred,ifelse(Pred==1,1,0),0)) %>%
  mutate(FN=ifelse(Y!=Pred,ifelse(Pred==0,1,0),0))

plot(ClassTreeQB, uniform=TRUE, main="Classification tree for QB's")
text(ClassTreeQB, use.n=TRUE, all=TRUE, cex=.5, pretty = 0)

PerfMeasTibble[1,1] = ((sum(CheckListQB$FP)+sum(CheckListQB$FN))/nrow(CheckListQB))

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
  method  = "class",
  control = list(cp = 0, minsplit = 1, maxdepth = 30, xval = 10))

printcp(ClassTreeWR)  
plotcp(ClassTreeWR) 

CheckList = as.data.frame(cbind(DtestWR$Drafted, predict(ClassTreeWR, DtestWR)))

CheckListWR = CheckList %>%
  mutate(Y=V1) %>%
  select(-V1) %>%
  mutate(Pred=ifelse(CheckList[,3]>0.5, 1,0)) %>%
  mutate(TP=ifelse(Y==Pred,ifelse(Pred==1,1,0),0)) %>%
  mutate(TN=ifelse(Y==Pred,ifelse(Pred==0,1,0),0)) %>%
  mutate(FP=ifelse(Y!=Pred,ifelse(Pred==1,1,0),0)) %>%
  mutate(FN=ifelse(Y!=Pred,ifelse(Pred==0,1,0),0))

plot(ClassTreeWR, uniform=TRUE, main="Classification tree for WR's")
text(ClassTreeWR, use.n=TRUE, all=TRUE, cex=.5, pretty = 0)

PerfMeasTibble[1,2] = ((sum(CheckListWR$FP)+sum(CheckListWR$FN))/nrow(CheckListWR))

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
  method  = "class",
  control = list(cp = 0, minsplit = 1, maxdepth = 30, xval = 10))

printcp(ClassTreeRB)  
plotcp(ClassTreeRB) 

CheckList = as.data.frame(cbind(DtestRB$Drafted, predict(ClassTreeRB, DtestRB)))

CheckListRB = CheckList %>%
  mutate(Y=V1) %>%
  select(-V1) %>%
  mutate(Pred=ifelse(CheckList[,3]>0.5, 1,0)) %>%
  mutate(TP=ifelse(Y==Pred,ifelse(Pred==1,1,0),0)) %>%
  mutate(TN=ifelse(Y==Pred,ifelse(Pred==0,1,0),0)) %>%
  mutate(FP=ifelse(Y!=Pred,ifelse(Pred==1,1,0),0)) %>%
  mutate(FN=ifelse(Y!=Pred,ifelse(Pred==0,1,0),0))

plot(ClassTreeRB, uniform=TRUE, main="Classification tree for RB's")
text(ClassTreeRB, use.n=TRUE, all=TRUE, cex=.5, pretty = 0)

PerfMeasTibble[1,3] = ((sum(CheckListRB$FP)+sum(CheckListRB$FN))/nrow(CheckListRB))

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
  method  = "class",
  control = list(cp = 0, minsplit = 1, maxdepth = 30, xval = 10))

printcp(ClassTreetog)  
plotcp(ClassTreetog) 

CheckList = as.data.frame(cbind(Dtesttog$Drafted, predict(ClassTreetog, Dtesttog)))

CheckListtog = CheckList %>%
  mutate(Y=V1) %>%
  select(-V1) %>%
  mutate(Pred=ifelse(CheckList[,3]>0.5, 1,0)) %>%
  mutate(TP=ifelse(Y==Pred,ifelse(Pred==1,1,0),0)) %>%
  mutate(TN=ifelse(Y==Pred,ifelse(Pred==0,1,0),0)) %>%
  mutate(FP=ifelse(Y!=Pred,ifelse(Pred==1,1,0),0)) %>%
  mutate(FN=ifelse(Y!=Pred,ifelse(Pred==0,1,0),0))

plot(ClassTreetog, uniform=TRUE, main="Classification tree for QB's / WR's / RB's together")
text(ClassTreetog, use.n=TRUE, all=TRUE, cex=.5, pretty = 0)


fancyRpartPlot(ClassTreetog)

savePlotToFile(file.name = "Togtree.jpg")

PerfMeasTibble[1,4] = ((sum(CheckListtog$FP)+sum(CheckListtog$FN))/nrow(CheckListtog))
