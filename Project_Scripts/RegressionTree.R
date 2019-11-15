# Load required packages

library(rsample)     # data splitting 
library(dplyr)       # data wrangling
library(rpart)       # performing regression trees
library(rpart.plot)  # plotting regression trees
library(ipred)       # bagging
library(caret)       # bagging
library(e1071)
library(tidyverse)

# 

load("../Data/CleanData/CleanClass2007to2014.Rdata")

CleanClass2007to2014_QB = CleanClass2007to2014 %>%
  filter(Position.x == "QB") %>%
  select(-c(Player.Code, Name.x, Team.Code.x, Class.x, Year, Position.x))

# Reshuffle the data and split in a testing and a training set
PercTrain = 0.7
iSplit = as.integer(PercTrain*nrow(CleanClass2007to2014_QB))
set.seed(1)
resh = sample(1:nrow(CleanClass2007to2014_QB))

Dtrain = CleanClass2007to2014_QB[resh[1:iSplit],]
Dtest = CleanClass2007to2014_QB[resh[(iSplit+1):nrow(CleanClass2007to2014_QB)],]
# Run a classification tree
ClassTreeQB = rpart(
  formula = Drafted ~ .,
  data    = Dtrain,
  method  = "class",
  control = list(cp = 0, minsplit = 1, maxdepth = 30, xval = 10))

printcp(ClassTreeQB)  
plotcp(ClassTreeQB) 

CheckList = as.data.frame(cbind(Dtest$Drafted, predict(ClassTreeQB, Dtest)))

CheckList1 = CheckList %>%
  mutate(Y=V1) %>%
  select(-V1) %>%
  mutate(Pred=ifelse(CheckList[,3]>0.5, 1,0)) %>%
  mutate(TP=ifelse(Y==Pred,ifelse(Pred==1,1,0),0)) %>%
  mutate(TN=ifelse(Y==Pred,ifelse(Pred==0,1,0),0)) %>%
  mutate(FP=ifelse(Y!=Pred,ifelse(Pred==1,1,0),0)) %>%
  mutate(FN=ifelse(Y!=Pred,ifelse(Pred==0,1,0),0))

plot(ClassTreeQB, uniform=TRUE, main="Classification tree for QB's")
text(ClassTreeQB, use.n=TRUE, all=TRUE, cex=.5, pretty = 0)


