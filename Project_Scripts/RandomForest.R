library(tidyverse)
library(randomForest)
library(ROCR)

# init ----

# Uncomment one of the lines to use the respective sampling
load("../Data/CleanData/CleanClass2007to2014_3.RData") # no sampling, don't comment this line out!
# load("../Data/CleanData/CleanClass2007to2013_3_oversampling.RData") # oversampling
# load("../Data/CleanData/CleanClass2007to2013_3_undersampling.RData") # undersampling
# load("../Data/CleanData/CleanClass2007to2013_3_Rose.both.RData") # ROSE both
# load("../Data/CleanData/CleanClass2007to2013_3_smote.RData") # SMOTE

# Uncomment one of the lines to use the respective sampling
cleanData <- as_tibble(CleanClass2007to2014_3) # no sampling, don't comment this line out!
# cleanData_s <- as_tibble(CleanClass2007to2014_3_oversampling) # oversampling
# cleanData_s <- as_tibble(CleanClass2007to2014_3_undersampling) # undersampling
# cleanData_s <- as_tibble(CleanClass2007to2014_3_Rose.both) # ROSE both
# cleanData_s <- as_tibble(cleanData_smote) # SMOTE

# Define performance measurement function
perfFun <- function(TP, FP, TN, FN){
  accuracy <- (TP + TN)/(TP + FP + TN + FN)
  precision <- TP/(TP + FP)
  recall <- TP/(TP + FN)
  F1 <- 2*((precision*recall)/(precision + recall))
  out <- data.frame("Accuracy" = accuracy, "Precision" = precision, "Recall" = recall, "F1" = F1)
  return(out)
}


# Random Forest for QBs ----
# Select years 2007 through 2013 as training data
cleanData_QB <- cleanData %>% filter(., Year < 2014, Position == "QB") %>% drop_na(.)
# cleanData_QB <- cleanData_s %>% filter(., Year < 2014, Position == "QB") %>% drop_na(.) # use this line when working with sampled data

x <- cleanData_QB %>% mutate(., "y" = as.factor(Drafted)) %>% select(., -Player.Code, -Name, -Class, -Position, -Year, -Drafted)

# Randomly shuffle the data for cross validation
set.seed(6969)
x <- x[sample(nrow(x)),]

# Create 10 folds
folds <- cut(seq(1,nrow(x)),breaks=10,labels=FALSE)

# Create placeholders for confusion matrices and variable importance
confusion_QB <- as.data.frame(matrix(NA, 6, 10))
importance_QB <- as.data.frame(matrix(NA, 24, 10))

confusion_QB_cv <- as.data.frame(matrix(NA, 6, 10))
importance_QB_cv <- as.data.frame(matrix(NA, 24, 10))
row.names(confusion_QB_cv) <- c("TN", "FP", "FN", "TP", "class error 0", "class error 1")

# Perform 10 fold cross validation using different numbers of trees in the model
for (j in 1:10){
  for(i in 1:10){
    # Segement data by fold
    testIndexes <- which(folds==i,arr.ind=TRUE)
    testData <- x[testIndexes, ]
    trainData <- x[-testIndexes, ]
    
    # Use the test and train data partitions to run Random Forest classifier
    RF_QB <- randomForest(y ~ ., data = x, ntree = j*100)
    
    # Save confusion matrix for j*100 trees
    confusion_QB[, i] <- as.vector(RF_QB$confusion)
    
    # Save variable importance for j*100 trees
    importance_QB[, i] <- as.vector(RF_QB$importance)
  }
  
  # Summarize cross validated confusion and variable importance for j*100 trees
  
  confusion_QB_cv[, j] <- rowMeans(confusion_QB)
  colnames(confusion_QB_cv)[j] <- as.character(j*100)
  
  importance_QB_cv[, j] <- rowMeans(importance_QB)
  colnames(importance_QB_cv)[j] <- as.character(j*100)
}

row.names(importance_QB_cv) <- names(RF_QB$forest$xlevels)

# Measure the performance of the models

perf <- perfFun(confusion_QB_cv["TP", 1], confusion_QB_cv["FP", 1], confusion_QB_cv["TN", 1], confusion_QB_cv["FN", 1])

for(i in 2:ncol(confusion_QB_cv)){
  perf <- rbind(perf, perfFun(confusion_QB_cv["TP", i], confusion_QB_cv["FP", i], confusion_QB_cv["TN", i], confusion_QB_cv["FN", i]))
}

row.names(perf) <- as.character(seq(100, 1000, by = 100))

# Select the number of trees that yielded the highest recall (this performance measurement is chosen arbitrarily out of the
# four calculated because it suits the purpose of the analysis, other options or a combination of different measures should
# be explored in the future)

ntrees <- 100*which.max(perf$Recall)

# Run the model with the best performing number of trees

RF_QB <- randomForest(y ~ ., data = x, ntree = ntrees)

# Use year 2014 for testing
cleanData_QB_test <- cleanData %>% filter(., Year == 2014, Position == "QB")
x_test <- cleanData_QB_test %>% select(., -Player.Code, -Name, -Class, -Position, -Year, -Drafted)
pred <- as.integer(as.vector(predict(RF_QB, x_test)))

# Combine predictions and player data
pred_QB <- tibble("Code" = cleanData_QB_test$Player.Code, "Name" = cleanData_QB_test$Name, "Drafted" = cleanData_QB_test$Drafted, "pred" = pred)

# Plot variable importance for the best cross validated model
varImpPlot(RF_QB, main = "Variable Importance QB")

# Plot the ROC curve for the best cross validated model
pred <- predict(RF_QB, x_test, type = "prob")
pred <- prediction(pred[,2], cleanData_QB_test$Drafted)
perf <- performance(pred, measure = "tpr", x.measure = "fpr")
plot(perf, colorize = TRUE, main = "ROC Curve QB")


# Random Forest for RBs ----
# Select years 2007 through 2013 as training data
cleanData_RB <- cleanData %>% filter(., Year < 2014, Position == "RB") %>% drop_na(.)
# cleanData_RB <- cleanData_s %>% filter(., Year < 2014, Position == "RB") %>% drop_na(.) # use this line when working with sampled data

x <- cleanData_RB %>% mutate(., "y" = as.factor(Drafted)) %>% select(., -Player.Code, -Name, -Class, -Position, -Year, -Drafted)

# Randomly shuffle the data for cross validation
set.seed(6969)
x <- x[sample(nrow(x)),]

# Create 10 folds
folds <- cut(seq(1,nrow(x)),breaks=10,labels=FALSE)

# Create placeholders for confusion matrices and variable importance
confusion_RB <- as.data.frame(matrix(NA, 6, 10))
importance_RB <- as.data.frame(matrix(NA, 24, 10))

confusion_RB_cv <- as.data.frame(matrix(NA, 6, 10))
importance_RB_cv <- as.data.frame(matrix(NA, 24, 10))
row.names(confusion_RB_cv) <- c("TN", "FP", "FN", "TP", "class error 0", "class error 1")

# Perform 10 fold cross validation using different numbers of trees in the model
for (j in 1:10){
  for(i in 1:10){
    # Segement data by fold
    testIndexes <- which(folds==i,arr.ind=TRUE)
    testData <- x[testIndexes, ]
    trainData <- x[-testIndexes, ]
    
    # Use the test and train data partitions to run Random Forest classifier
    RF_RB <- randomForest(y ~ ., data = x, ntree = j*100)
    
    # Save confusion matrix for j*100 trees
    confusion_RB[, i] <- as.vector(RF_RB$confusion)
    
    # Save variable importance for j*100 trees
    importance_RB[, i] <- as.vector(RF_RB$importance)
  }
  
  # Summarize cross validated confusion and variable importance for j*100 trees
  
  confusion_RB_cv[, j] <- rowMeans(confusion_RB)
  colnames(confusion_RB_cv)[j] <- as.character(j*100)
  
  importance_RB_cv[, j] <- rowMeans(importance_RB)
  colnames(importance_RB_cv)[j] <- as.character(j*100)
}

row.names(importance_RB_cv) <- names(RF_RB$forest$xlevels)

# Measure the performance of the models

perf <- perfFun(confusion_RB_cv["TP", 1], confusion_RB_cv["FP", 1], confusion_RB_cv["TN", 1], confusion_RB_cv["FN", 1])

for(i in 2:ncol(confusion_RB_cv)){
  perf <- rbind(perf, perfFun(confusion_RB_cv["TP", i], confusion_RB_cv["FP", i], confusion_RB_cv["TN", i], confusion_RB_cv["FN", i]))
}

row.names(perf) <- as.character(seq(100, 1000, by = 100))

# Select the number of trees that yielded the highest recall (this performance measurement is chosen arbitrarily out of the
# four calculated because it suits the purpose of the analysis, other options or a combination of different measures should
# be explored in the future)

ntrees <- 100*which.max(perf$Recall)

# Run the model with the best performing number of trees

RF_RB <- randomForest(y ~ ., data = x, ntree = ntrees)

# Use year 2014 for testing
cleanData_RB_test <- cleanData %>% filter(., Year == 2014, Position == "RB")
x_test <- cleanData_RB_test %>% select(., -Player.Code, -Name, -Class, -Position, -Year, -Drafted)
pred <- as.integer(as.vector(predict(RF_RB, x_test)))

# Combine predictions and player data
pred_RB <- tibble("Code" = cleanData_RB_test$Player.Code, "Name" = cleanData_RB_test$Name, "Drafted" = cleanData_RB_test$Drafted, "pred" = pred)

# Plot variable importance for the best cross validated model
varImpPlot(RF_RB, main = "Variable Importance RB")

# Plot the ROC curve for the best cross validated model
pred <- predict(RF_RB, x_test, type = "prob")
pred <- prediction(pred[,2], cleanData_RB_test$Drafted)
perf <- performance(pred, measure = "tpr", x.measure = "fpr")
plot(perf, colorize = TRUE, main = "ROC Curve RB")


# Random Forest for WRs ----
# Select years 2007 through 2013 as training data
cleanData_WR <- cleanData %>% filter(., Year < 2014, Position == "WR") %>% drop_na(.)
# cleanData_WR <- cleanData_s %>% filter(., Year < 2014, Position == "WR") %>% drop_na(.) # use this line when working with sampled data

x <- cleanData_WR %>% mutate(., "y" = as.factor(Drafted)) %>% select(., -Player.Code, -Name, -Class, -Position, -Year, -Drafted)

# Randomly shuffle the data for cross validation
set.seed(6969)
x <- x[sample(nrow(x)),]

# Create 10 folds
folds <- cut(seq(1,nrow(x)),breaks=10,labels=FALSE)

# Create placeholders for confusion matrices and variable importance
confusion_WR <- as.data.frame(matrix(NA, 6, 10))
importance_WR <- as.data.frame(matrix(NA, 24, 10))

confusion_WR_cv <- as.data.frame(matrix(NA, 6, 10))
importance_WR_cv <- as.data.frame(matrix(NA, 24, 10))
row.names(confusion_WR_cv) <- c("TN", "FP", "FN", "TP", "class error 0", "class error 1")

# Perform 10 fold cross validation using different numbers of trees in the model
for (j in 1:10){
  for(i in 1:10){
    # Segement data by fold
    testIndexes <- which(folds==i,arr.ind=TRUE)
    testData <- x[testIndexes, ]
    trainData <- x[-testIndexes, ]
    
    # Use the test and train data partitions to run Random Forest classifier
    RF_WR <- randomForest(y ~ ., data = x, ntree = j*100)
    
    # Save confusion matrix for j*100 trees
    confusion_WR[, i] <- as.vector(RF_WR$confusion)
    
    # Save variable importance for j*100 trees
    importance_WR[, i] <- as.vector(RF_WR$importance)
  }
  
  # Summarize cross validated confusion and variable importance for j*100 trees
  
  confusion_WR_cv[, j] <- rowMeans(confusion_WR)
  colnames(confusion_WR_cv)[j] <- as.character(j*100)
  
  importance_WR_cv[, j] <- rowMeans(importance_WR)
  colnames(importance_WR_cv)[j] <- as.character(j*100)
}

row.names(importance_WR_cv) <- names(RF_WR$forest$xlevels)

# Measure the performance of the models

perf <- perfFun(confusion_WR_cv["TP", 1], confusion_WR_cv["FP", 1], confusion_WR_cv["TN", 1], confusion_WR_cv["FN", 1])

for(i in 2:ncol(confusion_WR_cv)){
  perf <- rbind(perf, perfFun(confusion_WR_cv["TP", i], confusion_WR_cv["FP", i], confusion_WR_cv["TN", i], confusion_WR_cv["FN", i]))
}

row.names(perf) <- as.character(seq(100, 1000, by = 100))

# Select the number of trees that yielded the highest recall (this performance measurement is chosen arbitrarily out of the
# four calculated because it suits the purpose of the analysis, other options or a combination of different measures should
# be explored in the future)

ntrees <- 100*which.max(perf$Recall)

# Run the model with the best performing number of trees

RF_WR <- randomForest(y ~ ., data = x, ntree = ntrees)

# Use year 2014 for testing
cleanData_WR_test <- cleanData %>% filter(., Year == 2014, Position == "WR")
x_test <- cleanData_WR_test %>% select(., -Player.Code, -Name, -Class, -Position, -Year, -Drafted)
pred <- as.integer(as.vector(predict(RF_WR, x_test)))

# Combine predictions and player data
pred_WR <- tibble("Code" = cleanData_WR_test$Player.Code, "Name" = cleanData_WR_test$Name, "Drafted" = cleanData_WR_test$Drafted, "pred" = pred)

# Plot variable importance for the best cross validated model
varImpPlot(RF_WR, main = "Variable Importance WR")

# Plot the ROC curve for the best cross validated model
pred <- predict(RF_WR, x_test, type = "prob")
pred <- prediction(pred[,2], cleanData_WR_test$Drafted)
perf <- performance(pred, measure = "tpr", x.measure = "fpr")
plot(perf, colorize = TRUE, main = "ROC Curve WR")


# Random Forest for all positions ----
# Select years 2007 through 2013 as training data
cleanData_all <- cleanData %>% filter(., Year < 2014) %>% drop_na(.)
# cleanData_all <- cleanData_s %>% filter(., Year < 2014) %>% drop_na(.) # use this line when working with sampled data

x <- cleanData_all %>% mutate(., "y" = as.factor(Drafted)) %>% select(., -Player.Code, -Name, -Class, -Position, -Year, -Drafted)

# Randomly shuffle the data for cross validation
set.seed(6969)
x <- x[sample(nrow(x)),]

# Create 10 folds
folds <- cut(seq(1,nrow(x)),breaks=10,labels=FALSE)

# Create placeholders for confusion matrices and variable importance
confusion_all <- as.data.frame(matrix(NA, 6, 10))
importance_all <- as.data.frame(matrix(NA, 24, 10))

confusion_all_cv <- as.data.frame(matrix(NA, 6, 10))
importance_all_cv <- as.data.frame(matrix(NA, 24, 10))
row.names(confusion_all_cv) <- c("TN", "FP", "FN", "TP", "class error 0", "class error 1")

# Perform 10 fold cross validation using different numbers of trees in the model
for (j in 1:10){
  for(i in 1:10){
    # Segement data by fold
    testIndexes <- which(folds==i,arr.ind=TRUE)
    testData <- x[testIndexes, ]
    trainData <- x[-testIndexes, ]
    
    # Use the test and train data partitions to run Random Forest classifier
    RF_all <- randomForest(y ~ ., data = x, ntree = j*100)
    
    # Save confusion matrix for j*100 trees
    confusion_all[, i] <- as.vector(RF_all$confusion)
    
    # Save variable importance for j*100 trees
    importance_all[, i] <- as.vector(RF_all$importance)
  }
  
  # Summarize cross validated confusion and variable importance for j*100 trees
  
  confusion_all_cv[, j] <- rowMeans(confusion_all)
  colnames(confusion_all_cv)[j] <- as.character(j*100)
  
  importance_all_cv[, j] <- rowMeans(importance_all)
  colnames(importance_all_cv)[j] <- as.character(j*100)
}

row.names(importance_all_cv) <- names(RF_all$forest$xlevels)

# Measure the performance of the models

perf <- perfFun(confusion_all_cv["TP", 1], confusion_all_cv["FP", 1], confusion_all_cv["TN", 1], confusion_all_cv["FN", 1])

for(i in 2:ncol(confusion_all_cv)){
  perf <- rbind(perf, perfFun(confusion_all_cv["TP", i], confusion_all_cv["FP", i], confusion_all_cv["TN", i], confusion_all_cv["FN", i]))
}

row.names(perf) <- as.character(seq(100, 1000, by = 100))

# Select the number of trees that yielded the highest recall (this performance measurement is chosen arbitrarily out of the
# four calculated because it suits the purpose of the analysis, other options or a combination of different measures should
# be explored in the future)

ntrees <- 100*which.max(perf$Recall)

# Run the model with the best performing number of trees

RF_all <- randomForest(y ~ ., data = x, ntree = ntrees)

# Use year 2014 for testing
cleanData_all_test <- cleanData %>% filter(., Year == 2014)
x_test <- cleanData_all_test %>% select(., -Player.Code, -Name, -Class, -Position, -Year, -Drafted)
pred <- as.integer(as.vector(predict(RF_all, x_test)))

# Combine predictions and player data
pred_all <- tibble("Code" = cleanData_all_test$Player.Code, "Name" = cleanData_all_test$Name, "Drafted" = cleanData_all_test$Drafted, "pred" = pred)

# Plot variable importance for the best cross validated model
varImpPlot(RF_all, main = "Variable Importance All Positions")

# Plot the ROC curve for the best cross validated model
pred <- predict(RF_all, x_test, type = "prob")
pred <- prediction(pred[,2], cleanData_all_test$Drafted)
perf <- performance(pred, measure = "tpr", x.measure = "fpr")
plot(perf, colorize = TRUE, main = "ROC Curve All Positions")


# Aggregate Results ----
resultsComb_separate <- tibble("Player.Code" = c(pred_QB$Code, pred_RB$Code, pred_WR$Code),
                               "Name" = c(pred_QB$Name, pred_RB$Name, pred_WR$Name),
                               "Pred" = ifelse(c(pred_QB$pred, pred_RB$pred, pred_WR$pred) >= 0.5, 1, 0),
                               "Drafted" = c(pred_QB$Drafted, pred_RB$Drafted, pred_WR$Drafted))
resultsComb_separate <- resultsComb_separate %>% mutate(., "error" = ifelse(Pred != Drafted, 1, 0),
                                                        "TP" = ifelse(Pred == Drafted & Drafted == 1, 1, 0),
                                                        "FP" = ifelse(Pred != Drafted & Drafted == 0, 1, 0),
                                                        "TN" = ifelse(Pred == Drafted & Drafted == 0, 1, 0),
                                                        "FN" = ifelse(Pred != Drafted & Drafted == 1, 1, 0))

resultsComb_all <- tibble("Player.Code" = pred_all$Code,
                          "Name" = pred_all$Name,
                          "Pred" = ifelse(pred_all$pred >= 0.5, 1, 0),
                          "Drafted" = pred_all$Drafted)
resultsComb_all <- resultsComb_all %>% mutate(., "error" = ifelse(Pred != Drafted, 1, 0),
                                              "TP" = ifelse(Pred == Drafted & Drafted == 1, 1, 0),
                                              "FP" = ifelse(Pred != Drafted & Drafted == 0, 1, 0),
                                              "TN" = ifelse(Pred == Drafted & Drafted == 0, 1, 0),
                                              "FN" = ifelse(Pred != Drafted & Drafted == 1, 1, 0))

# Prepare for between-model comparison
randomForestPerfMeas = data.frame(Method = character(), Sampling = character(),
                                  QB_TP = integer(), QB_TN = integer(), QB_FP = integer(), QB_FN = integer(),
                                  WR_TP = integer(), WR_TN = integer(), WR_FP = integer(), WR_FN = integer(),
                                  RB_TP = integer(), RB_TN = integer(), RB_FP = integer(), RB_FN = integer(),
                                  Together_TP = integer(), Together_TN = integer(), Together_FP = integer(), Together_FN = integer(),
                                  stringsAsFactors = FALSE)

randomForestPerfMeas[1, 2] = "no_sampling"
randomForestPerfMeas[2, 2] = "oversampling"
randomForestPerfMeas[3, 2] = "undersampling"
randomForestPerfMeas[4, 2] = "Rose_both"
randomForestPerfMeas[5, 2] = "Smote"
randomForestPerfMeas$Method = "randomForest"

# Note: row index has to be changed depending on the dataset used
randomForestPerfMeas[1, "QB_TP"] = sum(ifelse(pred_QB$pred == pred_QB$Drafted & pred_QB$pred == 1, 1, 0))
randomForestPerfMeas[1, "QB_TN"] = sum(ifelse(pred_QB$pred == pred_QB$Drafted & pred_QB$pred == 0, 1, 0))
randomForestPerfMeas[1, "QB_FP"] = sum(ifelse(pred_QB$pred != pred_QB$Drafted & pred_QB$pred == 1, 1, 0))
randomForestPerfMeas[1, "QB_FN"] = sum(ifelse(pred_QB$pred != pred_QB$Drafted & pred_QB$pred == 0, 1, 0))

randomForestPerfMeas[1, "RB_TP"] = sum(ifelse(pred_RB$pred == pred_RB$Drafted & pred_RB$pred == 1, 1, 0))
randomForestPerfMeas[1, "RB_TN"] = sum(ifelse(pred_RB$pred == pred_RB$Drafted & pred_RB$pred == 0, 1, 0))
randomForestPerfMeas[1, "RB_FP"] = sum(ifelse(pred_RB$pred != pred_RB$Drafted & pred_RB$pred == 1, 1, 0))
randomForestPerfMeas[1, "RB_FN"] = sum(ifelse(pred_RB$pred != pred_RB$Drafted & pred_RB$pred == 0, 1, 0))

randomForestPerfMeas[1, "WR_TP"] = sum(ifelse(pred_WR$pred == pred_WR$Drafted & pred_WR$pred == 1, 1, 0))
randomForestPerfMeas[1, "WR_TN"] = sum(ifelse(pred_WR$pred == pred_WR$Drafted & pred_WR$pred == 0, 1, 0))
randomForestPerfMeas[1, "WR_FP"] = sum(ifelse(pred_WR$pred != pred_WR$Drafted & pred_WR$pred == 1, 1, 0))
randomForestPerfMeas[1, "WR_FN"] = sum(ifelse(pred_WR$pred != pred_WR$Drafted & pred_WR$pred == 0, 1, 0))

randomForestPerfMeas[1, "Together_TP"] = sum(ifelse(pred_all$pred == pred_all$Drafted & pred_all$pred == 1, 1, 0))
randomForestPerfMeas[1, "Together_TN"] = sum(ifelse(pred_all$pred == pred_all$Drafted & pred_all$pred == 0, 1, 0))
randomForestPerfMeas[1, "Together_FP"] = sum(ifelse(pred_all$pred != pred_all$Drafted & pred_all$pred == 1, 1, 0))
randomForestPerfMeas[1, "Together_FN"] = sum(ifelse(pred_all$pred != pred_all$Drafted & pred_all$pred == 0, 1, 0))

# Save the results for model comparison
save(randomForestPerfMeas, file = "../Data/PerformanceMeasurement/randomForestPerfMeas.Rdata")

