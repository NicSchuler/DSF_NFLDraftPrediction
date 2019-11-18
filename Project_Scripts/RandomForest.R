library(tidyverse)
library(randomForest)

# init ----
load("../Data/CleanData/CleanClass2007to2014_2.RData")
cleanData <- as_tibble(CleanClass2007to2014_2)

# Random Forest for QBs ----
# Select years 2007 through 2013 as training data
cleanData_QB <- cleanData %>% filter(., Year < 2014, Position == "QB") %>% drop_na(.)

x <- cleanData_QB %>% mutate(., "y" = as.factor(Drafted)) %>% select(., -Player.Code, -Name, -Class, -Position, -Year, -Drafted)

# Run Random Forest classifier
RF_QB <- randomForest(y ~ ., data = x)

# Use year 2014 for testing
cleanData_QB_test <- cleanData %>% filter(., Year == 2014, Position == "QB")
x_test <- cleanData_QB_test %>% select(., -Player.Code, -Name, -Class, -Position, -Year, -Drafted)
pred <- as.integer(as.vector(predict(RF_QB, cleanData_QB_test)))

# Combine predictions and player data
pred_QB <- tibble("Code" = cleanData_QB_test$Player.Code, "Name" = cleanData_QB_test$Name, "Drafted" = cleanData_QB_test$Drafted, "pred" = pred)

# Random Forest for RBs ----
# Select years 2007 through 2013 as training data
cleanData_RB <- cleanData %>% filter(., Year < 2014, Position == "RB") %>% drop_na(.)

x <- cleanData_RB %>% mutate(., "y" = as.factor(Drafted)) %>% select(., -Player.Code, -Name, -Class, -Position, -Year, -Drafted)

# Run Random Forest classifier
RF_RB <- randomForest(y ~ ., data = x)

# Use year 2014 for testing
cleanData_RB_test <- cleanData %>% filter(., Year == 2014, Position == "RB")
x_test <- cleanData_RB_test %>% select(., -Player.Code, -Name, -Class, -Position, -Year, -Drafted)
pred <- as.integer(as.vector(predict(RF_RB, cleanData_RB_test)))

# Combine predictions and player data
pred_RB <- tibble("Code" = cleanData_RB_test$Player.Code, "Name" = cleanData_RB_test$Name, "Drafted" = cleanData_RB_test$Drafted, "pred" = pred)

# Random Forest for WRs ----
# Select years 2007 through 2013 as training data
cleanData_WR <- cleanData %>% filter(., Year < 2014, Position == "WR") %>% drop_na(.)

x <- cleanData_WR %>% mutate(., "y" = as.factor(Drafted)) %>% select(., -Player.Code, -Name, -Class, -Position, -Year, -Drafted)

# Run Random Forest classifier
RF_WR <- randomForest(y ~ ., data = x)

# Use year 2014 for testing
cleanData_WR_test <- cleanData %>% filter(., Year == 2014, Position == "WR")
x_test <- cleanData_WR_test %>% select(., -Player.Code, -Name, -Class, -Position, -Year, -Drafted)
pred <- as.integer(as.vector(predict(RF_WR, cleanData_WR_test)))

# Combine predictions and player data
pred_WR <- tibble("Code" = cleanData_WR_test$Player.Code, "Name" = cleanData_WR_test$Name, "Drafted" = cleanData_WR_test$Drafted, "pred" = pred)

# Random Forest for all positions combined ----
# Select years 2007 through 2013 as training data
cleanData_all <- cleanData %>% filter(., Year < 2014) %>% drop_na(.)

x <- cleanData_all %>% mutate(., "y" = as.factor(Drafted)) %>% select(., -Player.Code, -Name, -Class, -Position, -Year, -Drafted)

# Run Random Forest classifier
RF_all <- randomForest(y ~ ., data = x)

# Use year 2014 for testing
cleanData_all_test <- cleanData %>% filter(., Year == 2014)
x_test <- cleanData_all_test %>% select(., -Player.Code, -Name, -Class, -Position, -Year, -Drafted)
pred <- as.integer(as.vector(predict(RF_all, cleanData_all_test)))

# Combine predictions and player data
pred_all <- tibble("Code" = cleanData_all_test$Player.Code, "Name" = cleanData_all_test$Name, "Drafted" = cleanData_all_test$Drafted, "pred" = pred)

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

