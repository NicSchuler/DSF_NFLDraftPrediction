# This script's goal is to aggregate the results of all the different models to compare them

# Load all the results of the different models
load("../Data/PerformanceMeasurement/ClassificationTreePerfMeas.Rdata")
load("../Data/PerformanceMeasurement/KNNPerfMeas.Rdata")
load("../Data/PerformanceMeasurement/NaiveBayesPerfMeas.Rdata")
load("../Data/PerformanceMeasurement/randomForestPerfMeas.Rdata")
load("../Data/PerformanceMeasurement/ANNPerfMeas.Rdata")
load("../Data/PerformanceMeasurement/LogisticRegressionPerfMeas.Rdata")

# Bring all the pieces together
PerfMeas07to13 = as.data.frame(rbind(ClassificationTreePerfMeas,KNNPerfMeas,NaiveBayesPerfMeas,randomForestPerfMeas, ANNPerfMeas, LogisticRegressionPerfMeas))

# Check whether the columns have always the same value inside (which means, that all methods
# used the unsampled data set and filtered by the right position for checking).
CheckTibble = data.frame(Method = PerfMeas07to13$Method, Sampling = PerfMeas07to13$Sampling, QB = NA, WR = NA, RB = NA, Together = NA, stringsAsFactors = FALSE)
for(i in 1:nrow(PerfMeas07to13)){
  CheckTibble$QB[i] = PerfMeas07to13$QB_TP[i] + PerfMeas07to13$QB_TN[i] + PerfMeas07to13$QB_FN[i] + PerfMeas07to13$QB_FP[i]
  CheckTibble$WR[i] = PerfMeas07to13$WR_TP[i] + PerfMeas07to13$WR_TN[i] + PerfMeas07to13$WR_FN[i] + PerfMeas07to13$WR_FP[i]
  CheckTibble$RB[i] = PerfMeas07to13$RB_TP[i] + PerfMeas07to13$RB_TN[i] + PerfMeas07to13$RB_FN[i] + PerfMeas07to13$RB_FP[i]
  CheckTibble$Together[i] = PerfMeas07to13$Together_TP[i] + PerfMeas07to13$Together_TN[i] + PerfMeas07to13$Together_FN[i] + PerfMeas07to13$Together_FP[i]
}


# Compute the Accuracy (based on the number of correct classifications)
PerfMeasTibble = data.frame(Method = PerfMeas07to13$Method, Sampling = PerfMeas07to13$Sampling, QB = NA, WR = NA, RB = NA, Together = NA, stringsAsFactors = FALSE)
for(i in 1:nrow(PerfMeas07to13)){
  PerfMeasTibble$QB[i] = (PerfMeas07to13$QB_TP[i] + PerfMeas07to13$QB_TN[i])/(PerfMeas07to13$QB_TP[i] + PerfMeas07to13$QB_TN[i] + PerfMeas07to13$QB_FN[i] + PerfMeas07to13$QB_FP[i])
  PerfMeasTibble$WR[i] = (PerfMeas07to13$WR_TP[i] + PerfMeas07to13$WR_TN[i])/(PerfMeas07to13$WR_TP[i] + PerfMeas07to13$WR_TN[i] + PerfMeas07to13$WR_FN[i] + PerfMeas07to13$WR_FP[i])
  PerfMeasTibble$RB[i] = (PerfMeas07to13$RB_TP[i] + PerfMeas07to13$RB_TN[i])/(PerfMeas07to13$RB_TP[i] + PerfMeas07to13$RB_TN[i] + PerfMeas07to13$RB_FN[i] + PerfMeas07to13$RB_FP[i])
  PerfMeasTibble$Together[i] = (PerfMeas07to13$Together_TP[i] + PerfMeas07to13$Together_TN[i])/(PerfMeas07to13$Together_TP[i] + PerfMeas07to13$Together_TN[i] + PerfMeas07to13$Together_FN[i] + PerfMeas07to13$Together_FP[i])
}

# Look for the best Combinations
# Create an empty dataframe
ResultTibble = data.frame(Position = c("QB", "WR", "RB", "Together"), Method = NA, Sampling = NA, Accuracy = NA)

# Fill the Methods
ResultTibble$Method[1] = PerfMeasTibble$Method[which.max(PerfMeasTibble$QB)]
ResultTibble$Method[2] = PerfMeasTibble$Method[which.max(PerfMeasTibble$WR)]
ResultTibble$Method[3] = PerfMeasTibble$Method[which.max(PerfMeasTibble$RB)]
ResultTibble$Method[4] = PerfMeasTibble$Method[which.max(PerfMeasTibble$Together)]

# Fill the Sampling methods
ResultTibble$Sampling[1] = PerfMeasTibble$Sampling[which.max(PerfMeasTibble$QB)]
ResultTibble$Sampling[2] = PerfMeasTibble$Sampling[which.max(PerfMeasTibble$WR)]
ResultTibble$Sampling[3] = PerfMeasTibble$Sampling[which.max(PerfMeasTibble$RB)]
ResultTibble$Sampling[4] = PerfMeasTibble$Sampling[which.max(PerfMeasTibble$Together)]

# Fill the value of Accuracy
ResultTibble$Accuracy[1] = PerfMeasTibble$QB[which.max(PerfMeasTibble$QB)]
ResultTibble$Accuracy[2] = PerfMeasTibble$WR[which.max(PerfMeasTibble$WR)]
ResultTibble$Accuracy[3] = PerfMeasTibble$RB[which.max(PerfMeasTibble$RB)]
ResultTibble$Accuracy[4] = PerfMeasTibble$Together[which.max(PerfMeasTibble$Together)]

save(ResultTibble, file="../Data/PerformanceMeasurement/BestModels.Rdata")
save(PerfMeasTibble, file="../Data/PerformanceMeasurement/PerfMeasAllModels.Rdata")




# 2014 data --------------
load("../Data/PerformanceMeasurement/randomForestPerfMeas2014.Rdata")

CheckTibble14 = data.frame(Method = randomForestPerfMeas2014$Method, Sampling = randomForestPerfMeas2014$Sampling, QB = NA, WR = NA, RB = NA, Together = NA, stringsAsFactors = FALSE)
for(i in 1:nrow(randomForestPerfMeas2014)){
  CheckTibble14$QB[i] = randomForestPerfMeas2014$QB_TP[i] + randomForestPerfMeas2014$QB_TN[i] + randomForestPerfMeas2014$QB_FN[i] + randomForestPerfMeas2014$QB_FP[i]
  CheckTibble14$WR[i] = randomForestPerfMeas2014$WR_TP[i] + randomForestPerfMeas2014$WR_TN[i] + randomForestPerfMeas2014$WR_FN[i] + randomForestPerfMeas2014$WR_FP[i]
  CheckTibble14$RB[i] = randomForestPerfMeas2014$RB_TP[i] + randomForestPerfMeas2014$RB_TN[i] + randomForestPerfMeas2014$RB_FN[i] + randomForestPerfMeas2014$RB_FP[i]
  CheckTibble14$Together[i] = randomForestPerfMeas2014$Together_TP[i] + randomForestPerfMeas2014$Together_TN[i] + randomForestPerfMeas2014$Together_FN[i] + randomForestPerfMeas2014$Together_FP[i]
}


# Compute the Accuracy (based on the number of correct classifications)
PerfMeasTibble14 = data.frame(Method = randomForestPerfMeas2014$Method, Sampling = randomForestPerfMeas2014$Sampling, QB = NA, WR = NA, RB = NA, Together = NA, stringsAsFactors = FALSE)
for(i in 1:nrow(randomForestPerfMeas2014)){
  PerfMeasTibble14$QB[i] = (randomForestPerfMeas2014$QB_TP[i] + randomForestPerfMeas2014$QB_TN[i])/(randomForestPerfMeas2014$QB_TP[i] + randomForestPerfMeas2014$QB_TN[i] + randomForestPerfMeas2014$QB_FN[i] + randomForestPerfMeas2014$QB_FP[i])
  PerfMeasTibble14$WR[i] = (randomForestPerfMeas2014$WR_TP[i] + randomForestPerfMeas2014$WR_TN[i])/(randomForestPerfMeas2014$WR_TP[i] + randomForestPerfMeas2014$WR_TN[i] + randomForestPerfMeas2014$WR_FN[i] + randomForestPerfMeas2014$WR_FP[i])
  PerfMeasTibble14$RB[i] = (randomForestPerfMeas2014$RB_TP[i] + randomForestPerfMeas2014$RB_TN[i])/(randomForestPerfMeas2014$RB_TP[i] + randomForestPerfMeas2014$RB_TN[i] + randomForestPerfMeas2014$RB_FN[i] + randomForestPerfMeas2014$RB_FP[i])
  PerfMeasTibble14$Together[i] = (randomForestPerfMeas2014$Together_TP[i] + randomForestPerfMeas2014$Together_TN[i])/(randomForestPerfMeas2014$Together_TP[i] + randomForestPerfMeas2014$Together_TN[i] + randomForestPerfMeas2014$Together_FN[i] + randomForestPerfMeas2014$Together_FP[i])
}
