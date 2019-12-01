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


# Create an empty tibble
PerfMeasTibble = data.frame(Method = PerfMeas07to13$Method, Sampling = PerfMeas07to13$Sampling, QB_Acc = NA, QB_Pre=NA, QB_Rec= NA, QB_F1=NA, WR_Acc = NA, WR_Pre=NA, WR_Rec=NA, WR_F1=NA, RB_Acc = NA, RB_Pre=NA, RB_Rec=NA, RB_F1=NA, Together_Acc = NA, Together_Pre=NA, Together_Rec=NA, Together_F1=NA, stringsAsFactors = FALSE)
# Compute the Accuracy (based on the number of correct classifications)
for(i in 1:nrow(PerfMeas07to13)){
  PerfMeasTibble$QB_Acc[i] = (PerfMeas07to13$QB_TP[i] + PerfMeas07to13$QB_TN[i])/(PerfMeas07to13$QB_TP[i] + PerfMeas07to13$QB_TN[i] + PerfMeas07to13$QB_FN[i] + PerfMeas07to13$QB_FP[i])
  PerfMeasTibble$WR_Acc[i] = (PerfMeas07to13$WR_TP[i] + PerfMeas07to13$WR_TN[i])/(PerfMeas07to13$WR_TP[i] + PerfMeas07to13$WR_TN[i] + PerfMeas07to13$WR_FN[i] + PerfMeas07to13$WR_FP[i])
  PerfMeasTibble$RB_Acc[i] = (PerfMeas07to13$RB_TP[i] + PerfMeas07to13$RB_TN[i])/(PerfMeas07to13$RB_TP[i] + PerfMeas07to13$RB_TN[i] + PerfMeas07to13$RB_FN[i] + PerfMeas07to13$RB_FP[i])
  PerfMeasTibble$Together_Acc[i] = (PerfMeas07to13$Together_TP[i] + PerfMeas07to13$Together_TN[i])/(PerfMeas07to13$Together_TP[i] + PerfMeas07to13$Together_TN[i] + PerfMeas07to13$Together_FN[i] + PerfMeas07to13$Together_FP[i])
}
# Compute the Presicion TP/(TP+FP)
for(i in 1:nrow(PerfMeas07to13)){
  PerfMeasTibble$QB_Pre[i] = (PerfMeas07to13$QB_TP[i])/(PerfMeas07to13$QB_TP[i] + PerfMeas07to13$QB_FP[i])
  PerfMeasTibble$WR_Pre[i] = (PerfMeas07to13$WR_TP[i])/(PerfMeas07to13$WR_TP[i] + PerfMeas07to13$WR_FP[i])
  PerfMeasTibble$RB_Pre[i] = (PerfMeas07to13$RB_TP[i])/(PerfMeas07to13$RB_TP[i] + PerfMeas07to13$RB_FP[i])
  PerfMeasTibble$Together_Pre[i] = (PerfMeas07to13$Together_TP[i])/(PerfMeas07to13$Together_TP[i] + PerfMeas07to13$Together_FP[i])
}
# Compute the Recall TP/(TP+FN)
for(i in 1:nrow(PerfMeas07to13)){
  PerfMeasTibble$QB_Rec[i] = (PerfMeas07to13$QB_TP[i])/(PerfMeas07to13$QB_TP[i] + PerfMeas07to13$QB_FN[i])
  PerfMeasTibble$WR_Rec[i] = (PerfMeas07to13$WR_TP[i])/(PerfMeas07to13$WR_TP[i] + PerfMeas07to13$WR_FN[i])
  PerfMeasTibble$RB_Rec[i] = (PerfMeas07to13$RB_TP[i])/(PerfMeas07to13$RB_TP[i] + PerfMeas07to13$RB_FN[i])
  PerfMeasTibble$Together_Rec[i] = (PerfMeas07to13$Together_TP[i])/(PerfMeas07to13$Together_TP[i] + PerfMeas07to13$Together_FN[i])
}
# Compute the F1-Score = 2*(Precision*Recall/(Precision+Recall))
for(i in 1:nrow(PerfMeasTibble)){
  PerfMeasTibble$QB_F1[i] = 2*(PerfMeasTibble$QB_Pre[i]*PerfMeasTibble$QB_Rec[i])/(PerfMeasTibble$QB_Pre[i] + PerfMeasTibble$QB_Rec[i])
  PerfMeasTibble$WR_F1[i] = 2*(PerfMeasTibble$WR_Pre[i]*PerfMeasTibble$WR_Rec[i])/(PerfMeasTibble$WR_Pre[i] + PerfMeasTibble$WR_Rec[i])
  PerfMeasTibble$RB_F1[i] = 2*(PerfMeasTibble$RB_Pre[i]*PerfMeasTibble$RB_Rec[i])/(PerfMeasTibble$RB_Pre[i] + PerfMeasTibble$RB_Rec[i])
  PerfMeasTibble$Together_F1[i] = 2*(PerfMeasTibble$Together_Pre[i]*PerfMeasTibble$Together_Rec[i])/(PerfMeasTibble$Together_Pre[i] + PerfMeasTibble$Together_Rec[i])
}


# Look for the best Combinations
# Create an empty dataframe
ResultTibble = data.frame(Position = c("QB", "WR", "RB", "Together"), Method = NA, Sampling = NA, F1 = NA, Accuracy = NA)

# Fill the Methods
ResultTibble$Method[1] = PerfMeasTibble$Method[which.max(PerfMeasTibble$QB_F1)]
ResultTibble$Method[2] = PerfMeasTibble$Method[which.max(PerfMeasTibble$WR_F1)]
ResultTibble$Method[3] = PerfMeasTibble$Method[which.max(PerfMeasTibble$RB_F1)]
ResultTibble$Method[4] = PerfMeasTibble$Method[which.max(PerfMeasTibble$Together_F1)]

# Fill the Sampling methods
ResultTibble$Sampling[1] = PerfMeasTibble$Sampling[which.max(PerfMeasTibble$QB_F1)]
ResultTibble$Sampling[2] = PerfMeasTibble$Sampling[which.max(PerfMeasTibble$WR_F1)]
ResultTibble$Sampling[3] = PerfMeasTibble$Sampling[which.max(PerfMeasTibble$RB_F1)]
ResultTibble$Sampling[4] = PerfMeasTibble$Sampling[which.max(PerfMeasTibble$Together_F1)]

# Fill the value of F1-Score
ResultTibble$F1[1] = PerfMeasTibble$QB_F1[which.max(PerfMeasTibble$QB_F1)]
ResultTibble$F1[2] = PerfMeasTibble$WR_F1[which.max(PerfMeasTibble$WR_F1)]
ResultTibble$F1[3] = PerfMeasTibble$RB_F1[which.max(PerfMeasTibble$RB_F1)]
ResultTibble$F1[4] = PerfMeasTibble$Together_F1[which.max(PerfMeasTibble$Together_F1)]

# Fill the value of Accuracy
ResultTibble$Accuracy[1] = PerfMeasTibble$QB_Acc[which.max(PerfMeasTibble$QB_F1)]
ResultTibble$Accuracy[2] = PerfMeasTibble$WR_Acc[which.max(PerfMeasTibble$WR_F1)]
ResultTibble$Accuracy[3] = PerfMeasTibble$RB_Acc[which.max(PerfMeasTibble$RB_F1)]
ResultTibble$Accuracy[4] = PerfMeasTibble$Together_Acc[which.max(PerfMeasTibble$Together_F1)]



save(ResultTibble, file="../Data/PerformanceMeasurement/BestModels.Rdata")
save(PerfMeasTibble, file="../Data/PerformanceMeasurement/PerfMeasAllModels.Rdata")




# Checking with the 2014 data--------------
load("../Data/PerformanceMeasurement/randomForestPerfMeas2014.Rdata")
load("../Data/PerformanceMeasurement/ClassificationTreePerfMeas14.Rdata")
load("../Data/PerformanceMeasurement/ANNPerfMeas2014.Rdata")
load("../Data/PerformanceMeasurement/NaiveBayesPerfMeasTest.Rdata")
load("../Data/PerformanceMeasurement/KNNPerfMeasTest.Rdata")

PerfMeas14 = as.data.frame(rbind(ClassificationTreePerfMeas14, randomForestPerfMeas2014, ANNPerfMeas2014, KNNPerfMeasTest, NaiveBayesPerfMeasTest))

CheckTibble14 = data.frame(Method = PerfMeas14$Method, Sampling = PerfMeas14$Sampling, QB = NA, WR = NA, RB = NA, Together = NA, stringsAsFactors = FALSE)
for(i in 1:nrow(PerfMeas14)){
  CheckTibble14$QB[i] = PerfMeas14$QB_TP[i] + PerfMeas14$QB_TN[i] + PerfMeas14$QB_FN[i] + PerfMeas14$QB_FP[i]
  CheckTibble14$WR[i] = PerfMeas14$WR_TP[i] + PerfMeas14$WR_TN[i] + PerfMeas14$WR_FN[i] + PerfMeas14$WR_FP[i]
  CheckTibble14$RB[i] = PerfMeas14$RB_TP[i] + PerfMeas14$RB_TN[i] + PerfMeas14$RB_FN[i] + PerfMeas14$RB_FP[i]
  CheckTibble14$Together[i] = PerfMeas14$Together_TP[i] + PerfMeas14$Together_TN[i] + PerfMeas14$Together_FN[i] + PerfMeas14$Together_FP[i]
}


# Create an empty tibble
PerfMeasTibble14 = data.frame(Method = PerfMeas14$Method, Sampling = PerfMeas14$Sampling, QB_Acc = NA, QB_Pre=NA, QB_Rec= NA, QB_F1=NA, WR_Acc = NA, WR_Pre=NA, WR_Rec=NA, WR_F1=NA, RB_Acc = NA, RB_Pre=NA, RB_Rec=NA, RB_F1=NA, Together_Acc = NA, Together_Pre=NA, Together_Rec=NA, Together_F1=NA, stringsAsFactors = FALSE)
# Compute the Accuracy (based on the number of correct classifications)
for(i in 1:nrow(PerfMeas14)){
  PerfMeasTibble14$QB_Acc[i] = (PerfMeas14$QB_TP[i] + PerfMeas14$QB_TN[i])/(PerfMeas14$QB_TP[i] + PerfMeas14$QB_TN[i] + PerfMeas14$QB_FN[i] + PerfMeas14$QB_FP[i])
  PerfMeasTibble14$WR_Acc[i] = (PerfMeas14$WR_TP[i] + PerfMeas14$WR_TN[i])/(PerfMeas14$WR_TP[i] + PerfMeas14$WR_TN[i] + PerfMeas14$WR_FN[i] + PerfMeas14$WR_FP[i])
  PerfMeasTibble14$RB_Acc[i] = (PerfMeas14$RB_TP[i] + PerfMeas14$RB_TN[i])/(PerfMeas14$RB_TP[i] + PerfMeas14$RB_TN[i] + PerfMeas14$RB_FN[i] + PerfMeas14$RB_FP[i])
  PerfMeasTibble14$Together_Acc[i] = (PerfMeas14$Together_TP[i] + PerfMeas14$Together_TN[i])/(PerfMeas14$Together_TP[i] + PerfMeas14$Together_TN[i] + PerfMeas14$Together_FN[i] + PerfMeas14$Together_FP[i])
}
# Compute the Presicion TP/(TP+FP)
for(i in 1:nrow(PerfMeas14)){
  PerfMeasTibble14$QB_Pre[i] = (PerfMeas14$QB_TP[i])/(PerfMeas14$QB_TP[i] + PerfMeas14$QB_FP[i])
  PerfMeasTibble14$WR_Pre[i] = (PerfMeas14$WR_TP[i])/(PerfMeas14$WR_TP[i] + PerfMeas14$WR_FP[i])
  PerfMeasTibble14$RB_Pre[i] = (PerfMeas14$RB_TP[i])/(PerfMeas14$RB_TP[i] + PerfMeas14$RB_FP[i])
  PerfMeasTibble14$Together_Pre[i] = (PerfMeas14$Together_TP[i])/(PerfMeas14$Together_TP[i] + PerfMeas14$Together_FP[i])
}
# Compute the Recall TP/(TP+FN)
for(i in 1:nrow(PerfMeas14)){
  PerfMeasTibble14$QB_Rec[i] = (PerfMeas14$QB_TP[i])/(PerfMeas14$QB_TP[i] + PerfMeas14$QB_FN[i])
  PerfMeasTibble14$WR_Rec[i] = (PerfMeas14$WR_TP[i])/(PerfMeas14$WR_TP[i] + PerfMeas14$WR_FN[i])
  PerfMeasTibble14$RB_Rec[i] = (PerfMeas14$RB_TP[i])/(PerfMeas14$RB_TP[i] + PerfMeas14$RB_FN[i])
  PerfMeasTibble14$Together_Rec[i] = (PerfMeas14$Together_TP[i])/(PerfMeas14$Together_TP[i] + PerfMeas14$Together_FN[i])
}
# Compute the F1-Score = 2*(Precision*Recall/(Precision+Recall))
for(i in 1:nrow(PerfMeasTibble14)){
  PerfMeasTibble14$QB_F1[i] = 2*(PerfMeasTibble14$QB_Pre[i]*PerfMeasTibble14$QB_Rec[i])/(PerfMeasTibble14$QB_Pre[i] + PerfMeasTibble14$QB_Rec[i])
  PerfMeasTibble14$WR_F1[i] = 2*(PerfMeasTibble14$WR_Pre[i]*PerfMeasTibble14$WR_Rec[i])/(PerfMeasTibble14$WR_Pre[i] + PerfMeasTibble14$WR_Rec[i])
  PerfMeasTibble14$RB_F1[i] = 2*(PerfMeasTibble14$RB_Pre[i]*PerfMeasTibble14$RB_Rec[i])/(PerfMeasTibble14$RB_Pre[i] + PerfMeasTibble14$RB_Rec[i])
  PerfMeasTibble14$Together_F1[i] = 2*(PerfMeasTibble14$Together_Pre[i]*PerfMeasTibble14$Together_Rec[i])/(PerfMeasTibble14$Together_Pre[i] + PerfMeasTibble14$Together_Rec[i])
}





save(PerfMeasTibble14, file="../Data/PerformanceMeasurement/PerfMeasRF14.Rdata")