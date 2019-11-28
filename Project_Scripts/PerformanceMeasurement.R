# This script's goal is to aggregate the results of all the different models to compare them

# Load all the results of the different models
load("../Data/PerformanceMeasurement/ClassificationTreePerfMeas.Rdata")


# Bring all the pieces together
PerfMeas07to13 = as.data.frame(rbind(ClassificationTreePerfMeas,))

# Check whether the columns have always the same value inside (which means, that all methods used the unsampled data set for checking)
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
