# This script's goal is to aggregate the results of all the different models to compare them

# Load all the results of the different models
load("../Data/PerformanceMeasurement/ClassificationTreePerfMeas.Rdata")


# Bring all the pieces together
PerfMeasDraftPredict_0 = as.data.frame(rbind(ClassificationTreePerfMeas,))

# The performance measurement formula we developed, since the error does not really explain much
# -> See ReadMe on Performance Measurement for further explanation
PerfMeas = function(TP,TN,FP,FN){
  Result <- ((TP)/(sum(1,FN,FP)*sum(TP,FN)))
  return(Result)
}

# Run the performance measurement formula we developed to obtain a tibble with all the results
PerfMeasDraftPredict_1 = PerfMeasDraftPredict_0 %>%
  mutate(QB = PerfMeas(QB_TP,QB_TN,QB_FP,QB_FN)) %>%
  mutate(WR = PerfMeas(WR_TP,WR_TN,WR_FP,WR_FN)) %>%
  mutate(RB = PerfMeas(RB_TP,RB_TN,RB_FP,RB_FN)) %>%
  mutate(Together = PerfMeas(Together_TP,Together_TN,Together_FP,Together_FN)) %>%
  select(Method,QB,WR,RB,Together)
