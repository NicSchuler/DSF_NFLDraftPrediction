
rm(list=ls())
graphics.off()

# Libraries
library(ROSE)  # Random Over-Sampling Examples:
               # It involves the creation of  a new  data set by suitably resampling the observations 
               # belonging to the two classes. Function ovun.sample embeds consolidated resampling techniques 
               # to perform such a task and considers different sampling schemes. It is endowed with the argument 
               # method, which takes one value among "over","under" and "both".

# Load data
load("../Data/CleanData/CleanClass2007to2014_3.Rdata")

# Drop testing data (Year 2014)
CleanClass2007to2013_3 <- CleanClass2007to2014_3[CleanClass2007to2014_3$Year != 2014,]

# Class distribution
table(CleanClass2007to2013_3$Drafted)

# 1. Oversampling ------

# We randomly duplicate samples from the class with fewer instances, here class 1 (drafted), so as to match the number of samples in each class. 
# While we avoid loosing information with this approach, we also run the risk of overfitting our model as we are more likely to get 
# the same samples in the training data. This could lead to an overestimation of our model’s performance and generalizability.


# In ROSE: Option "over" determines oversampling with replacement from the minority class, here class 1 (drafted), until 
# the specified sample size N is reached. Since the prevalent class, here class 0, amounts to 2011 observations, to obtain a balanced 
# sample by oversampling, we need to set the new sample size to 4022 (=2*2011). (Alternatively, we may design the oversampling 
# by setting argument p, which represents the probability of the positive class in the new augmented sample. In this case, 
# the proportion of positive examples will be only approximatively equal to the specified p.)

CleanClass2007to2013_3_oversampling <- ovun.sample(Drafted~., data=CleanClass2007to2013_3, method="over",N=4022)
CleanClass2007to2013_3_oversampling <- as.data.frame(CleanClass2007to2013_3_oversampling$data) 

table(CleanClass2007to2013_3_oversampling$Drafted)

save(CleanClass2007to2013_3_oversampling, file="../Data/CleanData/CleanClass2007to2013_3_oversampling.Rdata")

# 2. Undersampling ------

# We randomly select a subset of samples from the class with more instances, here class 0, to match the number of samples coming from 
# each class. In our context, we randomly pick 327 out of the 2011 not drafted cases. The main disadvantage of undersampling is that 
# we loose potentially relevant information from the left-out samples.

# In ROSE: Option "under" determines simple undersampling without replacement of the majority class, here class 0, until the specified
# sample size N is reached. Since the minority class, here class 1, amounts to 327 observations, to obtain a balanced sample by undersampling, 
# we need to set the new sample size to 654 (=2*327). (Alternatively, we may design the undersampling by setting argument p, see explenation above).

CleanClass2007to2013_3_undersampling <- ovun.sample(Drafted~., data=CleanClass2007to2013_3, method="under" ,N=654)
CleanClass2007to2013_3_undersampling <- as.data.frame(CleanClass2007to2013_3_undersampling$data) 

table(CleanClass2007to2013_3_undersampling$Drafted)

save(CleanClass2007to2013_3_undersampling, file="../Data/CleanData/CleanClass2007to2013_3_undersampling.Rdata")

# 3. Both; ROSE  ------

# When option "both" is selected, both the minority class, here class 1, is oversampled with replacement and the majority class, here class 0, is 
# undersampled (without replacement). In this case, both the arguments N and p have to be set to establish the amount of oversampling and undersampling. 
# Essentially, the minority class is oversampled to reach a size determined as a realization of a binomial random variable with size N and probability p. 
# Undersampling is then performed accordingly, to abide by the specified N.

CleanClass2007to2013_3_Rose.both <- ovun.sample(Drafted~., data=CleanClass2007to2013_3, method="both", 
                                                p=0.5,      # probability of the minority class, by default 0.5.
                                                seed=6969,  # specify random seed
                                                N=2338)     # total specified sampled according to initial sample size of our train data

CleanClass2007to2013_3_Rose.both <- as.data.frame(CleanClass2007to2013_3_Rose.both$data) 

table(CleanClass2007to2013_3_Rose.both$Drafted)

save(CleanClass2007to2013_3_Rose.both, file="../Data/CleanData/CleanClass2007to2013_3_Rose.both.Rdata")


