library(tidyverse)
library(smotefamily)

load("../Data/CleanData/CleanClass2007to2014_3.RData")
cleanData <- as_tibble(CleanClass2007to2014_3) %>% drop_na(.)

# SMOTE sampling ----
# Synthetic minority over-sampling technique (SMOTE) combats class imbalance in datasets used for machine learning
# by artificially inflating the smaller class. In order to achieve this, synthetic values are created for the
# minority class by placing them between existing values. This script uses the SMOTE() function from the "smotefamily"
# package which takes the parameters K = number of nearest neighbors taken into account during sampling; and dup_size =
# target factor for number of synthetic values of the minority class over the original number of majority instances (e.g.
# if dup_size = 2 and the original imbalance was 1/10 minority to majority instances, the resulting imbalance will be
# 2/10). To achieve a roughly balanced sample, the inverse of the original imbalance is passed as dup_size in this script.

# Preparing for the loops
cleanData_smote <- rep(NA, 29)
posit <- c("QB", "RB", "WR")
for (pos in posit) {
  for (j in 1:7) {
    # Drop variables that will not be replicated
    cleanData_var <- cleanData %>% filter(., Year == 2006 + j, Position == pos) %>% drop_na(.) %>%
      mutate(., "y" = as.factor(Drafted)) %>% select(., -Player.Code, -Name, -Class, -Position, -Year, -Drafted)
    
    # Perform SMOTE sampling on training data with the target of equally sized classes
    set.seed(6969)
    D_smote <- SMOTE(X = select(cleanData_var, -y),
                             target = cleanData_var$y,
                             K = 5, dup_size = length(which(cleanData_var$y == 0))/length(which(cleanData_var$y == 1)))
    
    # Add the player names, codes, positions and years back to the non-synthetic values in the dataset
    orig_all <- cleanData %>% filter(., Year == 2006 + j, Position == pos) %>% arrange(., desc(Drafted)) %>% select(., Player.Code, Name, Position, Year, Drafted)
    orig_N <- D_smote$orig_N %>% mutate(., "Player.Code" = filter(orig_all, orig_all$Drafted == 0)$Player.Code,
                                                "Name" = filter(orig_all, orig_all$Drafted == 0)$Name,
                                                "Position" = filter(orig_all, orig_all$Drafted == 0)$Position,
                                                "Year" = filter(orig_all, orig_all$Drafted == 0)$Year,
                                                "Drafted" = class) %>%
      select(., -class)
    
    orig_P <- D_smote$orig_P %>% mutate(., "Player.Code" = filter(orig_all, orig_all$Drafted == 1)$Player.Code,
                                                "Name" = filter(orig_all, orig_all$Drafted == 1)$Name,
                                                "Position" = filter(orig_all, orig_all$Drafted == 1)$Position,
                                                "Year" = filter(orig_all, orig_all$Drafted == 1)$Year,
                                                "Drafted" = class) %>%
      select(., -class)
    
    # For the synthetic data, add "syn" as player code and name
    syn_data <- D_smote$syn_data %>% mutate(., "Player.Code" = "syn",
                                                    "Name" = "syn",
                                                    "Position" = pos,
                                                    "Year" = 2006 + j,
                                                    "Drafted" = class) %>%
      select(., -class)
    
    # Combine the data
    cleanData_smote <- rbind(cleanData_smote, orig_N, orig_P, syn_data)
  }
}

# Remove the first row (which is NA) from the data
cleanData_smote <- cleanData_smote[-1,]

# Rearrange the data
cleanData_smote <- cleanData_smote %>% select(., Player.Code, Name, Position, Year, Drafted, everything()) %>%
  arrange(., desc(Year))

# Save the SMOTE sampled data as a new dataset
save(cleanData_smote, file="../Data/CleanData/CleanClass2007to2013_3_smote.Rdata")

