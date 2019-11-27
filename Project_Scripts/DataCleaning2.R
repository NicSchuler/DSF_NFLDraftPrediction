library(tidyverse)
library(dbplyr)
library(dplyr)

# Load the function to clean the data for the different draft years
# the function is coded in "functionGetCleanClass.R"
load("getCleanClass2.R")

# 2014 Draft---------------------------------
# Load the different raw data sets
GameSummary2013_0 <- read.csv("../Data/cfbstats-com-2013-1-5-20/player-game-statistics.csv")
PlayerList2013 <- read.csv("../Data/cfbstats-com-2013-1-5-20/player.csv")
Drafts <- read.csv("../Data/DraftedQBRBWR05_19.txt")

GameSummary2012_0 <- read.csv("../Data/cfbstats-com-2012-1-5-4/player-game-statistics.csv")
PlayerList2012 <- read.csv("../Data/cfbstats-com-2012-1-5-4/player.csv")

# Compute the clean data set with the function
Class2014_2 = getCleanClass2(2014, Drafts, GameSummary2013_0, PlayerList2013, GameSummary2012_0, PlayerList2012)

# Save the Dataset separately
save(Class2014_2, file="../Data/CleanData/CleanClass2014_2.Rdata")



# 2013 Draft---------------------------------
# Load the different raw data sets
GameSummary2012_0 <- read.csv("../Data/cfbstats-com-2012-1-5-4/player-game-statistics.csv")
PlayerList2012 <- read.csv("../Data/cfbstats-com-2012-1-5-4/player.csv")
Drafts <- read.csv("../Data/DraftedQBRBWR05_19.txt")

GameSummary2011_0 <- read.csv("../Data/cfbstats-com-2011-1-5-0/player-game-statistics.csv")
PlayerList2011 <- read.csv("../Data/cfbstats-com-2011-1-5-0/player.csv")

# Compute the clean data set with the function
Class2013_2 = getCleanClass2(2013, Drafts, GameSummary2012_0, PlayerList2012, GameSummary2011_0, PlayerList2011)

# Save the Dataset separately
save(Class2013_2, file="../Data/CleanData/CleanClass2013_2.Rdata")



# 2012 Draft---------------------------------
# Load the different raw data sets
GameSummary2011_0 <- read.csv("../Data/cfbstats-com-2011-1-5-0/player-game-statistics.csv")
PlayerList2011 <- read.csv("../Data/cfbstats-com-2011-1-5-0/player.csv")
Drafts <- read.csv("../Data/DraftedQBRBWR05_19.txt")

GameSummary2010_0 <- read.csv("../Data/cfbstats-com-2010-1-5-0/player-game-statistics.csv")
PlayerList2010 <- read.csv("../Data/cfbstats-com-2010-1-5-0/player.csv")

# Compute the clean data set with the function
Class2012_2 = getCleanClass2(2012, Drafts, GameSummary2011_0, PlayerList2011, GameSummary2010_0, PlayerList2010)

# Save the Dataset separately
save(Class2012_2, file="../Data/CleanData/CleanClass2012_2.Rdata")



# 2011 Draft---------------------------------
# Load the different raw data sets
GameSummary2010_0 <- read.csv("../Data/cfbstats-com-2010-1-5-0/player-game-statistics.csv")
PlayerList2010 <- read.csv("../Data/cfbstats-com-2010-1-5-0/player.csv")
Drafts <- read.csv("../Data/DraftedQBRBWR05_19.txt")

GameSummary2009_0 <- read.csv("../Data/cfbstats-com-2009-1-5-0/player-game-statistics.csv")
PlayerList2009 <- read.csv("../Data/cfbstats-com-2009-1-5-0/player.csv")

# Compute the clean data set with the function
Class2011_2 = getCleanClass2(2011, Drafts, GameSummary2010_0, PlayerList2010, GameSummary2009_0, PlayerList2009)

# Save the Dataset separately
save(Class2011_2, file="../Data/CleanData/CleanClass2011_2.Rdata")


# 2010 Draft---------------------------------
# Load the different raw data sets
GameSummary2009_0 <- read.csv("../Data/cfbstats-com-2009-1-5-0/player-game-statistics.csv")
PlayerList2009 <- read.csv("../Data/cfbstats-com-2009-1-5-0/player.csv")
Drafts <- read.csv("../Data/DraftedQBRBWR05_19.txt")

GameSummary2008_0 <- read.csv("../Data/cfbstats-com-2008-1-5-0/player-game-statistics.csv")
PlayerList2008 <- read.csv("../Data/cfbstats-com-2008-1-5-0/player.csv")

# Compute the clean data set with the function
Class2010_2 = getCleanClass2(2010, Drafts, GameSummary2009_0, PlayerList2009, GameSummary2008_0, PlayerList2008)

# Save the Dataset separately
save(Class2010_2, file="../Data/CleanData/CleanClass2010_2.Rdata")



# 2009 Draft---------------------------------
# Load the different raw data sets
GameSummary2008_0 <- read.csv("../Data/cfbstats-com-2008-1-5-0/player-game-statistics.csv")
PlayerList2008 <- read.csv("../Data/cfbstats-com-2008-1-5-0/player.csv")
Drafts <- read.csv("../Data/DraftedQBRBWR05_19.txt")

GameSummary2007_0 <- read.csv("../Data/cfbstats-com-2007-1-5-0/player-game-statistics.csv")
PlayerList2007 <- read.csv("../Data/cfbstats-com-2007-1-5-0/player.csv")

# Compute the clean data set with the function
Class2009_2 = getCleanClass2(2009, Drafts, GameSummary2008_0, PlayerList2008, GameSummary2007_0, PlayerList2007)

# Save the Dataset separately
save(Class2009_2, file="../Data/CleanData/CleanClass2009_2.Rdata")



# 2008 Draft---------------------------------
# Load the different raw data sets
GameSummary2007_0 <- read.csv("../Data/cfbstats-com-2007-1-5-0/player-game-statistics.csv")
PlayerList2007 <- read.csv("../Data/cfbstats-com-2007-1-5-0/player.csv")
Drafts <- read.csv("../Data/DraftedQBRBWR05_19.txt")

GameSummary2006_0 <- read.csv("../Data/cfbstats-com-2006-1-5-0/player-game-statistics.csv")
PlayerList2006 <- read.csv("../Data/cfbstats-com-2006-1-5-0/player.csv")

# Compute the clean data set with the function
Class2008_2 = getCleanClass2(2008, Drafts, GameSummary2007_0, PlayerList2007, GameSummary2006_0, PlayerList2006)

# Save the Dataset separately
save(Class2008_2, file="../Data/CleanData/CleanClass2008_2.Rdata")



# 2007 Draft---------------------------------
# Load the different raw data sets
GameSummary2006_0 <- read.csv("../Data/cfbstats-com-2006-1-5-0/player-game-statistics.csv")
PlayerList2006 <- read.csv("../Data/cfbstats-com-2006-1-5-0/player.csv")
Drafts <- read.csv("../Data/DraftedQBRBWR05_19.txt")

GameSummary2005_0 <- read.csv("../Data/cfbstats-com-2005-1-5-0/player-game-statistics.csv")
PlayerList2005 <- read.csv("../Data/cfbstats-com-2005-1-5-0/player.csv")

# Compute the clean data set with the function
Class2007_2 = getCleanClass2(2007, Drafts, GameSummary2006_0, PlayerList2006, GameSummary2005_0, PlayerList2005)

# Save the Dataset separately
save(Class2007_2, file="../Data/CleanData/CleanClass2007_2.Rdata")


# Putting the pieces together---------------------------------
load("../Data/CleanData/CleanClass2014_2.Rdata")
load("../Data/CleanData/CleanClass2013_2.Rdata")
load("../Data/CleanData/CleanClass2012_2.Rdata")
load("../Data/CleanData/CleanClass2011_2.Rdata")
load("../Data/CleanData/CleanClass2010_2.Rdata")
load("../Data/CleanData/CleanClass2009_2.Rdata")
load("../Data/CleanData/CleanClass2008_2.Rdata")
load("../Data/CleanData/CleanClass2007_2.Rdata")

CleanClass07to14_0_2 = rbind(Class2014_2,Class2013_2,Class2012_2,Class2011_2,Class2010_2,Class2009_2,Class2008_2,Class2007_2)

# Remove duplicates (this is still necessary, because some players are only picked in the Draft in their
# second year that they are eligable). Thanks to the descending order of the years the irrelevant older
# years are removed.
CleanClass2007to2014_2 = CleanClass07to14_0_2[!(duplicated(CleanClass07to14_0_2$Player.Code)),]

# Update the levels for the categorical variables, since the old layers like "FR" and "SO" we filtered out
# still are still stored
CleanClass2007to2014_2$Class = factor(CleanClass2007to2014_2$Class)
CleanClass2007to2014_2$Position = factor(CleanClass2007to2014_2$Position)


save(CleanClass2007to2014_2, file="../Data/CleanData/CleanClass2007to2014_2.Rdata")


# Looking at the data-------------
# Lets have a look at the data by plotting histograms of Games.Played over all Players and only the Drafted ones
hist(CleanClass2007to2014_2$Games.Played)
hist(CleanClass2007to2014_2$Games.Played[CleanClass2007to2014_2$Drafted==1])

# We can already see, that there is a huge amount of Players with less than 10 games played
# that, a priori don't have a big chance of being drafted
# Now we look at the same problem but in a quantitative way

# Percentage of Drafted Players in the whole dataset
sum(CleanClass2007to2014_2$Drafted)/length(CleanClass2007to2014_2$Drafted)
# Amount of Drafted Players with less than 10 games
sum(CleanClass2007to2014_2$Drafted[CleanClass2007to2014_2$Games.Played<10])

# And the Undrafted players
# Amount of Undrafted Players with less than 10 games
(length(CleanClass2007to2014_2$Drafted[CleanClass2007to2014_2$Games.Played<10])-sum(CleanClass2007to2014_2$Drafted[CleanClass2007to2014_2$Games.Played<10]))

# Percentage of Drafted Players if we cut away all players with less than 10 games
sum(CleanClass2007to2014_2$Drafted[CleanClass2007to2014_2$Games.Played>=10])/length(CleanClass2007to2014_2$Drafted[CleanClass2007to2014_2$Games.Played>=10])

# By filtering out all players with less than 10 games, we can increase the percentage of drafted
# Players from 6.2% up to 12.4%, which is better for machine learning (according to various sources)

# Prepare a Plot to see the Percentage depending on the cutoff point
Cutoff = seq(1:28)
PlotTibble = as.tibble(Cutoff) %>%
  mutate(Cutoff=value) %>%
  select(-value) %>%
  mutate(DataLength=NA) %>%
  mutate(DraftLength=NA) %>%
  mutate(DraftPerc=NA)

for(i in PlotTibble$Cutoff){
  PlotTibble$DataLength[i]=length(CleanClass2007to2014_2$Drafted[CleanClass2007to2014_2$Games.Played >= i])
  PlotTibble$DraftLength[i]=sum(CleanClass2007to2014_2$Drafted[CleanClass2007to2014_2$Games.Played >= i])
  PlotTibble$DraftPerc[i]=(PlotTibble$DraftLength[i])/(PlotTibble$DataLength[i])
}

# Optimizing the data
CleanClass2007to2014_3 = CleanClass2007to2014_2 %>%
  filter(Games.Played >= 10)
save(CleanClass2007to2014_3, file="../Data/CleanData/CleanClass2007to2014_3.Rdata")
