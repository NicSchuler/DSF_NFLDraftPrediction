library(tidyverse)
library(dbplyr)
library(dplyr)

# Load the function to clean the data for the different draft years
# the function is coded in "functionGetCleanClass.R"
load("getCleanClass.R")

# 2014 Draft---------------------------------
# Load the different raw data sets
GameSummary2013_0 <- read.csv("../Data/cfbstats-com-2013-1-5-20/player-game-statistics.csv")
PlayerList2013 <- read.csv("../Data/cfbstats-com-2013-1-5-20/player.csv")
Combine2014 <- read.csv("../Data/NFLcombine/2014Offense.csv")
Drafts <- read.csv("../Data/DraftedQBRBWR05_19.txt")

GameSummary2012_0 <- read.csv("../Data/cfbstats-com-2012-1-5-4/player-game-statistics.csv")
PlayerList2012 <- read.csv("../Data/cfbstats-com-2012-1-5-4/player.csv")

# Compute the clean data set with the function
Class2014 = getCleanClass(2014, Drafts, Combine2014, GameSummary2013_0, PlayerList2013, GameSummary2012_0, PlayerList2012)

# Save the Dataset separately
save(Class2014, file="../Data/CleanData/CleanClass2014.Rdata")



# 2013 Draft---------------------------------
# Load the different raw data sets
GameSummary2012_0 <- read.csv("../Data/cfbstats-com-2012-1-5-4/player-game-statistics.csv")
PlayerList2012 <- read.csv("../Data/cfbstats-com-2012-1-5-4/player.csv")
Combine2013 <- read.csv("../Data/NFLcombine/2013Offense.csv")
Drafts <- read.csv("../Data/DraftedQBRBWR05_19.txt")

GameSummary2011_0 <- read.csv("../Data/cfbstats-com-2011-1-5-0/player-game-statistics.csv")
PlayerList2011 <- read.csv("../Data/cfbstats-com-2011-1-5-0/player.csv")

# Compute the clean data set with the function
Class2013 = getCleanClass(2013, Drafts, Combine2013, GameSummary2012_0, PlayerList2012, GameSummary2011_0, PlayerList2011)

# Save the Dataset separately
save(Class2013, file="../Data/CleanData/CleanClass2013.Rdata")



# 2012 Draft---------------------------------
# Load the different raw data sets
GameSummary2011_0 <- read.csv("../Data/cfbstats-com-2011-1-5-0/player-game-statistics.csv")
PlayerList2011 <- read.csv("../Data/cfbstats-com-2011-1-5-0/player.csv")
Combine2012 <- read.csv("../Data/NFLcombine/2012Offense.csv")
Drafts <- read.csv("../Data/DraftedQBRBWR05_19.txt")

GameSummary2010_0 <- read.csv("../Data/cfbstats-com-2010-1-5-0/player-game-statistics.csv")
PlayerList2010 <- read.csv("../Data/cfbstats-com-2010-1-5-0/player.csv")

# Compute the clean data set with the function
Class2012 = getCleanClass(2012, Drafts, Combine2012, GameSummary2011_0, PlayerList2011, GameSummary2010_0, PlayerList2010)

# Save the Dataset separately
save(Class2012, file="../Data/CleanData/CleanClass2012.Rdata")



# 2011 Draft---------------------------------
# Load the different raw data sets
GameSummary2010_0 <- read.csv("../Data/cfbstats-com-2010-1-5-0/player-game-statistics.csv")
PlayerList2010 <- read.csv("../Data/cfbstats-com-2010-1-5-0/player.csv")
Combine2011 <- read.csv("../Data/NFLcombine/2011Offense.csv")
Drafts <- read.csv("../Data/DraftedQBRBWR05_19.txt")

GameSummary2009_0 <- read.csv("../Data/cfbstats-com-2009-1-5-0/player-game-statistics.csv")
PlayerList2009 <- read.csv("../Data/cfbstats-com-2009-1-5-0/player.csv")

# Compute the clean data set with the function
Class2011 = getCleanClass(2011, Drafts, Combine2011, GameSummary2010_0, PlayerList2010, GameSummary2009_0, PlayerList2009)

# Save the Dataset separately
save(Class2011, file="../Data/CleanData/CleanClass2011.Rdata")


# 2010 Draft---------------------------------
# Load the different raw data sets
GameSummary2009_0 <- read.csv("../Data/cfbstats-com-2009-1-5-0/player-game-statistics.csv")
PlayerList2009 <- read.csv("../Data/cfbstats-com-2009-1-5-0/player.csv")
Combine2010 <- read.csv("../Data/NFLcombine/2010Offense.csv")
Drafts <- read.csv("../Data/DraftedQBRBWR05_19.txt")

GameSummary2008_0 <- read.csv("../Data/cfbstats-com-2008-1-5-0/player-game-statistics.csv")
PlayerList2008 <- read.csv("../Data/cfbstats-com-2008-1-5-0/player.csv")

# Compute the clean data set with the function
Class2010 = getCleanClass(2010, Drafts, Combine2010, GameSummary2009_0, PlayerList2009, GameSummary2008_0, PlayerList2008)

# Save the Dataset separately
save(Class2010, file="../Data/CleanData/CleanClass2010.Rdata")



# 2009 Draft---------------------------------
# Load the different raw data sets
GameSummary2008_0 <- read.csv("../Data/cfbstats-com-2008-1-5-0/player-game-statistics.csv")
PlayerList2008 <- read.csv("../Data/cfbstats-com-2008-1-5-0/player.csv")
Combine2009 <- read.csv("../Data/NFLcombine/2009Offense.csv")
Drafts <- read.csv("../Data/DraftedQBRBWR05_19.txt")

GameSummary2007_0 <- read.csv("../Data/cfbstats-com-2007-1-5-0/player-game-statistics.csv")
PlayerList2007 <- read.csv("../Data/cfbstats-com-2007-1-5-0/player.csv")

# Compute the clean data set with the function
Class2009 = getCleanClass(2009, Drafts, Combine2009, GameSummary2008_0, PlayerList2008, GameSummary2007_0, PlayerList2007)

# Save the Dataset separately
save(Class2009, file="../Data/CleanData/CleanClass2009.Rdata")



# 2008 Draft---------------------------------
# Load the different raw data sets
GameSummary2007_0 <- read.csv("../Data/cfbstats-com-2007-1-5-0/player-game-statistics.csv")
PlayerList2007 <- read.csv("../Data/cfbstats-com-2007-1-5-0/player.csv")
Combine2008 <- read.csv("../Data/NFLcombine/2008Offense.csv")
Drafts <- read.csv("../Data/DraftedQBRBWR05_19.txt")

GameSummary2006_0 <- read.csv("../Data/cfbstats-com-2006-1-5-0/player-game-statistics.csv")
PlayerList2006 <- read.csv("../Data/cfbstats-com-2006-1-5-0/player.csv")

# Compute the clean data set with the function
Class2008 = getCleanClass(2008, Drafts, Combine2008, GameSummary2007_0, PlayerList2007, GameSummary2006_0, PlayerList2006)

# Save the Dataset separately
save(Class2008, file="../Data/CleanData/CleanClass2008.Rdata")



# 2007 Draft---------------------------------
# Load the different raw data sets
GameSummary2006_0 <- read.csv("../Data/cfbstats-com-2006-1-5-0/player-game-statistics.csv")
PlayerList2006 <- read.csv("../Data/cfbstats-com-2006-1-5-0/player.csv")
Combine2007 <- read.csv("../Data/NFLcombine/2007Offense.csv")
Drafts <- read.csv("../Data/DraftedQBRBWR05_19.txt")

GameSummary2005_0 <- read.csv("../Data/cfbstats-com-2005-1-5-0/player-game-statistics.csv")
PlayerList2005 <- read.csv("../Data/cfbstats-com-2005-1-5-0/player.csv")

# Compute the clean data set with the function
Class2007 = getCleanClass(2007, Drafts, Combine2007, GameSummary2006_0, PlayerList2006, GameSummary2005_0, PlayerList2005)

# Save the Dataset separately
save(Class2007, file="../Data/CleanData/CleanClass2007.Rdata")


# Putting the pieces together---------------------------------
load("../Data/CleanData/CleanClass2014.Rdata")
load("../Data/CleanData/CleanClass2013.Rdata")
load("../Data/CleanData/CleanClass2012.Rdata")
load("../Data/CleanData/CleanClass2011.Rdata")
load("../Data/CleanData/CleanClass2010.Rdata")
load("../Data/CleanData/CleanClass2009.Rdata")
load("../Data/CleanData/CleanClass2008.Rdata")
load("../Data/CleanData/CleanClass2007.Rdata")

CleanClass07to14_0 = rbind(Class2014,Class2013,Class2012,Class2011,Class2010,Class2009,Class2008,Class2007)

# Remove duplicates (this is still necessary, because some players are only picked in the Draft in their
# second year that they are eligable). Thanks to the descending order of the years the irrelevant older
# years are removed.
CleanClass2007to2014 = CleanClass07to14_0[!(duplicated(CleanClass07to14_0$Player.Code)),]
  

save(CleanClass2007to2014, file="../Data/CleanData/CleanClass2007to2014.Rdata")
