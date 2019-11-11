library(tidyverse)
library(dbplyr)
library(dplyr)


# Data for the NFL-Draft in 2013
# Since the NFL-Draft always takes place in spring and CFB in fall, we have to match the
# season data of the previous year. In other words: A player which is drafted in 2013 has 
# played his last CFB season in 2012.

GameSummary2012_0 <- read.csv("../Data/cfbstats-com-2012-1-5-4/player-game-statistics.csv")
PlayerList2012 <- read.csv("../Data/cfbstats-com-2012-1-5-4/player.csv")
Combine2013 <- read.csv("../Data/NFLcombine/2013Offense.csv")
Drafts <- read.csv("../Data/DraftedQBRBWR05_19.txt")

GameSummary2011_0 <- read.csv("../Data/cfbstats-com-2011-1-5-0/player-game-statistics.csv")
PlayerList2011 <- read.csv("../Data/cfbstats-com-2011-1-5-0/player.csv")

# Create a function, that returns directly the cleaned data for a draft class

getCleanClass <- function(draftyear, Drafts, Combine2013, GameSummary2012_0, PlayerList2012, GameSummary2011_0, PlayerList2011){
  
  
  # Remove the columns that are only relevant for defensive players, Kickers and Punters
  # (since we only analize Quarterbacks, Runningbacks and Wide Receivers), such as Point
  # (since they are just a linear combination of 6*Touchdowns + 2*2PtConversion)
  GameSummary2012_1 <- GameSummary2012_0 %>%
    select(-c(Game.Code,Tackle.Solo, Tackle.Assist, Tackle.For.Loss, Tackle.For.Loss.Yard,
              Kick.Punt.Blocked, Pass.Broken.Up, Fumble.Forced, QB.Hurry, Sack, Sack.Yard,
              Kickoff, Kickoff.Onside, Kickoff.Out.Of.Bounds, Kickoff.Touchback, Kickoff.Yard,
              Punt, Punt.Yard, Points, Def.2XP.Att, Def.2XP.Made, Off.XP.Kick.Made, Off.XP.Kick.Att,
              Field.Goal.Att, Field.Goal.Made, Misc.Ret, Misc.Ret.Yard, Misc.Ret.TD, Int.Ret, Int.Ret.Yard,
              Int.Ret.TD, Fum.Ret, Fum.Ret.Yard, Fum.Ret.TD))
  
  # Remove all the rows that just contain 0's and add a col for the number of games played
  GameSummary2012_1 = GameSummary2012_1[apply(GameSummary2012_1[,2:ncol(GameSummary2012_1)], 1, function(x) {!all(x==0)}),]
  
  # Group by players and find out how many games they played in that season
  GameNmb <- GameSummary2012_1 %>%
    select(., Player.Code) %>%
    mutate(Games.Played = 1) %>%
    group_by(., Player.Code) %>%
    summarise_all(., sum)
  
  # Group by players and summarize by the mean, which gives us the average performance of the players
  # in the games in which they played in 2012
  GameSummary2012_2 <- GameSummary2012_1 %>%
    group_by(., Player.Code) %>%
    summarise_all(., mean)
  
  GameSummary2012_Grp = merge(x = GameSummary2012_2, y = GameNmb, by = "Player.Code", all.x = TRUE)
  
  # Dismiss uninteresting columns about the players
  PlayerList2012_1 = PlayerList2012 %>%
    select(-c(Uniform.Number, Home.Town, Home.State, Home.Country, Last.School))
  
  # Match some Information about the players and dismiss the rows with name "Team Team" and filter
  # Class in order to only keep players in their junior (3rd) or senior (4th) year of college
  # since other players are not yet eligable in the Draft
  Data2012 <- as.data.frame(merge(y = GameSummary2012_Grp, x = PlayerList2012_1, by = "Player.Code", all.y = TRUE))
  Data2012_1 = Data2012 %>%
    filter(., First.Name != "Team") %>%
    filter(., Position %in% c("QB", "RB", "WR")) %>%
    mutate(Name= paste(First.Name, Last.Name, sep=" ")) %>%
    filter(., Class %in% c("SR", "JR"))
  
  # Filter the NFL-Combine data by the target positions
  Combine2013_1 <- Combine2013 %>%
    filter(Pos %in% c("QB", "RB", "WR"))
  
  # Prepare the Draft-Data for the match with the combine data, the "Drafted" column will be the Y!
  Drafted2013 = Drafts %>%
    mutate(Name = substr(as.character(Player), start = 1, stop = nchar(as.character(Player))-9)) %>%
    filter(Drafts$Year==draftyear) %>%
    mutate(Drafted = 1)
  
  DraftCombine2013 = merge(x = Combine2013_1, y = Drafted2013, by = "Player", all.x = TRUE, all.y = TRUE)
  for(i in 1:length(DraftCombine2013$Name)){
    DraftCombine2013$Name[i] = ifelse(is.na(DraftCombine2013$Name[i]), substr(as.character(DraftCombine2013$Player[i]), start = 1, stop = nchar(as.character(DraftCombine2013$Player[i]))-9), DraftCombine2013$Name[i])
    DraftCombine2013$Drafted[i] = ifelse(is.na(DraftCombine2013$Drafted[i]), 0, 1)
  }
  
  # Drop all rows that are not needed
  DraftCombine2013_1 = DraftCombine2013 %>%
    select(c("Name", "X40YD", "Vertical", "Broad.Jump", "BenchReps", "Shuttle", "Drafted"))
  
  # Match the Draft, Combine and -1 Year data
  DraftCombine1Season2013 <- merge(x = DraftCombine2013_1, y = Data2012_1, by = "Name", all.y = TRUE, all.x = TRUE)
  
  
  
  # Prepare the data of the season 2 years before the actual draft (same code as before)
  GameSummary2011_1 <- GameSummary2011_0 %>%
    select(-c(Game.Code,Tackle.Solo, Tackle.Assist, Tackle.For.Loss, Tackle.For.Loss.Yard,
              Kick.Punt.Blocked, Pass.Broken.Up, Fumble.Forced, QB.Hurry, Sack, Sack.Yard,
              Kickoff, Kickoff.Onside, Kickoff.Out.Of.Bounds, Kickoff.Touchback, Kickoff.Yard,
              Punt, Punt.Yard, Points, Def.2XP.Att, Def.2XP.Made, Off.XP.Kick.Made, Off.XP.Kick.Att,
              Field.Goal.Att, Field.Goal.Made, Misc.Ret, Misc.Ret.Yard, Misc.Ret.TD, Int.Ret, Int.Ret.Yard,
              Int.Ret.TD, Fum.Ret, Fum.Ret.Yard, Fum.Ret.TD))
  
  # Remove all the rows that just contain 0's and add a col for the number of games played
  GameSummary2011_1 = GameSummary2011_1[apply(GameSummary2011_1[,2:ncol(GameSummary2011_1)], 1, function(x) {!all(x==0)}),]
  
  # Group by players and find out how many games they played in that season
  GameNmb <- GameSummary2011_1 %>%
    select(., Player.Code) %>%
    mutate(Games.Played = 1) %>%
    group_by(., Player.Code) %>%
    summarise_all(., sum)
  
  # Group by players and summarize by the mean, which gives us the average performance of the players
  # in the games in which they played in 2012
  GameSummary2011_2 <- GameSummary2011_1 %>%
    group_by(., Player.Code) %>%
    summarise_all(., mean)
  
  GameSummary2011_Grp = merge(x = GameSummary2011_2, y = GameNmb, by = "Player.Code", all.x = TRUE)
  
  # Dismiss uninteresting columns about the players
  PlayerList2011_1 = PlayerList2011 %>%
    select(-c(Uniform.Number, Home.Town, Home.State, Home.Country, Last.School))
  
  # Match some Information about the players and dismiss the rows with name "Team Team" and filter
  # Class in order to only keep players in their junior (3rd) or senior (4th) year of college
  # since other players are not yet eligable in the Draft
  Data2011 <- as.data.frame(merge(y = GameSummary2011_Grp, x = PlayerList2011_1, by = "Player.Code", all.y = TRUE))
  Data2011_1 = Data2011 %>%
    filter(., First.Name != "Team") %>%
    filter(., Position %in% c("QB", "RB", "WR")) %>%
    mutate(Name= paste(First.Name, Last.Name, sep=" "))
  
  # Match the Draft, Combine, -1 Year and -2 Year data
  Class2013_0 <- merge(x = DraftCombine1Season2013, y = Data2011_1, by = "Player.Code", all.x = TRUE)
  
  # Fill general player information with -2 Year data if missing in -1
  Class2013_1 = Class2013_0
  for(i in 1:nrow(Class2013_1)){
    Class2013_1$Player.Code.x[i] = ifelse(is.na(Class2013_1$Player.Code.x[i]), Class2013_1$Player.Code.y[i], Class2013_1$Player.Code.x[i])
    Class2013_1$Team.Code.x[i] = ifelse(is.na(Class2013_1$Team.Code.x[i]), Class2013_1$Team.Code.y[i], Class2013_1$Team.Code.x[i])
    Class2013_1$Last.Name.x[i] = (if(is.na(Class2013_1$Last.Name.x[i])){Class2013_1$Last.Name.y[i]} else{Class2013_1$Last.Name.x[i]})
    Class2013_1$First.Name.x[i] = (if(is.na(Class2013_1$First.Name.x[i])){Class2013_1$First.Name.y[i]} else{Class2013_1$First.Name.x[i]})
    Class2013_1$Class.x[i] = (if(is.na(Class2013_1$Class.x[i])){Class2013_1$Class.y[i]} else{Class2013_1$Class.x[i]})
    Class2013_1$Position.x[i] = (if(is.na(Class2013_1$Position.x[i])){Class2013_1$Position.y[i]} else{Class2013_1$Position.x[i]})
    Class2013_1$Height.x[i] = ifelse(is.na(Class2013_1$Height.x[i]), Class2013_1$Height.y[i], Class2013_1$Height.x[i])
    Class2013_1$Weight.x[i] = ifelse(is.na(Class2013_1$Weight.x[i]), Class2013_1$Weight.y[i], Class2013_1$Weight.x[i])
    Class2013_1$Drafted[i] = ifelse(is.na(Class2013_1$Drafted[i]), 0, Class2013_1$Drafted[i])
  }
  
  # remove personal information that now is available twice
  Class2013clean = Class2013_1 %>%
    mutate(Year = draftyear) %>%
    select(-c(Last.Name.x, First.Name.x, Team.Code.y, Last.Name.y, First.Name.y, Class.y, Position.y, Height.y, Weight.y, Name.y)) %>%
    select(Player.Code, Name.x, Team.Code.x, Class.x, Position.x, Year, Drafted, everything())
  
  # Remove the Players that couln't be matched with any season data
  Class2013clean1 = Class2013clean[apply(Class2013clean[,15:ncol(Class2013clean)], 1, function(x) {!all(is.na(x))}),]
  
  # Remove dublicated players (or players with the same name that could not be matched)
  CleanClassYear = Class2013clean1[!(duplicated(Class2013clean1$Name.x)),]
  
  return(CleanClassYear)
}

save(getCleanClass, file = "getCleanClass.R")
