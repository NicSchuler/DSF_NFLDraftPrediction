 Explanation of the data:
 
 All the columns can be devided in 5 parts:
 
 ############################################
 --Part ONE: Information about the Players:--
 ############################################
  
 "Player.Code"
 A unique Number for matching the data
 
 "Name.x"
 Name of the Player
 
 "Team.Code.x"
 Code for the college team the player played at. (could be matched with team.csv)
 
 "Class.x"
 Shows the college year the player was in when being in draft class (sometimes wrong)
 FR=Fresher (1.year), SO=Sophomore (2.year), JR=Junior (3.year), SR=Senior (4.year)
 
 "Position.x"
 Position of the Player (filtered for only QB=Quarterback, RB=Runningback, WR=Wide Receiver)
 
 "Year"
 Shows the year the player was in the draft class
 
 
 ###################
 --Part TWO: The Y--
 ###################
 
 "Drafted"
 Is 1 when a player was drafted and 0 when a player was not drafted
 
 
 
 ###################################
 --Part THREE: The Combine Results--
 ###################################
 
 "X40YD"
 Time for 40yds dash (in [s])
 
 "Vertical"
 High jump (in [inch])
 
 "Broad.Jump"
 horizontal jump (in [inch])
 
 "BenchReps"
 Reps in bench press
 
 "Shuttle"
 Sprints
 
 "Height.x"
 Height (in [inch])
 
 "Weight.x"
 Weight (in [pounds])
 
 
 ######################################################
 --Part Four: The Results one season before the draft--
 ######################################################
 
 --> .x at the end of the Col-name is always the season before the draft! <--
 --> all the values are the average performance per game for that season <--
 
 "Rush.Att.x"
 Average rushing attempts per Game (mainly for RB)
 
 "Rush.Yard.x"
 Average rushing yards per Game (mainly for RB)
 
 "Rush.TD.x"
 Average rushing TD per Game (mainly for RB)
 
 "Pass.Att.x"
 Average passing attempts per Game (mainly for QB)
 
 "Pass.Comp.x"
 Average passing completions per Game (mainly for QB)
 
 "Pass.Yard.x"       
 Average passing yards per Game (mainly for QB)
 
 "Pass.TD.x"
 Average passing TD per Game (mainly for QB)
 
 "Pass.Int.x"
 Average Inteceptions thrown per Game (mainly for QB)
 
 "Pass.Conv.x"
 
 "Rec.x"
 Average receptions per Game (mainly for WR)
 
 "Rec.Yards.x"
 Average reception yards per Game (mainly for WR)
 
 "Rec.TD.x" 
 Average reception TD per Game (mainly for WR)
 
 "Kickoff.Ret.x"
 Average Kickoff returns per Game (mainly for WR/RB (but not so important))
 
 "Kickoff.Ret.Yard.x"
 Average Kickoff return yards per Game (mainly for WR/RB (but not so important))
 
 "Kickoff.Ret.TD.x"
 Average Kickoff return TD per Game (mainly for WR/RB (but not so important))
 
 "Punt.Ret.x"
 Average punt returns per Game (mainly for WR/RB (but not so important))
 
 "Punt.Ret.Yard.x"
 Average punt return yards per Game (mainly for WR/RB (but not so important))
 
 "Punt.Ret.TD.x"
 Average punt return TD per Game (mainly for WR/RB (but not so important))
 
 "Off.2XP.Att.x"
 Average 2 point conversion attempts per Game
 
 "Off.2XP.Made.x"
 Average 2 point conversions made per Game
 
 "Safety.x"
 Being tackeled in the own end zone (=2 pt for opponent)
 
 "Fumble.x"
 Dropped balls (total)
 
 "Fumble.Lost.x"
 Dropped balls recovered by opponent
 
 "Games.Played.x"
 Number of games played in that season


 #######################################################
 --Part Five: The Results two seasons before the draft--
 #######################################################
 
 --> .y at the end of the Col-name is always 2 seasons before the draft! <--
 exactly the same as Part FOUR (just one year earlier)

 "Rush.Att.y"

 "Rush.Yard.y"       

 "Rush.TD.y"
 
 "Pass.Att.y"
 
 "Pass.Comp.y"
 
 "Pass.Yard.y"       
 
 "Pass.TD.y"
 
 "Pass.Int.y"
 
 "Pass.Conv.y"
 
 "Rec.y"
 
 "Rec.Yards.y"
 
 "Rec.TD.y"
 
 "Kickoff.Ret.y"
 
 "Kickoff.Ret.Yard.y"
 
 "Kickoff.Ret.TD.y"
 
 "Punt.Ret.y"
 
 "Punt.Ret.Yard.y"
 
 "Punt.Ret.TD.y"     

 "Off.2XP.Att.y"
 
 "Off.2XP.Made.y"
 
 "Safety.y"
 
 "Fumble.y"          
 
 "Fumble.Lost.y"
 
 "Games.Played.y" 