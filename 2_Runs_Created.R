# slugging% = TotalBases(TB)/AtBats(AB) where
# TB = (Singles) + (2XDoubles(2B)) + (3xTriples(3B)) + (4xHomeRuns(HR))

# Ichiro 2004 vs Nomar 1997
library("Lahman")
library(tidyverse)
library(gt)
data(Batting)
my_stats = Batting[
  (Batting$playerID == "suzukic01" & Batting$yearID == 2004) |
    (Batting$playerID == "garcino01" & Batting$yearID == 1997) |
    (Batting$playerID == "bondsba01" & Batting$yearID == 2004), ]
#Number of Singles
my_stats$Singles = my_stats$H - (my_stats$X2B + my_stats$X3B + my_stats$HR) 
#Total Bases
my_stats$TB = (my_stats$Single)+(my_stats$X2B * 2)+(my_stats$X3B * 3)+(my_stats$HR*4)
#Slugging Percentage
my_stats$SLG = my_stats$TB / my_stats$AB
#Walks + Hit by Pitch
my_stats$BB_HBP = my_stats$BB + my_stats$HBP
#Batting Average
my_stats$BA = my_stats$H/my_stats$AB                                        
#Game Outs Used
my_stats$Outs = ((.982*my_stats$AB) - (my_stats$H + my_stats$GIDP + my_stats$SF + my_stats$SH + my_stats$CS))
#Game outs used per Game
my_stats$OutsPerGame = ((.982*my_stats$AB) - (my_stats$H + my_stats$GIDP + my_stats$SF + my_stats$SH + my_stats$CS)) / 26.72
#Runs Created
my_stats$RunsCreated = ((my_stats$H + my_stats$BB + my_stats$HBP)*my_stats$TB)/(my_stats$AB + my_stats$BB + my_stats$HBP)
#Runs Created per Game
my_stats$RCperGame = my_stats$RunsCreated/my_stats$OutsPerGame

#Pretty it Up
final = my_stats %>% select(AB, BA, SLG, H, Singles ,X2B, X3B, HR, BB_HBP, RunsCreated, Outs, RCperGame)
final
t(final)                                              #transpose table
tibble <- as_tibble(t(final), rownames="rownames")    #includes rownames
                                                      #rename columns to player names + year
tibble <- rename(tibble, `Nomar1997`=`75793`, `Bonds2004`=`84778`, `Ichiro2004`=`85831`)
tibble %>% gt() %>% fmt_number(columns = 2:4, drop_trailing_zeros=TRUE)  #must be tibble to use gt

#Team batting 2000
team_batting = Batting[(Batting$yearID == 2000),]
team_batting %>% mutate(Singles = H-(X2B+X3B+HR), 
                        Walks = BB+HBP,
                        TB = Singles+(2*X2B)+(3*X3B)+(4*HR)) %>%
  group_by(teamID) %>%
  summarize(Runs=sum(R), TotalBases= sum(TB), AtBats=sum(AB), Hits=sum(H), 
            Singles=sum(Singles), Doubles=sum(X2B),
            Triples=sum(X3B), Homeruns=sum(HR), Walks=sum(Walks)) %>% 
  mutate(RC = (((Hits+Walks)*TotalBases)/(AtBats+Walks)),
         RC_Error = (abs((RC-Runs)/Runs)*100)) %>%
  summarize(teamID, Runs, TotalBases, AtBats, Hits, Singles,
            Doubles, Triples, Homeruns, Walks, RunsCreated=RC, PercentError=RC_Error) %>%
  gt %>%
  tab_header(title=md("Team Batting data for 2000"))


