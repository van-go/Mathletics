library("Lahman")
library(tidyverse)
library(gt)
data("Batting")
# Part 1 - Linear Regression  to search for the set of weights and constant to give
#          the best forcast for Runs Scored. team_data includes SB and CS, team_data2 does not. 
  #grab the data from 2000-2006
my_data = Batting[(Batting$yearID >= 2000 & Batting$yearID <= 2006),]
  #determine singles and walks
my_data$Singles = my_data$H - (my_data$X2B + my_data$X3B + my_data$HR) 
my_data$Walks = my_data$BB + my_data$HBP
  #select batting info
my_data %>% select(R, Singles, X2B, X3B, HR, Walks,SB, CS)
  #group by team and year
team_data = my_data %>% 
  group_by(teamID, yearID) %>%
  summarise(R=sum(R), Singles=sum(Singles), X2B=sum(X2B), X3B=sum(X3B), 
            HR=sum(HR), Walks=sum(Walks), SB=sum(SB), CS=sum(CS)) 
  #linear model
lin_reg <- lm(team_data$R ~ team_data$Singles + team_data$X2B + team_data$X3B + team_data$HR +
                team_data$Walks + team_data$SB + team_data$CS) 
summary(lin_reg)
# Results -> predicted runs = -563.09 + .63(Singles) + .72(Doubles) + 1.24(Triples) + 1.5(HR)
#                            + .35(Walks) + .06(SB) + .02(CS)
# A single 'creates' .63 runs, a double .72, triple 1.24, HR 1.5, etc.
# An R^2 of 0.9102 indicated that the independent variables explain 91% of the variation in 
# the number of runs a team actually scores during a season. 
# They all have a p-value of <.05, so they all pass the test of statistical significance. 
#
# Same data, but without Stolen Bases and Caught Stealing
team_data2 = my_data %>% 
  group_by(teamID, yearID) %>%
  summarise(R=sum(R), Singles=sum(Singles), X2B=sum(X2B), X3B=sum(X3B), 
            HR=sum(HR), Walks=sum(Walks)) 
#linear model
lin_reg2 <- lm(team_data2$R ~ team_data2$Singles + team_data2$X2B + team_data2$X3B + team_data2$HR +
                team_data2$Walks) 
summary(lin_reg2)
#
# Part 2 - Accuracy of the Linear Weights
results = my_data %>% 
  group_by(teamID, yearID) %>%
  summarise(R=sum(R), Singles=sum(Singles), X2B=sum(X2B), X3B=sum(X3B), 
            HR=sum(HR), Walks=sum(Walks)) %>%
  mutate(PredRun = -563.09 + (.63*Singles) + (.72*X2B) + (1.24*X3B) + (1.5*HR) + (.35*Walks)) %>%
  mutate(AbsError = abs(R-PredRun),
         ErrPerc = (abs(R-PredRun)/R)*100)
mean(results$AbsError)
mean(results$ErrPerc)
# mean(AbsError)=18.71, meaning the Linear Weights was off by an average of 18.71 runs
# mean(ErrPerc) = 2.45, meaning the Linear Weights was off by an average if 2.5% per team
#
# Part 3 - Linear Weights estimates of runs per game created by Bonds, Suzuki, and Nomar
my_stats = Batting[
  (Batting$playerID == "suzukic01" & Batting$yearID == 2004) |
    (Batting$playerID == "garcino01" & Batting$yearID == 1997) |
    (Batting$playerID == "bondsba01" & Batting$yearID == 2004), ]
# 26.72 x 162 = 4329 team outs per season
player_data = my_stats %>%
  summarise(playerID, AB, H, X2B, X3B, HR, BB, HBP, SF, SH, CS, GIDP) %>%
  mutate(Singles = H - (X2B + X3B + HR),
         Walks = BB + HBP,
         Outs = (.982*AB)-(H+SF+SH+CS+GIDP),
         ExpHR = 4329*(HR/((.982*AB)-(H+SF+SH+CS+GIDP)))) %>%
  mutate(ScaleFactor = 4329/Outs) %>%
  summarise(playerID, ScaleFactor, SF_AB=ScaleFactor*AB, SF_S=ScaleFactor*Singles, SF_H=ScaleFactor*H, 
            SF_2B=ScaleFactor*X2B, SF_3B=ScaleFactor*X3B, SF_HR=ScaleFactor*HR, SF_W=ScaleFactor*Walks) %>%
  mutate(LinWghtRuns = -563.09 + (.63*SF_S) + (.72*SF_2B) + (1.24*SF_3B) + (1.5*SF_HR) + (.35*SF_W),
         PredRunPerGame = (-563.09 + (.63*SF_S) + (.72*SF_2B) + (1.24*SF_3B) + (1.5*SF_HR) + (.35*SF_W))/162) %>%
  summarise(playerID, ScaleFactor, SF_AB, SF_S, SF_H, SF_2B, SF_3B, SF_HR, SF_W, LinWghtRuns, PredRunPerGame) 
player_data # <- view data in console
# Using our Linear Weights, we would predict a team of ALL Bonds to score 22.11 runs per game.  
#
# Part 4 - OBP, SLG, OBP+SLG, and Runs Created
# OBP = On-Base Percentage; OPS = On-Base Plus Slugging
slug_data = my_data %>%
  group_by(teamID, yearID) %>%
  summarise(R=sum(R), AB=sum(AB), H=sum(H), Singles=sum(Singles), X2B=sum(X2B), X3B=sum(X3B), 
            HR=sum(HR), Walks=sum(Walks), SF=sum(SF)) %>%
  mutate(SLG = (Singles + (2*X2B) + (3*X3B) + (4*HR))/AB,
         OBP = (H+Walks)/(AB+Walks+SF)) %>%
  mutate(PredRuns= -989.24 + 1728.04*SLG + 3076.37*OBP) %>%
  mutate(Residual = R - PredRuns)
lin_reg3 <- lm(slug_data$R ~ slug_data$SLG + slug_data$OBP)
summary(lin_reg3)
#
# Part 5 - Runs Created above Average
ichiro = Batting[
  (Batting$playerID == "suzukic01" & Batting$yearID == 2004),] %>%
  select(AB, R, H, X2B, X3B, HR, BB, HBP, SF, SH, CS, GIDP) %>%
  mutate(Singles = H - (X2B + X3B + HR),
         Walks = BB + HBP,
         Outs = (.982*AB)-(H+SF+SH+CS+GIDP)) %>%
  select(R, Singles, X2B, X3B, HR, Walks, Outs)
team_avg = Batting[
  (Batting$yearID >= 2000 & Batting$yearID <= 2006),] %>%
  group_by(teamID, yearID) %>%
  select(AB, R, H, X2B, X3B, HR, BB, HBP, SF, SH, CS, GIDP) %>%
  mutate(Singles = H - (X2B + X3B + HR),
         Walks = BB + HBP,
         Outs = (.982*AB)-(H+SF+SH+CS+GIDP)) %>%
  summarise(AB=sum(AB), R=sum(R), H=sum(H), Singles=sum(Singles), X2B=sum(X2B), X3B=sum(X3B), 
            HR=sum(HR), Walks=sum(Walks), Outs=sum(Outs)) %>%
  ungroup() %>%
  summarise(R=mean(R), Singles=mean(Singles), X2B=mean(X2B), X3B=mean(X3B), HR=mean(HR), 
            Walks=mean(Walks), Outs=mean(Outs)) 
# using outs to determine the team multiplyer
teammult=(team_avg$Outs-ichiro$Outs)/team_avg$Outs
# determine values with Ichiro added to 'avg' team
ichiro_added <- c(R=team_avg$R*teammult+ichiro$R, Singles=team_avg$Singles*teammult+ichiro$Singles, 
                  X2B=team_avg$X2B*teammult+ichiro$X2B, X3B=team_avg$X3B*teammult++ichiro$X3B, 
                  HR=team_avg$HR*teammult+ichiro$HR, Walks=team_avg$Walks*teammult+ichiro$Walks)

ichiro_added_DF <- as.data.frame(t(ichiro_added))
# Use the linear weights for team data in lin_reg2 to determine 
# Predicted Num of Runs by adding Ichiro to an average team:
IchAddedRuns = (-563.09 + (.63*ichiro_added_DF$Singles) + (.72*ichiro_added_DF$X2B) + (1.24*ichiro_added_DF$X3B) + 
                  (1.5*ichiro_added_DF$HR) + (.35*ichiro_added_DF$Walks))
# ^ 825.54 is how many runs we'd expect a team with Ichiro to score
IchAddedRuns - team_avg$R
# ^ 50.04 is how many runs we'd expect Ichiro to add to a team
#
# Part 5 - List of top 25 Runs above Average performances for players with at least 350 AB. 
top_runs=Batting[
  (Batting$yearID >= 2001 & Batting$yearID <=2006),] %>%
  group_by(playerID, yearID) %>%
  select(playerID, AB, R, H, X2B, X3B, HR, BB, HBP, SF, SH, CS, GIDP) %>%
  mutate(Singles = H - (X2B + X3B + HR),
         Walks = BB + HBP,
         Outs = (.982*AB)-(H+SF+SH+CS+GIDP),
         teammult = (team_avg$Outs-Outs)/team_avg$Outs) %>%
  mutate(new_R = team_avg$R*teammult+R, new_Sing = team_avg$Singles*teammult+Singles,
         new_X2B = team_avg$X2B*teammult+X2B, new_X3B = team_avg$X3B*teammult+X3B, 
         new_HR = team_avg$HR*teammult+HR, new_W = team_avg$Walks*teammult+Walks) %>%
  mutate(PlayerAdd = -563.09 + (.63*new_Sing) + (.72*new_X2B) + (1.24*new_X3B) + 
           (1.5*new_HR) + (.35*new_W)) %>%
  mutate(PredAddedRUns = PlayerAdd - team_avg$R) %>%
  select(yearID, playerID, PredAddedRUns, R, new_R, Singles, new_Sing, X2B, new_X2B, X3B, new_X3B, HR, new_HR, 
         Walks, new_W,PlayerAdd) %>%
  arrange(desc(PredAddedRUns))
# Top 25 above average performances
top_n(as_tibble(top_runs),25) %>%
  gt() %>%
  tab_header(
    title = md("Top Runs Above Average Performances"),
    subtitle = md("2001-2006")
  )

