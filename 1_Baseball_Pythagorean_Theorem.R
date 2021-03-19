#http://statisland.com/mathletics/Chapter_1.html

library(Lahman)
data("Teams")                                         #loads team data
Teams
team_df <- subset(Teams, yearID >= 1980 & yearID <=2006,
                  select=c("yearID","teamID","W","L","R","RA"))

team_df$Scoring.Ratio = team_df$R/team_df$RA
team_df$Predicted.Win.Pct = team_df$Scoring.Ratio^2/
  ((team_df$Scoring.Ratio^2)+1)
team_df$Actual.Win.Pct = team_df$W/(team_df$W+team_df$L)
team_df$Absolute.Error = abs(
  team_df$Actual.Win.Pct-team_df$Predicted.Win.Pct)
team_df

# EASIER WAY TO DO THIS USING DPLYR
library(dplyr)
team_df2 = Teams %>% 
  filter(yearID >= 1980, Teams$yearID <= 2006) %>%
  select(yearID, teamID, W, L, R, RA) %>%
  mutate(Scoring.Ratio = R/RA,
         Predicted.Win.Pct = Scoring.Ratio^2/((Scoring.Ratio^2)+1),
         Actual.Win.Pct = W/(W+L),
         Absolute.Error = abs(Actual.Win.Pct-Predicted.Win.Pct))
team_df2
mean(team_df2$Absolute.Error)

#MAD: Mean Absolute Deviation
exponent = seq (1,3,0.1)
MAD = sapply(exponent, function(x){
      mean(abs(team_df$Scoring.Ratio^x/((team_df$Scoring.Ratio^x)+1)
      -team_df2$Actual.Win.Pct))})
MAD_df <- data.frame(exponent,MAD)
max(MAD_df$MAD)                              # max value in MAD column
which.max((as.numeric(unlist(MAD_df$MAD))))  # had to unlist, then make numeric, then found row number for max MAD col

# Baseball/Pyth Th
library(scales)                               #to format percentages
                                              #read the csv from github
all_series = read.csv(
  "https://raw.githubusercontent.com/capstat/mathletics/master/Chapter_1/mlb_playoffs.csv")
                                              #just playoffs from the years 1980 to 2007 
series_80_07 = all_series[all_series$year >= 1980 & 
                            all_series$year <= 2007 &
                            all_series$series != "World Series",]
                                              #add a column for scoring ratio
series_80_07$Ratio = series_80_07$R/series_80_07$pR
                                              #data frame for the winners and losers
winners = series_80_07[seq(1,nrow(series_80_07),2), c(1:3,5:6,12,45,33,65)]
losers = series_80_07[seq(2,nrow(series_80_07),2), c(6,12,45,33,65)]
                                              #rename the losers columns
colnames(losers) = paste0("L", colnames(losers))
                                              #combine the winners and losers
series_df = cbind(winners, losers)
                                              #was the winner win % greater than the loser?
series_df$W.W.Greater = ifelse(series_df$pW > series_df$LLpW, TRUE, FALSE)
series_df$W.Ratio.Greater = ifelse((series_df$Ratio) > (series_df$LLRatio),
                                   TRUE, FALSE)

sum(series_df$W.W.Greater)/length(series_df$W.W.Greater)
sum(series_df$W.Ratio.Greater)/length(series_df$W.Ratio.Greater)

# Football/Pyt Th
                                              #read the csv off github
nfl_standings = read.csv(
  "https://raw.githubusercontent.com/capstat/mathletics/master/Chapter_1/nfl_standings.csv")
                                              #look at just 2 seasons
nfl_05_07 = nfl_standings[nfl_standings$Year >= 2005 &
                            nfl_standings$Year <= 2007,]
#pyt win % using exp=2.7
nfl_05_07$Win.Pct.2.7 = (nfl_05_07$Ratio^2.7)/((nfl_05_07$Ratio^2.7)+1)
#pyt win % using morely exp=2.37
nfl_05_07$Win.Pct.2.37 = (nfl_05_07$Ratio^2.37)/((nfl_05_07$Ratio^2.37)+1)
#absolute error exp=2.7
nfl_05_07$Error.2.7 = abs(nfl_05_07$Win.Pct.2.7 - nfl_05_07$W.L.)
mean(nfl_05_07$Error.2.7)
#absolute error morely exp=2.37
nfl_05_07$Error.2.37 = abs(nfl_05_07$Win.Pct.2.37 - nfl_05_07$W.L.)
mean(nfl_05_07$Error.2.37)

#Basketball/Pyth Th
nba_standings = read.csv(
  "https://raw.githubusercontent.com/capstat/mathletics/master/Chapter_1/nba_standings.csv")
nba_04_07 = nba_standings[nba_standings$Year >= 2005 & nba_standings$Year <=2007,]
#pyt win % using exp=15.4
nba_04_07$Win.Pct.15.4 = (nba_04_07$Ratio^15.4)/((nba_04_07$Ratio^15.4)+1)
#pyt win % using morely exp=13.91
nba_04_07$Win.Pct.13.91 = (nba_04_07$Ratio^13.91)/((nba_04_07$Ratio^13.91)+1)
#absolute error exp=15.4
nba_04_07$Error.15.4 = abs(nba_04_07$Win.Pct.15.4-nba_04_07$W.L.)
mean(nba_04_07$Error.15.4)
#absolute error exp=13.91
nba_04_07$Error.13.91 = abs(nba_04_07$Win.Pct.13.91 -nba_04_07$W.L.)
mean(nba_04_07$Error.13.91)
