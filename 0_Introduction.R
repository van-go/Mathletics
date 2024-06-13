# http://statisland.com/mathletics/Introduction.html

Year = seq(2014,2016)             # array of 2014-2016
Team = rep("New York Mets", 3)    # repeats MYM 3 times
W = c(79, 90, 87)                 
L = c(83, 72, 75)
mets = data.frame(Year, Team, W, L) #df with 4 columns

mets                              # view of mets df
mets$Year
mets$W[mets$W >= 81]
mets[2,3]                         # value in column 2, row 3
mets[ , c("W", "L")]
mets[mets$Year==2016, c(3, 4)]
mets[mets$W > mets$L, "Year"]
max(mets$Year)
sum(mets$W)
mean(mets$W)
paste(mets$Year, mets$Team, sep="---")    # "2014---New York Mets" "2015---New York Mets" "2016---New York Mets"
sapply(mets$Year, function(x){ x-2000 })  # apply a fuction to every element in a vector. prints: 14 15 16
mets$Games = mets$W + mets$L      # create a new column
mets$Games
mets$W.pct = round(mets$W/mets$Games, 3)  #rounds to 3 places
mets$W.pct

mets$`Over500` = ifelse(mets$W > mets$L, TRUE, FALSE) 
mets$`Over500`

next_year = c(2017, "New York Mets", 162, 0, 162, 1.000, TRUE)  #adding new row with rbind
mets2=rbind(mets, next_year)                                          
mets3=cbind(mets2, League=rep("NL", 4))                          #cbind adds columns
mets3
