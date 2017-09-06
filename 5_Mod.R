# setwd("C:/Users/Jabez James/Desktop/Football")
rm(list=ls(all=TRUE))
load("rawweeklystats.rda")
load("previousSTATS.rda")
library(car)


##1 Remove Title
remove_wk_title=which(with( weeklystats, Week=='W#' ))
weeklystats_1<-weeklystats[-remove_wk_title,]

## 2 Remove Dates & Playoffs
remove_wk_other_names=which(with(weeklystats_1, Date=='Playoffs' | Date=='Date'  ))
weeklystats_2<-weeklystats_1[-remove_wk_other_names,]

## 3 Remove last 
weeklystats_3<-weeklystats_2[-3205,]

## 4 Determine who won home or away; modify superbowl
away.team.won=as.matrix(ifelse(weeklystats_3[,6]=='@',1,0))
superbowl.det=as.matrix(ifelse(weeklystats_3[,1]=='SuperBowl',1,0))
sup.det=ifelse(away.team.won==0 & superbowl.det==1,2,0 )
## away.team.won is a superbowl: 2 in column 
Away.Team.Won=sup.det+away.team.won
weeklystats_4=cbind(weeklystats_3,Away.Team.Won)

## 5 Remove Blank and At
weeklystats_5 <-weeklystats_4[,-c(4,6)]


## 6 Rearrange Data
weeklystats_6<-subset(weeklystats_5, 
                      select=c(Week,Year,Win.Team,Lose.Team,
                               Away.Team.Won,Points.Win,Points.Lose,
                               YardsGained.Win,Turnovers.Win,
                               YardsGained.Lose,Turnovers.Lose))
## 7 Get only Regular Season Games
weeklystats_7=subset(weeklystats_6, Week!="WildCard" & Week!="Division"
                     & Week!="ConfChamp" & Week!="SuperBowl")



## Years and Teams in dataset
years=unique(weeklystats_7[,2])
teams=as.character(unique(weeklystats_7[,3]))


##  8 Figure out How many games had been played previously
mod_data1=weeklystats_7[,2:4]
all.years.Total=vector()
for(yy in 1:length(years)){
  mod_data2=subset(mod_data1,Year==years[yy])
  Total.Games=cbind(0,0)
  for(i in 2:nrow(mod_data2)){
    prevgame=mod_data2[1:i-1,2:3]
    winteam=as.character(mod_data2[i,2]);losteam=as.character(mod_data2[i,3])
    Win.Total.Games=sum(prevgame==winteam)
    Loss.Total.Games=sum(prevgame==losteam)
    Total.Games.tmp<- cbind(Win.Total.Games,Loss.Total.Games)
    Total.Games<-rbind(Total.Games,Total.Games.tmp)
  }
  all.years.Total=rbind(all.years.Total,Total.Games)
}

weeklystats_8<-cbind(weeklystats_7,all.years.Total)





## 9 Only Look at games 10-16 and Years 2004-2013
weeklystats_8[,1] <-as.numeric(as.character(weeklystats_8[,1])) 
weeklystats_9 <- subset(weeklystats_8, Week>9  & Year>2003)

nr_w9 <- nrow(weeklystats_9) ## New Sample Size
Win.Loss <- rbinom(nr_w9,1,0.5) ## Which team is determine as winner/loser
weeklystats_9 <- cbind(weeklystats_9,Win.Loss)




Points.Diff=vector()
for(i in 1:nrow(weeklystats_9)){
  w.p <- as.numeric(as.character(weeklystats_9[i,6]))
  l.p <- as.numeric(as.character(weeklystats_9[i,7]))
if(Win.Loss[i]==1){tmp1 <- w.p-l.p}else {tmp1 <- l.p-w.p}
Points.Diff<-rbind(Points.Diff,tmp1)
}
colnames(Points.Diff) <- "Points.Diff"
## Function Begins
DATA=vector()
for(j in 1:nr_w9){
  test_row <- weeklystats_9[j,]
  tmp1 <- get_dat(test_row)
  DATA=rbind(DATA,tmp1)
}

tmp_all = cbind(Win.Loss,DATA)
save(tmp_all, file = "winlossdata.rda")


tmp_all2 = cbind(Points.Diff,DATA)
save(tmp_all2, file = "pddata.rda")
