library(gtools)
library(MASS)
library(glmnet)
setwd("C:/Users/Jabez James/Desktop/Football")
rm(list=ls(all=TRUE))
load("rawweeklystats.rda")
load("previousSTATS.rda")


## Function to Find Cumlative Sum
row_csum=function(X){
  empt_gam=vector()
  for(j in 1:ncol(X)){
    tmp_gam=as.matrix(rowSums(as.matrix(X[,1:j]),na.rm = T))
    empt_gam=cbind(empt_gam,tmp_gam)
  }
  return(empt_gam)
}

## Function to remove NA with previous value from row
replace_na_prev_value=function(X){
  for(i in 1:ncol(X)){
    for(j in 1:nrow(X)){
      X[j,i]=ifelse(is.na(X[j,i])==T,X[j,i-1],X[j,i])
    }
  }
  return(X)
}

## Function used to determine wins and losses by team
win_loss_determin=function(tmp_win_loss,teams_names){
  empt_vecwl=vector()
  for(i in 1:32){
    teamwl=as.matrix(colSums((teams_names[i,1]==tmp_win_loss)+0))
    if(teamwl[1,1]==1)
    {tmp_gm="W"} else if (teamwl[2,1]==1)
    {tmp_gm="L"} else if(teamwl[2,1]==0 && teamwl[1,1]==0)
    {tmp_gm="B"}
    empt_vecwl=rbind(empt_vecwl,tmp_gm)
  }
  return(empt_vecwl)
}




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
                      select=c(Week,Day,Date,Year,Win.Team,Lose.Team,
                               Away.Team.Won,Points.Win,Points.Lose,
                               YardsGained.Win,Turnovers.Win,
                               YardsGained.Lose,Turnovers.Lose))

## 7 Determine teams Division and Conference
teams_names=rbind('Denver Broncos','Tennessee Titans','Indianapolis Colts',
                  'Kansas City Chiefs','Miami Dolphins','New York Jets',
                  'New England Patriots','Houston Texans','Baltimore Ravens',
                  'Pittsburgh Steelers','Cincinnati Bengals','Oakland Raiders',
                  'Jacksonville Jaguars','Cleveland Browns','Buffalo Bills',
                  'San Diego Chargers','Seattle Seahawks','St. Louis Rams',
                  'San Francisco 49ers','Detroit Lions','New Orleans Saints',
                  'Chicago Bears','Dallas Cowboys','Philadelphia Eagles',
                  'Carolina Panthers','Arizona Cardinals','Green Bay Packers',
                  'Minnesota Vikings','Atlanta Falcons','New York Giants',
                  'Tampa Bay Buccaneers','Washington Redskins')
football_conf=rbind('AFC','AFC','AFC','AFC','AFC','AFC','AFC','AFC','AFC',
                    'AFC','AFC','AFC','AFC','AFC','AFC','AFC','NFC','NFC','NFC',
                    'NFC','NFC','NFC','NFC','NFC','NFC','NFC','NFC','NFC','NFC',
                    'NFC','NFC','NFC')
football_div=rbind('West','South','South','West','East','East','East','South',
                   'North','North','North','West','South','North','East','West',
                   'West','West','West','North','South','North','East','East',
                   'South','West','North','North','South','East','South','East')

foot_name=cbind(teams_names,football_conf,football_div)


all_vector=weeklystats_6

all_games_conf_div_win=vector()
all_games_conf_div_loss=vector()
for(i in 1:nrow(all_vector)){
  tmp_win=which(as.character(all_vector[i,5])==foot_name)
  tmp_loss=which(as.character(all_vector[i,6])==foot_name)
  name_of_win=foot_name[tmp_win,2:3]
  name_of_loss=foot_name[tmp_loss,2:3]
  all_games_conf_div_win=rbind(all_games_conf_div_win,name_of_win)
  all_games_conf_div_loss=rbind(all_games_conf_div_loss,name_of_loss)
}
game_div_con=cbind(all_games_conf_div_win,all_games_conf_div_loss)
colnames(game_div_con)<-c("Win.Conference","Win.Division",
                          "Lose.Conference","Lose.Division")
weeklystats_7<-cbind(weeklystats_6,game_div_con)



## 8 Rearrange Data
weeklystats_8<-subset(weeklystats_7, 
                      select=c(Week,Day,Date,Year,Win.Team,
                               Win.Conference, Win.Division,Points.Win,Turnovers.Win,
                               YardsGained.Win,Lose.Team,Lose.Conference,Lose.Division,
                               Points.Lose,Turnovers.Lose,YardsGained.Lose,Away.Team.Won))


## 9 Get only Regular Season Games
weeklystats_9=subset(weeklystats_8, Week!="WildCard" & Week!="Division"
                     & Week!="ConfChamp" & Week!="SuperBowl")

## Determine Unique Years in Dataset
year=as.matrix(unique(weeklystats_9[,4])) 


## 10 Make Data into a list by year
weeklystats_10=list()
for(i in 1:12){
  tmp=subset(weeklystats_9,weeklystats_9[,4]==year[13-i,])
  weeklystats_10[i]<-list(tmp)
}


##-----------------------------------------------------------------




## Two For loops
## 1) For loop looks at only 2004 to 2013 ## start yy from 3 so that it denotes 2004
## 2) For loop only gathers past two years
## Gets relevant data based needed statistics
team_poins=weeklystats_9[,c(1,4,5,11)]
data_sub3=vector()
for(yy in 1:10){
  data_sub2=vector()
  for(ww in 1:17){
    data_sub1=as.matrix(subset(team_poins,team_poins[,1]==ww & team_poins[,2]==year[yy,]))
    ## All past two stats
    y_min_1=noquote(subset(tte,tte[,110]==year[yy+1,]))
    y_min_2=noquote(subset(tte,tte[,110]==year[yy+2,]))
    ## Get Data from past 2 years by winners and losers
    win.past.1.year.stat=as.matrix(y_min_1[as.character(data_sub1[,3]),])
    ## Change Column Names
    colnames(win.past.1.year.stat)<-paste(colnames(win.past.1.year.stat),rep(".Win1",110), sep = "")
    los.past.1.year.stat=y_min_1[as.character(data_sub1[,4]),]
    colnames(los.past.1.year.stat)<-paste(colnames(los.past.1.year.stat),rep(".Los1",110), sep = "")
    win.past.2.year.stat=y_min_2[as.character(data_sub1[,3]),]
    colnames(win.past.2.year.stat)<-paste(colnames(win.past.2.year.stat),rep(".Win2",110), sep = "")
    los.past.2.year.stat=y_min_2[as.character(data_sub1[,4]),]
    colnames(los.past.2.year.stat)<-paste(colnames(los.past.2.year.stat),rep(".Los2",110), sep = "")
    ttmp1=cbind(data_sub1,win.past.1.year.stat,win.past.2.year.stat,los.past.1.year.stat,los.past.2.year.stat)
    data_sub2=rbind(data_sub2,ttmp1)
  }
  data_sub3=rbind(data_sub3,data_sub2)
}
data_sub3=as.data.frame(data_sub3)
##----
data_sub3[, "Cmp_per_PAS.Los2"]<-sapply(strsplit(as.character(data_sub3$Cmp_per_PAS.Los2),"%"),
                                        function(x) {x <- as.numeric(x);x[1]})
data_sub3[, "Cmp_per_PAS.Los1"]<-sapply(strsplit(as.character(data_sub3$Cmp_per_PAS.Los1),"%"),
                                        function(x) {x <- as.numeric(x);x[1]})
data_sub3[, "Cmp_per_PAS.Win2"]<-sapply(strsplit(as.character(data_sub3$Cmp_per_PAS.Win2),"%"),
                                        function(x) {x <- as.numeric(x);x[1]})
data_sub3[, "Cmp_per_PAS.Win1"]<-sapply(strsplit(as.character(data_sub3$Cmp_per_PAS.Win1),"%"),
                                        function(x) {x <- as.numeric(x);x[1]})


tmp2=data.frame(cbind(weeklystats_9[-c(2561:3072),],data_sub3))
tic<-c('Tm.Los2', 'G.Los2','Tm.Los1', 'G.Los1','Tm.Win2', 'G.Win2',
       'Tm.Win1', 'G.Win1','Lose.Team.1','Win.Team.1',
       'Year.Los2','Year.Los1','Year.Win2','Year.Win1',
       'Rk_DR.Los1','G_DR.Los1','Rk_TS.Los1','G_TS.Los1','Rk_FG.Los1',
       'G_FG.Los1','Rk_RETK.Los1','G_RETK.Los1','Rk_RUS.Los1',
       'G_RUS.Los1','Rk_PAS.Los1','G_PAS.Los1','Rk_TO.Los1','G.Los1',
       'Rk_DR.Win1','G_DR.Win1','Rk_TS.Win1','G_TS.Win1','Rk_FG.Win1',
       'G_FG.Win1','Rk_RETK.Win1','G_RETK.Win1','Rk_RUS.Win1',
       'G_RUS.Win1','Rk_PAS.Win1','G_PAS.Win1','Rk_TO.Win1','G.Win1',
       'Rk_DR.Los2','G_DR.Los2','Rk_TS.Los2','G_TS.Los2','Rk_FG.Los2',
       'G_FG.Los2','Rk_RETK.Los2','G_RETK.Los2','Rk_RUS.Los2',
       'G_RUS.Los2','Rk_PAS.Los2','G_PAS.Los2','Rk_TO.Los2','G.Los2',
       'Rk_DR.Win2','G_DR.Win2','Rk_TS.Win2','G_TS.Win2','Rk_FG.Win2',
       'G_FG.Win2','Rk_RETK.Win2','G_RETK.Win2','Rk_RUS.Win2',
       'G_RUS.Win2','Rk_PAS.Win2','G_PAS.Win2','Rk_TO.Win2','G.Win2',
       'QBR_PAS.Los2','QBR_PAS.Los1','QBR_PAS.Win2','QBR_PAS.Win1',
       'FGA_FG.Los2','FGM_FG.Los2','PR_TD_TS.Los2','KR_TD_TS.Los2',
       'FblTD_TS.Los2','IntTD_TS.Los2','OthTD_TS.Los2','X2PM_TS.Los2',
       'Sfty_TS.Los2','FGA_FG.Los1','FGM_FG.Los1','PR_TD_TS.Los1',
       'KR_TD_TS.Los1','FblTD_TS.Los1','IntTD_TS.Los1','OthTD_TS.Los1',
       'X1PM_TS.Los1','Sfty_TS.Los1','FGA_FG.Win2','FGM_FG.Win2',
       'PR_TD_TS.Win2','KR_TD_TS.Win2','FblTD_TS.Win2','IntTD_TS.Win2',
       'OthTD_TS.Win2','X2PM_TS.Win2','Sfty_TS.Win2','FGA_FG.Win1',
       'FGM_FG.Win1','PR_TD_TS.Win1','KR_TD_TS.Win1','FblTD_TS.Win1',
       'IntTD_TS.Win1','OthTD_TS.Win1','X1PM_TS.Win1','Sfty_TS.Win1',
       'X2PM_TS.Los1','X2PM_TS.Los2','X2PM_TS.Win1','X2PM_TS.Win2',
       'X4QC_PAS.Los2','GWD_PAS.Los2','X4QC_PAS.Los1','GWD_PAS.Los1',
       'X4QC_PAS.Win2','GWD_PAS.Win2','X4QC_PAS.Win1','GWD_PAS.Win1',
       'Day','Date','Week.1','Year.1','Week','Year')

tmp3=tmp2[,!(names(tmp2) %in% tic)]
## from columns 1 to column 14 can be seen as variances



tmp4=as.data.frame(tmp3[,-c(1:13)])
## Make data into a dataframe
tmp4 <- data.frame(lapply(tmp4, as.numeric), stringsAsFactors=FALSE)



## Determine which games are wins and losses
set.seed(1)
random_games=as.matrix(sort(rbinom(nrow(tmp4),1,0.5)))
t1=ifelse(random_games==0,1,NA);t1=na.exclude(t1)
t2=ifelse(random_games==1,0,NA);t2=na.exclude(t2)
random_games.t=rbind(t2,t1)
vecwl=cbind("Win","Los")

col.nam.wi=grep("Win",colnames(tmp4))
col.nam.lo=grep("Los",colnames(tmp4))
colnam.it=sapply(strsplit(colnames(tmp4[,col.nam.wi]),"Win"),
       function(x) {x <- as.character(x);paste0(x[1],x[2],"")})
## Real Data set
all_gamesl=vector()
for(i in 1:1329){
  if(random_games[i,1]==0){ttwl=vecwl[1,2]} else {ttwl=vecwl[1,1]}
  colnwl=grep(ttwl,colnames(tmp4))
  gamei=as.data.frame(tmp4[i,colnwl])
  all_gamesl=rbind(all_gamesl,gamei)
}
all_gamesw=vector()
for(i in 1330:2560){
  if(random_games[i,1]==0){ttwl=vecwl[1,2]} else {ttwl=vecwl[1,1]}
  colnwl=as.numeric(grep(ttwl,colnames(tmp4)))
  gamei=as.data.frame(tmp4[i,colnwl])
  all_gamesw=rbind(all_gamesw,gamei)
}
colnames(all_gamesl)<-NULL
colnames(all_gamesw)<-NULL
compl.data=data.frame(rbind(as.matrix(all_gamesl),as.matrix(all_gamesw)))
colnames(compl.data)<-colnam.it
data.sca<-scale(compl.data)
d.n<-cbind(1,data.sca)



all_gamesl.t=vector()
for(i in 1:1231){
  if(random_games.t[i,1]==0){ttwl=vecwl[1,2]} else {ttwl=vecwl[1,1]}
  colnwl=grep(ttwl,colnames(tmp4))
  gamei=as.data.frame(tmp4[i,colnwl])
  all_gamesl.t=rbind(all_gamesl.t,gamei)
}
all_gamesw.t=vector()
for(i in 1232:2560){
  if(random_games.t[i,1]==0){ttwl=vecwl[1,2]} else {ttwl=vecwl[1,1]}
  colnwl=as.numeric(grep(ttwl,colnames(tmp4)))
  gamei=as.data.frame(tmp4[i,colnwl])
  all_gamesw.t=rbind(all_gamesw.t,gamei)
}
colnames(all_gamesl.t)<-NULL
colnames(all_gamesw.t)<-NULL
compl.data.t=data.frame(rbind(as.matrix(all_gamesl.t),as.matrix(all_gamesw.t)))
colnames(compl.data.t)<-colnam.it
data.sca.t<-scale(compl.data.t)
d.n.t<-cbind(1,data.sca.t)


## Linear Regression
B.t=ginv(t(d.n)%*%d.n)%*%t(d.n)%*%random_games
fit1=d.n%*%B.t>0.5
## Error
mean((fit1!=random_games))

## Cross Validation
fit1.cv=d.n.t%*%B.t>0.5
mean((fit1.cv!=random_games.t))


## PCA 
d.n_pca=svd(data.sca)
plot(data.sca%*%d.n_pca$v[,1],data.sca%*%d.n_pca$v[,2])
plot(d.n_pca$d) ## First 4 factors explain the data really well
##Logistic
