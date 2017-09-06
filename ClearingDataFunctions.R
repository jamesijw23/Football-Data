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



tmp2=data.frame(cbind(weeklystats_9[-c(2561:3072),],data_sub3))
tic<-c('Tm.Los2', 'G.Los2','Tm.Los1', 'G.Los1','Tm.Win2', 'G.Win2',
       'Tm.Win1', 'G.Win1','Lose.Team.1','Win.Team.1')
tmp3=tmp2[,!(names(tmp2) %in% tic)]
head(tmp3)


##------------------------------------------------
## Extension Data Formation
## points; turnovers; yards

#####
### BIG 2) ****** Add Turnovers and Yards
#####



## Points List per Game 
off_points_all=list()
def_points_all=list()

## Turnover List per Game 
off_turnov_all=list()
def_turnov_all=list()

## Yards List per Game 
off_yards_all=list()
def_yards_all=list()

team_poins=weeklystats_9[,c(1,4,5,8,9,10,11,14,15,16)] ## Gets relevant data based needed statistics
for(yy in 1:12){
  off_points_sea=vector()
  def_points_sea=vector()
  
  off_turnov_sea=vector()
  def_turnov_sea=vector()
  
  off_yards_sea=vector()
  def_yards_sea=vector()
  for(ww in 1:17){
    data_sub1=subset(team_poins,team_poins[,1]==ww & team_poins[,2]==year[13-yy,])
   
    off_points=vector()
    def_points=vector()
    
    off_turnov=vector()
    def_turnov=vector()
    
    off_yards=vector()
    def_yards=vector()
    ## Determine teams offensive and defensive points, yards, turnovers
    for(tt in 1:32){
      team_sea=teams_names[tt,]
      if(sum(data_sub1==team_sea)==1){ 
        data_sub2=subset(data_sub1,data_sub1[,3]==team_sea | data_sub1[,7]==team_sea)
        if(data_sub2[,3]==team_sea){
          tmp_of_poi=as.matrix(data_sub2[,4]);tmp_de_poi=as.matrix(data_sub2[,8])
          tmp_of_turo=as.matrix(data_sub2[,5]);tmp_de_turo=as.matrix(data_sub2[,9])
          tmp_of_yard=as.matrix(data_sub2[,6]);tmp_de_yard=as.matrix(data_sub2[,10])
          } else {tmp_of_poi=as.matrix(data_sub2[,8]);tmp_de_poi=as.matrix(data_sub2[,4])
                  tmp_of_turo=as.matrix(data_sub2[,9]);tmp_de_turo=as.matrix(data_sub2[,5])
                  tmp_of_yard=as.matrix(data_sub2[,10]);tmp_de_yard=as.matrix(data_sub2[,6])}}else{tmp_of_poi=NA;tmp_de_poi=NA
                                                                                                   tmp_of_turo=NA;tmp_de_turo=NA
                                                                                                   tmp_of_yard=NA;tmp_de_yard=NA}
      off_points=rbind(off_points,tmp_of_poi)
      def_points=rbind(def_points,tmp_de_poi)
      
      off_turnov=rbind(off_turnov,tmp_of_turo)
      def_turnov=rbind(def_turnov,tmp_de_turo)
      
      off_yards=rbind(off_yards,tmp_of_yard)
      def_yards=rbind(def_yards,tmp_de_yard)
    }
    
    off_points_sea=cbind(off_points_sea,off_points)
    def_points_sea=cbind(def_points_sea,def_points)
    
    off_turnov_sea=cbind(off_turnov_sea,off_turnov)
    def_turnov_sea=cbind(def_turnov_sea,def_turnov)
    
    off_yards_sea=cbind(off_yards_sea,off_yards)
    def_yards_sea=cbind(def_yards_sea,def_yards)
    
  }
  off_points_sea=matrix(as.numeric(off_points_sea),32,17)
  def_points_sea=matrix(as.numeric(def_points_sea),32,17)
  
  off_turnov_sea=matrix(as.numeric(off_turnov_sea),32,17)
  def_turnov_sea=matrix(as.numeric(def_turnov_sea),32,17)
  
  off_yards_sea=matrix(as.numeric(off_yards_sea),32,17)
  def_yards_sea=matrix(as.numeric(def_yards_sea),32,17)
  
  
  
  rownames(off_points_sea)<-teams_names
  rownames(def_points_sea)<-teams_names
  
  rownames(off_turnov_sea)<-teams_names
  rownames(def_turnov_sea)<-teams_names
  
  rownames(off_yards_sea)<-teams_names
  rownames(def_yards_sea)<-teams_names
  
  
  
  off_points_all[yy]<-list(off_points_sea)
  def_points_all[yy]<-list(def_points_sea)
  
  off_turnov_all[yy]<-list(off_turnov_sea)
  def_turnov_all[yy]<-list(def_turnov_sea)
  
  off_yards_all[yy]<-list(off_yards_sea)
  def_yards_all[yy]<-list(def_yards_sea)
  
}




off_points_all_sum=list()
def_points_all_sum=list()
year_off_points=vector()
year_def_points=vector()

off_turnov_all_sum=list()
def_turnov_all_sum=list()
year_off_turnov=vector()
year_def_turnov=vector()

off_yards_all_sum=list()
def_yards_all_sum=list()
year_off_yards=vector()
year_def_yards=vector()



for(yy in 1:12){
  tmp_off_points=row_csum(off_points_all[[yy]])
  tmp_def_points=row_csum(def_points_all[[yy]])
  year_off_points=cbind(year_off_points,as.matrix(tmp_off_points[,17]))
  year_def_points=cbind(year_def_points,as.matrix(tmp_def_points[,17]))
  off_points_all_sum[yy]=list(tmp_off_points)
  def_points_all_sum[yy]=list(tmp_def_points) 
  
  tmp_off_turnov=row_csum(off_turnov_all[[yy]])
  tmp_def_turnov=row_csum(def_turnov_all[[yy]])
  year_off_turnov=cbind(year_off_turnov,as.matrix(tmp_off_turnov[,17]))
  year_def_turnov=cbind(year_def_turnov,as.matrix(tmp_def_turnov[,17]))
  off_turnov_all_sum[yy]=list(tmp_off_turnov)
  def_turnov_all_sum[yy]=list(tmp_def_turnov) 
  
  
  tmp_off_yards=row_csum(off_yards_all[[yy]])
  tmp_def_yards=row_csum(def_yards_all[[yy]])
  year_off_yards=cbind(year_off_yards,as.matrix(tmp_off_yards[,17]))
  year_def_yards=cbind(year_def_yards,as.matrix(tmp_def_yards[,17]))
  off_yards_all_sum[yy]=list(tmp_off_yards)
  def_yards_all_sum[yy]=list(tmp_def_yards)  
  
}






sum_years_off_points=cbind(0,as.matrix(row_csum(year_off_points)))
sum_years_def_points=cbind(0,as.matrix(row_csum(year_def_points)))
year_off_points=cbind(0,year_off_points)
year_def_points=cbind(0,year_def_points)


sum_years_off_turnov=cbind(0,as.matrix(row_csum(year_off_turnov)))
sum_years_def_turnov=cbind(0,as.matrix(row_csum(year_def_turnov)))
year_off_turnov=cbind(0,year_off_turnov)
year_def_turnov=cbind(0,year_def_turnov)


sum_years_off_yards=cbind(0,as.matrix(row_csum(year_off_yards)))
sum_years_def_yards=cbind(0,as.matrix(row_csum(year_def_yards)))
year_off_yards=cbind(0,year_off_yards)
year_def_yards=cbind(0,year_def_yards)




## replace NAs
past_games_point_off=list()
past_games_point_def=list()


past_games_turnov_off=list()
past_games_turnov_def=list()

past_games_yards_off=list()
past_games_yards_def=list()
for(y in 1:12){
  
tmp1_off_points=replace_na_prev_value(off_points_all[[y]])
tmp1_def_points=replace_na_prev_value(def_points_all[[y]])
past_games_point_off[y]=list(tmp1_off_points)
past_games_point_def[y]=list(tmp1_def_points)


tmp1_off_turnov=replace_na_prev_value(off_turnov_all[[y]])
tmp1_def_turnov=replace_na_prev_value(def_turnov_all[[y]])
past_games_turnov_off[y]=list(tmp1_off_turnov)
past_games_turnov_def[y]=list(tmp1_def_turnov)


tmp1_off_yards=replace_na_prev_value(off_yards_all[[y]])
tmp1_def_yards=replace_na_prev_value(def_yards_all[[y]])
past_games_yards_off[y]=list(tmp1_off_yards)
past_games_yards_def[y]=list(tmp1_def_yards)

}



## 1) For All Years: sum_years_off_points and sum_years_def_points
## 2) For Previous year: year_off_points and year_def_points
## 3) For Current Season: off_points_all and def_points_all
## 4) For Previous Game: off_points_all and def_points_all

## 1) Determine Previous All Past Points scored cumsum (11)
weeklystats_11<-list()
for(y in 1:12){
  All.Points.Offense.Win=vector() ##  
  All.Points.Defense.Win=vector() ## 
  All.Points.Offense.Los=vector() ##  
  All.Points.Defense.Los=vector() ##
  
  All.Turn.Over.Offense.Win=vector() ##  
  All.Turn.Over.Defense.Win=vector() ## 
  All.Turn.Over.Offense.Los=vector() ##  
  All.Turn.Over.Defense.Los=vector() ##
  
  All.Yards.Offense.Win=vector() ##  
  All.Yards.Defense.Win=vector() ## 
  All.Yards.Offense.Los=vector() ##  
  All.Yards.Defense.Los=vector() ##
  
  
  for(w in 1:17){
    year1_gams=weeklystats_10[[y]][,c(1,5,11)]
    
    win_teams=as.character(subset(year1_gams,Week==w)[,2])
    los_teams=as.character(subset(year1_gams,Week==w)[,3])
    
    off_points_win=as.matrix(sum_years_off_points[win_teams,y])
    def_points_win=as.matrix(sum_years_def_points[win_teams,y])
    off_points_los=as.matrix(sum_years_off_points[los_teams,y])
    def_points_los=as.matrix(sum_years_def_points[los_teams,y])
    
    off_turnov_win=as.matrix(sum_years_off_turnov[win_teams,y])
    def_turnov_win=as.matrix(sum_years_def_turnov[win_teams,y])
    off_turnov_los=as.matrix(sum_years_off_turnov[los_teams,y])
    def_turnov_los=as.matrix(sum_years_def_turnov[los_teams,y])
    
    off_yards_win=as.matrix(sum_years_off_yards[win_teams,y])
    def_yards_win=as.matrix(sum_years_def_yards[win_teams,y])
    off_yards_los=as.matrix(sum_years_off_yards[los_teams,y])
    def_yards_los=as.matrix(sum_years_def_yards[los_teams,y])
    
    
 
    
    
    
    ## Combine Data
    All.Points.Offense.Win=rbind(All.Points.Offense.Win,off_points_win) ##  
    All.Points.Defense.Win=rbind(All.Points.Defense.Win,def_points_win) ## 
    All.Points.Offense.Los=rbind(All.Points.Offense.Los,off_points_los) ##  
    All.Points.Defense.Los=rbind(All.Points.Defense.Los,def_points_los) ## 

    All.Turn.Over.Offense.Win=rbind(All.Turn.Over.Offense.Win,off_turnov_win) ##  
    All.Turn.Over.Defense.Win=rbind(All.Turn.Over.Defense.Win,def_turnov_win) ## 
    All.Turn.Over.Offense.Los=rbind(All.Turn.Over.Offense.Los,off_turnov_los) ##  
    All.Turn.Over.Defense.Los=rbind(All.Turn.Over.Defense.Los,def_turnov_los) ## 
    
    All.Yards.Offense.Win=rbind(All.Yards.Offense.Win,off_yards_win) ##  
    All.Yards.Defense.Win=rbind(All.Yards.Defense.Win,def_yards_win) ## 
    All.Yards.Offense.Los=rbind(All.Yards.Offense.Los,off_yards_los) ##  
    All.Yards.Defense.Los=rbind(All.Yards.Defense.Los,def_yards_los) ## 
    
  }
  
  row.names(All.Points.Offense.Win) <- NULL 
  row.names(All.Points.Defense.Win) <- NULL 
  row.names(All.Points.Offense.Los) <- NULL 
  row.names(All.Points.Defense.Los) <- NULL 
  
  row.names(All.Turn.Over.Offense.Win) <- NULL 
  row.names(All.Turn.Over.Defense.Win) <- NULL 
  row.names(All.Turn.Over.Offense.Los) <- NULL 
  row.names(All.Turn.Over.Defense.Los) <- NULL 
  
  row.names(All.Yards.Offense.Win) <- NULL 
  row.names(All.Yards.Defense.Win) <- NULL 
  row.names(All.Yards.Offense.Los) <- NULL 
  row.names(All.Yards.Defense.Los) <- NULL 
  
  
  tmp1=cbind(weeklystats_10[[y]],All.Points.Offense.Win,All.Points.Defense.Win,All.Points.Offense.Los,All.Points.Defense.Los,
             All.Turn.Over.Offense.Win,All.Turn.Over.Defense.Win,All.Turn.Over.Offense.Los,All.Turn.Over.Defense.Los,
             All.Yards.Offense.Win,All.Yards.Defense.Win,All.Yards.Offense.Los,All.Yards.Defense.Los)
  weeklystats_11[y]<-list(tmp1)
  
}


## 2) Determine Previous Year Past Points scored  (12)
weeklystats_12<-list()
for(y in 1:12){
  Prev.Year.Points.Offense.Win=vector() ##  
  Prev.Year.Points.Defense.Win=vector() ## 
  Prev.Year.Points.Offense.Los=vector() ##  
  Prev.Year.Points.Defense.Los=vector() ##
  
  Prev.Year.Turn.Over.Offense.Win=vector() ##  
  Prev.Year.Turn.Over.Defense.Win=vector() ## 
  Prev.Year.Turn.Over.Offense.Los=vector() ##  
  Prev.Year.Turn.Over.Defense.Los=vector() ##
  
  Prev.Year.Yards.Offense.Win=vector() ##  
  Prev.Year.Yards.Defense.Win=vector() ## 
  Prev.Year.Yards.Offense.Los=vector() ##  
  Prev.Year.Yards.Defense.Los=vector() ##
  
  for(w in 1:17){
    year1_gams=weeklystats_11[[y]][,c(1,5,11)]
    
    win_teams=as.character(subset(year1_gams,Week==w)[,2])
    los_teams=as.character(subset(year1_gams,Week==w)[,3])
    
    off_points_win=as.matrix(year_off_points[win_teams,y])
    def_points_win=as.matrix(year_def_points[win_teams,y])
    off_points_los=as.matrix(year_off_points[los_teams,y])
    def_points_los=as.matrix(year_def_points[los_teams,y])
    
    
    off_turnov_win=as.matrix(year_off_points[win_teams,y])
    def_turnov_win=as.matrix(year_def_points[win_teams,y])
    off_turnov_los=as.matrix(year_off_points[los_teams,y])
    def_turnov_los=as.matrix(year_def_points[los_teams,y])
    
    
    off_yards_win=as.matrix(year_off_points[win_teams,y])
    def_yards_win=as.matrix(year_def_points[win_teams,y])
    off_yards_los=as.matrix(year_off_points[los_teams,y])
    def_yards_los=as.matrix(year_def_points[los_teams,y])
    
    
    Prev.Year.Points.Offense.Win=rbind(Prev.Year.Points.Offense.Win,off_points_win) ##  
    Prev.Year.Points.Defense.Win=rbind(Prev.Year.Points.Defense.Win,def_points_win) ## 
    Prev.Year.Points.Offense.Los=rbind(Prev.Year.Points.Offense.Los,off_points_los) ##  
    Prev.Year.Points.Defense.Los=rbind(Prev.Year.Points.Defense.Los,def_points_los) ## 
    
    
    Prev.Year.Turn.Over.Offense.Win=rbind(Prev.Year.Turn.Over.Offense.Win,off_turnov_win) ##  
    Prev.Year.Turn.Over.Defense.Win=rbind(Prev.Year.Turn.Over.Defense.Win,def_turnov_win) ## 
    Prev.Year.Turn.Over.Offense.Los=rbind(Prev.Year.Turn.Over.Offense.Los,off_turnov_los) ##  
    Prev.Year.Turn.Over.Defense.Los=rbind(Prev.Year.Turn.Over.Defense.Los,def_turnov_los) ## 
    
    
    Prev.Year.Yards.Offense.Win=rbind(Prev.Year.Yards.Offense.Win,off_yards_win) ##  
    Prev.Year.Yards.Defense.Win=rbind(Prev.Year.Yards.Defense.Win,def_yards_win) ## 
    Prev.Year.Yards.Offense.Los=rbind(Prev.Year.Yards.Offense.Los,off_yards_los) ##  
    Prev.Year.Yards.Defense.Los=rbind(Prev.Year.Yards.Defense.Los,def_yards_los) ## 
  }
  
  row.names(Prev.Year.Points.Offense.Win) <- NULL 
  row.names(Prev.Year.Points.Defense.Win) <- NULL 
  row.names(Prev.Year.Points.Offense.Los) <- NULL 
  row.names(Prev.Year.Points.Defense.Los) <- NULL 
  
  row.names(Prev.Year.Turn.Over.Offense.Win) <- NULL 
  row.names(Prev.Year.Turn.Over.Defense.Win) <- NULL 
  row.names(Prev.Year.Turn.Over.Offense.Los) <- NULL 
  row.names(Prev.Year.Turn.Over.Defense.Los) <- NULL 
  
  row.names(Prev.Year.Yards.Offense.Win) <- NULL 
  row.names(Prev.Year.Yards.Defense.Win) <- NULL 
  row.names(Prev.Year.Yards.Offense.Los) <- NULL 
  row.names(Prev.Year.Yards.Defense.Los) <- NULL 
  
  tmp1=cbind(weeklystats_11[[y]],Prev.Year.Points.Offense.Win,Prev.Year.Points.Defense.Win,Prev.Year.Points.Offense.Los ,Prev.Year.Points.Defense.Los,
             Prev.Year.Turn.Over.Offense.Win,Prev.Year.Turn.Over.Defense.Win,Prev.Year.Turn.Over.Offense.Los ,Prev.Year.Turn.Over.Defense.Los,
             Prev.Year.Yards.Offense.Win,Prev.Year.Yards.Defense.Win,Prev.Year.Yards.Offense.Los ,Prev.Year.Yards.Defense.Los)
  weeklystats_12[y]<-list(tmp1)
  
}



## 3) Determine Current Season Past Points scored  (13)
weeklystats_13<-list()
for(y in 1:12){
  Curr.Sea.Points.Offense.Win=vector() ##  
  Curr.Sea.Points.Defense.Win=vector() ## 
  Curr.Sea.Points.Offense.Los=vector() ##  
  Curr.Sea.Points.Defense.Los=vector() ##
  
  Curr.Sea.Turn.Over.Offense.Win=vector() ##  
  Curr.Sea.Turn.Over.Defense.Win=vector() ## 
  Curr.Sea.Turn.Over.Offense.Los=vector() ##  
  Curr.Sea.Turn.Over.Defense.Los=vector() ##
  
  Curr.Sea.Yards.Offense.Win=vector() ##  
  Curr.Sea.Yards.Defense.Win=vector() ## 
  Curr.Sea.Yards.Offense.Los=vector() ##  
  Curr.Sea.Yards.Defense.Los=vector() ##
  
  
  for(w in 1:17){
    year1_gams=weeklystats_11[[y]][,c(1,5,11)]
    
    win_teams=as.character(subset(year1_gams,Week==w)[,2])
    los_teams=as.character(subset(year1_gams,Week==w)[,3])
    
    sea_off_points=cbind(0,off_points_all_sum[[y]])
    sea_def_points=cbind(0,def_points_all_sum[[y]])
    
    sea_off_turnov=cbind(0,off_turnov_all_sum[[y]])
    sea_def_turnov=cbind(0,def_turnov_all_sum[[y]])
    
    sea_off_yards=cbind(0,off_yards_all_sum[[y]])
    sea_def_yards=cbind(0,def_yards_all_sum[[y]])
    
    
    off_points_win=as.matrix(sea_off_points[win_teams,w])
    def_points_win=as.matrix(sea_def_points[win_teams,w])
    off_points_los=as.matrix(sea_off_points[los_teams,w])
    def_points_los=as.matrix(sea_def_points[los_teams,w])
    
      
    off_turnov_win=as.matrix(sea_off_turnov[win_teams,w])
    def_turnov_win=as.matrix(sea_def_turnov[win_teams,w])
    off_turnov_los=as.matrix(sea_off_turnov[los_teams,w])
    def_turnov_los=as.matrix(sea_def_turnov[los_teams,w])
    
    
    
    off_yards_win=as.matrix(sea_off_yards[win_teams,w])
    def_yards_win=as.matrix(sea_def_yards[win_teams,w])
    off_yards_los=as.matrix(sea_off_yards[los_teams,w])
    def_yards_los=as.matrix(sea_def_yards[los_teams,w])
    
    
    Curr.Sea.Points.Offense.Win=rbind(Curr.Sea.Points.Offense.Win,off_points_win) ##  
    Curr.Sea.Points.Defense.Win=rbind(Curr.Sea.Points.Defense.Win,def_points_win) ## 
    Curr.Sea.Points.Offense.Los=rbind(Curr.Sea.Points.Offense.Los,off_points_los) ##  
    Curr.Sea.Points.Defense.Los=rbind(Curr.Sea.Points.Defense.Los,def_points_los) ## 
    
    
    Curr.Sea.Turn.Over.Offense.Win=rbind(Curr.Sea.Turn.Over.Offense.Win,off_turnov_win) ##  
    Curr.Sea.Turn.Over.Defense.Win=rbind(Curr.Sea.Turn.Over.Defense.Win,def_turnov_win) ## 
    Curr.Sea.Turn.Over.Offense.Los=rbind(Curr.Sea.Turn.Over.Offense.Los,off_turnov_los) ##  
    Curr.Sea.Turn.Over.Defense.Los=rbind(Curr.Sea.Turn.Over.Defense.Los,def_turnov_los) ## 
    
    Curr.Sea.Yards.Offense.Win=rbind(Curr.Sea.Yards.Offense.Win,off_yards_win) ##  
    Curr.Sea.Yards.Defense.Win=rbind(Curr.Sea.Yards.Defense.Win,def_yards_win) ## 
    Curr.Sea.Yards.Offense.Los=rbind(Curr.Sea.Yards.Offense.Los,off_yards_los) ##  
    Curr.Sea.Yards.Defense.Los=rbind(Curr.Sea.Yards.Defense.Los,def_yards_los) ## 
  }
  
  row.names(Curr.Sea.Points.Offense.Win) <- NULL 
  row.names(Curr.Sea.Points.Defense.Win) <- NULL 
  row.names(Curr.Sea.Points.Offense.Los) <- NULL 
  row.names(Curr.Sea.Points.Defense.Los) <- NULL 
  
  
  row.names(Curr.Sea.Turn.Over.Offense.Win) <- NULL 
  row.names(Curr.Sea.Turn.Over.Defense.Win) <- NULL 
  row.names(Curr.Sea.Turn.Over.Offense.Los) <- NULL 
  row.names(Curr.Sea.Turn.Over.Defense.Los) <- NULL 
  
  
  row.names(Curr.Sea.Yards.Offense.Win) <- NULL 
  row.names(Curr.Sea.Yards.Defense.Win) <- NULL 
  row.names(Curr.Sea.Yards.Offense.Los) <- NULL 
  row.names(Curr.Sea.Yards.Defense.Los) <- NULL 
  
  tmp1=cbind(weeklystats_12[[y]],Curr.Sea.Points.Offense.Win,Curr.Sea.Points.Defense.Win,Curr.Sea.Points.Offense.Los,Curr.Sea.Points.Defense.Los,
             Curr.Sea.Turn.Over.Offense.Win,Curr.Sea.Turn.Over.Defense.Win,Curr.Sea.Turn.Over.Offense.Los,Curr.Sea.Turn.Over.Defense.Los,
             Curr.Sea.Yards.Offense.Win,Curr.Sea.Yards.Defense.Win,Curr.Sea.Yards.Offense.Los,Curr.Sea.Yards.Defense.Los)
  weeklystats_13[y]<-list(tmp1)
  
}

## 4) Determine Previous Game Points scored  (14)
weeklystats_14<-list()
for(y in 1:12){
  Past.Game.Points.Offense.Win=vector() ##  
  Past.Game.Points.Defense.Win=vector() ## 
  Past.Game.Points.Offense.Los=vector() ##  
  Past.Game.Points.Defense.Los=vector() ##
  
  Past.Game.Turn.Over.Offense.Win=vector() ##  
  Past.Game.Turn.Over.Defense.Win=vector() ## 
  Past.Game.Turn.Over.Offense.Los=vector() ##  
  Past.Game.Turn.Over.Defense.Los=vector() ##
  
  Past.Game.Yards.Offense.Win=vector() ##  
  Past.Game.Yards.Defense.Win=vector() ## 
  Past.Game.Yards.Offense.Los=vector() ##  
  Past.Game.Yards.Defense.Los=vector() ##
  
  for(w in 1:17){
    
    year1_gams=weeklystats_11[[y]][,c(1,5,11)]
    
    win_teams=as.character(subset(year1_gams,Week==w)[,2])
    los_teams=as.character(subset(year1_gams,Week==w)[,3])
    
    sea_off_points=cbind(0,past_games_point_off[[y]])
    sea_def_points=cbind(0,past_games_point_def[[y]])
    
    sea_off_turnov=cbind(0,past_games_turnov_off[[y]])
    sea_def_turnov=cbind(0,past_games_turnov_def[[y]])
    
    sea_off_yards=cbind(0,past_games_yards_off[[y]])
    sea_def_yards=cbind(0,past_games_yards_def[[y]])
    
    
    
    
    off_points_win=as.matrix(sea_off_points[win_teams,w])
    def_points_win=as.matrix(sea_def_points[win_teams,w])
    off_points_los=as.matrix(sea_off_points[los_teams,w])
    def_points_los=as.matrix(sea_def_points[los_teams,w])
    
    
    off_turnov_win=as.matrix(sea_off_turnov[win_teams,w])
    def_turnov_win=as.matrix(sea_def_turnov[win_teams,w])
    off_turnov_los=as.matrix(sea_off_turnov[los_teams,w])
    def_turnov_los=as.matrix(sea_def_turnov[los_teams,w])
    
    
    off_yards_win=as.matrix(sea_off_yards[win_teams,w])
    def_yards_win=as.matrix(sea_def_yards[win_teams,w])
    off_yards_los=as.matrix(sea_off_yards[los_teams,w])
    def_yards_los=as.matrix(sea_def_yards[los_teams,w])
    
    Past.Game.Points.Offense.Win=rbind(Past.Game.Points.Offense.Win,off_points_win) ##  
    Past.Game.Points.Defense.Win=rbind(Past.Game.Points.Defense.Win,def_points_win) ## 
    Past.Game.Points.Offense.Los=rbind(Past.Game.Points.Offense.Los,off_points_los) ##  
    Past.Game.Points.Defense.Los=rbind(Past.Game.Points.Defense.Los,def_points_los) ## 
    
    Past.Game.Turn.Over.Offense.Win=rbind(Past.Game.Turn.Over.Offense.Win,off_turnov_win) ##  
    Past.Game.Turn.Over.Defense.Win=rbind(Past.Game.Turn.Over.Defense.Win,def_turnov_win) ## 
    Past.Game.Turn.Over.Offense.Los=rbind(Past.Game.Turn.Over.Offense.Los,off_turnov_los) ##  
    Past.Game.Turn.Over.Defense.Los=rbind(Past.Game.Turn.Over.Defense.Los,def_turnov_los) ## 
    
    Past.Game.Yards.Offense.Win=rbind(Past.Game.Yards.Offense.Win,off_yards_win) ##  
    Past.Game.Yards.Defense.Win=rbind(Past.Game.Yards.Defense.Win,def_yards_win) ## 
    Past.Game.Yards.Offense.Los=rbind(Past.Game.Yards.Offense.Los,off_yards_los) ##  
    Past.Game.Yards.Defense.Los=rbind(Past.Game.Yards.Defense.Los,def_yards_los) ## 
  }
  
  row.names(Past.Game.Points.Offense.Win) <- NULL 
  row.names(Past.Game.Points.Defense.Win) <- NULL 
  row.names(Past.Game.Points.Offense.Los) <- NULL 
  row.names(Past.Game.Points.Defense.Los) <- NULL 
  
  row.names(Past.Game.Turn.Over.Offense.Win) <- NULL 
  row.names(Past.Game.Turn.Over.Defense.Win) <- NULL 
  row.names(Past.Game.Turn.Over.Offense.Los) <- NULL 
  row.names(Past.Game.Turn.Over.Defense.Los) <- NULL
  
  row.names(Past.Game.Yards.Offense.Win) <- NULL 
  row.names(Past.Game.Yards.Defense.Win) <- NULL 
  row.names(Past.Game.Yards.Offense.Los) <- NULL 
  row.names(Past.Game.Yards.Defense.Los) <- NULL
  
  tmp1=cbind(weeklystats_13[[y]],Past.Game.Points.Offense.Win,Past.Game.Points.Defense.Win,Past.Game.Points.Offense.Los,Past.Game.Points.Defense.Los,
             Past.Game.Turn.Over.Offense.Win,Past.Game.Turn.Over.Defense.Win,Past.Game.Turn.Over.Offense.Los,Past.Game.Turn.Over.Defense.Los,
             Past.Game.Yards.Offense.Win,Past.Game.Yards.Defense.Win,Past.Game.Yards.Offense.Los,Past.Game.Yards.Defense.Los)
  weeklystats_14[y]<-list(tmp1)
  
}

dim(weeklystats_14[[1]])

## ----------------------------------------------
## Win analysis




## Step 1) Determin Wins, Losses and Byes for each team 
games_all=vector()
for(y in 1:12){
  games_oney=vector()
  for(g in 1:17){
    week_year=as.matrix(which(weeklystats_9[,1]==g & weeklystats_9[,4]==year[y,1]))
    tmp_win_loss=weeklystats_9[week_year,c(5,11)]
    tic=win_loss_determin(tmp_win_loss,teams_names)
    games_oney=cbind(games_oney,tic)
  }
  games_all=cbind(games_all,games_oney)
}
rownames(games_all)<-teams_names ## Put Team Names on 

## Step 1) Make Wins and Losses 1,0s and make Byes NAs
wins_all=ifelse(games_all=="W",1,0)
loss_all=ifelse(games_all=="L",2,0)
not_bye=wins_all+loss_all
gam_win=ifelse(not_bye==0,NA,not_bye)
gam_win_los=ifelse(gam_win==2,0,gam_win)


## Step 3) Determine Wins by season and make it into a list
By.Season.Win=list()
for(i in 1:12 ){
  tic=(17*i)
  gam_thr=(tic-16):tic
  game_year=gam_win_los[,gam_thr]
  tmp=row_csum(game_year)
  By.Season.Win[13-i]<-list(tmp)
  #names(By.Season.Win)[i]<-year[i,]
}


#####
### BIG 2) ****** Start Review
#####


##  Step 4) Determine Previous Wins by Year (15)
weeklystats_15<-list()
for(y in 1:12){
  Previous.Win.RecordY=vector() ## previous teams total wins (winning) ## Year 
  Previous.Los.RecordY=vector() ## previous teams total wins (lossing) ## Year
  
  for(w in 1:17){
    year1_gams=weeklystats_14[[y]][,c(1,5,11)]
    wins_lost_year1=cbind(0,By.Season.Win[[y]])[,1:17]
    win_teams_ply=as.character(subset(year1_gams,Week==w)[,2])
    los_teams_ply=as.character(subset(year1_gams,Week==w)[,3])
    rec_prev_w=as.matrix(wins_lost_year1[win_teams_ply,w])
    rec_prev_l=as.matrix(wins_lost_year1[los_teams_ply,w])
    Previous.Win.RecordY=rbind(Previous.Win.RecordY,rec_prev_w) ## previous teams total wins (winning)
    Previous.Los.RecordY=rbind(Previous.Los.RecordY,rec_prev_l) ## previous teams total wins (lossing)
  }
  
  row.names(Previous.Win.RecordY) <- NULL 
  row.names(Previous.Los.RecordY) <- NULL 
  tmp1=cbind(weeklystats_14[[y]],Previous.Win.RecordY,Previous.Los.RecordY)
  weeklystats_15[y]<-list(tmp1)
}




## Step 5) Find Previous Wins from past years
all_games_years=vector()
for(i in 1:12){
  tmpwin=By.Season.Win[[i]][,17]
  all_games_years=cbind(all_games_years,tmpwin)
}
game_winsall_year=cbind(0,row_csum(all_games_years)) ## Assume we do not know anything before 2002




##  Step 6) Determine Previous Wins by Year (16)
weeklystats_16<-list()
for(y in 1:12){
  Previous.Win.Record.All=vector() ## previous teams total wins (winning) ## Year 
  Previous.Los.Record.All=vector() ## previous teams total wins (lossing) ## Year
  
  for(w in 1:17){
    year1_gams=weeklystats_15[[y]][,c(1,5,11)]
    wins_lost_years=as.matrix(game_winsall_year[,y])
    win_teams_ply=as.character(subset(year1_gams,Week==w)[,2])
    los_teams_ply=as.character(subset(year1_gams,Week==w)[,3])
    rec_prev_ws=as.matrix(wins_lost_years[win_teams_ply,])
    rec_prev_ls=as.matrix(wins_lost_years[los_teams_ply,])
    Previous.Win.Record.All=rbind(Previous.Win.Record.All,rec_prev_ws) ## previous teams total wins (winning)
    Previous.Los.Record.All=rbind(Previous.Los.Record.All,rec_prev_ls) ## previous teams total wins (lossing)
  }
  
  row.names(Previous.Win.Record.All) <- NULL 
  row.names(Previous.Los.Record.All) <- NULL 
  tmp1=cbind(weeklystats_15[[y]],Previous.Win.Record.All,Previous.Los.Record.All)
  weeklystats_16[y]<-list(tmp1)
}


## Step 7) Get Previous Year's Win
prev_year_game=vector()
for(i in 1:12){
  tmp_p=as.matrix(By.Season.Win[[i]][,17])
  prev_year_game=cbind(prev_year_game,tmp_p)
}

prev_year_game=cbind(0,prev_year_game)


## Step 8) Previous Total wins (17)
weeklystats_17<-list()
for(y in 1:12){
  Previous.Win.Record.Season=vector() ## previous teams total wins (winning) ## Year 
  Previous.Los.Record.Season=vector() ## previous teams total wins (lossing) ## Year
  
  for(w in 1:17){
    year1_gams=weeklystats_16[[y]][,c(1,5,11)]
    wins_lost_years=as.matrix(prev_year_game[,y])
    win_teams_ply=as.character(subset(year1_gams,Week==w)[,2])
    los_teams_ply=as.character(subset(year1_gams,Week==w)[,3])
    rec_prev_ws=as.matrix(wins_lost_years[win_teams_ply,])
    rec_prev_ls=as.matrix(wins_lost_years[los_teams_ply,])
    Previous.Win.Record.Season=rbind(Previous.Win.Record.Season,rec_prev_ws) ## previous teams total wins (winning)
    Previous.Los.Record.Season=rbind(Previous.Los.Record.Season,rec_prev_ls) ## previous teams total wins (lossing)
  }
  
  row.names(Previous.Win.Record.Season) <- NULL 
  row.names(Previous.Los.Record.Season) <- NULL 
  tmp1=cbind(weeklystats_16[[y]],Previous.Win.Record.Season,Previous.Los.Record.Season)
  weeklystats_17[y]<-list(tmp1)
}


## 14 Make One Dataset
weeklystats_18=vector()
for(i in 1:12){
  tmp2=weeklystats_17[[i]]
  weeklystats_18=rbind(weeklystats_18,tmp2)
}


dim(weeklystats_18)


## Rearrange Data
# weeklystats_14<-subset(weeklystats_13, 
#                        select=c(Week,Day,Date,Year,Win.Team,
#                                 Win.Conference, Win.Division,
#                                 Points.Win,Turnovers.Win,
#                                 YardsGained.Win,Previous.Win.Record.All,
#                                 Previous.Win.RecordY,Lose.Team,Lose.Conference,
#                                 Lose.Division,Points.Lose,Turnovers.Lose,YardsGained.Lose,
#                                 Previous.Los.Record.All,Previous.Los.RecordY,Away.Team.Won))
# 
# 


# ## 15 Creation of new Variables
# Point.Difference=as.numeric(weeklystats_14[,8])-as.numeric(weeklystats_14[,16])
# All.Wins.Difference=weeklystats_14[,11]-weeklystats_14[,19]
# Seasonal.Wins.Difference=weeklystats_14[,12]-weeklystats_14[,20]
# Yards.Difference=as.numeric(weeklystats_14[,10])-as.numeric(weeklystats_14[,18])
# Turnover.Difference=as.numeric(weeklystats_14[,9])- as.numeric(weeklystats_14[,17])
# 
# weeklystats_15=cbind(weeklystats_14,Point.Difference,
# All.Wins.Difference,Seasonal.Wins.Difference,
# Yards.Difference,Turnover.Difference)
# 
# head(weeklystats_15)







# ## Fix Tie later
# tie=which(as.matrix(weeklystats_8[,8])==as.matrix(weeklystats_8[,14]))
# stats_tie=weeklystats_8[tie,]
# team_t=stats_tie[,5],
# which(teams_names[team_t,])
# games_all[team_t,]

