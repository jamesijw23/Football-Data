library(XML)
library(plyr)


URLpart1 = "http://www.pro-football-reference.com/years/"

all_summ_data=vector()

for(i in 2002:2013){
  
  URL = paste(URLpart1, as.character(i), sep = "")
  tablefromURL = readHTMLTable(URL)
  
  ## colnames(tablefromURL$team_stats) 
  ## New Column Names
  new_TO_cnam=c("Rk_TO","Tm","G","Pts_TO","Yds_TOT","Ply_TO","Y_P_TO","TO_TO",
                "FL_TO","1stPy_TOT","1stD_TOT","Cmp_TO","Att_TOP","Yds_TOP","TD_TOP",
                "Int_TO","NY_A_TO","1stD_TOP","Att_TOR","Yds_TOR","TD_TOR","Y_A_TO",
                "1stD_TOR","Sc_per_TO","TO_per_TO","EXP_TO")
  ## Get Correct Data and Remove average row
  tmp1=tablefromURL$team_stats[-33,]
  ## Change the column names
  colnames(tmp1)<-new_TO_cnam
  
  
  
  
  
  #colnames(tablefromURL$passing)
  new_PAS_cnam=c("Rk_PAS","Tm","G_PAS","Cmp_PAS","Att_PAS","Cmp_per_PAS","Yds_PAS",
                 "TD_PAS","TD_per_PAS","Int_PAS","Int_per_PAS","Lng_PAS","Y_A_PASs",
                 "AY_A_PAS","Y_C_PAS","Y_G_PAS","Rate_PAS","QBR_PAS","Sk_PAS","Yds_PAS",
                 "NY_A_PAS","ANY_A_PAS","Sk_per_PAS","4QC_PAS","GWD_PAS","EXP_PAS")
  ## Get Correct Data and Remove average row
  tmp2=tablefromURL$passing[-33,]
  ## Change the column names
  colnames(tmp2)<-new_PAS_cnam
  
  
  
  #colnames(tablefromURL$rushing)
  new_RUS_cnam=c("Rk_RUS","Tm","G_RUS","Att_RUS","Yds_RUS","TD_RUS",
                 "Lng_RUS","Y_A_RUS","Y_G_RUS","Fmb_RUS","EXP_RUS")
  ## Get Correct Data and Remove average row
  tmp3=tablefromURL$rushing[-33,]
  ## Change the column names
  colnames(tmp3)<-new_RUS_cnam
  
  
  
  
  colnames(tablefromURL$returns)
  new_RETK_cnam=c("Rk_RETK","Tm","G_RETK","Ret_RETK","Yds_RETK","TD_RETK",
                  "Lng_RETK","Y_R_RETK","Rt_RETK","Yds_RETK","TD_RETK","Lng_RETK",
                  "Y_Rt_RETK", "APYd_RETK")
  ## Get Correct Data and Remove average row
  tmp4=tablefromURL$returns[-33,]
  ## Change the column names
  colnames(tmp4)<-new_RETK_cnam
  
  
  
  
  #colnames(tablefromURL$kicking)
  new_FG_cnam=c("Rk_FG","Tm","G_FG","FGA_FG","FGM_FG","FGA_FG","FGM_FG","FGA_FG","FGM_FG", 
                "FGA_FG","FGM_FG","FGA_FG","FGM_FG","FGA_FG","FGM_FG","FG_per_FG","XPA_FG","XPM_FG", 
                "XP_per_FG","Pnt_FG","Yds_FG","Lng_FG","Blck_FG","Y_P_FG")
  ## Get Correct Data and Remove average row
  tmp5=tablefromURL$kicking[-33,]
  ## Change the column names
  colnames(tmp5)<-new_FG_cnam
  
  
  
  
  
  #colnames(tablefromURL$team_scoring)
  new_TS_cnam=c("Rk_TS","Tm","G_TS","RshTD_TS","RecTD_TS","PR_TD_TS","KR_TD_TS","FblTD_TS",
                "IntTD_TS","OthTD_TS","AllTD_TS","2PM_TS","XPM_TS","FGM_TS","Sfty_TS","Pts_TS",  
                "Pts_G_TS")
  tmp6=tablefromURL$team_scoring[-33,]
  ## Change the column names
  colnames(tmp6)<-new_TS_cnam
  
  
  
  
  #colnames(tablefromURL$drives)
  new_DR_cnam<-c("Rk_DR","Tm","G_DR","num_Dr_DR","Plays_DR","Sc_per_DR","TO_per_DR","Plays_DR",
                 "Yds_DR","Start_DR","Time_DR","Pts_DR") 
  tmp7=tablefromURL$drives[-33,]
  ## Change the column names
  colnames(tmp7)<-new_DR_cnam
  
  
  
  
  tmp_all=join_all(list(tmp1,tmp2,tmp3,tmp4,tmp5,tmp6,tmp7), by = 'Tm', type = 'full')
  
  
  
  
  
  #   
  #   tmp_all[, "XP_per_FG"]<-as.numeric(sub("%", "", tmp_all$XP_per_FG))
  #   tmp_all[, "FG_per_FG"]<-as.numeric(sub("%", "", tmp_all$FG_per_FG))
  #   tmp_all[, "Start_DR"]<-as.numeric(sub("Own ", "", tmp_all$Start_DR))
  #   
  #   tmp_all[, "Time_DR"]<-sapply(strsplit(as.character(tmp_all$Time_DR),":"),
  #                                function(x) {x <- as.numeric(x);x[1]+x[2]/60})
  
  #tmp_all<noquote(ifelse(tmp_all=="",NA,tmp_all))
  #rownames(tmp_all)<-tmp_all[,2]
  
  tmp_all=cbind(tmp_all,i)
  all_summ_data=rbind(all_summ_data,tmp_all)
}


## Goal: 384 x 109, no repeating columns and all numbers
all_summ_data[, "Time_DR"]<-sapply(strsplit(as.character(all_summ_data$Time_DR),":"),
                                   function(x) {x <- as.numeric(x);x[1]+x[2]/60})
all_summ_data[, "Start_DR"]<-as.numeric(sub("Own ", "", all_summ_data$Start_DR))
all_summ_data[, "XP_per_FG"]<-sapply(strsplit(as.character(all_summ_data$XP_per_FG),"%"),
                                     function(x) {x <- as.numeric(x);x[1]})
all_summ_data[, "FG_per_FG"]<-sapply(strsplit(as.character(all_summ_data$FG_per_FG),"%"),
                                     function(x) {x <- as.numeric(x);x[1]})

colnames(all_summ_data)[110]<-"Year"
tte=as.matrix(all_summ_data)
rownames(tte)<-as.character(all_summ_data[,2])
save(tte, file = "previousSTATS.rda")
