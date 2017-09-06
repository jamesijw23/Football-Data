splt_sea=function(twe){
  if(twe==8){tmp=c(3,3,2)} 
  else if(twe==9){tmp=c(3,3,3)} 
  else if(twe==10) { tmp=c(4,3,3)}
  else if(twe==11) { tmp=c(4,4,3)}
  else if(twe==12) { tmp=c(4,4,4)}
  else if(twe==13) { tmp=c(5,4,4)}
  else if(twe==14) { tmp=c(5,5,4)}
  else if(twe==15) { tmp=c(5,5,5)}
  else if(twe==16) { tmp=c(6,5,5)}
  return(tmp)
}



change.colname=function(tnn,sec){
  col.old <- names(tnn)
  col.new <- paste(col.old,sec,sep = "")
  tnn<-as.matrix(tnn)
  rownames(tnn)<- NULL
  rownames(tnn)<- col.new
  return(t(tnn))
  
}


get_dat=function(test_row){
  ## Choose Teams and How many games prior
  outcome_team <- as.character(ifelse(test_row[14]==1,test_row[3],test_row[4])[[1]])
  snc= ifelse(test_row[14]==1,test_row[12],test_row[13])[[1]]
  
  ## Choose Team from Data set
  data1a=subset(weeklystats_8,Win.Team==outcome_team | Lose.Team==outcome_team)              
  ## Choose Current Year and past two years
  data_cur=subset(data1a,Year==as.numeric(test_row[2]))
  data_prev_1=subset(data1a,Year==as.numeric(test_row[2]-1))
  data_prev_2=subset(data1a,Year==as.numeric(test_row[2]-2))
  
  ## Determine Seasons Outcomes Current Year and past two years
  Oc.Cur=ifelse(data_cur[,3]==outcome_team,1,0)
  Oc.Prev.1=ifelse(data_prev_1[,3]==outcome_team,1,0)
  Oc.Prev.2=ifelse(data_prev_2[,3]==outcome_team,1,0)
  
  
  
  
  ## Current Season
  ## a) Yards, Points and Turn Overs by Offense 
  Yards.Cur=as.numeric(ifelse(Oc.Cur==1,as.matrix(data_cur[,8]),as.matrix(data_cur[,10])))
  Poins.Cur=as.numeric(ifelse(Oc.Cur==1,as.matrix(data_cur[,6]),as.matrix(data_cur[,7])))
  Turns.Cur=as.numeric(ifelse(Oc.Cur==1,as.matrix(data_cur[,9]),as.matrix(data_cur[,11])))
  Poins.Op.Cur=as.numeric(ifelse(Oc.Cur==0,as.matrix(data_cur[,6]),as.matrix(data_cur[,7])))
  Poind.Cur=Poins.Cur-Poins.Op.Cur
  ## Combine all variables
  tmp1_cur=cbind(Yards.Cur,Poins.Cur,Turns.Cur,Poind.Cur,Oc.Cur)
  
  ## Break up Current Sections
  ## Split season into three parts
  spsc<-splt_sea(snc)
  sea_cur_sect1 <- colMeans(tmp1_cur[1:spsc[1],])
  sea_cur_sect1<-change.colname(sea_cur_sect1,1)
  sea_cur_sect2 <- colMeans(tmp1_cur[(spsc[1]+1):(spsc[1]+spsc[2]),])
  sea_cur_sect2<-change.colname(sea_cur_sect2,2)
  sea_cur_sect3 <- colMeans(tmp1_cur[(spsc[1]+spsc[2]+1):(spsc[1]+spsc[2]+spsc[3]),])
  sea_cur_sect3<-change.colname(sea_cur_sect3,3)
  
  ## Combine all sections into 1 row
  Cur.Se <- cbind(sea_cur_sect1,
                  sea_cur_sect2,
                  sea_cur_sect3)
  
  ## Previous 1 Season
  ## a) Yards, Points and Turn Overs by Offense 
  Yards.Prev.1=as.numeric(ifelse(Oc.Prev.1==1,as.matrix(data_prev_1[,8]),as.matrix(data_prev_1[,10])))
  Poins.Prev.1=as.numeric(ifelse(Oc.Prev.1==1,as.matrix(data_prev_1[,6]),as.matrix(data_prev_1[,7])))
  Turns.Prev.1=as.numeric(ifelse(Oc.Prev.1==1,as.matrix(data_prev_1[,9]),as.matrix(data_prev_1[,11])))
  Poins.Op.Prev.1=as.numeric(ifelse(Oc.Prev.1==0,as.matrix(data_prev_1[,6]),as.matrix(data_prev_1[,7])))
  Poind.Prev.1=Poins.Prev.1-Poins.Op.Prev.1
  ## Combine all variables
  tmp1.p1=cbind(Yards.Prev.1,Poins.Prev.1,Turns.Prev.1,Poind.Prev.1,Oc.Prev.1)
  
  ## Break up Previous 1 Season Sections
  ## Split season into three parts
  spsa<-splt_sea(16)
  sea_prev.1_sect1 <- colMeans(tmp1.p1[1:spsa[1],])
  sea_prev.1_sect1<-change.colname(sea_prev.1_sect1,1)
  sea_prev.1_sect2 <- colMeans(tmp1.p1[(spsa[1]+1):(spsa[1]+spsa[2]),])
  sea_prev.1_sect2<-change.colname(sea_prev.1_sect2,2)
  sea_prev.1_sect3 <- colMeans(tmp1.p1[(spsa[1]+spsa[2]+1):(spsa[1]+spsa[2]+spsa[3]),])
  sea_prev.1_sect3<-change.colname(sea_prev.1_sect3,3)
  
  ## Combine all sections into 1 row
  Prev.1.Se <- cbind(sea_prev.1_sect1,
                     sea_prev.1_sect2,
                     sea_prev.1_sect3)
  
  
  ## Previous 2 Season
  ## a) Yards, Points and Turn Overs by Offense 
  Yards.Prev.2=as.numeric(ifelse(Oc.Prev.2==1,as.matrix(data_prev_2[,8]),as.matrix(data_prev_2[,10])))
  Poins.Prev.2=as.numeric(ifelse(Oc.Prev.2==1,as.matrix(data_prev_2[,6]),as.matrix(data_prev_2[,7])))
  Turns.Prev.2=as.numeric(ifelse(Oc.Prev.2==1,as.matrix(data_prev_2[,9]),as.matrix(data_prev_2[,11])))
  Poins.Op.Prev.2=as.numeric(ifelse(Oc.Prev.2==0,as.matrix(data_prev_2[,6]),as.matrix(data_prev_2[,7])))
  Poind.Prev.2=Poins.Prev.2-Poins.Op.Prev.2
  ## Combine all variables
  tmp1.p2=cbind(Yards.Prev.2,Poins.Prev.2,Turns.Prev.2,Poind.Prev.2,Oc.Prev.2)
  
  ## Break up Previous 2 Season Sections
  ## Split season into three parts
  spsa<-splt_sea(16)
  sea_prev.2_sect1 <- colMeans(tmp1.p2[1:spsa[1],])
  sea_prev.2_sect1<-change.colname(sea_prev.2_sect1,1)
  sea_prev.2_sect2 <- colMeans(tmp1.p2[(spsa[1]+1):(spsa[1]+spsa[2]),])
  sea_prev.2_sect2<-change.colname(sea_prev.2_sect2,2)
  sea_prev.2_sect3 <- colMeans(tmp1.p2[(spsa[1]+spsa[2]+1):(spsa[1]+spsa[2]+spsa[3]),])
  sea_prev.2_sect3<-change.colname(sea_prev.2_sect3,3)
  
  ## Combine all sections into 2 row
  Prev.2.Se <- cbind(sea_prev.2_sect1,
                     sea_prev.2_sect2,
                     sea_prev.2_sect3)
  
  all.stats <- cbind(Cur.Se,Prev.1.Se,Prev.2.Se)
  
  return(all.stats)
}
