rm(list=ls(all=TRUE))
setwd("C:\\Users\\Jabez James\\Desktop\\Football")
library(XML)
library(RCurl)
weeklystats = as.data.frame(matrix(ncol = 14))  # Initializing our empty dataframe

names(weeklystats) = c("Week", "Day", "Date", "Blank", "Win.Team", "At", "Lose.Team", 
                       "Points.Win", "Points.Lose", "YardsGained.Win", "Turnovers.Win", "YardsGained.Lose", 
                       "Turnovers.Lose", "Year")  # Naming columns

URLpart1 = "http://www.pro-football-reference.com/years/"
URLpart3 = "/games.htm"

#### Our workhorse function ####

getData = function(URLpart1, URLpart3) {
  for (i in 2002:2013) {
    URL = paste(URLpart1, as.character(i), URLpart3, sep = "")
    tablefromURL = readHTMLTable(URL)
    table = tablefromURL[[1]]
    names(table) = c("Week", "Day", "Date", "Blank", "Win.Team", "At", "Lose.Team", 
                     "Points.Win", "Points.Lose", "YardsGained.Win", "Turnovers.Win", 
                     "YardsGained.Lose", "Turnovers.Lose")
    table$Year = i  # Inserting a value for the year 
    weeklystats = rbind(table, weeklystats)  # Appending happening here
  }
  return(weeklystats)
}
weeklystats = getData(URLpart1, URLpart3)  # Calling on our workhorse to do its job and saving the raw data results in weeklystats
save(weeklystats, file = "rawweeklystats.rda")
