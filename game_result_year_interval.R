library(tidyverse)
library(rvest)


#This will make a data table of game results for team ("t" as a string) in a input interval of year from n to m.

Game_level_data_function <- function(n,m,t){
  b <- data_frame(NULL)
  for(i in n:m){
    
    #get game date data and put it into YYYY-MM-DD format
    a <- read_html(paste("https://www.baseball-reference.com/teams/",t,"/",i,"-schedule-scores.shtml#all_team_schedule", sep = "")) %>% html_nodes("a[title^=Click]") %>% html_text()
    a <- paste(a, i, sep = " ")
    a <- as.Date(a, "%A, %b %d %Y")
    assign(paste(t,"_", i,"_date", sep = "" ), a)
    
    #Scrape teamID column from baseball reference for a given year (between n and m) and team (t)
    assign(paste(t,"_", i,"_teamID", sep = "" ), read_html(paste("https://www.baseball-reference.com/teams/",t,"/",i,"-schedule-scores.shtml#all_team_schedule", sep = "")) %>% html_nodes("td.left[data-stat=team_ID]") %>% html_text())
    
    #Scrape opponent column from baseball reference for a given year (between n and m) and team (t)
    assign(paste(t,"_", i,"_oppID", sep = "" ), read_html(paste("https://www.baseball-reference.com/teams/",t,"/",i,"-schedule-scores.shtml#all_team_schedule", sep = "")) %>% html_nodes("td.left[data-stat=opp_ID]") %>% html_text())
    
    #Scrape Run column from baseball reference for a given year (between n and m) and team (t)
    assign(paste(t,"_", i,"_R", sep = "" ), read_html(paste("https://www.baseball-reference.com/teams/",t,"/",i,"-schedule-scores.shtml#all_team_schedule", sep = "")) %>% html_nodes("td.right[data-stat=R]") %>% html_text())
    
    #Scrape Runs allowed column from baseball reference for a given year (between n and m) and team (t)
    assign(paste(t,"_", i,"_RA", sep = "" ), read_html(paste("https://www.baseball-reference.com/teams/",t,"/",i,"-schedule-scores.shtml#all_team_schedule", sep = "")) %>% html_nodes("td.right[data-stat=RA]") %>% html_text())
    
    #Scrape home or away column from baseball reference for a given year (between n and m) and team (t)
    assign(paste(t,"_", i,"_HomeorRoad", sep = "" ), read_html(paste("https://www.baseball-reference.com/teams/",t,"/",i,"-schedule-scores.shtml#all_team_schedule", sep = "")) %>% html_nodes("td.left[data-stat=homeORvis]") %>% html_text())
    
    #Scrape Game result column from baseball reference for a given year (between n and m) and team (t)
    assign(paste(t,"_", i,"_Result", sep = "" ), read_html(paste("https://www.baseball-reference.com/teams/",t,"/",i,"-schedule-scores.shtml#all_team_schedule", sep = "")) %>% html_nodes("td.left[data-stat=win_loss_result]") %>% html_text())
    
    #Scrape team record column from baseball reference for a given year (between n and m) and team (t)
    assign(paste(t,"_", i,"_Record", sep = "" ), read_html(paste("https://www.baseball-reference.com/teams/",t,"/",i,"-schedule-scores.shtml#all_team_schedule", sep = "")) %>% html_nodes("td.right[data-stat=win_loss_record]") %>% html_text())
    
    #Scrape W/L streak column from baseball reference for a given year (between n and m) and team (t)
    assign(paste(t,"_", i,"_Streak", sep = "" ), read_html(paste("https://www.baseball-reference.com/teams/",t,"/",i,"-schedule-scores.shtml#all_team_schedule", sep = "")) %>% html_nodes("td.left[data-stat=win_loss_streak]") %>% html_text())
    
    #create a data frame out of all columns scraped for a single team season
    assign(paste(t,"_", i,"_Game_Level", sep = ""), data_frame(Date = get(paste(t,"_", i,"_date", sep = "" )), Team = get(paste(t,"_", i,"_teamID", sep = "" )), Home_or_Road = get(paste(t,"_", i,"_HomeorRoad", sep = "" )), Opponent = get(paste(t,"_", i,"_oppID", sep = "" )), Result = get(paste(t,"_", i,"_Result", sep = "" )), R = get(paste(t,"_", i,"_R", sep = "" )), RA = get(paste(t,"_", i,"_RA", sep = "" )), Record = get(paste(t,"_", i,"_Record", sep = "" )), Streak = get(paste(t,"_", i,"_Streak", sep = "" ))))
    
    #combine data frame from year i to a single data frame which will span year n to m after the loop completes
    b <- rbind(b,get(paste(t,"_", i,"_Game_Level", sep = "")))
    
  }
  
  #save csv to working directory with appropriate name based on the team (t) and year range (n to m)
  write_csv(b,paste(t,"_", n,"_",m,"_Game_Level.csv", sep = ""))
  
}