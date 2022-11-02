#Scrap teams all game from all teams in period 2012 - 2022 
# Each team has its own page for each year
# this page is saved in data/1.lookups/teams_urls.csv
#Thus, go to each page and extract the data
# the output is saved in data/2.scrapped/all_competitions.csv

library(httr)
library(rvest)
library(tibble)
library(rio)
library(dplyr)



#define parameters -----------------------------------------------------------
indir <- "data/1.lookups"
infile <- file.path(indir, "teams_urls.csv")



exdir <- "data/2.scrapped/fbref"
exfile <- file.path(exdir,"WC_matches.csv")


#read urls of teams -----------------------------------------------------------
teams_urls <- import(infile)

#define years of query
years <- as.character(2022)

teams_urls$url

#run the function for each country in the teams_url dataset (this dataset is 
#created in 1.Scrap_lookup_teams.R)

read_teams <- lapply(1:nrow(teams_urls), function(i){
  
  url <- teams_urls$url[i]
  team <-teams_urls$team[i]
  
  message(team)
  
  website <- read_html(url)
  
  #Fixture (all matches in that year)-------------------------------------------------
  fixture <- website %>%
    html_element("#matchlogs_for") %>%
    html_table() %>%
    mutate(team = team, 
           qatar = TRUE) %>%
    relocate(team) %>%
    filter(Comp == "World Cup") %>%
    select(-c(`Match Report`, Poss, Notes, Formation)) 
})
  
 

matches_wc <- do.call(rbind, read_teams) 
  



#export

export(matches_wc,exfile)
