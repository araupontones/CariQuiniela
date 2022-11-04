#Scrap teams 2022 games
# Each team has its own page
# this page is saved in data/1.lookups/teams_urls.csv
#Thus, go to each page and extract the data

library(httr)
library(rvest)
library(tibble)
library(rio)
library(dplyr)



#define parameters -----------------------------------------------------------
indir <- "data/1.lookups"
infile <- file.path(exdir, "teams_urls.csv")

exdir <- "data/2.scrapped"
exfile <- file.path(exdir,"2022_All_competitions.csv")


#read urls of teams -----------------------------------------------------------
teams_urls <- import(infile)



read_teams <- lapply(1:nrow(teams_urls), function(i){
  
  url <- teams_urls$url[i]
  team <-teams_urls$team[i]
  message(team)
  
  website <- read_html(url)
  
  #Fixture (all matches in 2022)-------------------------------------------------
  fixture <- website %>%
    html_element("#matchlogs_for") %>%
    html_table() 
  
  
  #get links to other tables ----------------------------------------------------
  # Apart from fixture, there are three tables with relevant indicators
  # misc, shooting and keeper
  #lets get the link to those
  links_to_tables <- website %>%
    html_elements("a") %>%
    html_attr("href") 
  
  all_comps <- links_to_tables[grepl("all_comps", links_to_tables)] %>% unique() 
  url_stats <- paste0("https://fbref.com/",all_comps[grepl("misc|shooting|keeper", all_comps)])
  
  
  
  #get the tables
  
  lista_tablas <- lapply(url_stats, function(x){
    
    message(x)
    
    #get the table
    table_stats <- read_html(x) %>%
      html_element("#matchlogs_for") %>%
      html_table()
    
    
    #make the first row the table names
    names(table_stats) <- as.character(table_stats[1,])
    print(names(table_stats))
    
    #now get rid of the first row and delete redundant variables
    #this variables are already in fixture
    table_stats_final <- table_stats[-1, ] %>%
      select(-c(Round,	Day, Time, Comp,	Venue,	Result, GA, GF, `Match Report`))
    
    
    
    
  })
  
  #join all tables
  data_tablas <- plyr::join_all(lista_tablas, by = c("Date", "Opponent")) %>%
    select(-PKatt)
  
  #join with fixture
  final_tabla <- fixture %>%
    left_join(data_tablas, by = c("Date", "Opponent")) %>%
    mutate(team = team) %>%
    relocate(team)
  
  
  
  
})


data_Teams <- do.call(rbind, read_teams)

export(data_Teams, exfile)





