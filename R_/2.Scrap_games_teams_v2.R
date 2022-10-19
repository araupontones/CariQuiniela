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
infile <- file.path(indir, "teams_urls.csv")

exdir <- "data/2.scrapped"
exfile <- file.path(exdir,"All_competitions.csv")


#read urls of teams -----------------------------------------------------------
teams_urls <- import(infile)
years <- as.character(seq(2012,2022,1))
                      


read_teams <- lapply(1:nrow(teams_urls), function(i){
  
  url <- teams_urls$url[i]
  team <-teams_urls$team[i]
  
 
  
    
  get_years <- lapply(years, function(y){
    message(paste(team, y))
    #define the url for that year
    url_year <- gsub("2022", y, url)
    
    #read the website
    website <- read_html(url_year)
    
    #Fixture (all matches in that year)-------------------------------------------------
      fixture <- website %>%
      html_element("#matchlogs_for") 
    
    
    if(length(fixture) > 0){
      
      fixture <- fixture %>%
        html_table() 
      #get links to other tables ----------------------------------------------------
      # Apart from fixture, there are three tables with relevant indicators
      # misc, shooting and keeper
      #lets get the link to those
      links_to_tables <- website %>%
        html_elements("a") %>%
        html_attr("href")
      # 
      all_comps <- links_to_tables[grepl("all_comps", links_to_tables)] %>% unique()
      url_stats <- paste0("https://fbref.com/",all_comps[grepl("misc|shooting|keeper", all_comps)])
      # 
      # 
      # 
      # #get the tables
      lista_tablas <- lapply(url_stats, function(x){

        #message(x)

        #get the table
        table_stats <- read_html(x) %>%
          html_element("#matchlogs_for") %>%
          html_table()


        #make the first row the table names

        #
        # #now get rid of the first row and delete redundant variables (duplicates)
        # #this variables are already in fixture
        new_names <- as.character(table_stats[1, ])
        df_names <- tibble(names = new_names) %>% group_by(names) %>% mutate(row = row_number()) %>% ungroup()



        table_stats_unique <- table_stats[ , df_names$row == 1]
        names(table_stats_unique) <- table_stats_unique[1,]



        table_stats_final <- table_stats_unique[-1, ] %>%
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

      
      
    } else {
      
      cli::cli_alert("No existe")
      NULL
    }
      
    })
      
    
    
  
  all_years <- do.call(plyr::rbind.fill, get_years)
  
  
  
})
  

all <- do.call(plyr::rbind.fill, read_teams)

#export
export(all,exfile)
