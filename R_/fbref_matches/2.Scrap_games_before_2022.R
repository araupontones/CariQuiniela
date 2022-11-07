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
exfile <- file.path(exdir,"matches_before_2022.csv")


#read urls of teams -----------------------------------------------------------
teams_urls <- import(infile)

#define years of query
years <- as.character(seq(2012,2021,1))
                      


#run the function for each country in the teams_url dataset (this dataset is 
#created in 1.Scrap_lookup_teams.R)

read_teams <- lapply(1:nrow(teams_urls), function(i){
  
  url <- teams_urls$url[i]
  team <-teams_urls$team[i]
  
 
  #Loop over the years
  get_years <- lapply(years, function(y){
    message(paste(team, y))
    
    #define the url for that year
    url_year <- gsub("2022", y, url)
    
    #read the website
    website <- read_html(url_year)
    
    #Fixture (all matches in that year)-------------------------------------------------
      fixture <- website %>%
      html_element("#matchlogs_for") 
    
    #only scrap if the data exists for this country in this year
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
       
      all_comps <- links_to_tables[grepl("all_comps", links_to_tables)] %>% unique()
      url_stats <- paste0("https://fbref.com/",all_comps[grepl("misc|shooting|keeper", all_comps)])
       
       
       
      #Loop over the urls of indicators (misc, shooting, keeper) and get the tables
      lista_tablas <- lapply(url_stats, function(x){

       
        #get the table
        table_stats <- read_html(x) %>%
          html_element("#matchlogs_for") %>%
          html_table()


        # #now get rid of the first row and delete redundant variables (duplicates)
        new_names <- as.character(table_stats[1, ])
        df_names <- tibble(names = new_names) %>% group_by(names) %>% mutate(row = row_number()) %>% ungroup()

        table_stats_unique <- table_stats[ , df_names$row == 1]
        names(table_stats_unique) <- table_stats_unique[1,]



        table_stats_final <- table_stats_unique[-1, ] %>%
          #this variables exist in fixture already
          select(-c(Round,	Day, Time, Comp,	Venue,	Result, GA, GF, `Match Report`))




      })

      #join all tables (misc, shooting, keeper)
      data_tablas <- plyr::join_all(lista_tablas, by = c("Date", "Opponent")) %>%
        select(-PKatt)

      # And join with fixture
      final_tabla <- fixture %>%
        left_join(data_tablas, by = c("Date", "Opponent")) %>%
        mutate(team = team) %>%
        relocate(team)

      
      
    } else {
      
      cli::cli_alert("No existe")
      NULL
    }
      
    })
      
    
    
  #append data from all years for this country
  all_years <- do.call(plyr::rbind.fill, get_years)
  
  
  
})
  

all <- do.call(plyr::rbind.fill, read_teams)


#export
export(all,exfile)
