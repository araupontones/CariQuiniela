#clean and create indicators
library(dplyr)
library(tidyr)
library(rio)
library(stringr)
library(janitor)

indir <- "data/2.scrapped/fbref"
#all_competitions.csv is created in R_/scrap_games_teams.R
infile <- file.path(indir,"All_competitions.csv")

#Exit paths ---------------------------------------------------------------------
exdir <- "data/3.clean"
exfile_matches <- file.path(exdir, "ind_all_matches.csv")
exfile_year_team <-file.path(exdir, "ind_teams_year.csv")


#read scrapped data ----------------------------------------------------------
all_comps <- import(infile)


#Clean table of all matches & define year variable ---------------------------


db_matches <- all_comps %>%
  #drop not played matches (these are matches to be played in the future)
  filter(GF != "") %>%
  #drop penalties in drawn matches, penalties are reported within parenthesis.
  mutate(across(c(GF, GA) , function(x) as.numeric(str_replace(x, "\\([0-9]\\)",""))),
         year = str_sub(Date,1,4),
         Opponent = str_remove(Opponent, "^[a-z][a-z] ")
         ) %>%
  select(year, team, Opponent, Date,Time, Comp, Day, Venue, Result, GF, GA) %>%
  #remove duplicates (there were some duplicated matches while scrapping)
  distinct()



         




#==============================================================================
# Data of teams by year (indicators by Away, Home, Neutral and all matches)

# Function to create indicators at the different venue -------------------------
indicators_matches <- function(.data, prefix, venues = c("Away", "Home", "Neutral")){
  
  .data %>%
    #keep only venue of interest
  filter(Venue %in% venues) %>%
  group_by(team, year) %>%
  summarise("matches" := n(),
            "matches_win" := sum(Result == "W"),
            "matches_lost" := sum(Result == "L"),
            "matches_drawn" := sum(Result == "D"),
            "efectividad" := matches_win/matches,
            "fail_GF" := sum(GF == 0),
            "fail_GA" := sum(GF == 0),
            "GF" := sum(GF),
            "GA" := sum(GA),
            "Gdiff" = GF - GA,
            .groups = 'drop'
  ) %>%
    #normalized by the number of matches played
    mutate(across(-c(team, year, starts_with("matches"), efectividad), ~ .x/matches, .names = "nrm_{.col}")) %>%
    relocate(year, team, starts_with("matches"), GF, GA, Gdiff) %>%
    #rename variables based on the venue played
    rename_at(vars(-c("team", "year")), function(x)paste(prefix,x, sep = "_"))
  
}



# function to loop over all the venue types to create indicators --------------
create_data_year <- function(.data, venues = c("Away", "Home", "Neutral")){
  
  #all games
  all_ <- indicators_matches(.data, prefix = "all", venues )
  
  #games by venue
  my_list <- lapply(venues, function(v){
    
    print(v)
    my_data <- indicators_matches(.data, prefix = v, venues = v)
    
    
  })
  
  #append data
  appended_venues <- plyr::join_all(my_list, by = c("team", "year"))
  appended <- left_join(all_, appended_venues, by = c("team", "year"))
  
}



# create data at the team and year level --------------------------------------
data_year_team <- create_data_year(db_matches)


View(data_year_team)

#===============================================================================
#export data====================================================================

export(db_matches, exfile_matches)
export(data_year_team, exfile_year_team)
