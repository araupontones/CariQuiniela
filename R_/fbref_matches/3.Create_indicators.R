#clean and create indicators
library(dplyr)
library(tidyr)
library(rio)
library(stringr)
library(janitor)

message("Cleaning matches played .....")
indir <- "data/2.scrapped/fbref"
#all_competitions.csv is created in R_/scrap_games_teams.R
infile_WC <-  file.path(indir,"WC_matches.csv")
infile_all <- file.path(indir,"All_competitions.csv")
look_up <- import("data/1.lookups/teams_urls.csv") %>% select(team, id)

#Exit paths ---------------------------------------------------------------------
exdir <- "data/3.clean"
exfile_matches <- file.path(exdir, "ind_all_matches.csv")
exfile_year_team <-file.path(exdir, "ind_teams_year.csv")


#read scrapped data ----------------------------------------------------------
all_comps <- import(infile_all) %>% mutate(qatar = FALSE)  %>% filter(GF != "")
wc <- import(infile_WC)




#Clean table of all matches & define year variable ---------------------------


db_matches <- all_comps %>%
  #select only variables that match with the WC matches
  select(names(wc)) %>%
  rbind(wc) %>%
  #drop penalties in drawn matches, penalties are reported within parenthesis.
  mutate(across(c(GF, GA) , function(x) as.numeric(str_replace(x, "\\([0-9]\\)",""))),
         year = str_sub(Date,1,4),
         #remove iso2 and iso 3 from Opponent's name
         Opponent = str_remove(Opponent, "^[a-z][a-z] "),
         Opponent = str_remove(Opponent, "^[a-z][a-z][a-z] ")
  ) %>%
  select(year, team, Opponent, Date,Time, Comp, Day, Venue, Result, GF, GA, qatar) %>%
  #remove duplicates (there were some duplicated matches while scrapping)
  distinct() %>%
  #id match 
  left_join(look_up, by = "team") %>%
  rename(id_team = id) %>%
  left_join(look_up, by = c("Opponent"= "team")) %>%
  rename(id_opponent = id) %>%
  rowwise() %>%
  #get id of match by combining the ID of the two teams
  mutate(id_match = paste(sort(c(id_team, id_opponent)), collapse =  "-"),
         id_match = paste(id_match, Date, sep = "-")) %>%
  ungroup()


#transform matches as local vs visitante format ------------------------------
#this is the data to be exported
db_matches_unique <- db_matches %>% group_by(id_match) %>% 
  arrange(id_match) %>%
  mutate(Venue = ifelse(is.na(id_opponent) & Venue == "Away", "awayNoWC", Venue)) %>%
  #only keep home and neutral matches to capture goles local and goles visitante
  filter(Venue %in% c("Home", "Neutral", "awayNoWC")) %>%
  slice(1) %>%
  #correct for teams that are not going to the WC
  mutate(bucket = team,
         team = ifelse(Venue == "awayNoWC", Opponent, team),
         Opponent = ifelse(Venue == "awayNoWC", bucket, Opponent),
         goles_local = case_when(Venue == "awayNoWC" ~ GA,
                                 T ~ GF),
         goles_visitante = case_when(Venue == "awayNoWC" ~ GF,
                                     T ~ GA),
         neutral = Venue == "Neutral",
         .after = "GA"
         ) %>%
  select(-bucket) %>%
  ungroup() %>%
  select(-c(Venue, Result, GF, GA, starts_with("id_"))) %>%
  arrange(desc(Date))
  



#==============================================================================
# Data of teams by year (indicators by Away, Home, Neutral and all matches)

# Function to create indicators at the different venue -------------------------
indicators_matches <- function(.data, prefix, venues = c("Away", "Home", "Neutral")){
  
  .data %>%
    #keep only venue of interest
    filter(Venue %in% venues) %>%
    group_by(team, year) %>%
    filter(!is.na(GF)) %>%
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


#===============================================================================
#export data====================================================================

export(db_matches_unique, exfile_matches)
export(data_year_team, exfile_year_team)
message("Matches are cleaned!")
