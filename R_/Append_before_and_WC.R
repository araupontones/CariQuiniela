message("Creating indicators and appending WC and  pre matches")
sistema <- Sys.info()['sysname']

exfile_matches <- file.path(dir_data, "3.clean/ind_all_matches.csv")
exfile_year_team <- file.path(dir_data, "3.clean/ind_teams_year.csv")
#import intermediate data

if(sistema == "Windows"){
  
  pre_wc <- rio::import(file.path(dir_data, "2.1.intermediate/matches_before_WC.rds"))
  
} else {
  
  pre_wc <-rio::import("https://github.com/araupontones/CariQuiniela/blob/main/data/2.1.intermediate/matches_before_WC.rds?raw=true")
  
}

wc <-  import(file.path(dir_data, "2.1.intermediate/WC_matches.rds"))






#Create ind_all_matches.csv
all_matches <-  rbind(wc$scores, pre_wc$scores ) %>% mutate(Date = as.Date(Date)) %>% arrange(desc(Date))
 





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
data_year_team <- create_data_year(rbind(pre_wc$by_team, wc$by_team))

#===============================================================================
#export data====================================================================

export(all_matches, exfile_matches)
export(data_year_team, exfile_year_team)
message("Matches are cleaned!")
