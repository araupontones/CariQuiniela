message("Creating indicators and appending WC and  pre matches")
sistema <- Sys.info()['sysname']

exfile_matches <- file.path(dir_data, "3.clean/ind_all_matches.csv")
exfile_year_team <- file.path(dir_data, "3.clean/ind_teams_year.csv")
rankings_fifa <- import("https://raw.githubusercontent.com/araupontones/CariQuiniela/main/data/3.clean/fifa_rankings.csv")

#import intermediate data ------------------------------------------------------

if(sistema == "Windows"){
  
  pre_wc <- rio::import(file.path(dir_data, "2.1.intermediate/matches_before_WC.rds"))
  
} else {
  
  pre_wc <-rio::import("https://github.com/araupontones/CariQuiniela/blob/main/data/2.1.intermediate/matches_before_WC.rds?raw=true")
  
}

wc <-  import(file.path(dir_data, "2.1.intermediate/WC_matches.rds"))





#===============================================================================
#Create ind_all_matches.csv
all_matches <-  rbind(wc$scores, pre_wc$scores ) %>% 
  filter(year > 2010) %>%
  #get fifa ranks
  mutate(Date = as.Date(Date)) %>% 
  arrange(desc(Date)) %>%
  create_quarter(.,Date) %>%
  mutate(quarter = ifelse(qatar, "WC", quarter)) %>%
  get_rankings(., rankings_fifa) %>%
  select(-c(month,quarter)) 
 






 
#==============================================================================
# Data of teams by year (indicators by Away, Home, Neutral and all matches)

# Function to create indicators at the different venue -------------------------
indicators_matches <- function(.data, prefix, venues = c("Away", "Home", "Neutral")){
  
  .data %>%
    #keep only venue of interest
    filter(Venue %in% venues) %>%
    group_by(team, year) %>%
    filter(!is.na(GF)) %>%
    #normalize by FIFA index
    mutate(fail_GF_index = (GF==0) * visitante_fifa_index,
           fail_GA_index = (GA==0) * 1/visitante_fifa_index,
           GF_index = GF * visitante_fifa_index,
           GA_index = GA * 1/visitante_fifa_index
           
           ) %>%
    #Puntos roy
    mutate(puntos_roy = case_when(GF > 0 & GA == 0 ~ 5,
                                  GF > 0 & Result == "W" ~ 4,
                                  GF > 0 & (GA - GF ==1) & Result == "L" ~ 3,
                                  GF > 0 & Result == "D" ~ 2,
                                  GF > 0 & (GA - GF > 1) & Result == "L" ~ 1,
                                  T ~ 0
                                  ),
           puntos_roy_index = puntos_roy * visitante_fifa_index
           ) %>%
    #summarise by year
    summarise("matches" := n(),
              "matches_win" := sum(Result == "W"),
              "matches_lost" := sum(Result == "L"),
              "matches_drawn" := sum(Result == "D"),
              "efectividad" := matches_win/matches,
              "fail_GF" := sum(GF == 0),
              fail_GF_index = sum(fail_GF_index,na.rm = T),
              "fail_GA" := sum(GA == 0),
              fail_GA_index = sum(fail_GA_index, na.rm = T),
              "GF" := sum(GF),
              GF_index = sum(GF_index, na.rm = T),
              "GA" := sum(GA),
              GA_index = sum(GA_index, na.rm = T),
              "Gdiff" := GF - GA,
              puntos_roy = sum(puntos_roy),
              puntos_roy_index = sum(puntos_roy_index),
              .groups = 'drop'
    ) %>%
    #normalized by the number of matches played
    mutate(across(-c(team, year, starts_with("matches"), efectividad), ~ .x/matches, .names = "nrm_{.col}")) %>%
    relocate(year, team, starts_with("matches"), GF, GF_index, GA,GA_index, Gdiff) %>%
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


#===============================================================================
# create data at the team and year level --------------------------------------



data_year_team <- rbind(pre_wc$by_team, wc$by_team) %>%
  #get fifa quarters
  create_quarter(., Date) %>%
  mutate(quarter = ifelse(qatar, "WC", quarter)) %>%
  get_rankings(., rankings_fifa) %>%
  #clean rankings of missing countries in ranking
  mutate(visitante_fifa_index = case_when(Opponent == "Cape Verde" ~ 0.7530017,
                                          Opponent == "N. Macedonia" ~ 0.73779938,
                                          Opponent == "Guadeloupe" ~ 0.008130081,
                                          Opponent == "Martinique" ~ 0.008130081,
                                          Opponent == "Eswatini" ~ 0.008130081,
                                          Opponent == "Gibraltar" ~ 0.009130081,
                                          Opponent == "French Guiana" ~ 0.009130081,
                                          Opponent == "SÃ£o TomÃ©" ~ 0.008130081,
                                          Opponent == "San Marino" ~ 0.009130081,
                                          T ~ visitante_fifa_index)) %>%
  
  create_data_year(.) 


#===============================================================================
#export data====================================================================


export(all_matches, exfile_matches)
export(data_year_team, exfile_year_team)
message("Matches are cleaned!")
