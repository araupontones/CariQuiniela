

#import look up. This is used to identify teams that have not qualified to WC
look_up <- import("data/1.lookups/teams_urls.csv")




indicators_matches <- function(.data, prefix, 
                               venues = c("Away", "Home", "Neutral"), 
                               type = c("WC", "Others"), 
                               wc_teams =look_up$team
                               ){
  
  #define loosing and winning for WC teams and other teams -------------------
  if(type == "WC"){
    
    ganar = "W"
    perder = "L"
  }
  
  if(type == "Others"){
    ganar = "L"
    perder="W"
    
  }
  
  data_venue <- .data %>% filter(!is.na(GF),Venue %in% venues)
  
  #Group accordingly  ---------------------------------------------------------
  if(type == "WC"){
    
    db_group <- data_venue %>%group_by(team, year)
    
  }
  
  if(type == "Others"){
    
    db_group <- data_venue %>% 
      select(-team) %>%
      filter(!Opponent %in% wc_teams) %>% group_by(Opponent, year) %>% 
      rename(team = Opponent) %>%
      mutate(perro = GF,
             GF = GA,
             GA = perro) %>%
      select(-perro)
  }
  
  
  #create indexes --------------------------------------------------------------
  db_index <- db_group %>% 
    #normalize by FIFA index
    mutate(#fail to score
      GF_index = GF * visitante_fifa_index,
      GA_index = GF * (1 - visitante_fifa_index),
      did_score_index = (GF > 0) * visitante_fifa_index,
      fail_to_score_index = (GF == 0) * (1 - visitante_fifa_index),
      did_receive_goal_index = (GA > 0) * visitante_fifa_index,
      didnt_receive_goal_index = (GA == 0) * (1 - visitante_fifa_index)
    ) %>%
    
    
    #summarise by year -------------------------------------------------------
  summarise("matches" := n(),
            "matches_win" := sum(Result == ganar),
            "matches_lost" := sum(Result == perder),
            "matches_drawn" := sum(Result == "D"),
            "efectividad" := matches_win/matches,
            #did score ---------------------------------------------------
            did_score = sum(GF >0),
            #did_score_index = ({{goles_favor}} > 0) * visitante_fifa_index,
            
            #fail to score --------------------------------------------------
            "fail_to_score" := sum(GF == 0),
            #fail_to_score_index = ({{goles_favor}} == 0) * (1 - visitante_fifa_index),
            
            #did receive goal -----------------------------------------------
            did_receive_goal = sum(GA >0, na.rm = T),
            #did_receive_goal_index = ({{goles_contra}} > 0) * visitante_fifa_index,
            
            #didnt receive goal ---------------------------------------------
            didnt_receive_goal = sum(GA == 0, na.rm = T),
            #didnt_receive_goal_index = ({{goles_contra}} == 0) * (1 - visitante_fifa_index),
            
            #goles a favor, en contra y diferencia ----------------------------
            GF = sum(GF),
            GF_index = sum(GF_index, na.rm = T),
            fail_to_score_index = sum(fail_to_score_index, na.rm = T),
            
            GA = sum(GA),
            GA_index = sum(GA_index, na.rm = T),
            
            
            "Gdiff" := GF - GA,
            .groups = 'drop'
  ) %>%
    #normalized by the number of matches played
    mutate(across(-c(team, year, starts_with("matches"), efectividad), ~ .x/matches, .names = "nrm_{.col}")) %>%
    relocate(year, team, starts_with("matches"), GF, GA, Gdiff) %>%
    #rename variables based on the venue played
    rename_at(vars(-c("team", "year")), function(x)paste(prefix,x, sep = "_"))
  
}




# function to loop over all the venue types to create indicators --------------
create_data_year <- function(.data, venues = c("Away", "Home", "Neutral"),type, wc_teams){
  
  #all {{goles_contra}}mes
  all_ <- indicators_matches(.data, prefix = "all", 
                             venues, 
                             type = type, 
                             wc_teams =wc_teams)
  
  #{{goles_contra}}mes by venue
  my_list <- lapply(venues, function(v){
    
    print(v)
    my_data <- indicators_matches(.data, prefix = v, 
                                  venues = v,
                                  type = type,
                                  wc_teams =look_up$team
                                  )
    
    
  })
  
  #append data
  appended_venues <- plyr::join_all(my_list, by = c("team", "year"))
  appended <- left_join(all_, appended_venues, by = c("team", "year"))
  
}

