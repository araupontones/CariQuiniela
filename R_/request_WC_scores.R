#to request scores of WC. Clean on day 1 of tournament
message("Getting WC matches")
look_up <- import(file.path(dir_data,"1.lookups/teams_urls.csv")) %>% select(team, id)
exfile <- file.path(dir_data, "2.1.intermediate/WC_matches.rds")


scores <- "https://api.the-odds-api.com/v4/sports/soccer_fifa_world_cup/scores/?daysFrom=1&apiKey=324654c6d22a577030fead8bed15fc9d"

sc <- GET(scores)
r_s <- content(sc)

transforma <- lapply(r_s, function(m){
  
  match_data <-tibble(team = m$home_team,
         Opponent = m$away_team,
         Date = stringr::str_sub(m$commence_time, 1,10),
         Venue = "Neutral",
         qatar = TRUE)
  #if the match has not been completed
  if(!m$completed){
    
    match_data$goles_local = NA
    match_data$goles_visitante = NA
  } else {
    match_data$goles_local = m$scores[[1]]$score
    match_data$goles_visitante = m$scores[[2]]$score
    
  }
  
  return(match_data)
  
})



#unique matches in format local visitante
scores_matches <- do.call(rbind, transforma) %>% 
  clean_teams() %>% 
  mutate(across(starts_with("goles"), function(x)as.numeric(x)),
         year = stringr::str_sub(Date, 1,4)) %>%
  select(year, team, Opponent, Date, goles_local, goles_visitante, qatar)


message("Matches downladed:", nrow(scores_matches))

#create table to get GF and GA for all teams-------------------------------------
#start with local teams

#locales
locals <- scores_matches %>%
  mutate(GF = goles_local,
         GA = goles_visitante,
         Result = case_when(is.na(goles_local) ~ NA_character_,
                          GF > GA ~ "W",
                            GF < GA ~ "L",
                            GF == GA ~ "D"
                            )
         )

#visitantes -----------------------------------------------------------------
visitantes <- scores_matches %>%
  mutate(bucket = Opponent,
         Opponent = team,
         team = bucket,
         GF = goles_visitante,
         GA = goles_local,
         Result = case_when(is.na(goles_local) ~ NA_character_,
                            GF > GA ~ "W",
                            GF < GA ~ "L",
                            GF == GA ~ "D"
         )
  ) %>%
  select(-bucket)

  
#matches by team -------------------------------------------------------------
matches_by_team <- rbind(locals, visitantes) %>%
  mutate(Comp = "WC",
         Venue = "Neutral"
         ) %>%
  select(-starts_with("goles")) %>%
  create_ids(., look_up)




world_cup_matches <- list(scores_matches, matches_by_team)
names(world_cup_matches) <- c("scores", "by_team")



export(world_cup_matches, exfile)
