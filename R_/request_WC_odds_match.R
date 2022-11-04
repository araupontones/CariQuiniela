#Request WC matches odds ------------------------------------------------------

library(jsonlite)
message("downloading WC odds matches....")

exfile = file.path(dir_data, "3.clean/WC_matches_odds.csv")
look_up <- import(file.path(dir_data,"1.lookups/teams_urls.csv")) %>% select(team, id)



#Request matches-------------------------------------------------------------
wc <- "https://api.the-odds-api.com/v4/sports/soccer_fifa_world_cup/odds/?regions=us&oddsFormat=american&apiKey=324654c6d22a577030fead8bed15fc9d"
w <- GET(wc)
rsp_w <- content(w)


#Unlist all information from the response
test <- lapply(rsp_w, function(x){
  
#match info
  date = x$commence_time


#get bookmakers (the odds for all the bet houses)
books <- x$bookmakers

#Each book has an odd for the match
markets <- lapply(books, function(b){
  
  #using this to avoid duplicates when iterating and transforming into a database
  name = b$key
 
  
  return(list(name = name, b =b$markets ))
})



#let's get all the odds for all the markets
odds <- lapply(markets, function(m){
  
  mercado = (m$name)
  
 outcomes <- lapply(m$b, function(o){
   
  
   tibble(
     team = o$outcomes[[1]]$name,
     "local_odd_{mercado}" := o$outcomes[[1]]$price,
     Opponent = o$outcomes[[2]]$name,
     "visitante_odd_{mercado}":= o$outcomes[[2]]$price,
     "empate_{mercado}" := o$outcomes[[3]]$price,
     Date = date
   )
   
 
   
 })
 
 #join all odds for this match
 plyr::join_all(outcomes, by = c("team", "Opponent", "Date"))
  
  
  
})

#join all odds for all matches
odds_2 <- plyr::join_all(odds, by = c("team", "Opponent", "Date"))

return(odds_2)
  
})

#bind all the matches into a single table
matches_raw <- do.call(plyr::rbind.fill, test)

#create average of odds ------------------------------------------------------
matches_clean <- matches_raw %>%
  rowwise() %>%
  mutate(local = mean(c_across(starts_with("local_odd")), na.rm = T),
         visitante = mean(c_across(starts_with("visitante_odd")), na.rm = T),
         empate = mean(c_across(starts_with("empate")), na.rm = T),
         .after = Opponent
         ) %>%
  ungroup()%>%
  mutate(Date = stringr::str_sub(Date,1,10)) %>%
  select(-contains("_")) %>%
  clean_teams() %>%
  create_ids(., look_up)


View(matches_clean)

rio::export(matches_clean, exfile)

message("WC odds matches downladed")
