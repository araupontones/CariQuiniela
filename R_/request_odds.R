#From https://the-odds-api.com/
#test

library(httr)
library(tidyr)

message("requesting odds .....")
exfile <- 'data/3.clean/ind_ods_winner.csv'

#look up table to clean countries 
look_up <- import("data/1.lookups/teams_urls.csv") %>% select(team, id)


#define token
token <- "324654c6d22a577030fead8bed15fc9d"

#url to request oods
wc_url <- glue("https://api.the-odds-api.com/v4/sports/soccer_fifa_world_cup_winner/odds/?apiKey=324654c6d22a577030fead8bed15fc9d&regions=us")
wc_url

#request the odds--------------------------------------------------------------
wc <- GET(wc_url)
response <- content(wc)
bookmakers <- response[[1]]$bookmakers



#Transform the response into a readable object ---------------------------------
list_odds <- lapply(bookmakers, function(x){
  
  house= x$key
  print(house)
  outcomes <- x$markets[[1]]$outcomes
  df <- data.frame(matrix(unlist(outcomes), nrow=length(outcomes), byrow=TRUE)) 
  names(df) <- c("team", house)
  
  return(df)  
})

#join all odds
all_ods <- plyr::join_all(list_odds, by = "team") %>%
  mutate(team = as.character(team),
    team = case_when(team == "USA" ~ "United States",
                           team == "Iran" ~ "IR Iran",
                           team == "South Korea" ~ "Korea Republic",
                           T ~ team)
         )


#summarise data ---------------------------------------------------------
sum_ods <- all_ods %>%
  pivot_longer(-team) %>%
  group_by(team) %>%
  summarise(odd_win = mean(as.numeric(value)), .groups = 'drop') %>%
  arrange(odd_win)


export(sum_ods, exfile)
message(("odds saved in data clean!"))