library(tidyr)
#crete rankings
elos <- rio::import("elo/elo_rankings.csv") %>%
  select(date, team, opponent, rating_team, rating_opponent) 




elos_n <- elos %>%
  select(date, team, opponent, rating_team, rating_opponent ) %>%
  pivot_longer(-c(date, rating_team, rating_opponent)) %>%
  mutate(rating = case_when(name == "team" ~ rating_team,
                            name == "opponent" ~ rating_opponent)) %>%
  select(date, team = value, rating) %>%
  arrange(team, desc(date)) %>%
  mutate(as_of = Sys.Date())
  



rio::export(elos_n,"elo/pre_wc_ratings.csv")


