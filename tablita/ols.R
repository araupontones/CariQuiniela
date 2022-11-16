library(rio)
library(dplyr)
years <- import("https://raw.githubusercontent.com/araupontones/CariQuiniela/rankings/data/3.clean/ind_teams_year.csv")
matches <- import("https://raw.githubusercontent.com/araupontones/CariQuiniela/rankings/data/3.clean/ind_all_matches.csv")


#clean data matches
locales <- matches %>%
  select(year, team, Opponent, Date, GF = goles_local, GA = goles_visitante, local_fifa_index, visitante_fifa_index)

visitantes <- matches %>%
  select(year, team = Opponent, Date,Opponent = team, GF = goles_visitante, GA = goles_local, local_fifa_index = visitante_fifa_index, 
         visitante_fifa_index = local_fifa_index)



matches_clean <- rbind(locales, visitantes) %>%
  left_join(years, by= c("year", "team")) %>%
  left_join(years, by = c("year", "Opponent" ="team"), suffix = c("_local", "_visitante")) %>%
  select(-starts_with("Neutral"), - starts_with("Home"), -starts_with("Away")) %>%
  select(year, team, Opponent, Date, GF, GA, contains("fifa"), contains("nrm"), contains("eff"))



linearMod <- lm(GF ~ local_fifa_index + visitante_fifa_index + all_nrm_GF_local +all_nrm_didnt_receive_goal_visitante , data=matches_clean)
linearMod$coefficients

export(matches_clean, "tablita/tablita.csv")

