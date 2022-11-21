library(rio)
library(dplyr)
library(stringr)
library(jtools)
library(ggplot2)


matches <- import("https://raw.githubusercontent.com/araupontones/CariQuiniela/main/tablita/tablita_gordo.csv")
matches <- import('tablita/tablita_gordo.csv')

names(matches)
#import("https://raw.githubusercontent.com/araupontones/CariQuiniela/rankings/data/3.clean/ind_teams_year.csv")
# years <- import("data/3.clean/ind_teams_year.csv")
#  
# matches <- import("https://raw.githubusercontent.com/araupontones/CariQuiniela/rankings/data/3.clean/ind_all_matches.csv")
# odds<- import("https://raw.githubusercontent.com/araupontones/CariQuiniela/rankings/data/3.clean/ind_ods_winner.csv")
# 
# 
# 
# #clean data matches
# locales <- matches %>%
#   select(year, team, Opponent, Date, GF = goles_local, GA = goles_visitante, local_fifa_index, visitante_fifa_index)
# 
# visitantes <- matches %>%
#   select(year, team = Opponent, Date,Opponent = team, GF = goles_visitante, GA = goles_local, local_fifa_index = visitante_fifa_index, 
#          visitante_fifa_index = local_fifa_index)
# 
# 
# matches_clean <- rbind(locales, visitantes) %>%
#   left_join(years, by= c("year", "team")) %>%
#   left_join(years, by = c("year", "Opponent" ="team"), suffix = c("_local", "_visitante")) %>%
#   select(-starts_with("Neutral"), - starts_with("Home"), -starts_with("Away")) %>%
#   select(year, team, Opponent, Date, GF, GA, contains("fifa"), contains("nrm"), contains("ef")) %>%
#   rename_all(function(x)str_remove(x, "all_nrm_|all_")) %>%
#   filter(!is.na(fail_to_score_local)) %>%
#   # filter(!is.na(GF)) %>%
#   mutate(year = as.factor(year)) %>%
#   rowwise() %>%
#   mutate(id_match = paste(sort(c(team, Opponent, Date)), collapse =  "-")) %>%
#   ungroup() %>%
#   group_by(id_match) %>%
#   slice(1) %>%
#   ungroup()

#Model ---------------------------------------------------------------------
linearMod <- lm(GF ~ 
                  #performance team
                 pre_rating_team +
                  pre_rating_opponent
                #efectividad_local
                  + GF_last_5 
                 
                  #performance Opponent
                  
              
                ,
                data=matches)

#inspect model
summary(linearMod)


# plot_summs(linearMod, linearMod, robust = list(FALSE, "HC0"),
#                                model.names = c("OLS")) +
#   theme(
#     panel.grid.major.y = element_line(colour = 'gray')
#   )

 


  View(matches)  

  predicted_data <- matches %>% filter(is.na(GF))
  View(predicted_data)
  predicted_data$GF <- predict(linearMod, newdata = predicted_data)

  View(predicted_data)
  