library(rio)
library(dplyr)
library(stringr)
library(jtools)
library(ggplot2)


#matches <- import("https://raw.githubusercontent.com/araupontones/CariQuiniela/main/tablita/tablita_gordo.csv")
matches <- import('tablita/tablita_ols.csv')

matches <- matches %>%
  mutate(qatar =ifelse(is.na(qatar), FALSE, qatar),
         GF = ifelse(qatar, NA, GF))


names(matches)
#Model ---------------------------------------------------------------------
linearMod <- lm(GF ~ 
                  #performance team
                 # pre_rating_team +
                 #  pre_rating_opponent
                 
                  + GF_last_5
                #   + pre_rating_team
                # + pre_rating_opponent
                +GA_last_5_opponent
                
                  +dr_pre
                #efectividad_local
                 
                 
                  #performance Opponent
                  
              
              
                ,
                data=matches)

#inspect model
summary(linearMod)




  predicted_data <- matches %>% filter(is.na(GF))
  predicted_data$GF <- predict(linearMod, newdata = predicted_data) 
  
  final <- predicted_data %>%
    select(date, team, opponent, GF) %>%
    left_join(select(predicted_data, c(date, team = opponent, GA = GF))) %>%
    rowwise() %>%
    mutate(id = paste0(date,paste(sort(c(team, opponent)), collapse =  "-"))) %>%
  ungroup() %>%
    filter(!is.na(GA)) %>%
    group_by(id) %>%
    slice(1) %>%
    ungroup()%>%
    filter(date == "2022-11-23")

    
 