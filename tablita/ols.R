library(rio)
library(dplyr)
library(stringr)
library(jtools)
library(ggplot2)


#matches <- import("https://raw.githubusercontent.com/araupontones/CariQuiniela/main/tablita/tablita_gordo.csv")
matches <- import('tablita/tablita_ols.csv')

matches <- rio::import("https://raw.githubusercontent.com/araupontones/CariQuiniela/walle/data/3.clean/tablita_ols.csv")


#Model ---------------------------------------------------------------------
linearMod <- lm(GF ~ 
                  #performance team
                 # pre_rating_team +
                 #  pre_rating_opponent
                 
                 
                  +magic_johnson_team
                #efectividad_local
                 
                 
                  #performance Opponent
                  
              
              
                ,
                data=matches)

#inspect model
summary(linearMod)


  predicted_data <- matches %>% filter(qatar)
  predicted_data$GF <- predict(linearMod, newdata = predicted_data) 
  
  
  final <- predicted_data %>%
    filter(!is.na(GF)) %>%
    select(date, team, opponent, GF) %>%
    left_join(select(predicted_data, c(date, team = opponent, GA = GF))) %>%
    rowwise() %>%
    mutate(id = paste0(date,paste(sort(c(team, opponent)), collapse =  "-"))) %>%
  ungroup() %>%
    filter(!is.na(GA)) %>%
    group_by(id) %>%
    slice(1) %>%
    ungroup %>%
     mutate(GF = round(GF),
            GA = round(GA)) %>%
    filter(date== "2022-12-10")

  View(final)
  pson <- function(x){
    
   d<- janitor::tabyl(rpois(100,x)) %>% arrange(desc(percent))
    
   max(d$percent)
    
  }
View(final)

rpm(1.7944278)

library(dplyr)

 