library(stringr)
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
                                          T ~ visitante_fifa_index)) 

  mundial <- create_data_year(data_year_team, 
                               venues = c("Away", "Home", "Neutral"), 
                               type = "WC", 
                               wc_teams =look_up$team,
                              gf = GF,
                               gc = GA) 

 
  
  no_qualified <- create_data_year(data_year_team, 
                                   venues = c("Away", "Home", "Neutral"), 
                                   type = "Others", 
                                   wc_teams =look_up$team,
                                   gf = GF,
                                   gc = GA) 
      
  

  data_year_team_all <- plyr::rbind.fill(mundial,no_qualified)

 


  #===============================================================================
#export data====================================================================


export(all_matches, exfile_matches)
export(data_year_team_all, exfile_year_team)
message("Matches are cleaned!")
