#clean and create indicators
library(dplyr)
library(tidyr)
library(rio)
library(stringr)
library(janitor)

#clean_all_matches_before_WC.R

message("Cleaning matches played  before WC.....")
indir <- file.path(dir_data,"2.scrapped/fbref")

#all_competitions.csv is created in R_/scrap_games_teams.R

infile_before_2022 <- file.path(indir,"matches_before_2022.csv") #from R_scrap_games_before_2022.R
#infile_2022 <-  file.path(indir,"matches_2022_before_WC.csv") #from R_/2.scrap_games_2022_before_WC.R
infile_2022 <- "https://raw.githubusercontent.com/araupontones/CariQuiniela/rankings/data/2.scrapped/fbref/matches_2022_before_WC.csv"



look_up <- import(file.path(dir_data,"1.lookups/teams_urls.csv")) %>% select(team, id)

#Exit paths ---------------------------------------------------------------------
exdir <- file.path(dir_data,"2.1.intermediate")
exfile <- file.path(exdir, "matches_before_WC.rds")





#read scrapped data ----------------------------------------------------------
before_2022 <- import(infile_before_2022) %>% mutate(qatar = FALSE)  %>% filter(GF != "", !is.na(Date))
during_2022<- import(infile_2022)




#Clean table of all matches & define year variable ---------------------------
db_matches <- before_2022 %>%
  #bind with matches of 2022 previous WC
  plyr::rbind.fill(during_2022) %>%
  #drop penalties in drawn matches, penalties are reported within parenthesis.
  mutate(across(c(GF, GA) , function(x) as.numeric(str_replace(x, "\\([0-9]\\)",""))),
         year = str_sub(Date,1,4)
  ) %>%
  clean_Opponent()%>%
  clean_teams(., team) %>%
  clean_teams(.,Opponent)%>%
  select(year, team, Opponent, Date, Comp, Venue, Result, GF, GA, qatar) %>%
  #remove duplicates (there were some duplicated matches while scrapping)
  distinct() %>%
  #id match 
  create_ids(., look_up) %>%
  ungroup() 



#clean matches manually because FBREF has shot us down--------------------------

clean_scores <- function(data, local, date, GF, GA, result){
  
  data$GF[data$team == local & data$Date == date] <- GF
  data$GA[data$team == local & data$Date == date] <- GA
  data$Result[data$team == local & data$Date == date] <- result
  
  return(data)
  
  
}




 db_matches <- clean_scores(db_matches, "Argentina","2022-11-16",5,0,"W")
 db_matches <- clean_scores(db_matches, "IR Iran","2022-11-16",0,2,"L")
 db_matches <- clean_scores(db_matches, "Saudi Arabia","2022-11-16",0,1,"L")
 db_matches <- clean_scores(db_matches, "Poland","2022-11-16",1,0,"W")
 db_matches <- clean_scores(db_matches, "Mexico","2022-11-16",2,1,"L")
 db_matches <- clean_scores(db_matches, "Germany","2022-11-16",1,0,"W")
 
 
 
 
 
 
 
 

 
 


 # db_matches <- clean_scores(db_matches, "Saudi Arabia","2022-11-10",1,1,"D")








#transform matches as local vs visitante format ------------------------------
#this is the data to be exported
db_matches_unique <- db_matches %>%
  filter(!qatar) %>%
  group_by(id_match) %>%
  arrange(id_match) %>%
  mutate(Venue = ifelse(is.na(id_opponent) & Venue == "Away", "awayNoWC", Venue)) %>%
  #only keep home and neutral matches to capture goles local and goles visitante
  filter(Venue %in% c("Home", "Neutral", "awayNoWC")) %>%
  slice(1) %>%
  #correct for teams that are not going to the WC
  mutate(bucket = team,
         team = ifelse(Venue == "awayNoWC", Opponent, team),
         Opponent = ifelse(Venue == "awayNoWC", bucket, Opponent),
         goles_local = case_when(Venue == "awayNoWC" ~ GA,
                                 T ~ GF),
         goles_visitante = case_when(Venue == "awayNoWC" ~ GF,
                                     T ~ GA),
         neutral = Venue == "Neutral",
         .after = "GA"
         ) %>%
  select(-bucket) %>%
  ungroup() %>%
  select(-c(Venue, Result, GF, GA, starts_with("id_"))) %>%
  arrange(desc(Date))  %>%
  select(year, team, Opponent, Date, goles_local, goles_visitante, qatar) %>%
  mutate(Date = as.character(Date))
  

pre_cup_matches <- list(db_matches_unique, db_matches)
names(pre_cup_matches) <- c("scores", "by_team")


export(pre_cup_matches, exfile)
paste("saved to:", exfile)
