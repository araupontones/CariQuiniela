#clean and create indicators
library(dplyr)
library(tidyr)
library(rio)
library(stringr)
library(janitor)

indir <- "data/2.scrapped"
#all_competitions.csv is created in R_/scrap_games_teams.R
infile <- file.path(indir,"All_competitions.csv")

#Exit paths ---------------------------------------------------------------------
exdir <- "data/3.clean"
exfile_matches <- file.path(exdir, "ind_all_matches.csv")
exfile_year_team <-file.path(exdir, "ind_teams_year.csv")


#read scrapped data ----------------------------------------------------------
all_comps <- import(infile)


#Clean table of all matches & define year variable ---------------------------


db_matches <- all_comps %>%
  #drop not played matches (these are matches to be played in the future)
  filter(GF != "") %>%
  #drop penalties in drawn matches, penalties are reported within parenthesis.
  mutate(across(c(GF, GA) , function(x) as.numeric(str_replace(x, "\\([0-9]\\)",""))),
         year = str_sub(Date,1,4),
         Opponent = str_remove(Opponent, "^[a-z][a-z] ")
         ) %>%
  select(year, team, Opponent, Date,Time, Comp, Day, Venue, Result, GF, GA) %>%
  #remove duplicates (there were some duplicated matches while scrapping)
  distinct()



         

 

#Define indicators by year and country ---------------------------------------

data_year_team <- db_matches %>%
  #create indicators by team and year
  group_by(team, year) %>%
  summarise(matches = n(),
            matches_win = sum(Result == "W"),
            matches_lost = sum(Result == "L"),
            matches_drawn = sum(Result == "D"),
            efectividad = matches_win/matches,
            fail_GF = sum(GF == 0),
            fail_GA = sum(GF == 0),
            GF = sum(GF),
            GA = sum(GA),
            Gdiff = GF - GA,
            .groups = 'drop'
            ) %>%
  #norm indicators by number of matches played
  mutate(across(-c(team, year, starts_with("matches"), efectividad), ~ .x/matches, .names = "nrm_{.col}")) %>%
  relocate(year, team, starts_with("matches"), GF, GA, Gdiff)


#===============================================================================
#export data====================================================================

export(db_matches, exfile_matches)
export(data_year_team, exfile_year_team)
