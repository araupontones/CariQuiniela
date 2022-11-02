#Daily flow
library(dplyr)
library(httr)
library(rio)
library(rvest)
library(tibble)
library(tidyr)

#Scrap the world cup matches --------------------------------------------------
source("R_/fbref_matches/2.Scrap_WC_games.R", encoding = "UTF-8")

#Clean matches ------------------------------------------------------------------
#Create tables "ind_all_matches.csv" that contains all the matches played since 2012 and to be played until the end of the WC
#ind_teams_year.csv" that contains the summary stats of each country since 2012
source("R_/fbref_matches/3.create_indicators.R", encoding = "UTF-8")

#Download oods to win the World Cup --------------------------------------------
source("R_/request_odds.R", encoding = "UTF-8")









