#Daily flow c
library(dplyr)
library(httr)
library(rio)
library(rvest)
library(tibble)
library(tidyr)

#define working directory so linux can run with no problem

gmdacr::load_functions("functions")

sistema <- Sys.info()['sysname']


get_proj_dir <- function(){
  
  if(sistema == "Windows"){
    
    my_dir <- getwd()
  } else {
    
    my_dir = "/home/rstudio/CariQuniela"
  }
  
}

dir_data <- file.path(get_proj_dir(), "data")
dir_r <- file.path(get_proj_dir(), "R_")

#1. Get matches of 2022 but before the WC (only run before the WC)

#source(file.path(dir_r,"fbref_matches/2.Scrap_games_2022_before_WC.R")) #exports to data/2.scrapped/fbref/matches_2022_before_WC.csv

#2. Clean matches before the WC

#source(file.path(dir_r,"fbref_matches/4.clean_matches_before_WC.R")) #exports to data/2.1intermediate/matches_before_WC.rds



#Scrap the world cup matches --------------------------------------------------
source(file.path(dir_r,"request_WC_scores.R"), encoding = "UTF-8") #exports to data/2.1.intermediate/WC_matches.rds


#Clean matches ------------------------------------------------------------------
#Create tables "ind_all_matches.csv" that contains all the matches played since 2012 and to be played until the end of the WC
#ind_teams_year.csv" that contains the summary stats of each country since 2012
source(file.path(dir_r, "Append_before_and_WC.R"), encoding = "UTF-8")

#Download oods to win the World Cup --------------------------------------------
source(file.path(dir_r,"request_WC_odds_winners.R"), encoding = "UTF-8")


#Commit and push to repo -----------------------------------------------------
temp_terminal <- rstudioapi::terminalExecute("myterminal.sh")
rstudioapi::terminalKill(temp_terminal)








