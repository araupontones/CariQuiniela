# A robot to predict the World Cup matches

To automatically update the input data, run:
"R_/Run_Walle_Data_System.R"

R dependencies:

library(dplyr)
library(httr)
library(rio)
library(rvest)
library(tibble)
library(tidyr)


## data

The relevant data for the analysis is stored in data/3.clean

* "ind_all_matches.csv" : every result for all the qualified teams of Qatar 2022

*  "ind_teams_year.csv" : indicators summarized by year and team

* ind_ods_winner.csv : a daiy updated table of the teams' odds to win the WC



