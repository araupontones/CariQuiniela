# A robot to predict the World Cup matches

Experiment to predict World Cup matches using historical data of qualified teams.

#Description of directories

## R_
Code

* 1.Scrap_lookup_teams_urls.R : It scraps the list of qualified countries from (fbref)["https://fbref.com/en/comps/1/World-Cup-Stats"]. Each country has its own unique ID that is later used to extract their historical data. The output of this scripts is data/1.lookups/teams_urls.csv that is later used as the lookup table to interact across all the IDs in the system.

* 2.Scrap_games_teams.R : It uses the output from 1.Scrap_*_urls.R and downloads all the matches for all the teams since 2012. The output of this code is data/2.scrapped/All_competitions.csv that is later used to create some of the indicators

* 3.Create_indicators.R : It takes the output from 2.Scrap_*_teams.R, it cleans it and creates indicators at the year and team level. the output is stored in "data/3.clean"

## data

The relevant data for the analysis is stored in data/3.clean

* "ind_all_matches.csv" : every result for all the qualified teams of Qatar 2022

*  "ind_teams_year.csv" : indicators summarized by year and team




