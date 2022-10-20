#clean and create indicators
library(dplyr)
library(tidyr)
library(rio)
library(stringr)
library(janitor)

indir <- "data/2.scrapped"
infile <- file.path(indir,"All_competitions.csv")

all_comps <- import(infile)


#Define key variables for grouping  & drop duplicates---------------------------

db_to_group <- all_comps %>%
  #drop not played matches
  filter(GF != "") %>%
  #drop penalties in drawn matches
  mutate(across(c(GF, GA) , function(x) as.numeric(str_replace(x, "\\([0-9]\\)",""))),
         year = str_sub(Date,1,4)
         ) %>%
  relocate(year) %>%
  distinct()
         

  db_to_group %>% janitor::tabyl(Sh)

#Define indicators by year and country ---------------------------------------

data_year <- db_to_group %>%
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
  mutate(across(-c(team, year, starts_with("matches"), efectividad), ~ .x/matches, .names = "nrm_{.col}")) %>%
  relocate(year, team, starts_with("matches"), GF, GA, Gdiff)
