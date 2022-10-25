#Create indicators winners
library(dplyr)
library(rio)
library(tidyr)
library(ggplot2)

indir <- "data/2.scrapped/oddschecker"
infile <- file.path(indir,"odds_winner.csv")

exdir <- 'data/3.clean'
exfile <- file.path(exdir, "ind_ods_winner.csv")

exchart <- 'charts/exploratory/odds_win.png'
#read data ---------------------------------------------------------------------
lookup <- rio::import('data/1.lookups/teams_urls.csv')
odds_raw <- import(infile)


#check that the countries match the lookup table ------------------------------
countries <-  lookup$team

setdiff(odds_raw$country, countries)
setdiff(countries, odds_raw$country)

odds_clean <- odds_raw %>%
  rename(team = country) %>%
  mutate(team = case_when(team == "USA" ~ "United States",
                   team == "South Korea" ~ "Korea Republic",
                   team == "Iran" ~ "IR Iran",
                   T ~ team
                   ))


#create mean of odds to win the WC by team ------------------------------------

odds_ind <- odds_clean %>%
  #remove winonly because we dont know what it is
  select(-ends_with("Winonly")) %>%
  pivot_longer(-team,
               values_to = "odd_win") %>%
  group_by(team)%>%
  summarise(odd_win = mean(odd_win))





#inspect data-------------------------------------------------------------------
chart <- odds_ind %>%
  ggplot(aes(y = reorder(team, odd_win),
             x = odd_win)) +
  geom_col() +
  labs(x = "Odds to win WC",
       y = "") +
  theme(axis.text.y = element_text(hjust = 0),
        axis.ticks = element_blank() ,
        panel.background = element_blank(),
        plot.background = element_blank(),
        panel.grid.major.x = element_line(linetype = "dotted", color = "black"),
        panel.grid.minor.x = element_line(linetype = "dotted", color = "black"),
        
        )

#================================================================================
#export

export(odds_ind, exfile)
ggsave(exchart, chart)
