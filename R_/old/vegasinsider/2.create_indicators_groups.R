library(dplyr)
library(tidyr)

#From : "https://www.vegasinsider.com/soccer/odds/world-cup/"

indir <- "data/2.scrapped/vegasinsider"
infile <- file.path(exdir, "odds_groups.csv")

exdir <- 'data/3.clean'
exfile <- file.path(exdir, "ind_ods_groups.csv")

exchart <- 'charts/exploratory/odds_groups.png'
#read data ---------------------------------------------------------------------
lookup <- rio::import('data/1.lookups/teams_urls.csv')
odds_raw <- import(infile)

names(odds_raw)

#check that the countries match the lookup table ------------------------------
countries <-  lookup$team

setdiff(odds_raw$Team, countries)
setdiff(countries, odds_raw$Team)

odds_clean <- odds_raw %>%
  rename(team = Team) %>%
  mutate(team = case_when(team == "USA" ~ "United States",
                          T ~ team
  ))

setdiff(odds_clean$team, countries)


View(chart)
#inspect data-------------------------------------------------------------------
chart <- odds_clean %>%
  mutate(team = forcats::fct_reorder(team, -Group_Winner)) %>%
  pivot_longer(-c(team, grupo),
               names_to = "ind",
               values_to = "odd") %>%
  
  ggplot(aes(y = team,
             x = odd,
             fill = ind)) +
  geom_col(position = 'dodge') +
  geom_vline(xintercept = 0,
             size = 1)+
  facet_wrap(~grupo, scales = "free_y", ncol = 2) +
  labs(x = "Odds to win WC",
       y = "") +
  theme(axis.text.y = element_text(hjust = 0),
        axis.ticks = element_blank() ,
        panel.background = element_blank(),
        plot.background = element_blank(),
        panel.grid.major.x = element_line(linetype = "dotted", color = "black"),
        panel.grid.minor.x = element_line(linetype = "dotted", color = "black"),
        legend.position = "top",
        legend.title = element_blank(),
        strip.text = element_text(face = "bold", size = 12)
        
  )


#export =======================================================================
export(odds_clean, exfile)
ggsave(exchart, chart)

