
library(rio)
library(dplyr)
library(ggplot2)
exfile <- file.path("data/3.clean/fifa_rankings.csv")

lookup <- import("data/3.clean/ind_all_matches.csv", encoding = "UTF-8") 
rankings <- rio::import("fifa_rankings/fifa_ranking-2022-10-06.csv", encoding = "UTF-8")
odds <- rio::import("data/3.clean/ind_ods_winner.csv") # to check correlation between ranking fifa and odds



#names of teams in lookup 
locales <- unique(lookup$team)
visitantes <- unique(lookup$Opponent)
teams <- sort(unique(c(locales, visitantes)))




#clean rankings fifa ------------------------------------------------------------



rankings_clean <- rankings %>%
  rename(team = country_full,
         Date = rank_date) %>%
   clean_teams(.,team ) %>%
  create_quarter(., Date) %>%
  filter(year >= 2011) %>%
  group_by(team, quarter) %>%
  #create rankings by quarter to enable joining with all_maches
  summarise(rank = mean(rank),
            total_points = mean(total_points),
            previous_points = mean(previous_points),
            .groups = 'drop'
            )
  
View(rankings_clean)

export(rankings_clean, exfile)

#check with odds




teams_fifa_clean <- sort(unique(rankings_clean$team))

setdiff(teams, teams_fifa_clean)

#check correlation with odds
last_ranking <- rankings_clean %>% dplyr::filter(quarter == "2022.4")



# 
# data_plot <- odds %>%
#   arrange(odd_win) %>%
#   mutate(ranking_ods = row_number()) %>%
#   left_join(last_ranking)
# 
# View(data_plot)
# 
# #by odds
# data_plot %>%
#   ggplot(aes(x = ranking_ods,
#              y = rank,
#              label = team)) +
#   geom_point() +
#  geom_smooth()+
#   ggrepel::geom_text_repel() +
#   labs(x = "Rank Odds",
#        y = "Rank Fifa",
#        title = "Ranking odds vs Ranking Fifa")
#   
# 
# #by points
# #by odds
# data_plot %>%
#   ggplot(aes(x = odd_win,
#              y = total_points,
#              label = team)) +
#   geom_point() +
#   geom_smooth()+
#   ggrepel::geom_text_repel() +
#   labs(x = "odds to win",
#        y = "fifa points",
#        title = "Odds to win vs Total points FIFA")


