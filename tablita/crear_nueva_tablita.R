library(zoo)
library(dplyr)
library(stringr)
library(dplyr)

exfile = 'tablita/tablita_gordo.csv'
exfile_ols = 'tablita/tablita_ols.csv'


# read data =====================================================================
#Read elo ratings that is created in elo/append_ratings.R
elo <- rio::import("elo/elo_rankings.csv") %>% mutate(qatar = FALSE)


#read all matches created by walle 
wc_matches <- rio::import("https://raw.githubusercontent.com/araupontones/CariQuiniela/walle/data/3.clean/ind_all_matches.csv")


#make wv tables compatible with elo in terms of names 
wc_matches <- filter(wc_matches, qatar) %>%
  rename(date = Date,
         opponent = Opponent,
         goles_team = goles_local,
         goles_opponent = goles_visitante
         ) %>%
  mutate(torneo = "World Cup" )



#bind tables ===================================================================
all_matches <- plyr::rbind.fill(elo, wc_matches)





#Bring oponentes into the teams colum ==========================================
# I am doing this so each time has its unique history of GF and GA
oponentes <- all_matches

names(oponentes) <- c("date", "year",
                      "opponent", "team",
                      "goles_opponent", "goles_team",
                      "torneo", "sede",
                      "rating_change_opponent", "rating_change_team",
                      "rating_opponent", "rating_team",
                      "rank_change_opponent", "rank_change_team",
                      "rank_opponent", "rank_team", "qatar"
                      )



matches <- rbind(all_matches, oponentes)




# Create indicators ===========================================================
#funciton to sum criterias

suma_rol <- function(criteria){zoo::rollsum(criteria,5, align = 'left', fill = NA)}



elos <- rio::import("elo/pre_wc_ratings.csv")

View(tablita)
names(elos)
tablita <- matches %>%
  get_pre_rating(., elos) %>%
  select(date,team, opponent, starts_with("pre")) %>%
  filter(team == "Qatar")
  #rename variables to ease its analysis
  rename(GF = goles_team,
         GA = goles_opponent,
         post_rating_team = rating_team,
         post_rating_opponent = rating_opponent
         ) %>%
  #create indicators by team 
  group_by(team) %>%
  arrange(team, desc(year),desc(date)) %>%
  #create pre ranking ---------------------------------------------------------
    mutate(last_match = lead(date),
           pre_rating_team = get_pre_rating(elos, team, date=last_match, post_rating_team)
           ) %>%
  ungroup() %>%
  mutate(pre_rating_opponent = get_opponent_pre_rating(.,opponent, date), .after = sede) 
#create indicators ------------------------------------------------------------
arrange(team, desc(year),desc(date)) %>%
  mutate(
    #ind_did_score = suma_rol(GF>0),
    ind_fail_to_score = suma_rol(GF==0),
    #ind_did_receive_goal = suma_rol(GA>0),
    ind_not_receive_goal = suma_rol(GA==0),
    ind_GF_last_5 = rollsum(GF, 5,align = 'left', fill = NA),
    ind_GA_last_5 = rollsum(GA, 5,align = 'left', fill = NA),
    #ind_pre_rating_team = lead(post_rating_team),
    #ind_pre_rating_opponent = lead(post_rating_opponent))
    #so take the ones before the match
    across(starts_with("ind"), ~lead(.x)),
         .after = GA ) %>%
  ungroup()%>%
  select(date, year, team, opponent,GF,GA, starts_with("ind"), starts_with("pre"),starts_with("post"),qatar) %>%
  rename_at(vars(starts_with("ind_")), function(x)str_remove_all(x,"ind_")) %>%
  filter(year > 2017)
  #calculate elo
  mutate(
  result = result(GF, GA),
  expected_result = diff_ratings(pre_rating_team,pre_rating_opponent),
  elo = elo_score(pre_rating_team, 60, result, expected_result)
  ) %>%
  filter(qatar)
 
View(tablita)


#create table for OLS ==========================================================

opponents <- tablita %>%
  select(team, date, GA_last_5, not_receive_goal)
tablita_final <- tablita %>%
  left_join(opponents, by = c("opponent" = "team", "date"), suffix = c("_team", "_opponent"))





#Create table for roy ==========================================================
roys <- all_matches %>%
  filter(year > 2017) %>%
  select(date,team) %>%
  #get indicators of team
  left_join(select(tablita,-c(year), -Home, -starts_with("post")), by= c("team","date")) %>%
  #get indicators of opponent & dropping duplicated variables
  left_join(select(tablita,
                   -c(starts_with('pre'),
                   starts_with('post'),
                   year,
                   GF,
                   GA,
                   Home,
                   qatar,
                   dr_pre,
                   opponent)
                   ), by = c("opponent"= "team", "date"), suffix = c("_team", "_opponent")) %>%
  arrange(team, desc(date))







vars <- names(roys)
roys2 <- roys %>% filter(!qatar) %>% filter(is.na(did_score_team))
View(roys2)

lapply(vars, function(x){
  
  
  message(x)
  print(sum(is.na(roys2[[x]])))
})


rio::export(roys, exfile)
rio::export(tablita_final, exfile_ols)
