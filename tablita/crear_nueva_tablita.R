library(zoo)

exfile = 'tablita/tablita_gordo.csv'
#Read elo ratings that is created in elo/append_ratings.R
elo <- rio::import("elo/elo_rankings.csv")
#read all matches created by walle 
wc_matches <- rio::import("https://raw.githubusercontent.com/araupontones/CariQuiniela/walle/data/3.clean/ind_all_matches.csv")

#make tabless compatible by names 
wc_matches <- filter(wc_matches, qatar) %>%
  rename(date = Date,
         opponent = Opponent,
         goles_team = goles_local,
         goles_opponent = goles_visitante
         ) %>%
  mutate(torneo = "World Cup" )


#bind tables
all_matches <- plyr::rbind.fill(elo, wc_matches)





#create a single column of team ===============================================
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




# Create tablita variables ====================================================
#funciton to sum criterias
suma_rol <- function(criteria){zoo::rollsum(criteria,5, align = 'left', fill = NA)}

tablita <- matches %>%
  rename(GF = goles_team,
         GA = goles_opponent,
         post_rating_team = rating_team,
         post_rating_opponent = rating_opponent
         ) %>%
  group_by(team) %>%
  arrange(team, desc(year),desc(date)) %>%
  mutate(
    ind_did_score = suma_rol(GF>0),
    ind_fail_to_score = suma_rol(GF==0),
    ind_did_receive_goal = suma_rol(GA>0),
    ind_not_receive_goal = suma_rol(GA==0),
    ind_GF_last_5 = rollsum(GF, 5,align = 'left', fill = NA),
    ind_GA_last_5 = rollsum(GA, 5,align = 'left', fill = NA),
    ind_rating_team = post_rating_team,
    ind_rating_opponent = post_rating_opponent,
    #so take the ones before the match
    across(starts_with("ind"), ~lead(.x)),
    
   Home = str_detect(sede, team),
   Home = ifelse(is.na(Home), FALSE, Home),
   
   #rating_difference
   
    
   # #calculate rantings
   # tournament_type = case_when(torneo == "World Cup" ~ "WC_finals"
   #                             torneo %in% c("African Nations Cup",
   #                                           "Arab Nations Cup",
   #                                           "CONCACAF Championship",
   #                                           
   #                                           
   #                                           )
   #                             
   #                             ),
   
         .after = GA ) %>%
  select(date, year, team, opponent, Home, starts_with("ind"), starts_with("post"), qatar) %>%
  rename(pre_rating_team = ind_rating_team,
         pre_rating_opponent = ind_rating_opponent) %>%
  rename_at(vars(starts_with("ind_")), function(x)str_remove_all(x,"ind_")) %>%
  #difference
  mutate(dr_pre = ifelse(Home, pre_rating_team - pre_rating_opponent + 100, pre_rating_team - pre_rating_opponent)) %>%
  filter(year > 2017)
 

rio::export(tablita, exfile)
