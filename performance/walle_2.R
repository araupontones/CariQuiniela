library(dplyr)
library(rio)
library(stringr)
library(janitor)
library(ggplot2)
library(scales)
library(glue)
library(extrafont)
source("functions/consistent_matches.R")
source("functions/plot_performance.R")
dir_walle <- "C:/repositaries/4.personal/walle"


#Import results of Qatar 2022 ==================================================
results_wc <- rio::import("https://raw.githubusercontent.com/araupontones/CariQuiniela/walle/data/3.clean/tablita_gordo.csv") %>%
  filter(qatar) %>%
  consistent_elo(.) %>%
  consistent_matches(.) %>%
  select(date, team, opponent, GF, GA, result,rating_team, rating_opponent, rating_dif)

# Import walle =================================================================
#walle_1 <- import(file.path(dir_walle, "results_walle_1.csv"))
walle_2 <- import(file.path(dir_walle, "results_walle_2.csv"))

walle <- walle_2 %>%
  consistent_matches(.) %>%
  select(date, team, opponent, GF, GA, result, probability)


#join results & predictions ====================================================
joint <- results_wc %>%
  left_join(walle, by = c("date", "team", "opponent"), suffix =c("_wc", "_walle"))


#create indicators or performance ==============================================
performance <- joint %>%
  mutate(hit = result_wc == result_walle,
         hit_score = GF_wc == GF_walle & GA_wc == GA_walle,
         performance = case_when(hit_score ~ "Hit score",
                                 hit ~ "Hit result",
                                 !hit ~ "Missed"
         ),
         performance = factor(performance,
                              levels = c("Hit score", "Hit result", "Missed"))
         
  ) %>%
  filter(!is.na(performance))





perc_score <-percent(mean(performance$performance == "Hit score"))
perc_result <- percent(mean(performance$hit))


#hit: 46%
#hit score 13%

performance %>% plot_performance(., "Performance Walle 2", grupos = T)

ggsave(glue('charts/performance_walle_2.svg'),
       last_plot(),
       device = 'svg')


