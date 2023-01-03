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
source("functions/consistent_matches.R")


usuario <- "elleyva"







#create indicators or performance ==============================================
performance <- import("data/4.usuarios_quiniela/resultados_usuarios.csv") %>%
dplyr::filter(usuario == usuario)




perc_score <-percent(mean(performance$performance == "Hit score"))
perc_result <- percent(mean(performance$hit))


#hit: 46%
#hit score 13%
performance %>% plot_performance(., glue("Performance {usuario}"), grupos = T, prob = F)


ggsave(glue('charts/{usuario}.svg'),
       last_plot(),
       device = 'svg')


