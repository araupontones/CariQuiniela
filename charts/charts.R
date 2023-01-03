library(dplyr)
library(ggplot2)
library(ggflags)
library(tidyr)
library(ggbump)
library(glue)
library(extrafont)


#define dates of jornadas ------------------------------------------------------
end_jornada1 <- "2022-11-24"
end_jornada2 <- "2022-11-28"
start_jornadas <- c("2022-11-20", "2022-11-25") 
end_jornadas <- c("2022-11-24", "2022-11-28")


jornadas <- setNames(c(start_jornadas), c("jornada_1", "jornada_2"))
labels <- c("antes", "jornada 1", "jornada 2")

caption=  glue("Gráfico y análisis por Andrés Arau - Datos propios - {Sys.Date()}")

#define teams------------------------------------------------------------------
teamswc <- rio::import("https://raw.githubusercontent.com/araupontones/CariQuiniela/walle/data/1.lookups/teams_urls.csv") 
teamswc <- teamswc$team

#read elo ratings from walle ---------------------------------------------------
elo <- rio::import("https://raw.githubusercontent.com/araupontones/CariQuiniela/walle/data/3.clean/elo_ratings.csv") %>%
  filter(team %in% teamswc)



#datos de antes de la copa -----------------------------------------------------
before_wc <- elo %>% filter(date < "2022-11-20") %>%
  group_by(team) %>%
  filter(date == max(date)) %>%
  ungroup() %>%
  arrange(desc(rating)) %>%
  mutate(lugar = row_number(),
         jornada = "antes")


#datos jornada por jornada --------------------------------------------------


lista_jornadas <- lapply(1:length(jornadas), function(i){
  
  jornada <- names(jornadas[i]) #name
  start_jornada <- jornadas[[i]]
  end_jornada <- end_jornadas[[i]]
  
  if(i-1 == 0){
    
    start_jornada <- "2022-11-20"
  } else{
    
    start_jornada <- start_jornadas[[i]]
  }
  
  data_jornada <- elo %>% filter(date >= start_jornada & date <= end_jornada) %>%
    arrange(desc(rating)) %>%
    mutate(lugar = row_number(),
           jornada= jornada
    )
  
  
})


during_wc <- do.call(rbind, lista_jornadas) %>%
  arrange(team, desc(date)) %>%
  group_by(team) %>%
  mutate(rating = case_when(is.na(rating) & !is.na(lead(rating)) ~ lead(rating,1),
                            is.na(rating) & is.na(lead(rating,1)) & !is.na(lead(rating,2)) ~ lead(rating,2),
                            T ~ rating
  )
  
  ) %>%
  ungroup()
group_by(jornada) 
arrange(desc(rating)) %>%
  mutate(ranking = row_number()) %>%
  ungroup() %>%
  arrange(jornada, ranking)





last_rating <- paste0("rating_",names(jornadas)[length(jornadas)])
last_ranking <- paste0("lugar_",names(jornadas)[length(jornadas)])



#Plot cambio de elo ratings ---------------------------------------------------
data_plot <- before_wc %>%
  rbind(during_wc) %>%
  arrange(team, desc(date)) %>%
  group_by(team) %>%
  mutate(rating = case_when(is.na(rating) & !is.na(lead(rating)) ~ lead(rating,1),
                            is.na(rating) & is.na(lead(rating,1)) & !is.na(lead(rating,2)) ~ lead(rating,2),
                            T ~ rating
  )
  
  ) %>%
  ungroup %>%
  group_by(jornada) %>%
  arrange(desc(rating)) %>%
  mutate(ranking = row_number()) %>%
  ungroup() %>%
  arrange(jornada, ranking) %>%
  pivot_wider(id_cols = c(team),
              values_from = c(rating,lugar),
              names_from = jornada) %>%
  #filter if the team has not played in this round yet
  #filter(!is.na(.[[last_Rating]])) %>%
  mutate(change = .[[last_rating]] - rating_antes,
         ch_rating = lugar_antes- .[[last_ranking]],
         team = forcats::fct_reorder(team, rating_antes),
         color = ifelse(change > 0, "green", "red"),
         color_ch = factor(case_when(
           team %in% c("Morocco", "France")  ~ "green",
           team %in% c("Mexico", "Belgium") ~ "red",
           !team %in% c("Mexico", "Morocco") ~ "gray"
         ),
         levels = c("red", 'gray', "green")
         ))



View(data_plot)
#plot --------------------------
ggplot(data_plot,
       aes(x = team,
           y = change,
           fill = color
       )
) +
  geom_col() +
  labs(y = "Cambio en rating",
       x = "",
       title = "Cambios en Elo ratings después de la segunda jornada del mundial.",
       subtitle = "Marruecos ha ganado 61 puntos tras empatar con Croacia y ganar a Belgica.",
       caption=  glue("Gráfico y análisis por Andrés Arau - Datos propios - {Sys.Date()}")) +
  scale_fill_manual(values = c("#DA2677", "#484CA3")) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, hjust =  0),
        legend.position = "none",
        plot.title.position = "plot",
        text = element_text(family = "IBM Plex Mono"),
        plot.background = element_rect(fill = "white")
  )

ggsave(glue('charts/change_ratings_{Sys.Date()}.png'),
       last_plot(),
       device = 'png',
       width = 7.5,
       height = 5.68)


#Data bump ===================================================================       

keep <- paste0("lugar_", c("antes",names(jornadas)))


data_bump <- data_plot%>%
  select(all_of(c("team", keep, "color_ch"))) %>%
  pivot_longer(-c(team,color_ch), 
               values_to = "rank",
               names_to = "cuando"
  ) %>%
  mutate(cuando = stringr::str_remove(cuando, "lugar_|_"),
         cuando = stringr::str_replace(cuando, "_"," "),
         cuando = factor(cuando,
                         levels = labels),
         color_ch = as.character(color_ch)) 


View(data_bump)

ggplot(data_bump, aes(cuando, rank, direction = team, color = color_ch)) +
  #bumps 
  geom_point(size = 2) +
  geom_bump(smooth = 8, linewidth = 1) +
  #text
  geom_text(data = data_bump %>% filter(cuando == "antes"),
            aes(x = cuando, label = glue('({rank}) {team}')), size = 3, hjust = 1.1) +
  
  
  geom_text(data = data_bump %>% filter(cuando == labels[length(labels)]),
            aes(x =cuando, label = glue('({rank}) {team}')), size = 3, hjust = -.15) +
  
  
  #label
  labs(y = "",
       x = "",
       title = "Cambio en los rankings ELO después de la segunda jornada.",
       subtitle = 'Empatando con Croacia y ganando a Belgica, Marruecos paso del 24 al 17.',
       caption = caption) +
  
  #scales
  scale_y_reverse(breaks = 1:30) +
  scale_color_manual(values = c("gray", "#228422", "#CB212C")) +
  
  theme_minimal() +
  theme(legend.position = "none",
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.text = element_blank(),
        text = element_text("IBM Plex Mono"),
        plot.background = element_rect(fill = "white")
  )



ggsave(glue('charts/change_rankings_{Sys.Date()}.png'),
       last_plot(),
       device = 'png')
