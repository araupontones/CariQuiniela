library(dplyr)
library(ggplot2)
library(ggrepel)
library(extrafont)
library(glue)
caption=  glue("Gráfico y análisis por Andrés Arau - Datos propios - {Sys.Date()}")

matches <- rio::import("https://raw.githubusercontent.com/araupontones/CariQuiniela/walle/data/3.clean/tablita_ols.csv")

matches_wc <- matches %>% filter(qatar) %>%
  filter(!is.na(GF)) %>%
  rowwise() %>%
  mutate(id_match = paste(sort(c(team, opponent)), collapse =  "-"),
         id_match = paste(id_match, date, sep = "-")) %>%
  ungroup() %>%
  group_by(id_match) %>%
  arrange(id_match,desc(pre_rating_team)) %>%
  slice(1) %>%
  ungroup() %>%
  mutate(elo_diff = pre_rating_team - pre_rating_opponent,
         sorpresa = case_when(result == "L" ~ "Perdió",
                              result == "D" ~ "Empató",
                              result == "W" ~ "Ganó"),
         sorpresa = factor(sorpresa, levels = c("Perdió", "Empató", "Ganó")),
         match = paste(paste(team,GF), paste(GA,opponent), sep = "-")
         ) %>%
  select(year, date, team, opponent, GF, GA,starts_with("pre"), result, elo_diff,sorpresa, match)


color_perdio ="#122740"
color_gano = "#B9D5B2"
  color_empato = "#F4F6CC"

  janitor::tabyl(matches_wc, sorpresa)

  #7 games
  #32

View(filter(matches_wc, sorpresa == "Perdió"))

ggplot(matches_wc,
       aes(x = elo_diff,
           y = date,
           color = sorpresa,
           label = match
           )
       ) +
  geom_point(aes(size = elo_diff)) +
  geom_text_repel(data = filter(matches_wc, sorpresa=="Perdió"),
            show.legend = F,
            size = 2.7,
            color = "#525252",
            nudge_x =0, 
            nudge_y = -.55,
            direction = c("both", "y", "x"),
            segment.color = color_perdio,
            segment.size = 1,
            family = "IBM Plex Mono") +
  guides(size = F,
         color = guide_legend(title.position = "top", title.hjust=0.5)) +
  scale_color_manual(values = c(color_perdio,color_empato ,color_gano),
                     name = "Los colores de los puntos indican si el equipo favorito:") +
  
  scale_y_date(breaks = unique(matches_wc$date),
               labels = function(x)format(x, "%b %d")) +
  
  labs(y ="",
       x = "Diferencia entre los equipos (según ELO ratings) -->",
       title = 'Partidos de Catar 2022 que perdieron "los favoritos".',
       subtitle = 'La mayor sorpresa fue la victoria de Camerún a Brasil.',
       caption = caption
       )+
  theme_minimal() +
  theme(legend.position = "top",
        plot.title.position = 'plot',
        legend.title = element_text(colour = "#525252"),
        legend.text = element_text(color = '#525252'),
        axis.text = element_text(color ="#525252"),
        axis.title.x = element_text(margin = margin(t= 10), color ="#525252"),
        
        plot.title = element_text(face = 'bold'),
        plot.subtitle =element_text(margin = margin(b=10)),

        panel.grid.major.y =  element_blank(),
        panel.grid.minor.y =  element_blank(),
        text = element_text(family = "IBM Plex Mono"),
        plot.background = element_rect(fill = "white")
        )


ggsave(glue('charts/sorpresas_{Sys.Date()}.png'),
       last_plot(),
       device = 'png')
