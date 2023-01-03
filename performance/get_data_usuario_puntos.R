library(extrafont)
usuarios <- import("data/4.usuarios_quiniela/lkp_users.rds")

puntos_usuarios <- lapply(usuarios, function(usuario){
  
  message(usuario)
  scrap_puntos(usuario)
  
})

data_puntos <- do.call(rbind, puntos_usuarios)

View(data_puntos)

names(data_puntos)

data_fases <- data_puntos %>%
  group_by(usuario,modo) %>%
  summarise(puntos = sum(as.numeric(puntos)), .groups= 'drop') %>%
  group_by(modo) %>%
  arrange(modo,desc(puntos)) %>%
  mutate(rank = rank(puntos)) %>%
  ungroup() 

data_total <- data_fases %>%
  group_by(usuario) %>%
  summarise(puntos = sum(puntos),
            modo = "Total") %>%
  ungroup() %>%
  arrange(puntos) %>%
  mutate(rank = rank(-puntos)) %>%
  rbind(data_fases) %>%
  arrange(usuario, modo)


export(data_total, "data/4.usuarios_quiniela/puntos_usuarios.csv")


#see champs



data_plot <- function(mode, top){
  d <- data_fases  %>%
    dplyr::filter(modo == mode) %>%
    arrange(desc(puntos)) %>%
    dplyr::filter(row_number()<=top) %>%
    mutate(usuario = forcats::fct_reorder(usuario, puntos))
  
  
  d %>%
    ggplot(aes(
      y = usuario,
      x = puntos,
      label = puntos
    )) +
    geom_col( fill = alpha("#E12577", .5)) +
    geom_text(hjust = 1,
              color = "black",
              family = "IBM Plex Mono") +
    scale_y_discrete(labels = function(x) paste0(seq(nrow(d),1, -1),". ", x)) +
    labs(y = "",
         x = "Puntos",
         title = glue('Ranking {mode}'),
         caption = "Gráfico: Manuel Negrete"
        ) +
    theme_minimal() +
    theme(
      plot.title.position = 'plot',
      axis.text.y = element_text(hjust = 0),
      text = element_text(family = "IBM Plex Mono")
    )
  
}
  
tabyl(data_plot, mode)
data_plot("Extra", 41) #Mundial ideal, Quiniela, Total, Extra
View(data_plot)  


