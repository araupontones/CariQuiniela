library(extrafont)

#define usuarios ============================================================
usuario1 <- "walle"
usuario2 <- "rigelbarros"
usuarios <- c(usuario1, usuario2)

#define tipo de comparacion ================================================

por <- "result" #c(result, score)

if(por == "result"){
  comparacion <- "resultados"
} else {
  
  comparacion <- "marcadores"
}


#Automatico a partir de aqui
create_data <- function(usuarios){
  
  lista <- lapply(usuarios, function(usuario){
    
    results_usuario <- import(glue('data/4.usuarios_quiniela/{usuario}.csv')) %>%
      consistent_matches(.)  %>%
      select(date, team, opponent, GF, GA, result)
    
    
  })
  
  
  d <- lista %>% purrr::reduce(left_join, by = c("team", "opponent", "date")) %>%
    mutate(same_result = result.x == result.y,
           same_score = GF.x == GF.y & GA.x == GA.y,
           date = as.Date(date)) %>%
    group_by(date) %>%
    summarise(result = sum(same_result, na.rm = T),
              score = sum(same_score, na.rm = T),
              matches = n()) %>%
    ungroup() %>%
    pivot_longer(-date,
                 names_to = "indicator",
                 values_to = "value")
    
  
  
}



data_compare <- create_data(usuarios)

# crear zona gris entre copias y total de partidos -------------------------------
bounds <- data_compare %>%
  pivot_wider(date,
              names_from = "indicator",
              values_from = "value") %>%
  mutate(
    ymax = pmax(result, matches),
    ymin = pmin(result, matches),
    fill = result > matches
  )


#Plot 
  ggplot(data = dplyr::filter(data_compare, indicator == "result")) +
 geom_area(
   aes(x = date,
       y = value,
       ),
   fill = alpha("#E12577", .5)
   ) +
  geom_ribbon(data = bounds, aes(date, ymin = ymin, ymax = ymax, fill = fill), alpha = 0.4, show.legend = F) +
    scale_fill_manual("fill", values = "gray")+


  
  
  # geom_area(data = dplyr::filter(data_compare, indicator %in% c("result"))) +
  #grupos ---------------------------------------------------------------
annotate("text", y = 4, x = as.Date("2022-11-26"), label = "Grupos", 
         family = "IBM Plex Mono", hjust = .5, vjust = 1  ) +
  #line octavos -------------------------------------------------------------
geom_vline(aes(xintercept = as.Date("2022-12-03")-.5),
           linetype = "dashed")  +
  annotate("text", y = 4, x = as.Date("2022-12-05"), label = "1/16", 
           family = "IBM Plex Mono", hjust = .5, vjust = 1 ) +
  #line quartos ---------------------------------------------------
geom_vline(aes(xintercept = as.Date("2022-12-07")+ .5),
           linetype = "dashed") +
  annotate("text", y = 4, x = as.Date("2022-12-09")+.5, label = "1/4", 
           family = "IBM Plex Mono", hjust = .5, vjust = 1  ) +
  
  #line semis ---------------------------------------------------
geom_vline(aes(xintercept = as.Date("2022-12-11")+ .5),
           linetype = "dashed") +
  annotate("text", y = 4, x = as.Date("2022-12-13")+.5, label = "1/2", 
           family = "IBM Plex Mono", hjust = .5, vjust = 1  ) +
    scale_x_date(breaks = unique(data_compare$date),
                 labels = function(x)format(x, "%b %d")) +
    labs(y = "Partidos",
         x = "",
         title = glue('Comparación {comparacion} entre {usuario1} y {usuario2}'),
         subtitle = glue("La zona gris muestra el total de partidos, la rosa los {comparacion} iguales."),
         caption = "Análisis y gráfico: Luka Modric"
         )+
  
  theme_minimal() +
    theme(axis.text.x = element_text(angle = 90),
          text = element_text(family = "IBM Plex Mono" ),
          plot.title.position = 'plot'
          )
  







