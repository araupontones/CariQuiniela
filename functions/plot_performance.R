
color_score = "#118F46"
#color_score = "#B9D5B2"
color_result = "#A5D4AF"
#color_result = "#F4F6CC"
color_missed = "gray"
#color_missed ="#122740"
color_not_predicted = "#F76F72"


#"#122740"

paleta <- c(color_score,color_result,color_missed, color_not_predicted)



plot_performance <- function(.data, title, grupos= F, prob = T){
  
  
  
    if(prob){
      
      plot <-  .data %>%
        ggplot(aes(x = rating_dif,
                   y = date,
                   size = probability,
                   color = performance,
                   fill = performance
        ))  +
        geom_point(shape = 21,
                   stroke = 2,
                   alpha = .7) 
    } else {
      
      plot <-  .data %>%
        ggplot(aes(x = rating_dif,
                   y = date,
                   color = performance,
                   fill = performance
        )) +
        geom_point(shape = 21,
                   stroke = 2,
                   alpha = .7,
                   size = 4) 
      
    }
   
    
 
  

  plot <- plot  +
    geom_point(shape = 21,
               stroke = 2,
               alpha = .7) 
 
 if(grupos){
   
   
   plot <- plot +
     #grupos ---------------------------------------------------------------
   annotate("text", x = 400, y = as.Date("2022-11-26"), label = "Group stage", 
            family = "IBM Plex Mono", hjust = 0 ) +
     #line octavos -------------------------------------------------------------
   geom_hline(aes(yintercept = as.Date("2022-12-03")-.5),
              linetype = "dashed") +
     annotate("text", x = 400, y = as.Date("2022-12-05"), label = "Round of 16", 
              family = "IBM Plex Mono", hjust = 0 ) +
     #line quartos ---------------------------------------------------
   geom_hline(aes(yintercept = as.Date("2022-12-07")+ .5),
              linetype = "dashed") +
     annotate("text", x = 400, y = as.Date("2022-12-09")+.5, label = "Quarter finals", 
              family = "IBM Plex Mono", hjust = 0 ) +
   
   #line semis ---------------------------------------------------
   geom_hline(aes(yintercept = as.Date("2022-12-11")+ .5),
              linetype = "dashed") +
     annotate("text", x = 400, y = as.Date("2022-12-13")+.5, label = "Semi finals", 
              family = "IBM Plex Mono", hjust = 0 ) 
 }
    

 
 #labs & style
 plot +
    labs(y = "",
         x = "Difference in ELO ratings between teams",
         title = title,
         subtitle = glue('Hit results: {perc_result} / Hit scores: {perc_score}')
    ) +
    scale_color_manual(values = paleta) +
   scale_fill_manual(values = paleta) +
   scale_size(range = c(3, 14)) +
    scale_y_date(breaks = unique(performance$date),
                 labels = function(x)format(x, "%b %d")) +
   #guide legend ----------------------------------------------
    guides(
      size = guide_legend(override.aes = list(color= "black", stroke = 1),
                              title = "Probability", order = 2),
           
           fill = guide_legend(title.position = "top", title.hjust=0.5, 
                                override.aes = list(size = 4, stroke = 0), title = "Perfomance", order =1),
           color ="none"
    ) +
    theme_minimal() +
    theme(
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
  
  
  
}
