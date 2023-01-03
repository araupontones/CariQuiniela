

#analisis de mejor performance en la quiniela


caption <- "Análisis y gráfico: J.J Muñante"


data_users <-import("data/4.usuarios_quiniela/resultados_usuarios.csv")

#create indicators -------------------------------------------------------------

indicators <- data_users %>%
  dplyr::filter(performance != "Not predicted") %>%
  group_by(usuario) %>%
  summarise(hit = mean(hit),
         hit_score = mean(hit_score))%>%
  ungroup() 


#resultados
indicators %>%
  arrange(desc(hit_score)) %>%
  dplyr::filter(row_number() <=30) %>%
  arrange(desc(usuario)) 
  mutate(
          usuario = forcats::fct_reorder(usuario, hit_score)
         )%>%
  ggplot(aes(y = usuario,
             x = hit_score,
             label = percent(hit_score)
             )) +
  geom_col() +
  geom_text(hjust = 1,
            color = "white") +
  scale_x_continuous(labels = function(x)percent(x)) +
  theme_minimal() +
  labs(y = "",
       x = "Resultados acertados",
       title = "Porcentaje de resultados acertados",
       caption = caption
       ) +
  theme(axis.text.y = element_text(hjust = 0))


#marcadores
indicators %>%
  ggplot(aes(y = user_score,
             x = hit_score,
             label = percent(hit_score)
  )) +
  geom_col() +
  geom_text(hjust = 1,
            color = "white") +
  scale_x_continuous(labels = function(x)percent(x)) +
  theme_minimal() +
  labs(y = "",
       x = "Marcadores acertados",
       title = "Porcentaje de marcadores acertados",
       caption = caption
  ) +
  theme(axis.text.y = element_text(hjust = 0),
        plot.title.position = 'plot')

View(indicators)
