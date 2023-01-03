translate_teams <- function(.data, column){
  
  .data %>%
    mutate({{column}} := case_when({{column}} == "Catar" ~ "Qatar",
                                   {{column}} == "Inglaterra" ~ "England",
                                   {{column}} == "Estados Unidos" ~ "United States",
                                   {{column}} == "Holanda" ~ "Netherlands",
                                   {{column}} == "Gales" ~ "Wales",
                                   str_detect({{column}}, "Ir")  ~ "IR Iran",
                                   str_detect({{column}},"xico")  ~ "Mexico",
                                   {{column}} == "Polonia" ~ "Poland",
                                   {{column}} == "Arabia Saudita" ~ "Saudi Arabia",
                                   {{column}} == "Dinamarca" ~ "Denmark",
                                   {{column}} == "Francia" ~ "France",
                                   str_detect({{column}}, "nez") ~ "Tunisia",
                                   {{column}} == "Alemania" ~ "Germany",
                                   str_detect({{column}},"Espa") ~ "Spain",
                                   str_detect({{column}}, "Ja") ~ "Japan",
                                   {{column}} == "Marruecos" ~ "Morocco",
                                   str_detect({{column}}, "lgica") ~ "Belgium",
                                   {{column}} == "Croacia" ~ "Croatia",
                                   str_detect({{column}}, "Cana") ~ "Canada",
                                   {{column}} == "Suiza" ~ "Switzerland",
                                   {{column}} == "Brasil" ~ "Brazil",
                                   str_detect({{column}}, "Camer") ~ "Cameroon",
                                   {{column}} == "Corea del Sur" ~ "Korea Republic",
                                   
                                   
                                   
                                   T ~ {{column}}
                                   ))
}
