#clean team names

clean_teams <- function(.data, var_name){
  
  .data %>%
    mutate({{var_name}} := as.character({{var_name}}),
           {{var_name}} := case_when({{var_name}} == "USA" ~ "United States",
                                    {{var_name}} == "Iran" ~ "IR Iran",
                                    {{var_name}} == "South Korea" ~ "Korea Republic",
                                    T ~ {{var_name}})
           
           )
  
  
  }

clean_Opponent <- function(.data, var_name){
  
  .data %>%
  mutate(Opponent = stringr::str_remove(Opponent, "^[a-z][a-z] "),
         Opponent = stringr::str_remove(Opponent, "^[a-z][a-z][a-z] "))
}




     