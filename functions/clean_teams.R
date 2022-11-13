#clean team names

clean_teams <- function(.data, var_name){
  
  .data %>%
    mutate({{var_name}} := as.character({{var_name}}),
           {{var_name}} := case_when({{var_name}} == "USA" ~ "United States",
                                    {{var_name}} == "Iran" ~ "IR Iran",
                                    {{var_name}} == "South Korea" ~ "Korea Republic",
                                    
                                    #teams fifa
                                    {{var_name}} == "Antigua and Barbuda" ~ "Antigua",
                                    {{var_name}} == "Bosnia and Herzegovina" ~ "Bosnia & Herz'na",
                                    {{var_name}} == "Côte d'Ivoire" ~ "CÃ´te d'Ivoire",
                                    {{var_name}} == "Cape Verde Islands" ~ "Cape Verde",
                                    {{var_name}} == "Central African Republic" ~ "CAR",
                                    {{var_name}} == "Curacao" ~ "CuraÃ§ao",
                                    {{var_name}} == "Dominican Republic"~ "Dominican Rep.",
                                    {{var_name}} == "Guinea"~ "Equ. Guinea",
                                    {{var_name}} == "FYR Macedonia"  ~ "N. Macedonia",
                                    {{var_name}} == "Papua New Guinea" ~ "Papua NG",
                                    {{var_name}} == "Republic of Ireland" ~ "Rep. of Ireland",
                                    {{var_name}} == "São Tomé and Príncipe" ~ "SÃ£o TomÃ©",
                                    {{var_name}} == "St. Kitts and Nevis" ~ "St. Kitts & Nevis",
                                    {{var_name}} == "St. Vincent / Grenadines" ~ "St. Vincent",
                                    {{var_name}} == "Trinidad and Tobago" ~ "Trin & Tobago",
                                    {{var_name}} == "United Arab Emirates" ~ "UAE",
                                    
                                    
                                    
                                    T ~ {{var_name}})
           
           )
  
  
  }

clean_Opponent <- function(.data, var_name){
  
  .data %>%
  mutate(Opponent = stringr::str_remove(Opponent, "^[a-z][a-z] "),
         Opponent = stringr::str_remove(Opponent, "^[a-z][a-z][a-z] "))
}




     