#clean team names

clean_teams <- function(.data){
  
  .data %>%
    mutate(team = as.character(team),
           team = case_when(team == "USA" ~ "United States",
                            team == "Iran" ~ "IR Iran",
                            team == "South Korea" ~ "Korea Republic",
                            T ~ team))
}
