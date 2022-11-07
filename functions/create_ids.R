create_ids <- function(.data, db_lp){
  
  .data %>%
    left_join(db_lp, by = "team") %>%
    rename(id_team = id) %>%
    left_join(db_lp, by = c("Opponent"= "team")) %>%
    rename(id_opponent = id) %>%
    rowwise() %>%
    #get id of match by combining the ID of the two teams
    mutate(id_match = paste(sort(c(id_team, id_opponent)), collapse =  "-"),
           id_match = paste(id_match, Date, sep = "-"))
  
}

