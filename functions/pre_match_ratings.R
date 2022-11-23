




get_pre_rating <- function(.data, elos){
  

  
  db <- .data %>%
    group_by(team) %>%
    arrange(team, desc(year), desc(date)) %>%
    mutate(last_match= lead(date)) %>%
    ungroup() %>%
    left_join(elos, by = c("team", "last_match"="date")) %>%
    rename(pre_rating = rating)
  
  # 
  oponents <- db %>%
    select(opponent = team,
           date,
           pre_rating)


  final <- db %>%
    left_join(oponents, by = c('date', 'opponent'), suffix =c("_team", "_opponent"))

}

