get_rankings <- function(.data, rankings){
  
  rankings <- rankings %>% select(-year)
  rankings$quarter <- as.character(rankings$quarter)
  
  .data %>%
    left_join(rankings, by = c("team", "quarter")) %>%
    rename(local_fifa_rank = rank,
           local_fifa_points = total_points,
           local_fifa_prev_points =previous_points) %>%
    left_join(rankings, by = c("Opponent"="team", "quarter")) %>%
    rename(visitante_fifa_rank = rank,
           visitante_fifa_points = total_points,
           visitante_fifa_prev_points =previous_points)
  
  
}
