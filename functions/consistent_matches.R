#make matches consistent between walle and results
consistent_matches <- function(.data){
  
  .data %>%
    mutate(team2 = paste(team, GF),
           opponent2 = paste(opponent, GA)) %>%
    arrange(date) %>%
    rowwise() %>%
    mutate(match = paste(sort(c(team2, opponent2)), collapse = "-")
           
    ) %>%
    ungroup() %>%
    mutate(team = str_extract(match, "(?<=^).*?(?=[0-9])"),
           opponent =str_extract(match, "(?<=\\-).*?(?=[0-9])"),
           GF = as.numeric(str_extract(match, "[0-9]*?(?=-)")),
           GA = as.numeric(str_extract(match, "[0-9]$"))
    ) %>%
    
    
    mutate(result  = case_when(GF > GA ~ "W",
                               GF < GA ~ "L",
                               GF == GA ~ "D"
    )
    ) %>%
    arrange(date)
}

#consistent elo rankings =======================================================
consistent_elo <- function(.data){
  
  .data %>%
    mutate(team2 = paste(team, pre_rating_team),
           opponent2 = paste(opponent, pre_rating_opponent)
           )%>%
    arrange(date) %>%
    rowwise() %>%
    mutate(match = paste(sort(c(team2, opponent2)), collapse = "-")
           
     ) %>%
    ungroup() %>%
    mutate(rating_team = as.numeric(str_extract(match, "[0-9]*?(?=-)")),
           rating_opponent = as.numeric(str_extract(match, "[0-9]{1,}$")),
           rating_dif = case_when(rating_team > rating_opponent ~ rating_team - rating_opponent,
                                  rating_team < rating_opponent ~ rating_opponent - rating_team,
                                  T ~ rating_team- rating_opponent
                                  )
    ) %>%


    
    arrange(date)
}
