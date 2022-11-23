
result <- function(GF, GA){
  
  case_when(GF > GA ~ "W",
            GF < GA ~ "L",
            GF == GA ~ "D"
            )
  
}

diff_ratings <- function(rating_team, rating_opponent){
  dr = rating_team - rating_opponent
  
  1/(10^(-dr/400) +1)
  
}

elo_score <- function(R_0, k, result, diff_ratings){
  
  w = case_when(result == "W" ~ 1,
                result == "D" ~ 0.5,
                result == "L" ~ 0)
  
  round(R_0 + k * (w-diff_ratings))
  
}
