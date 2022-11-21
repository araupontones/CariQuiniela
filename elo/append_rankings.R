library(httr)


years <- c(2017,2018,2020,2021, 2022)


# my_data <- read.delim("elo/2021.csv", 
#                       stringsAsFactor = FALSE)
# 
# 


lista_anos <- lapply(years, function(y){
  
  message(y)

  text <- read.delim(glue("elo/{y}.csv"), 
                  stringsAsFactor = FALSE)
  
  message(nrow(text)/16)
  start_rows <- seq(1,nrow(text),16)

  #transform matches into list
  list_matches <- lapply(start_rows, function(x){

    #define length of vector
    #each match has 16 variables
    
    end = x + 15
    long <- text[x:end,1]




  })

  #append all matches into a single data frame
  wide <- as.data.frame(do.call(rbind, list_matches))
  
  
})


#append all years 
results <- do.call(rbind, lista_anos) %>%
  mutate_all(function(x)str_replace(x, "âˆ’", "-"))


#define names of data frame
names(results) <- c("date", "year", "team","opponent", "goles_team", "goles_opponent",
                 "torneo", "sede", "rating_change_team", "rating_change_opponent",
                 "rating_team", "rating_opponent", "rank_change_team", "rank_change_opponent",
                 "rank_team", "rank_opponent"
                 
                 )

#format variable date
results_final <- results %>%
  mutate(date = as.Date(glue('{date}-{year}'), format = "%b-%d-%Y"),
         across(c(team, opponent), ~ case_when(.x == "Iran" ~ "IR Iran",
                                               .x == "South Korea" ~ "Korea Republic",
                                 T ~ .x))
         )


rio::export(results_final, "elo/elo_rankings.csv")
