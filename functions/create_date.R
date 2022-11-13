#create_quarter

create_quarter <- function(.data, date){
  
  .data %>%
    mutate(month = lubridate::month({{date}}, label = TRUE),
                  year = lubridate::year({{date}}),
                  quarter = as.character(lubridate::quarter({{date}}, fiscal_start = 1,with_year = T))
    )
  
}
