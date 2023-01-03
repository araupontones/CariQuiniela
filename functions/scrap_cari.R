#scrap cariquiniela
scrap_Cari <- function(url){
  
  page <- read_html(url)
  
  marcadores_raw <- page %>%
    rvest::html_elements(".row") %>%
    html_elements(".col-1") %>%
    html_text2() %>%
    tibble() %>%
    #remove headers
    .[-c(1:4),] 
  
  #remove total points
  last <- nrow(marcadores_raw)
  from <- nrow(marcadores_raw)-3
  marcadores_raw <- marcadores_raw[-c(from:last),]
  
  
  
  equipos_raw <- page %>%
    rvest::html_elements(".row") %>%
    html_elements(".col-2") %>%
    html_text2() %>%
    tibble()
  
  
  #clean equipos ================================================================
  names(equipos_raw) <- "team"
  
  equipos_clean <- equipos_raw %>%
    dplyr::filter(!str_detect(team, "Estadio|Total"),
           team != ""
    ) %>%
    mutate(date = ifelse(str_detect(team, "2022"), team, ""),
           date = str_sub(date, 1,10),
           team = str_remove(team, "\n")
    )
  
  
  iterate <- seq(3,nrow(equipos_clean), 3)  
  
  equipos_final <- lapply(iterate, function(i){
    s = i -2
    m = i -1
    
    d <- tibble(team = as.character(equipos_clean[s,1]),
                opponent = as.character(equipos_clean[m,1]),
                date = as.character(equipos_clean[i,2])
    )
    
    
  })
  
  equipos_final <- do.call(rbind, equipos_final)
  
  #Clean marcadores =============================================================
  
  names(marcadores_raw) <- "goles"
  
  marcadores_clean <- marcadores_raw %>%
    #remove penalties
    mutate(goles = str_remove(goles,'\\[.\\]'))
  
  
  iterate_g <- seq(4, nrow(marcadores_clean),4)
  
  
  marcadores_final <- lapply(iterate_g, function(i){
    
    s = i -3
    m = i -1
    
    d <- tibble(GF = as.numeric(marcadores_clean[s,1]),
                GA = as.numeric(marcadores_clean[m,1]),
                puntos= as.numeric(marcadores_clean[i,1])
    )
    
  })        
  
  marcadores_final <- do.call(rbind, marcadores_final)
  
  #join teams and marcadores ======================================================
  
  finaldb <- cbind(equipos_final, marcadores_final)
  return(finaldb)
  
}
