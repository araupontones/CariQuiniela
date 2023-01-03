library(rvest)
library(tibble)
library(stringr)
library(dplyr)
library(glue)
source("functions/scrap_cari.R")
source("functions/translate_teams.R")


usuarios <- import("data/4.usuarios_quiniela/lkp_users.rds")


#Import results of Qatar 2022 ==================================================
results_wc <- rio::import("https://raw.githubusercontent.com/araupontones/CariQuiniela/walle/data/3.clean/tablita_gordo.csv") %>%
  dplyr::filter(qatar) %>%
  consistent_elo(.) %>%
  consistent_matches(.) %>%
  select(date, team, opponent, GF, GA, result,rating_team, rating_opponent, rating_dif) %>%
  mutate(date = as.character(date))




#start iteration of usuarios here ----------------------------------------------

lista_resultados <- lapply(usuarios, function(usuario){
  
  
  message(usuario)
  grupos <- LETTERS[1:8]
  fases <- c(2:6)
  
  #data grupos ======================================================================
  
  grupos_list <- lapply(grupos, function(grupo){
    message(paste("Grupo ",grupo ))
    url <- glue("https://cariquiniela.com/detptsmq?usr={usuario}&pagori=detalleuser&ronda=1&grupo={grupo}&descpag=Puntos%20-%20{usuario}")
    print(url)
    data_grupos <- scrap_Cari(url)
    
    
  })
  
  
  
  data_grupos <- do.call(rbind, grupos_list)
  
  #data fases ======================================================================
  
  fases_list <- lapply(fases, function(fase){
    
    url <- glue("https://cariquiniela.com/detptsmq?usr={usuario}&ronda={fase}&pagori=detalleuser")
    data_fase <- scrap_Cari(url)
    
  })
  
  data_fases <- do.call(rbind, fases_list)
  
  #bind grupos y fases ==========================================================
  data_user <- rbind(data_grupos, data_fases) %>%
    translate_teams(., team) %>%
    translate_teams(., opponent) %>%
    consistent_matches(.)  %>%
    select(date, team, opponent, GF, GA, result) 
  
  
  
  #join results & predictions ====================================================
  joint <- results_wc %>%
    left_join(data_user, by = c("date", "team", "opponent"), suffix =c("_wc", "_walle"))
  
  
  
  
  
  
  #create indicators or performance ==============================================
  performance <- joint %>%
    mutate(hit = result_wc == result_walle & !is.na(GF_walle),
           hit_score = GF_wc == GF_walle & GA_wc == GA_walle,
           performance = case_when(is.na(GF_walle) ~ "Not predicted",
                                   hit_score ~ "Hit score",
                                   hit ~ "Hit result",
                                   !hit ~ "Missed"
           ),
           performance = factor(performance,
                                levels = c("Hit score", "Hit result", "Missed", "Not predicted"))
           
    ) %>%
    dplyr::filter(!is.na(performance)) %>%
    mutate(probability = 0)
  
  
  
  
  
  
})

#I forgot to add usuario in the function. Thus, lets get it!
get_usuario_name <- lapply(1:length(usuarios), function(i){
  
 
  usuario <- str_replace_all(usuarios[i], "%20", " ")
  message(usuario)
  
  d_u <- lista_resultados[[i]] %>%
    mutate(usuario = usuario)
  
  
  
})


#export ========================================================================
data_resultados <- do.call(rbind, get_usuario_name)



rio::export(data_resultados, glue('data/4.usuarios_quiniela/resultados_usuarios.csv'))


