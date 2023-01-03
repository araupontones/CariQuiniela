#scrap puntos usuario

scrap_puntos <- function(usuario){
  
  #define url
  url_tabla <- glue("https://cariquiniela.com/detalleuser?usr={usuario}&descpag=Puntos%20-%20{usuario}&pagori=posiciones")
  
  page <- read_html(url_tabla)
  
  #read tabla
  tabla <- page %>%
    html_elements(".row") %>%
    html_elements(".col-4") %>%
    html_text2() 
  
  #format tabla
  l <- length(tabla)-1 #drop total points
  
  fases <- tabla[seq(1,l,2)]
  puntos <- tabla[seq(2,l,2)]
  
  
  #convert to tibble
  puntos_usuario <- tibble(fases = fases,
                           puntos = puntos,
                           modo = c(rep("Quiniela",6),rep("Mundial ideal", 6), "Extra"),
                           usuario = usuario)
  
  
  return(puntos_usuario)
  
}