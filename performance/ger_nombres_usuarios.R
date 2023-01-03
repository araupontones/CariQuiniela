#define urls
urls <- c("https://cariquiniela.com/inicio",
          paste0("https://cariquiniela.com/inicio?pagina=", c(2:4))
)


#function to scrap'em
read_page <- function(url){
  
  
  page <- read_html(url) %>%
  html_elements("a.liga") %>%
  html_text2() %>%
  tibble()
names(page) <- "users"

return(page)

}


#read each page and transfrom to data
pages <- lapply(urls, function(url){
  
  read_page(url)
})

usuarios <- do.call(rbind, pages)


#clean data

usuarios_clean <- usuarios %>% 
  mutate(users = str_remove(users, "[0-9]{1,}\\.\\-"),
         users = str_remove(users, "\\[.*"),
         users = str_trim(users),
         users = str_replace_all(users, " ", "%20")) %>%
  dplyr::filter(!users %in% c("1", "2", "3", "4"))


export(usuarios_clean$users , "data/4.usuarios_quiniela/lkp_users.rds")
