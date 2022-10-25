#Scrap urls of teams
# Each team has its own url in fbref
#Thus, go to the WC site and scrap the table of each group to get the teams name and url

library(httr)
library(rvest)
library(tibble)
library(rio)
library(stringr)


#define parameters -----------------------------------------------------------
exdir <- "data/1.lookups"
exfile <- file.path(exdir, "teams_urls.csv")



#get all the  <a> elements of the website


url <- "https://fbref.com/en/comps/1/World-Cup-Stats"

website <- read_html(url)
links <- website %>%
  html_elements("td") %>%
  html_elements("a")


#get the names of the teams ---------------------------------------------------
teams <- links %>%
  html_text2()

  

#get the urls ------------------------------------------------------------------
urls <- links %>%
  html_attr("href")
  

#join names and urls to create a table -----------------------------------------
teams_pages <- tibble(
  
  team = teams,
  url = paste0("https://fbref.com", urls)
  
) %>%
  mutate(id_country = str_extract(url, "(?<=squads\\/)(.*)(?=\\/)"),
         id_country = gsub("\\/2022","", id_country),
         label_country = str_replace(team, " ", "-"),
         url = glue::glue("https://fbref.com/en/squads/{id_country}/2022/{label_country}-Men-Stats")
         )



#export -----------------------------------------------------------------------
  rio::export(teams_pages, exfile)
