
library(rvest)
library(janitor)
library(dplyr)

exdir <- "data/2.scrapped"
exfile <- file.path(exdir,"odds_winner.csv")


#read website ----------------------------------------------------------------
# from https://www.oddschecker.com/football/world-cup/winner
# the site was locked so I created a manually downloaded the .html



url <-"raw_htmls/winner.html"
website <- read_html(url)

#read table of odds -----------------------------------------------------------

odds_table_frac <- website %>%
  rvest::html_elements("table") %>%
  html_elements("tbody") %>%
  html_table() %>%
  .[[1]] %>%
  janitor::remove_empty(which = "cols") 

#transform fractions to decimals
#Skip if country
skip <- odds_table_frac$X1
odds_table<- apply(odds_table_frac, c(1, 2), function(x) if(x %in% skip){x} else {eval(parse(text = x))}) %>% as.data.frame()



#ways -------------------------------------------------------------
#each bet was a way (1/2, 1/4. etc)
# I download these separately so I can create the names of the variables later

ways <- website %>%
  rvest::html_elements("table") %>%
  html_elements("thead") %>%
  html_table() %>%
  .[[1]] %>%
  filter(X1 == "Each-way terms") %>%
  #mutate(across(starts_with("X"), ~ ifelse(.x == "", NA, .x))) %>%
  janitor::remove_empty(which = "cols")
  
#vector of ways
ways_ <- as.character(ways[1,-1])





#names of the bet companies ----------------------------------------
names_houses <- website %>%
  html_elements(".bk-logo-main-90") %>%
  html_attr("title") 


#keep only those that exist in the relevant table (i know this by the odds)
house_ <- names_houses[1:length(ways_)]

#create the names of the oods table ---------------------------------------

#combine company with ways
house_ways <- paste(house_, ways_, sep = "-")
#add country to the namesgi
col_names <- c("country", house_ways)
names(odds_table) <- col_names


#================================================================================
#export 

rio::export(odds_table, exfile)
