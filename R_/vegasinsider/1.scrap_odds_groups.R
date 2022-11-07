library(rvest)
library(stringr)

#From : "https://www.vegasinsider.com/soccer/odds/world-cup/"

exdir <- "data/2.scrapped/vegasinsider"
exfile <- file.path(exdir, "odds_groups.csv")



#Iterate across all groups to download data -----------------------------------
groups <- letters[1:8]

list_groups <- lapply(groups, function(g){
  
  message(paste("grupo", g))
  
  url_wc <- "https://www.vegasinsider.com/soccer/world-cup/group"
  url_group <- file.path(url_wc, g)
  
  table_dirty <- read_html(url_group) %>%
    #html_elements("figure") %>%
    html_elements(".wp-block-table") %>%
    html_table() %>%
    .[[1]] 
  
  #set first row as name columns
  names(table_dirty) <- as.character(table_dirty[1,])
  table_dirty <- table_dirty[-1,]
  table_dirty$grupo = g
  
  table_dirty
  
})


#append all groups & transform odds to numeric  --------------------------------
data_groups <- do.call(rbind, list_groups) %>%
  rename_all(function(x){stringr::str_replace_all(x," ","_")}) %>%
  #the odds come as character with + and - signs
  mutate(across(matches("Group"), ~ as.numeric(str_remove(.x, "\\+"))))




#===============================================================================
#export

rio::export(data_groups, exfile)



