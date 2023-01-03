library(dplyr)
elo <- rio::import("https://raw.githubusercontent.com/araupontones/CariQuiniela/walle/data/3.clean/tablita_gordo.csv") %>%
  filter(qatar) %>%
  filter(date== "2022-11-24")

View(elo)

t <- rio::import("elo/pre_wc_ratings.csv")

View(t)
