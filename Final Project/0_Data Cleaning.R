

# Load packages -----------------------------------------------------------

library(tidyverse)


# import data -------------------------------------------------------------

big5_2122_players <- read_csv("Final Project/big5_2122_playerstats.csv")

  # list of colnames
colname <- colnames(big5_2122_players)

tib_colname <- as_tibble(colname) %>% arrange(by = value)