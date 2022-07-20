

# Load packages -----------------------------------------------------------

library(tidyverse)


# import data -------------------------------------------------------------

pl_cb <- read_csv("Final Project/pl_cb.csv")


# Variable Selection ------------------------------------------------------


model_cb <- pl_cb %>%
  select_if(is.numeric) %>%
  select(-Season_End_Year) %>%
  remove_constant(na.rm = TRUE) %>%
  as.matrix()
model_cb <- t(na.omit(t(model_cb))) %>% as_tibble()


cor_matrix_cb <- cor(model_cb)
view(cor_matrix_cb) 



view(cor_matrix_cb[cor_matrix_cb == 1])




