library(dplyr)
library(readr)

eonia <- read_csv("eonia.csv") %>% arrange(date)

ester <- read_csv("ester.csv") %>% arrange(date)

euribor <- read_csv("euribor.csv") %>% arrange(date)

eonia %>% 
  left_join(ester, by="date") %>% 
  inner_join(euribor, by="date") %>%
  mutate(ester = if_else(is.na(ester), eonia - 0.085, ester)) %>% 
  write_csv("euro_benchmark.csv")
