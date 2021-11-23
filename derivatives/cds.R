library(dplyr)
library(readr)
library(lubridate)

last_day_of_month <- function(x) {
  
  make_date(
    year(x),
    month(x),
    days_in_month(x)
  )
}

high_yield_data <- read_csv("bofa_high_yield_index.csv") %>% 
  transmute(
    month = last_day_of_month(DATE),
    high_yield_index = as.numeric(BAMLHYH0A0HYM2TRIV)
  ) %>% 
  filter(!is.na(high_yield_index))

corp_data <-read_csv("bofa_corporate_index.csv") %>% 
  transmute(
    month = last_day_of_month(DATE),
    corp_index = as.numeric(BAMLCC0A0CMTRIV)
  ) %>% 
  filter(!is.na(corp_index))

corp_data %>% 
  inner_join(high_yield_data) %>% 
  mutate(
    corp_return = corp_index / lag(corp_index, default=first(corp_index)) - 1,
    high_yield_return = high_yield_index / lag(high_yield_index, default=first(high_yield_index)) - 1,
    corp_growth = cumprod(1 + corp_return),
    high_yield_growth = cumprod(1 + high_yield_return)
  ) %>% 
  write_csv("bofa_bond_indices.csv")
