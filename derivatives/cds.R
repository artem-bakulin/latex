library(dplyr)
library(readr)
library(lubridate)
library(curl)

last_day_of_month <- function(x) {
  
  make_date(
    year(x),
    month(x),
    days_in_month(x)
  )
}

download_fred_data <- function(series_name, frequency="Monthly", mode="eop") {
  
  fred_url <- "https://fred.stlouisfed.org/graph/fredgraph.csv?&id={series_name}&fq={frequency}&fam={mode}"
  fred_url <- gsub("{series_name}", series_name, fred_url, fixed=TRUE)
  fred_url <- gsub("{frequency}", frequency, fred_url, fixed=TRUE)
  fred_url <- gsub("{mode}", mode, fred_url, fixed=TRUE)
  
  tmp_file <- tempfile("series_name", fileext=".csv")
  curl_download(fred_url, tmp_file)
  
  data <- read_csv(tmp_file)
  file.remove(tmp_file)
  data
}

high_yield_data <- 
  download_fred_data("BAMLHYH0A0HYM2TRIV") %>%
  transmute(
    month = last_day_of_month(observation_date),
    high_yield_index = as.numeric(BAMLHYH0A0HYM2TRIV)
  ) %>% 
  filter(!is.na(high_yield_index))

corp_data <- 
  download_fred_data("BAMLCC0A0CMTRIV") %>%
  transmute(
    month = last_day_of_month(observation_date),
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

read_csv("bofa_bond_indices.csv") %>% 
  tail(-1) %>% 
  summarise(
    corp_sr = mean(corp_return) / sd(corp_return),
    hy_sr = mean(high_yield_return)/sd(high_yield_return)
  )
