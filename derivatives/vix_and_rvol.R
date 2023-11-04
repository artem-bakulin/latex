library(dplyr)
library(readr)
library(curl)
library(lubridate)

last_day_of_month <- function(x) {
  make_date(year(x), month(x), days_in_month(x))
}

download_from_cboe <- function(index_name) {

  full_url <- sprintf("https://cdn.cboe.com/api/global/us_indices/daily_prices/%s_History.csv", index_name)
  curl_download(full_url, "tmp.csv")
  data <- read_csv("tmp.csv")
  file.remove("tmp.csv")
  
  data <- data %>% 
    select(first(names(.)), last(names(.)))
  colnames(data) <- c("date", tolower(index_name))
  
  data %>% 
    mutate(
      date = as.Date(date, "%m/%d/%Y"),
    )
}

rvol_data <- download_from_cboe("RVOL")
vix_data <- download_from_cboe("VIX")

# rvol is looking back 21 days
vix_data %>% 
  left_join(
    rvol_data %>% mutate(date = lag(date, 21)),
    by="date"
  ) %>% 
  group_by(
    date = last_day_of_month(date)
  ) %>% 
  summarise(
    vix = last(vix),
    rvol = last(rvol)
  ) %>% 
  write_csv("vix_and_rvol.csv", na="nan")
