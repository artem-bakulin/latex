library(dplyr)
library(readr)
library(curl)
library(lubridate)
library(RcppRoll)

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

sp500_index <- yahoofinancer::Index$new("^GSPC")
sp500_data <- sp500_index$get_history(start="2025-01-01", end="2026-03-20", interval="1d")

RVOL_LOOKBACK_DAYS <- 21

rvol_backup <- as_tibble(sp500_data) %>%
  transmute(
    date = as.Date(date),
    sp500 = close
  ) %>%
  mutate(log_return = log(sp500 / lag(sp500))) %>%
  mutate(rvol_backup = 100*sqrt(252.0 / RVOL_LOOKBACK_DAYS * roll_sumr(log_return^2, RVOL_LOOKBACK_DAYS))) %>%
  filter(!is.na(rvol_backup))

vix_data %>% 
  left_join(
    rvol_data %>% mutate(date = lag(date, RVOL_LOOKBACK_DAYS)),
    by="date"
  ) %>% 
  left_join(
    rvol_backup %>% mutate(date = lag(date, RVOL_LOOKBACK_DAYS)),
    by="date"
  ) %>%
  group_by(
    date = last_day_of_month(date)
  ) %>% 
  summarise(
    vix = last(vix),
    rvol = last(if_else(is.na(rvol), rvol_backup, rvol))
  ) %>% 
  write_csv("vix_and_rvol.csv", na="nan")
