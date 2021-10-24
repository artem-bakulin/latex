library(dplyr)
library(readr)
library(lubridate)
library(xlsx)
library(curl)
library(ggplot2)

printf <- function(pattern, ...) {
  text <- sprintf(pattern, ...)
  write(text, "")
}

# Example: download_central_bank_fx_rates("USD", make_date(2002, 1, 1), make_date(2021, 1,1))
download_central_bank_fx_rates <- function(ccy, from_date, to_date) {
  
  CURRENCY_CODES = c("USD" = "R01235", "EUR" = "R01239")
  URL_PATTERN = "http://cbr.ru/Queries/UniDbQuery/DownloadExcel/98956?Posted=True&mode=1&VAL_NM_RQ=%s&From=%s&To=%s&FromDate=%s&ToDate=%s"
  from_date_1 = as.character(from_date, "%d.%m.%Y")
  from_date_2 = as.character(from_date, "%d/%m/%Y")
  to_date_1 = as.character(to_date, "%d.%m.%Y")
  to_date_2 = as.character(to_date, "%d/%m/%Y")
  
  if (!ccy %in% names(CURRENCY_CODES)) {
    stop(sprtinf("Unknown currency code %s", ccy))
  } 
  
  url <- sprintf(URL_PATTERN, CURRENCY_CODES[ccy], from_date_1, to_date_1, from_date_2, to_date_2)
  printf("Loading data from %s", url)
  
  temp_file_name <- "temp.xls"
  curl_download(url, temp_file_name)
  raw_data <- read.xlsx(temp_file_name, sheetIndex=1)
  file.remove(temp_file_name)
  
  data <- raw_data %>% 
    as_tibble() %>% 
    select(date = data, fx_rate = curs) %>% 
    mutate(ccy = ccy)
  
  data
}


# Example: get_central_bank_fx_rate("USD", 2002, 2020)
get_central_bank_fx_rate <- function(ccy, from_year, to_year) {
  
  local_cache_file_name <- paste(
    c("cbr", ccy, from_year, to_year, "csv"),
    collapse="."
  )
  
  if (file.exists(local_cache_file_name)) {
    printf("Reading data from file %s", local_cache_file_name)
    data <- read_csv(local_cache_file_name)
    return (data)
  }
  
  data <- download_central_bank_fx_rates(ccy, 
                                         make_date(from_year, 1, 1), 
                                         make_date(to_year+1, 1 ,1))
  
  printf("Saving data to local file %s", local_cache_file_name)
  write_csv(data, local_cache_file_name)
  data
}

get_central_bank_fx_rate("USD", 2012, 2021) %>% 
  mutate(log_return = log(fx_rate / lag(fx_rate))) %>% 
  filter(!is.na(log_return)) %>% 
  group_by(mid_month = make_date(year(date), month(date), 15)) %>% 
  summarise(realized_vol = sd(log_return) * sqrt(250)) %>% 
  write_csv("USDRUB_realized_vol.csv")
