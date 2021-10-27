library(dplyr)
library(readr)
library(lubridate)
library(xlsx)
library(curl)
library(ggplot2)
library(tidyr)

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



fly_spreads <- read_csv("usdrub_implied_vol.csv") %>% 
  mutate(
    fly =  lag(call) + lead(call) - 2*call,
  ) %>% 
  filter(
    !is.na(fly)
  ) %>% 
  mutate(
    fly = if_else(fly < 0, 0, fly)
  ) %>% 
  mutate(
    density_weight = fly / sum(fly)
  )

fly_spreads %>% 
  write_csv("usdrub_fly.csv")

kernel_density <- density(
  fly_spreads$strike,
  weights = fly_spreads$density_weight,
  kernel = "gaussian",
  bw = 0.55
)

density_fun <- approxfun(kernel_density$x, kernel_density$y, yleft=0, yright=0)

implied_mean <- integrate(function(x) {x*density_fun(x)}, lower=55, upper=95)[["value"]]

implied_std <- sqrt(integrate(function(x) {density_fun(x) * (x - implied_mean)^2}, lower=55, upper=95)[["value"]])

# https://en.wikipedia.org/wiki/Log-normal_distribution
mu <- log(implied_mean^2 / sqrt(implied_mean^2 + implied_std^2))
sigma <- sqrt(log(1 + implied_std^2 / implied_mean^2))

tibble(strike = seq(55, 95, 0.1)) %>% 
  mutate(
    implied_density = density_fun(strike),
    lognormal_density = dlnorm(strike, mu, sigma)
  ) %>% 
  write_csv("usdrub_implied_density.csv")



black_scholes_delta <- function(S, K, T, sigma, r, q) {
  
  d1 <- 1 / (sigma*sqrt(T)) * (log(S/K) + (r - q + sigma*sigma/2)*T)
  pnorm(d1)
}

tibble(S = seq(0, 1, 0.01)) %>% 
  mutate(
    call_delta = black_scholes_delta(S, 0.5, 1, 0.25, 0, 0),
    put_delta = call_delta - 1
  ) %>% 
  write_csv("black_scholed_delta.csv")
