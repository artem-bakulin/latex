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
  from_date_1 = format(from_date, format="%d.%m.%Y")
  from_date_2 = format(from_date, format="%d/%m/%Y")
  to_date_1 = format(to_date, format="%d.%m.%Y")
  to_date_2 = format(to_date, format="%d/%m/%Y")
  
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

get_central_bank_fx_rate("USD", 2012, 2025) %>% 
  mutate(log_return = log(fx_rate / lag(fx_rate))) %>% 
  filter(!is.na(log_return)) %>% 
  group_by(mid_month = make_date(year(date), month(date), 15)) %>% 
  summarise(realized_vol = sd(log_return) * sqrt(250)) %>% 
  filter(!is.na(realized_vol)) %>% 
  filter(mid_month != max(mid_month)) %>% 
  write_csv("USDRUB_realized_vol.csv")


get_moex_usdrub_options <- function(underlying, expiration_date) {
  
  URL_PATTERN <- "https://www.moex.com/en/derivatives/optionsdesk-csv.aspx?code=%s&marg=0&delivery=%s&type=2&sid=1"
  url <- sprintf(URL_PATTERN, underlying, format(expiration_date, "%d-%m-%y"))
  temp_file_name <- tempfile(tmpdir = ".")
  curl_download(url, temp_file_name)
  data <- readr::read_delim(temp_file_name, delim=";", )
  file.remove(temp_file_name)
  
  data%>% 
    transmute(
      strike = Strike / 1e6,
      iv = IV / 1e4,
      call = `...7` / 1e7,
      put = `...12` / 1e7
    )
}

compute_fly_spreads <- function(call_prices, strike_step=1) {
  
  call_prices %>% 
    mutate(
      left_call = approx(strike, y=call, xout=strike-strike_step, rule = 1)$y,
      right_call = approx(strike, y=call, xout=strike+strike_step, rule = 1)$y,
      fly =  left_call + right_call - 2*call,
    ) %>% 
    filter(
      !is.na(fly),
      round(strike) == strike
    ) %>% 
    mutate(
      fly = if_else(fly < 0, 0, fly)
    ) %>% 
    mutate(
      density_weight = fly / sum(fly)
    )
}

compute_implied_probability_density <- function(fly_spreads, strike_step=1) {
  
  kernel_density <- density(
    fly_spreads$strike,
    weights = fly_spreads$density_weight,
    kernel = "gaussian",
    bw = strike_step
  )
  
  density_fun <- approxfun(kernel_density$x, kernel_density$y, yleft=0, yright=0)
  
  # Add a bit of extra intgration nodes outside of the min/max range range
  lower_limit <- min(fly_spreads$strike) - strike_step
  upper_limit <- max(fly_spreads$strike) + strike_step
  
  implied_mean <- integrate(function(x) {x*density_fun(x)}, lower_limit, upper_limit)[["value"]]
  implied_std <- sqrt(integrate(function(x) {density_fun(x) * (x - implied_mean)^2}, lower_limit, upper_limit)[["value"]])
  
  # https://en.wikipedia.org/wiki/Log-normal_distribution
  mu <- log(implied_mean^2 / sqrt(implied_mean^2 + implied_std^2))
  sigma <- sqrt(log(1 + implied_std^2 / implied_mean^2))
  
  tibble(
    strike = seq(
      min(fly_spreads$strike),
      max(fly_spreads$strike),
      strike_step / 10
    )
  ) %>% 
    mutate(
      implied_density = density_fun(strike),
      lognormal_density = dlnorm(strike, mu, sigma)
    )
}

get_moex_usdrub_options("Si-6.25", make_date(2025, 6, 19)) %>% 
  write_csv("usdrub_implied_vol.csv")

usdrub_fly_spreads <- read_csv("usdrub_implied_vol.csv") %>% 
  compute_fly_spreads()

usdrub_fly_spreads %>% 
  write_csv("usdrub_fly.csv")

usdrub_fly_spreads %>% 
  compute_implied_probability_density() %>% 
  write_csv("usdrub_implied_density.csv")

sp500_strike_step <- 50
read_csv("spx-volatility-greeks-exp-2025-06-20-show-all-03-22-2025.csv") %>% 
  transmute(
    strike = as.numeric(gsub(",", "", Strike)),
    pv = `Theor.`,
    iv = as.numeric(gsub("%", "", IV)) / 100,
    type = tolower(Type)
  ) %>% 
  filter(!is.na(strike)) %>% 
  pivot_wider(names_from="type", values_from=c("pv", "iv")) %>% 
  transmute(
    strike,
    call = if_else(pv_call != 0, pv_call, NA),
    put = if_else(pv_put != 0, pv_put, NA),
    iv = iv_call
  ) %>% 
  filter(strike %% sp500_strike_step == 0, strike >= 4500, strike <= 7200, call > 0) %>% 
  write_csv("sp500_implied_vol.csv", na="NaN")

sp500_fly_spreads <- read_csv("sp500_implied_vol.csv") %>%
  filter(!strike %in% c(5000)) %>% 
  compute_fly_spreads(strike_step=sp500_strike_step) 

sp500_fly_spreads %>% 
  write_csv("sp500_fly.csv", na="NaN")

sp500_fly_spreads %>% 
  compute_implied_probability_density(strike_step=sp500_strike_step) %>% 
  write_csv("sp500_implied_density.csv", na="NaN")
