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

get_central_bank_fx_rate("USD", 2012, 2022) %>% 
  mutate(log_return = log(fx_rate / lag(fx_rate))) %>% 
  filter(!is.na(log_return)) %>% 
  group_by(mid_month = make_date(year(date), month(date), 15)) %>% 
  summarise(realized_vol = sd(log_return) * sqrt(250)) %>% 
  write_csv("USDRUB_realized_vol.csv")


fly_spreads <- read_csv("usdrub_implied_vol.csv") %>% 
  mutate(
    left_call = approx(strike, y=call, xout=strike-1, rule = 1)$y,
    right_call = approx(strike, y=call, xout=strike+1, rule = 1)$y,
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

fly_spreads %>% 
  write_csv("usdrub_fly.csv")

kernel_density <- density(
  fly_spreads$strike,
  weights = fly_spreads$density_weight,
  kernel = "gaussian",
  bw = 1.0
)

density_fun <- approxfun(kernel_density$x, kernel_density$y, yleft=0, yright=0)

implied_mean <- integrate(function(x) {x*density_fun(x)}, lower=60, upper=135)[["value"]]

implied_std <- sqrt(integrate(function(x) {density_fun(x) * (x - implied_mean)^2}, lower=60, upper=135)[["value"]])

# https://en.wikipedia.org/wiki/Log-normal_distribution
mu <- log(implied_mean^2 / sqrt(implied_mean^2 + implied_std^2))
sigma <- sqrt(log(1 + implied_std^2 / implied_mean^2))

tibble(strike = seq(60, 135, 0.1)) %>% 
  mutate(
    implied_density = density_fun(strike),
    lognormal_density = dlnorm(strike, mu, sigma)
  ) %>% 
  write_csv("usdrub_implied_density.csv")


black_scholes_d1 <- function(S, K, T, sigma, r, q) {
 
  1 / (sigma*sqrt(T)) * (log(S/K) + (r - q + sigma*sigma/2)*T) 
}

black_scholes_d2 <- function(S, K, T, sigma, r, q) {
  
  1 / (sigma*sqrt(T)) * (log(S/K) + (r - q - sigma*sigma/2)*T) 
}

black_scholes_delta <- function(S, K, T, sigma, r, q) {
  
  pnorm(black_scholes_d1(S, K, T, sigma, r, q))
}

black_scholes_vega <- function(S, K, T, sigma, r, q) {
  
  d1 <- 1 / (sigma*sqrt(T)) * (log(S/K) + (r - q + sigma*sigma/2)*T)
  S * dnorm(d1) * sqrt(T)
}

black_scholes_theta <- function(S, K, T, sigma, r, q) {

  d1 <- black_scholes_d1(S, K, T, sigma, r, q)
  d2 <- black_scholes_d2(S, K, T, sigma, r, q)
  
  -S * dnorm(d1) * sigma / (2*sqrt(T)) - r*K*exp(-r*T)*pnorm(d2)
}

black_scholes_gamma <- function(S, K, T, sigma, r, q) {
  
  d1 <- black_scholes_d1(S, K, T, sigma, r, q)
  
  dnorm(d1) / (S * sigma * sqrt(T))
}

tibble(S = seq(0, 1, 0.01)) %>% 
  mutate(
    call_delta = black_scholes_delta(S, 0.5, 1, 0.25, 0, 0),
    put_delta = call_delta - 1
  ) %>% 
  write_csv("black_scholes_delta.csv")

tibble(S = seq(0, 1, 0.01)) %>% 
  mutate(
    vega = black_scholes_vega(S, 0.5, 1, 0.2, 0, 0)
  ) %>% 
  write_csv("black_scholes_vega.csv")

tibble(S = seq(0, 1, 0.01)) %>% 
  mutate(
    theta = black_scholes_theta(S, K=0.5, T=1, sigma=0.2, r=0, q=0)
  ) %>% 
  write_csv("black_scholes_theta.csv")

tibble(T = seq(0.01, 1, 0.01)) %>% 
  mutate(
    theta_itm = black_scholes_theta(S=0.55, K=0.5, T, sigma=0.3, r=0, q=0),
    theta_atm = black_scholes_theta(S=0.50, K=0.5, T, sigma=0.3, r=0, q=0),
    theta_otm = black_scholes_theta(S=0.45, K=0.5, T, sigma=0.3, r=0, q=0)
  ) %>% 
  write_csv("black_scholes_theta_by_money.csv")

tibble(S = seq(0, 1, 0.01)) %>% 
  mutate(
    gamma = black_scholes_gamma(S, 0.5, 1, 0.2, 0, 0)
  ) %>% 
  write_csv("black_scholes_gamma.csv", na = "")

tibble(T = seq(0.01, 1, 0.01)) %>% 
  mutate(
    gamma_itm = black_scholes_gamma(S=0.55, K=0.5, T, sigma=0.3, r=0, q=0),
    gamma_atm = black_scholes_gamma(S=0.50, K=0.5, T, sigma=0.3, r=0, q=0),
    gamma_otm = black_scholes_gamma(S=0.45, K=0.5, T, sigma=0.3, r=0, q=0)
  ) %>% 
  write_csv("black_scholes_gamma_by_money.csv")