library(dplyr)
library(readr)
library(ggplot2)
library(tidyr)
library(lubridate)

as_of_join <- function(x, y, by_column="date") {
  
  x_dates <- x %>% select(all_of(by_column))
  y_dates <- y %>% select(all_of(by_column))
  all_dates <- x_dates %>% 
    union(y_dates) %>% 
    arrange(!!rlang::sym("date"))
  
  y <- y %>% 
    arrange(!!rlang::sym("date"))
  
  y_without_gaps <- all_dates %>% 
    left_join(y, by = by_column) %>% 
    fill(!all_of(by_column), .direction="down")
  
  x %>% 
    left_join(y_without_gaps, by=by_column)
}

first_day_of_month <- function(x) {
  make_date(year(x), month(x), 1)
}

last_day_of_month <- function(x) {
  make_date(year(x), month(x), days_in_month(x))
}

sortino_ratio <- function(excess_returns, min_acceptable_return = 0) {
  
  mean_return_above_threshold <- mean(excess_returns - min_acceptable_return)
  
  returns_below_threshold <- pmin(0, excess_returns - min_acceptable_return)
  
  downside_deviation <- sd(returns_below_threshold)
  
  mean_return_above_threshold / downside_deviation
}

moex_index <- read_csv("moex_index.csv")
total_return_gross_index <- read_csv("total_return_gross_index.csv")

risk_free <- read_tsv("cbr_key_rate.txt")

red <- read_csv("red_share_price.txt")

green <- read_csv("green_share_price.txt")

yellow <- read_csv("yellow_share_price.txt")

personal <- read_csv("personal_share_price.txt")


green_returns <- green %>% 
  mutate(month = first_day_of_month(date)) %>% 
  filter(month != lead(month, default=first(month))) %>% 
  mutate(green_return = green_share_price / lag(green_share_price) - 1) %>% 
  filter(!is.na(green_return)) %>% 
  select(month, green_return)

red_returns <- red %>% 
  mutate(month = first_day_of_month(date)) %>% 
  filter(month != lead(month, default=first(month))) %>% 
  mutate(red_return = red_share_price / lag(red_share_price) - 1) %>% 
  filter(!is.na(red_return)) %>% 
  select(month, red_return)

yellow_returns <- yellow %>% 
  mutate(month = first_day_of_month(date)) %>% 
  filter(month != lead(month, default=first(month))) %>% 
  mutate(yellow_return = yellow_share_price / lag(yellow_share_price) - 1) %>% 
  filter(!is.na(yellow_return)) %>% 
  select(month, yellow_return)

personal_returns <- personal %>% 
  mutate(personal_return = personal_share_price / lag(personal_share_price) - 1) %>% 
  select(-personal_share_price) %>% 
  filter(!is.na(personal_return))

benchmark_returns <- total_return_gross_index %>%
  as_of_join(risk_free) %>% 
  mutate(
    risk_free_return = lag(cbr_rate) * (as.numeric(date - lag(date), "days")) / 365
  ) %>% 
  mutate(month = first_day_of_month(date)) %>% 
  select(-date) %>% 
  inner_join(
    moex_index %>% 
      mutate(month = first_day_of_month(date)) %>% 
      select(-date),
  ) %>% 
  mutate(
    moex_return = moex_index / lag(moex_index) - 1,
    total_gross_return = total_return_gross_index / lag(total_return_gross_index) - 1,
    div_yield = total_gross_return - moex_return,
    div_yield_less_tax = (1 - 0.13) * div_yield,
    benchmark_return = moex_return + div_yield_less_tax
  ) %>% 
  select(month, benchmark_return, risk_free_return) %>% 
  filter(!is.na(benchmark_return), !is.na(risk_free_return))

all_returns <- benchmark_returns %>% 
  inner_join(red_returns) %>% 
  inner_join(yellow_returns) %>% 
  inner_join(green_returns) %>% 
  inner_join(personal_returns) %>% 
  mutate(
    benchmark_excess_return = benchmark_return - risk_free_return,
    red_excess_return = red_return - risk_free_return,
    yellow_excess_return = yellow_return - risk_free_return,
    green_excess_return = green_return - risk_free_return,
    personal_excess_return = personal_return - risk_free_return
  )

all_returns %>% 
  ggplot(aes(x = last_day_of_month(month))) +
  geom_line(aes(y = cumprod(1 + benchmark_return)), color="black") +
  geom_line(aes(y = cumprod(1 + risk_free_return)), color="black", linetype="dashed") +
  geom_line(aes(y = cumprod(1 + red_return)), color="red") +
  geom_line(aes(y = cumprod(1 + yellow_return)), color="orange") +
  geom_line(aes(y = cumprod(1 + green_return)), color="darkgreen") +
  geom_line(aes(y = cumprod(1 + personal_return)), color="blue")

all_returns %>% 
  lm(green_excess_return ~ benchmark_excess_return, data=.) %>% 
  summary()

all_returns %>% 
  lm(yellow_excess_return ~ benchmark_excess_return, data=.) %>% 
  summary()

all_returns %>% 
  lm(red_excess_return ~ benchmark_excess_return, data=.) %>% 
  summary()

all_returns %>% 
  lm(personal_excess_return ~ benchmark_excess_return, data=.) %>% 
  summary()

all_returns %>% 
  summarise(
    benchmark_sharpe =  mean(benchmark_excess_return) / sd(benchmark_excess_return) * sqrt(12),
    red_sharpe = mean(red_excess_return) / sd(red_excess_return) * sqrt(12),
    yellow_sharpe = mean(yellow_excess_return) / sd(yellow_excess_return) * sqrt(12),
    green_sharpe = mean(green_excess_return) / sd(green_excess_return) * sqrt(12),
    personal_sharpe = mean(personal_excess_return) / sd(personal_excess_return) * sqrt(12)
  )

all_returns %>% 
  summarise(
    benchmark_sortino = sortino_ratio(benchmark_excess_return, 0) * sqrt(12),
    red_sortino = sortino_ratio(red_excess_return, 0) * sqrt(12),
    yellow_sortino = sortino_ratio(yellow_excess_return, 0) * sqrt(12),
    green_sortino = sortino_ratio(green_excess_return, 0) * sqrt(12),
    personal_sortino = sortino_ratio(personal_excess_return, 0) * sqrt(12)
  )

fund_growth <- all_returns %>% 
  transmute(
    date = last_day_of_month(month),
    benchmark_growth = cumprod(1 + benchmark_return),
    risk_free_growth = cumprod(1 + risk_free_return),
    personal_growth = cumprod(1 + personal_return),
    green_growth = cumprod(1 + green_return),
    yellow_growth = cumprod(1 + yellow_return),
    red_growth = cumprod(1 + red_return)
  ) %>% 
  bind_rows(
    tibble(
      date = as.Date('2015-10-31'),
      benchmark_growth = 1,
      risk_free_growth = 1,
      personal_growth = 1,
      green_growth = 1,
      yellow_growth = 1,
      red_growth = 1
    )
  ) %>% 
  arrange(date)

fund_growth %>% 
  write_csv("fund_growth.csv")

all_returns %>% 
  transmute(
    month = month,
    benchmark_excess_return = 100 * benchmark_excess_return,
    personal_excess_return = 100 * personal_excess_return,
    red_excess_return = 100 * red_excess_return,
    yellow_excess_return = 100 * yellow_excess_return,
    green_excess_return = 100 * green_excess_return
  ) %>% 
  write_csv("fund_excess_return.csv")

all_returns %>% 
  transmute(
    date = last_day_of_month(month),
    benchmark_growth = cumprod(1 + benchmark_return),
    risk_free_growth = cumprod(1 + risk_free_return),
    personal_growth = cumprod(1 + personal_return),
    green_growth = cumprod(1 + green_return),
    yellow_growth = cumprod(1 + yellow_return),
    red_growth = cumprod(1 + red_return)
  ) %>% tail()

ru_factor_returns <- read_csv("ru_factors_data.csv") %>% 
  pivot_wider(names_from="factor", values_from="return")

all_returns %>% 
  inner_join(ru_factor_returns, by="month") %>% 
  lm(personal_excess_return ~ benchmark_excess_return + soe_ru, data=.) %>% 
  summary()
