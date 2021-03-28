library(dplyr)
library(readr)
library(tidyr)
library(lubridate)
library(ggplot2)
library(zoo)

last_day_of_month <- function(x) {
  make_date(year(x), month(x), days_in_month(x))
}

SAMPLE_QUANTILES <- c(0, 0.05, 0.25, 0.5, 0.75, 0.95, 1)

cape_yield_data <- read_csv("shiller_cape.csv")

summarize_cape_yield <- function(cape_yield_data) {
  cape_yield_data %>% 
    summarise(
      pct = sprintf("%d%%", 100*SAMPLE_QUANTILES),
      cape_yield = sprintf("%.2f%%", 100*quantile(cape_excess_yield, SAMPLE_QUANTILES))
    )
}

cape_yield_data %>% 
  filter(!is.na(cape_excess_yield)) %>% 
  group_by(
    period = case_when(
      month >= '1986-01-01' & month <= '2020-12-01' ~ "1986--2020",
      month >= '1951-01-01' & month <= '1985-12-01' ~ "1951--1985",
      month >= '1916-01-01' & month <= '1950-12-01' ~ "1916--1950",
      month >= "1881-01-01" & month <= '1915-12-01' ~ "1881--1915",
      TRUE ~ "Other"
    )
  ) %>% 
  summarize_cape_yield() %>% 
  ungroup() %>% 
  bind_rows(
    cape_yield_data %>%
      filter(month >= '1881-01-01', month <= '2020-12-01') %>% 
      group_by(period = "1881--2020") %>% 
      summarize_cape_yield()
  ) %>% 
  pivot_wider(names_from="pct", values_from="cape_yield") %>% 
  filter(!period == "Other")

cape_yield_data %>% 
  ggplot(aes(x = cape_excess_yield, y = subsequent_stock_return_10y)) +
  geom_point()


cape_yield_data %>% 
  summarise(
    cape_excess_yield = median(cape_excess_yield, na.rm=TRUE),
    subsequent_stock_return_10y = median(subsequent_stock_return_10y, na.rm=TRUE)
  )

CAPE_QUANTILES <- c(0, 0.1, 0.2, 0.3, 0.4, 0.5, 0.6, 0.7, 0.8, 0.9)

cape_yield_data %>% 
  filter(
    year(month) >= 1926,
    month(month) == 1,
    !is.na(cape_excess_yield),
    !is.na(subsequent_stock_return_10y)
  ) %>% 
  group_by(
    cape_excess_yield_group = findInterval(cape_excess_yield, quantile(cape_excess_yield, CAPE_QUANTILES))
  ) %>% 
  summarise(
    cape_yield_min  = sprintf("%.1f%%", min(cape_excess_yield)*100),
    cape_yield_max  = sprintf("%.1f%%", max(cape_excess_yield)*100),
    mean_10y_return = sprintf("%.1f%%", mean(subsequent_stock_return_10y) * 100),
    min_10y_return  = sprintf("%.1f%%", min(subsequent_stock_return_10y) * 100),
    max_10y_return  = sprintf("%.1f%%", max(subsequent_stock_return_10y) * 100)
  )

rolling_ecdf <- function(x, width) {
  zoo::rollapplyr(
    x,
    width = width,
    fill = NA,
    FUN = function(x) {ecdf(x)(last(x))}
  )
}

market_returns <- read_csv("fama_french_four_factors_data.csv") %>% 
  select(month, mkt_rf, rf)
  

backtest_data <- cape_yield_data %>% 
  select(month, cape_excess_yield) %>% 
  mutate(
    cape_quantile = rolling_ecdf(cape_excess_yield, 40*12),
    signal = case_when(
      cape_quantile >= 0.95 ~ 1.0,
      cape_quantile <= 0.05 ~ 0.0,
      TRUE ~ (cape_quantile - 0.05) / (0.95 - 0.05)
    )
  ) %>%  
  filter(
    !is.na(signal)
  ) %>% 
  inner_join(market_returns, by="month") %>% 
  mutate(
    benchmark_return = 0.5*(mkt_rf+rf) + 0.5*rf,
    strategy_return = signal*(mkt_rf+rf) + (1-signal)*rf
  )


summarize_backtest_resuls <- function(data) {
  data %>% 
    summarise(
      bench_mean = mean(benchmark_return - rf) * 12,
      bench_std  = sd(benchmark_return - rf) * sqrt(12),
      bench_sharpe = bench_mean / bench_std,
      bench_drawdown = min(cumprod(1 + benchmark_return) / cummax(cumprod(1 + benchmark_return))) - 1,
      strat_mean = mean(strategy_return - rf) * 12,
      strat_std = sd(strategy_return - rf) * sqrt(12),
      strat_sharpe = strat_mean / strat_std,
      strat_drawdown = min(cumprod(1 + strategy_return) / cummax(cumprod(1 + strategy_return))) - 1,
      mean_signal = mean(signal),
      median_cape = median(cape_excess_yield)
    )
}

backtest_data %>% 
  filter(month >= '1927-01-01' & month <= '2020-12-01') %>% 
  group_by(
    period = case_when(
      month >= '1927-01-01' & month <= '1959-12-01' ~ "1927--1959",
      month >= '1960-01-01' & month <= '1989-12-01' ~ "1960--1989",
      month >= '1990-01-01' & month <= '2020-12-01' ~ "1990--2020",
      TRUE ~ "Other"
    )
  ) %>% 
  summarize_backtest_resuls() %>% 
  bind_rows(
    backtest_data %>% 
      filter(month >= '1927-01-01' & month <= '2020-12-01') %>% 
      group_by(period = '1927--2020') %>% 
      summarize_backtest_resuls()
  )

prepare_growth_data <- function(backtest_data) {
  growth_data <- backtest_data %>% 
    transmute(
      date = last_day_of_month(month),
      benchmark_growth = cumprod(1 + benchmark_return),
      strategy_growth = cumprod(1 + strategy_return),
      mkt_growth = cumprod(1 + mkt_rf + rf),
      rf_growth = cumprod(1 + rf)
    )
  
  tibble(
    date = min(backtest_data$month),
    benchmark_growth = 1,
    strategy_growth = 1,
    mkt_growth = 1,
    rf_growth = 1
  ) %>% 
    bind_rows(growth_data)
}

backtest_data %>% 
  filter(month >= '1990-01-01') %>% 
  prepare_growth_data() %>% 
  write_csv("cape_strategy_growth_1990.csv")

backtest_data %>% 
  filter(month >= '1927-01-01') %>% 
  prepare_growth_data() %>% 
  write_csv("cape_strategy_growth_1927.csv")


