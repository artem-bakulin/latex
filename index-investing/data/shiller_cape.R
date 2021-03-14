library(dplyr)
library(readr)
library(tidyr)
library(lubridate)
library(ggplot2)

SAMPLE_QUANTILES <- c(0.05, 0.25, 0.5, 0.75, 0.95)

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
  filter(month(month) == 1) %>% 
  ggplot(aes(x = cape_excess_yield, y = subsequent_stock_return_10y)) +
  geom_point()
