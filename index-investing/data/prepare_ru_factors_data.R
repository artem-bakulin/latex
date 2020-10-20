library(curl)
library(readr)
library(dplyr)
library(tidyr)
library(lubridate)

parse_ru_month_year <- function(x) {
  
  months <- c(
    "янв" = 1,
    "фев" = 2,
    "мар" = 3,
    "апр" = 4,
    "май" = 5,
    "июн" = 6,
    "июл" = 7,
    "авг" = 8,
    "сен" = 9,
    "окт" = 10,
    "ноя" = 11,
    "дек" = 12
  )
  
  month_ru <- substr(x, 1, 3)
  month <- months[month_ru]
  year <- as.numeric(substring(x, 5, 9))
  make_date(year, month, 1)
}


download_ru_factor_file <- function(file_name, factor_name) {
 
  BASE_URL <- "https://ipei.ranepa.ru/images/2019/CAPMRU/factors/"
  url <- paste0(BASE_URL, file_name)
  
  curl_download(url, file_name)
  
  data <- read_delim(
    file_name,
    col_names=c("month_ru", "return_pct"),
    skip=1,
    delim=";",
    locale=locale(encoding="cp1251", decimal_mark=",")
  )
  
  file.remove(file_name)
  
  data %>% 
    transmute(
      month = parse_ru_month_year(month_ru),
      factor = factor_name,
      return = return_pct / 100
    ) %>% 
    filter(
      !is.na(return)
    )
}

if (!file.exists("ru_factors_data.csv")) {
  rmrf_ru <- download_ru_factor_file("RMRF_TR_may2020.csv", "rmrf_ru")
  smb_ru  <- download_ru_factor_file("SMB_TR_may2020.csv", "smb_ru")
  hml_ru  <- download_ru_factor_file("HML_TR_may2020.csv", "hml_ru")
  liq_ru  <- download_ru_factor_file("LIQ_TR_may2020.csv", "liq_ru")
  dy_ru   <- download_ru_factor_file("DY_TR_may2020.csv", "dy_ru")
  soe_ru  <- download_ru_factor_file("SOE_TR_may2020.csv", "soe_ru")
  
  ru_factors_normalized <- bind_rows(
    rmrf_ru,
    smb_ru,
    hml_ru,
    liq_ru,
    dy_ru,
    soe_ru
  )
  
  ru_factors_normalized %>% write_csv("ru_factors_data.csv")
}

ru_factors_normalized <- read_csv("ru_factors_data.csv")

ru_factors_normalized %>% 
  group_by(factor) %>% 
  summarise(
    return_mean = mean(return),
    return_std = sd(return),
    t_stat = t.test(return, mu=0)[["statistic"]],
    p_value = t.test(return, mu=0)[["p.value"]],
    ci99_L = t.test(return, conf.int=0.99)[["conf.int"]][[1]],
    ci99_R = t.test(return, conf.int=0.99)[["conf.int"]][[2]],
    n_obs = n(),
    from = min(month),
    to = max(month)
  ) %>% 
  transmute(
    latex_format = paste(
      toupper(factor),
      sprintf("%.1f\\%%", return_mean*100),
      sprintf("%.1f\\%%", return_std*100),
      sprintf("%.2f", t_stat), 
      if_else(p_value >= 0.001, sprintf("%.1f\\%%", p_value*100), "<0.1\\%"),
      sprintf("[%.1f\\%%, %.1f\\%%]", ci99_L*100, ci99_R*100),
      sep = " & "
    )
  )

ru_factors_first_month <- ru_factors_normalized %>% 
  group_by(factor) %>% 
  summarise(
    month = make_date(year(min(month)-1), month(min(month)-1)),
    return = 0
  )

ru_factors_growth <- ru_factors_first_month %>% 
  bind_rows(ru_factors_normalized) %>% 
  arrange(factor, month) %>% 
  group_by(factor) %>% 
  mutate(
    factor_growth = cumprod(1 + return)
  ) %>% 
  ungroup() %>% 
  select(-return) %>% 
  spread(factor, factor_growth) %>% 
  mutate(
    month = make_date(year(month), month(month), days_in_month(month))
  )

ru_factors_growth %>% 
  write_csv("ru_factors_cumulative_growth_data.csv", na="")

ru_factors_growth %>% 
  filter(month >= '2014-12-31') %>%
  pivot_longer(-starts_with("month"), names_to="factor", values_to="growth") %>% 
  group_by(factor) %>% 
  mutate(growth = growth / first(growth)) %>%
  pivot_wider(names_from="factor", values_from="growth") %>% 
  write_csv("ru_factors_cumulative_growth_data_2015.csv", na="")
