library(dplyr)
library(readr)
library(ggplot2)
library(tidyr)
library(lubridate)
library(curl)

download_fama_french_factors_data <- function() {
  
  curl_download(
    "https://mba.tuck.dartmouth.edu/pages/faculty/ken.french/ftp/F-F_Research_Data_Factors_CSV.zip",
    "F-F_Research_Data_Factors_CSV.zip"
  )

  data <- read_csv("F-F_Research_Data_Factors_CSV.zip", skip=3, skip_empty_rows=TRUE) %>% 
    filter(
      X1 >= 190000
    ) %>% 
    transmute(
      month = make_date(X1 %/% 100, X1 %% 100, 1),
      mkt_rf = `Mkt-RF` / 100,
      smb = SMB / 100,
      hml = HML / 100,
      rf = RF / 100
    )
  
  file.remove("F-F_Research_Data_Factors_CSV.zip")
  
  data
}


download_momentum_factor_data <- function() {
  
  curl_download(
    "https://mba.tuck.dartmouth.edu/pages/faculty/ken.french/ftp/F-F_Momentum_Factor_CSV.zip",
    "F-F_Momentum_Factor_CSV.zip"
  )
  
  data <- read_csv("F-F_Momentum_Factor_CSV.zip", skip=13, skip_empty_rows=TRUE) %>% 
    filter(
      X1 >= 190000
    ) %>% 
    transmute(
      month = make_date(X1 %/% 100, X1 %% 100, 1),
      mom = Mom / 100
    )
  
  file.remove("F-F_Momentum_Factor_CSV.zip")
  
  data
}


if (!file.exists("fama_french_four_factors_data.csv")) {
  
  three_factors <- download_fama_french_factors_data()
  momentum <- download_momentum_factor_data()
  
  four_factors <- three_factors %>% 
    inner_join(momentum, by="month")
  
  four_factors %>% 
    write_csv("fama_french_four_factors_data.csv")
}

fama_french_four_factors_data <- read_csv("fama_french_four_factors_data.csv")

curl_download(
  "https://fred.stlouisfed.org/graph/fredgraph.csv?bgcolor=%23e1e9f0&chart_type=line&drp=0&fo=open%20sans&graph_bgcolor=%23ffffff&height=450&mode=fred&recession_bars=on&txtcolor=%23444444&ts=12&tts=12&width=1168&nt=0&thu=0&trc=0&show_legend=yes&show_axis_titles=yes&show_tooltip=yes&id=CPIAUCNS&scale=left&cosd=1913-01-01&coed=2020-09-01&line_color=%234572a7&link_values=false&line_style=solid&mark_type=none&mw=3&lw=2&ost=-99999&oet=99999&mma=0&fml=a&fq=Monthly&fam=avg&fgst=lin&fgsnd=2020-02-01&line_index=1&transformation=lin&vintage_date=2020-10-20&revision_date=2020-10-20&nd=1913-01-01",
  "CPIAUCNS.csv"
)

inflation_data <- read_csv("CPIAUCNS.csv") %>% 
  transmute(
    month = DATE,
    cpi = CPIAUCNS
  )

annual_returns <- fama_french_four_factors_data %>% 
  group_by(
    year = year(month)
  ) %>% 
  summarise(
    mkt_rf = prod(1 + mkt_rf + rf) - prod(1 + rf),
    smb = prod(1 + smb) - 1,
    hml = prod(1 + hml) - 1,
    mom = prod(1 + mom) - 1
  ) %>% 
  pivot_longer(
    cols = -starts_with("year"),
    names_to = "factor",
    values_to = "return"
  )

sortino_ratio <- function(excess_returns, min_acceptable_return = 0) {
  
  mean_return_above_threshold <- mean(excess_returns - min_acceptable_return)
  
  returns_below_threshold <- pmin(0, excess_returns - min_acceptable_return)
  
  downside_deviation <- sd(returns_below_threshold)
  
  mean_return_above_threshold / downside_deviation
}

summarize_annual_returns <- function (data) {
  
  data %>% 
    summarise(
      return_mean = mean(return),
      return_std = sd(return),
      t_stat = t.test(return)[["statistic"]],
      p_value = t.test(return)[["p.value"]],
      ci99_L = t.test(return, conf.int=0.99)[["conf.int"]][[1]],
      ci99_R = t.test(return, conf.int=0.99)[["conf.int"]][[2]],
      sharpe_ratio = return_mean / return_std,
      sortino_ratio = sortino_ratio(return),
      return_geom_mean = prod(1 + return) ^ (1/n()) - 1
    ) %>% 
    ungroup()
}

annual_returns_summary <- annual_returns %>% 
  group_by(
    factor,
    period = case_when(
      year <= 1959 ~ "1927--1959",
      year <= 1989 ~ "1960--1989",
      TRUE ~ "1990--2020"
    )
  ) %>% 
  summarize_annual_returns() %>% 
  bind_rows(
    annual_returns %>% 
      filter(year >= 1960) %>% 
      group_by(
        factor,
        period = "1960--2020"
      ) %>% 
      summarize_annual_returns()
  ) %>% 
  bind_rows(
    annual_returns %>% 
      group_by(
        factor,
        period = "1927--2020"
      ) %>% 
      summarize_annual_returns()
  )

annual_returns_summary %>% 
  arrange(factor) %>% 
  transmute(
    latex_format = paste(
      factor,
      period,
      sprintf("%.1f\\%%", return_mean*100),
      sprintf("%.1f\\%%", return_std*100),
      sprintf("%.2f", t_stat),
      if_else(p_value >= 0.001, sprintf("%.1f\\%%", p_value*100), "<0.1\\%"),
      sprintf("[%.1f\\%%, %.1f\\%%]", ci99_L*100, ci99_R*100),
      sep = " & "
    )
  )

cumulative_growth_data <- fama_french_four_factors_data %>% 
  bind_rows(
    tibble(
      "month" = as.Date("1926-12-01"),
      mkt_rf = 0,
      smb = 0,
      hml = 0,
      rf = 0,
      mom = 0
    )
  ) %>% 
  arrange(month) %>% 
  left_join(inflation_data, by="month") %>% 
  transmute(
    date = make_date(year(month), month(month), days_in_month(month)),
    mkt = cumprod(1 + mkt_rf + rf),
    mkt_rf = cumprod(1 + mkt_rf),
    smb = cumprod(1 + smb),
    hml = cumprod(1 + hml),
    mom = cumprod(1 + mom),
    rf = cumprod(1 + rf),
    cpi = cpi / first(cpi)
  )

cumulative_growth_data %>% 
  write_csv("fama_french_cumulative_growth_data.csv")

sample_annualized_returns <- function(x, periods_per_year=12, years=1, n=50) {
  
  replicate(
    n = n,
    prod(1 + sample(x, periods_per_year*years)) ^ (1/years) - 1
  )
}

simulated_annual_returns <- fama_french_four_factors_data %>% 
  summarise(
    sample_mkt_1y = 100 * sample_annualized_returns(mkt_rf+rf, years=1),
    sample_mkt_15y = 100 * sample_annualized_returns(mkt_rf+rf, years=15),
    sample_rf_1y = 100 * sample_annualized_returns(rf, years=1),
    sample_rf_15y = 100 * sample_annualized_returns(rf, years=15),
    mkt_1y_rank = rank(sample_mkt_1y),
    mkt_15y_rank = rank(sample_mkt_15y),
    rf_1y_rank = rank(sample_rf_1y),
    rf_15y_rank = rank(sample_rf_15y)
  )

simulated_annual_returns %>% 
  write_csv("simulated_market_annual_returns.csv")
