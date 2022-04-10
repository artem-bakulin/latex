library(dplyr)
library(readr)
library(ggplot2)
library(tidyr)
library(lubridate)
library(curl)
library(RcppRoll)


string_to_date <- function(x) {
  
  yyyymm_or_yyyy <- as.numeric(x)
  res <- rep(make_date(), length(x))
  
  year_indices <- yyyymm_or_yyyy <= 10000
  res[year_indices] <- make_date(
    yyyymm_or_yyyy[year_indices],
    1, 
    1)
  
  month_indices <- !year_indices
  res[month_indices] <- make_date(
    yyyymm_or_yyyy[month_indices] %/% 100,
    yyyymm_or_yyyy[month_indices] %% 100, 
    1
  )
  
  res
}


download_generic_fama_french_data <- function(url, skip_rows, category=NA) {
  
  file_name <- basename(url)
  curl_download(url, file_name)
  
  raw_data <- read_csv(file_name, skip=skip_rows)
  
  category_and_tail <- if(is.na(category)) {
    raw_data
  } else {
    category_index <- match(category,  raw_data$`...1`)
    raw_data %>% tail(-category_index-1)
  }
  
  na_index <- match(TRUE, is.na(as.numeric(category_and_tail$`...1`)))
  category <- if(!is.na(na_index)) {
    category_and_tail %>% head(na_index - 1)
  } else {
    category_and_tail
  }
  
  category <- category %>% 
    mutate(`...1` = string_to_date(`...1`))
  
  category <- if (all(month(category$`...1`) == 1)) {
    category %>% 
      rename(year = `...1`)
  } else {
    category %>% 
      rename(month = `...1`)
  }
  
  file.remove(file_name)
  
  category
}


download_fama_french_factors_data <- function() {
  
  data <- download_generic_fama_french_data(
    "https://mba.tuck.dartmouth.edu/pages/faculty/ken.french/ftp/F-F_Research_Data_Factors_CSV.zip",
    skip_rows = 3
  )
  
  data %>% 
    transmute(
      month = month,
      mkt_rf = `Mkt-RF` / 100,
      smb = SMB / 100,
      hml = HML / 100,
      rf = RF / 100
    )
}


download_momentum_factor_data <- function() {
  
  data <- download_generic_fama_french_data(
    "https://mba.tuck.dartmouth.edu/pages/faculty/ken.french/ftp/F-F_Momentum_Factor_CSV.zip",
    skip_rows = 13
  )
  
  data %>% 
    transmute(
      month = month,
      mom = Mom / 100
    )
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
  "https://fred.stlouisfed.org/graph/fredgraph.csv?bgcolor=%23e1e9f0&chart_type=line&drp=0&fo=open%20sans&graph_bgcolor=%23ffffff&height=450&mode=fred&recession_bars=on&txtcolor=%23444444&ts=12&tts=12&width=1168&nt=0&thu=0&trc=0&show_legend=yes&show_axis_titles=yes&show_tooltip=yes&id=CPIAUCNS&scale=left&cosd=1913-01-01&coed=2022-02-01&line_color=%234572a7&link_values=false&line_style=solid&mark_type=none&mw=3&lw=2&ost=-99999&oet=99999&mma=0&fml=a&fq=Monthly&fam=avg&fgst=lin&fgsnd=2020-02-01&line_index=1&transformation=lin&vintage_date=2022-04-10&revision_date=2022-04-10&nd=1913-01-01",
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
      ci99_L = t.test(return, conf.level=0.99)[["conf.int"]][[1]],
      ci99_R = t.test(return, conf.level=0.99)[["conf.int"]][[2]],
      sharpe_ratio = return_mean / return_std,
      sortino_ratio = sortino_ratio(return),
      return_geom_mean = prod(1 + return) ^ (1/n()) - 1
    ) %>% 
    ungroup()
}

annual_returns_summary <- annual_returns %>% 
  filter(year >= 1927, year <= 2021) %>% 
  group_by(
    factor,
    period = case_when(
      year <= 1959 ~ "1927--1959",
      year <= 1989 ~ "1960--1989",
      TRUE ~ "1990--2021"
    )
  ) %>% 
  summarize_annual_returns() %>% 
  bind_rows(
    annual_returns %>% 
      filter(year >= 1960) %>% 
      group_by(
        factor,
        period = "1960--2021"
      ) %>% 
      summarize_annual_returns()
  ) %>% 
  bind_rows(
    annual_returns %>% 
      group_by(
        factor,
        period = "1927--2021"
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

annual_returns_summary %>% 
  filter(factor == "mkt_rf") %>% 
  transmute(
    latex_format = paste(
      period,
      sprintf("%.1f\\%%", return_mean*100),
      sprintf("%.1f\\%%", return_std*100),
      sprintf("%.1f\\%%", return_geom_mean*100),
      sprintf("%.2f", sharpe_ratio),
      sprintf("%.2f", sortino_ratio),
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
  write_csv("fama_french_cumulative_growth_data.csv", na="NaN")

cumulative_growth_data_1990 <- fama_french_four_factors_data %>% 
  filter(year(month) >= 1990) %>% 
  bind_rows(
    tibble(
      "month" = as.Date("1989-12-01"),
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

cumulative_growth_data_1990 %>% 
  write_csv("fama_french_cumulative_growth_data_1990.csv", na="NaN")

set.seed(1)
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

holding_years <- 1:30
holding_quantiles <- c(0, 0.01, 0.05, 0.25, 0.5, 0.75, 0.95, 0.99, 1)

cumulative_growth_data <- cumulative_growth_data %>% 
  filter(year(date) <= 2021)

returns_by_holding_period <- lapply(holding_years, function(years_in) {
  
  cumulative_growth_data %>% 
    transmute(
      holding_period = years_in,
      return = mkt / lag(mkt, 12*years_in) * lag(cpi, 12*years_in) / cpi - 1,
    ) %>% 
    filter(!is.na(return))
}) %>% bind_rows()

returns_by_holding_period %>% 
  group_by(holding_period) %>% 
  summarise(
    return_pct = sprintf("%d%%", 100*holding_quantiles),
    return = sprintf("%.1f%%", 100*quantile((1 + return)^(1/holding_period)-1, holding_quantiles))
  ) %>% 
  ungroup() %>% 
  pivot_wider(names_from = "return_pct", values_from="return")

returns_by_holding_period %>% 
  group_by(holding_period) %>% 
  summarise(
    return_pct = 100*holding_quantiles,
    return = 100*quantile((1 + return)^(1/holding_period)-1, holding_quantiles)
  ) %>% 
  ungroup() %>% 
  pivot_wider(names_from = "return_pct", values_from="return") %>% 
  write_csv("us_mkt_holding_periods.csv")

investment_outcome_by_period <- lapply(holding_years, function(years_in) {
  
  cumulative_growth_data %>% 
    mutate(
      mkt_real_growth = mkt / cpi,
      growth = mkt_real_growth * roll_sumr(1 / mkt_real_growth, years_in*12) / (years_in*12)
    ) %>% 
    filter(!is.na(growth)) %>% 
    summarise(
      holding_period = years_in,
      growth_pct = 100*holding_quantiles,
      growth = quantile(growth, holding_quantiles)
    )
}) %>% bind_rows()

investment_outcome_by_period %>% 
  pivot_wider(names_from="growth_pct", values_from="growth") %>% 
  write_csv("us_mkt_regular_investment.csv")



###############################################################################
# Download size and size/beta portfolios

size_portfolios <- download_generic_fama_french_data(
  "http://mba.tuck.dartmouth.edu/pages/faculty/ken.french/ftp/Portfolios_Formed_on_ME_CSV.zip",
  skip_rows=12,
  "Value Weight Returns -- Annual from January to December"
)

annual_rf <- fama_french_four_factors_data %>% 
  group_by(
    year = make_date(year(month), 1, 1)
  ) %>% 
  summarize(
    rf = prod(1 + rf) - 1
  )

size_portfolios %>% 
  pivot_longer(-year, names_to = "bucket", values_to = "bucket_return") %>% 
  mutate(bucket_return = bucket_return / 100) %>% 
  filter(bucket %in% c("Lo 20", "Qnt 2", "Qnt 3", "Qnt 4", "Hi 20")) %>% 
  inner_join(annual_rf, by="year") %>% 
  group_by(bucket) %>% 
  summarise(
    mean_excess_return = mean(bucket_return - rf)
  )

size_and_beta_portfolios <- download_generic_fama_french_data(
  "http://mba.tuck.dartmouth.edu/pages/faculty/ken.french/ftp/25_Portfolios_ME_BETA_5x5_CSV.zip",
  skip_rows = 16,
  "Average Value Weighted Returns -- Annual"
)

size_and_beta_portfolios %>% 
  pivot_longer(-year, names_to = "bucket", values_to = "bucket_return") %>% 
  mutate(bucket_return = bucket_return / 100) %>% 
  inner_join(annual_rf, by="year") %>% 
  group_by(bucket) %>% 
  summarise(
    mean_excess_return = mean(bucket_return - rf)
  ) 

value_portfolios <- download_generic_fama_french_data(
  "http://mba.tuck.dartmouth.edu/pages/faculty/ken.french/ftp/Portfolios_Formed_on_BE-ME_CSV.zip",
  skip_rows = 23,
  "Value Weight Returns -- Annual from January to December"
)

value_portfolios %>% 
  pivot_longer(-year, names_to = "bucket", values_to = "bucket_return") %>% 
  filter(bucket %in% c("Lo 20", "Qnt 2", "Qnt 3", "Qnt 4", "Hi 20")) %>% 
  mutate(bucket_return = bucket_return / 100) %>% 
  inner_join(annual_rf, by="year") %>% 
  group_by(bucket) %>% 
  summarise(
    mean_excess_return = mean(bucket_return - rf)
  )

###############################################################################
# Download international F-F factors

intl_ff_data <- download_generic_fama_french_data(
  "https://mba.tuck.dartmouth.edu/pages/faculty/ken.french/ftp/Developed_ex_US_3_Factors_CSV.zip",
  skip_rows = 6
) %>% 
  transmute(
    month = month,
    mkt_rf = as.numeric(`Mkt-RF`) / 100,
    smb = as.numeric(SMB) / 100,
    hml = as.numeric(HML) / 100,
    rf = as.numeric(RF) / 100
  )

intl_mom_data <- download_generic_fama_french_data(
  "https://mba.tuck.dartmouth.edu/pages/faculty/ken.french/ftp/Developed_ex_US_Mom_Factor_CSV.zip",
  skip_rows = 6
) %>% 
  transmute(
    month = month,
    mom = as.numeric(WML) / 100
  )

intl_growth_data <- intl_ff_data %>% 
  inner_join(intl_mom_data, by="month") %>% 
  transmute(
    date = make_date(year(month), month(month), days_in_month(month)),
    mkt_rf = cumprod(1 + mkt_rf),
    smb = cumprod(1 + smb),
    hml = cumprod(1 + hml),
    mom = cumprod(1 + mom)
  ) %>% 
  filter(date >= make_date(1990, 12, 31)) %>% 
  mutate(
    mkt_rf = mkt_rf / first(mkt_rf),
    smb = smb / first(smb),
    hml = hml / first(hml),
    mom = mom / first(mom)
  )

intl_growth_data %>% 
  write_csv("fama_french_international_cumulative_growth_data.csv")

fama_french_four_factors_data %>% 
  mutate(
    interval = case_when(
      month >= '1990-07-01' & month <= '1991-03-01' ~ "1990.07--1991.03",
      month >= '2000-03-01' & month <= '2000-11-01' ~ "2000.03--2000.11",
      month >= '2007-12-01' & month <= '2009-06-01' ~ "2007.12--2009.06",
      month >= '2020-02-01' & month <= '2020-04-01' ~ "2020.02--2020.04",
      TRUE ~ "Other"
    )
  ) %>% 
  filter(interval != "Other") %>% 
  group_by(interval) %>% 
  summarise(
    mkt_rf = prod(1 + mkt_rf) - 1,
    smb = prod(1 + smb) - 1,
    hml = prod(1 + hml) - 1,
    mom = prod(1 + mom) - 1
  )

fama_french_four_factors_data %>%
  filter(year(month) >= 2000) %>% 
  ggplot(aes(mkt_rf, hml)) + geom_point()
