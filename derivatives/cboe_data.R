library(dplyr)
library(readr)
library(curl)
library(lubridate)
library(xlsx)
library(tidyr)

last_day_of_month <- function(x) {
  make_date(year(x), month(x), days_in_month(x))
}


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


download_put_from_wbarchive <- function() {
  
  full_url <- "https://web.archive.org/web/20141013031827/http://www.cboe.com/micro/put/PUT_86-06.xls"
  curl_download(full_url, "put.xls")
  data <- read.xlsx("put.xls", sheetIndex=1, startRow=7, colIndex=c(1, 2), header=FALSE)
  file.remove("put.xls")
  
  data <- as_tibble(data)
  colnames(data) <- c("date", "put")
  data %>% 
    filter(!is.na(date), !is.na(put))
}


download_put_from_webarchive_2007 <- function() {
  
  full_url <- "https://web.archive.org/web/20150604222217/http://www.cboe.com/publish/scheduledtask/mktdata/datahouse/putdailyprice.csv"
  curl_download(full_url, "put.csv")
  data <- read_csv("put.csv", skip=7, col_names=c("date_mmddyyy", "put")) 
  file.remove("put.csv")
  
  data %>% 
    transmute(
      date = as.Date(date_mmddyyy, "%m/%d/%Y"),
      put = put
    ) %>% 
    filter(!is.na(date), !is.na(put))
}


download_from_cboe <- function(index_name) {

  full_url <- sprintf("https://cdn.cboe.com/api/global/us_indices/daily_prices/%s_History.csv", index_name)
  curl_download(full_url, "tmp.csv")
  data <- read_csv("tmp.csv")
  file.remove("tmp.csv")
  
  colnames(data) <- c("date", "index_level")
  data %>% 
    mutate(
      date = as.Date(date, "%m/%d/%Y"),
      index_name = index_name
    )
}


download_shiller_data <- function() {
  
  full_url <- "https://img1.wsimg.com/blobby/go/e5e77e0b-59d1-44d9-ab25-4763ac982e53/downloads/ie_data.xls"
  curl_download(full_url, "ie_data.xls")
  raw_data <- xlsx::read.xlsx("ie_data.xls", sheetName="Data", startRow=8)
  file.remove("ie_data.xls")
  
  raw_data %>% 
    as_tibble() %>%
    transmute(
      month = last_day_of_month(make_date((Date*100) %/% 100, (Date*100) %% 100, 1)),
      sp500 = as.numeric(P),
      dividend = as.numeric(D)
    ) %>% 
    filter(!is.na(month))
}

put_data_current <- download_from_cboe("PUT")
put_data_1986_2006 <- download_put_from_wbarchive()
put_data_2007 <- download_put_from_webarchive_2007()
put_data <- bind_rows(
  put_data_1986_2006 %>% filter(date <= "2006-12-31"),
  put_data_2007 %>% filter(date >= "2007-01-01", date <= "2007-06-19"),
  put_data_current %>% filter(date >= "2007-06-20") %>% transmute(date = date, put = index_level)
)
put_monthly <- put_data %>% 
  group_by(month = last_day_of_month(date)) %>% 
  summarise(put = last(put)) %>% 
  filter(month >= "1986-12-31") %>% 
  mutate(
    put_return = put / lag(put) - 1,
    put_return = if_else(is.na(put_return), 0, put_return),
    put_growth = cumprod(1 + put_return)
  )

puty_data <- download_from_cboe("PUTY")

puty_monthly <- puty_data %>% 
  group_by(month = last_day_of_month(date)) %>% 
  summarise(puty = last(index_level)) %>% 
  filter(month >= '1986-12-31') %>% 
  mutate(puty_return = puty / lag(puty) - 1) %>% 
  mutate(puty_return = if_else(is.na(puty_return), 0, puty_return)) %>% 
  mutate(puty_growth = cumprod(1 + puty_return))


shiller_data <- download_shiller_data()

sp500_monthly <- shiller_data %>% 
  filter(
    month >= "1986-12-31"
  ) %>% 
  mutate(
    sp500_return = (sp500 + if_else(is.na(dividend), 0, dividend/12)) / lag(sp500) - 1,
    sp500_return = if_else(is.na(sp500_return), 0, sp500_return),
    sp500_growth = cumprod(1 + sp500_return)
  )


ff_data <- download_fama_french_factors_data()

rf_monthly <- ff_data %>% 
  transmute(
    month = last_day_of_month(month),
    rf_return = rf
  ) %>% 
  filter(
    month >= '1986-12-31'
  ) %>% 
  mutate(
    rf_growth = cumprod(1 + rf_return)
  )


merged_data <- puty_monthly %>% 
  left_join(put_monthly, by="month") %>% 
  left_join(sp500_monthly, by="month") %>% 
  left_join(rf_monthly, by="month")

merged_data %>% write_csv("cboe_puty.csv", na="nan")

merged_data %>% 
  tail(-1) %>% 
  filter(month >= "1987-01-01", month <= "2023-12-31") %>% 
  lm(put_return - rf_return ~ sp500_return - rf_return, data=.) %>% 
  summary()

merged_data %>% 
  tail(-1) %>% # Remove the fist dummy row
  group_by(
    year = year(month)
  ) %>% 
  filter(year >= 2010) %>% 
  summarise(
    sp500 = prod(1 + sp500_return) - 1,
    put = prod(1 + put_return) - 1,
    puty = prod(1 + puty_return) - 1,
    rf = prod(1 + rf_return) - 1,
    sp500_excess = (1 + sp500) / (1 + rf) - 1,
    put_excess = (1 + put) / (1 + rf) - 1,
    puty_excess = (1 + puty) / (1 + rf) - 1
  ) %>% 
  summarise(
    from = min(year),
    to = max(year),
    sp500_mean = prod(1 + sp500) ^ (1/n()) - 1,
    put_mean = prod(1 + put) ^ (1/n()) - 1,
    puty_mean = prod(1 + puty) ^ (1/n()) - 1,
    rf_mean = prod(1 + rf) ^ (1/n()) - 1,
    sp500_excess_mean = prod(1 + sp500_excess) ^ (1/n())- 1,
    put_excess_mean = prod(1 + put_excess) ^ (1/n())- 1,
    puty_excess_mean = prod(1 + puty_excess) ^ (1/n())- 1,
    sp500_std = sd(sp500_excess),
    put_std = sd(put_excess),
    puty_std = sd(puty_excess),
    sp500_sharpe = sp500_excess_mean / sp500_std,
    put_sharpe = put_excess_mean / put_std,
    puty_sharpe = puty_excess_mean / puty_std
  )

merged_data %>% 
  tail(-1) %>% # Remove the fist dummy row
  group_by(
    year = year(month)
  ) %>% 
  filter(year >= 1987, year <= 2023) %>% 
  summarise(
    sp500 = prod(1 + sp500_return) - 1,
    put = prod(1 + put_return) - 1,
    puty = prod(1 + puty_return) - 1,
    rf = prod(1 + rf_return) - 1,
    sp500_excess = (1 + sp500) / (1 + rf) - 1,
    put_excess = (1 + put) / (1 + rf) - 1,
    puty_excess = (1 + puty) / (1 + rf) - 1
  ) %>% 
  summarise(
    from = min(year),
    to = max(year),
    sp500_mean = prod(1 + sp500) ^ (1/n()) - 1,
    put_mean = prod(1 + put) ^ (1/n()) - 1,
    puty_mean = prod(1 + puty) ^ (1/n()) - 1,
    rf_mean = prod(1 + rf) ^ (1/n()) - 1,
    sp500_excess_mean = prod(1 + sp500_excess) ^ (1/n())- 1,
    put_excess_mean = prod(1 + put_excess) ^ (1/n())- 1,
    puty_excess_mean = prod(1 + puty_excess) ^ (1/n())- 1,
    sp500_std = sd(sp500_excess),
    put_std = sd(put_excess),
    puty_std = sd(puty_excess),
    sp500_sharpe = sp500_excess_mean / sp500_std,
    put_sharpe = put_excess_mean / put_std,
    puty_sharpe = puty_excess_mean / puty_std
  )

puty_data %>% 
  filter(date %in% as.Date(c("1987-10-16", "1987-10-19"))) %>% 
  mutate(one_day_return = index_level / lag(index_level) - 1)

put_data %>% 
  filter(date %in% as.Date(c("1987-10-12", "1987-10-19"))) %>% 
  summarise(one_day_return = last(put) / first(put) - 1)

merged_data %>% 
  filter(month == "1987-10-31")

merged_data %>% 
  tail(-1) %>% # Remove the fist dummy row
  group_by(
    year = year(month)
  ) %>% 
  filter(year == 1987) %>% 
  summarise(
    sp500 = prod(1 + sp500_return) - 1,
    puty = prod(1 + puty_return) - 1,
    put = prod(1 + put_return) - 1
  )

sp500_index <- yahoofinancer::Index$new("^GSPC")
sp500_data <- sp500_index$get_history(start="1927-12-30", end="2023-10-27", interval="1d")

sp500_data %>% 
  as_tibble() %>% 
  transmute(
    date = as.Date(date, tz="UTC"),
    sp500 = close
  ) %>% 
  mutate(return_40 = sp500 / lag(sp500, 40) - 1) %>% 
  arrange(return_40)

sp500_data %>% 
  as_tibble() %>% 
  filter(date >= '1987-10-14') %>% 
  mutate(return = close / lag(close) - 1)

sp500_data %>% 
  as_tibble() %>% 
  filter(as.Date(date) %in% as.Date(c("1987-10-12", "1987-10-19"))) %>% 
  summarise(week_return = last(close) / first(close) - 1)
         
sp500_monthly %>% 
  filter(month == "1987-10-31")


gamma_data <- download_from_cboe("GAMMA")
gamma_data <- gamma_data %>%
  bind_rows(
    head(.) %>% mutate(date = date - 1),
    .
  ) %>%
  group_by(
    month = last_day_of_month(date)
  ) %>%
  summarise(gamma = last(index_level)) %>%
  mutate(
    gamma_return = gamma / lag(gamma,default=first(gamma)) - 1,
    inv_gamma_return = -gamma_return,
    gamma_growth = cumprod(1 + gamma_return),
    inv_gamma_growth = cumprod(1 + inv_gamma_return),
  )

# Gamma goes back to 2012 -> we can use SP500 total return from Yahoo Finance rather than from Shiller's website
sp500_tr_index <- yahoofinancer::Index$new("^SP500TR")
sp500_tr_data <- sp500_tr_index$get_history(start="2012-05-31", end="2024-11-07", interval="1d") %>%
  as_tibble() %>% 
  transmute(
    date = as.Date(date, tz="UTC"),
    sp500 = close
  ) %>%
  group_by(
    month = last_day_of_month(date)
  ) %>%
  summarise(
    sp500 = last(sp500)
  ) %>%
  mutate(
    sp500_return = sp500 / lag(sp500, default=first(sp500)) - 1,
    sp500_growth = cumprod(1 + sp500_return)
  )

merged_gamma_data <- gamma_data %>%
  left_join(sp500_tr_data, by="month") %>%
  left_join(rf_monthly, by="month") %>%
  mutate(rf_growth = rf_growth / first(rf_growth))

merged_gamma_data %>%
  write_csv("cboe_gamma.csv", na="nan")

merged_gamma_data <- merged_gamma_data %>% filter(month >= '2013-01-01', month<='2023-12-31')

merged_gamma_data %>%
  group_by(
    year = year(month)
  ) %>% 
  summarise(
    sp500 = prod(1 + sp500_return) - 1,
    inv_gamma = prod(1 + inv_gamma_return) - 1,
    rf = prod(1 + rf_return) - 1,
    sp500_excess = (1 + sp500) / (1 + rf) - 1,
    inv_gamma_excess = (1 + inv_gamma) / (1 + rf) - 1
  ) %>% 
  summarise(
    from = min(year),
    to = max(year),
    sp500_mean = prod(1 + sp500) ^ (1/n()) - 1,
    inv_gamma_mean = prod(1 + inv_gamma) ^ (1/n()) - 1,
    rf_mean = prod(1 + rf) ^ (1/n()) - 1,
    sp500_excess_mean = prod(1 + sp500_excess) ^ (1/n())- 1,
    inv_gamma_excess_mean = prod(1 + inv_gamma_excess) ^ (1/n())- 1,
    sp500_std = sd(sp500_excess),
    inv_gamma_std = sd(inv_gamma_excess),
    sp500_sharpe = sp500_excess_mean / sp500_std,
    inv_gamma_sharpe = inv_gamma_excess_mean / inv_gamma_std
  )
