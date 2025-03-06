library(dplyr)
library(curl)
library(xlsx)
library(readr)
library(lubridate)

download_acm_term_premium_data <- function() {
  
  url <- "https://www.newyorkfed.org/medialibrary/media/research/data_indicators/ACMTermPremium.xls"
  temp_file_name <- "act.xls"
  curl_download(url, temp_file_name)
  raw_data <- read.xlsx(temp_file_name, sheetName="ACM Monthly")  
  file.remove(temp_file_name)
  
  raw_data %>% 
    as_tibble() %>% 
    transmute(
      date = as.Date(DATE, "%d-%b-%Y"),
      acmtp02 = ACMTP02,
      acmtp05 = ACMTP05,
      acmtp10 = ACMTP10
    )
}

acm_data <- download_acm_term_premium_data() 

acm_data %>% 
  write_csv("acm_term_premium.csv")

acm_data %>%
  filter(date >= '1990-01-01', date <= '2024-12-31') %>%
  summarise(
    acmtp02 = mean(acmtp02),
    acmtp05 = mean(acmtp05),
    acmtp10 = mean(acmtp10)
  )

