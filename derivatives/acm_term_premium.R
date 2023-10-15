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

download_acm_term_premium_data() %>% 
  write_csv("acm_term_premium.csv")

