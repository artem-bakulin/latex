library(dplyr)
library(readr)
library(curl)


download_from_bundesbank <- function(series_url, series_name) {
  
  curl_download(series_url, "tmp.csv")
  
  data <- read_csv("tmp.csv", skip=8, col_names=c("date", "value", "comment"))
  
  data <- data %>% 
    mutate(value = as.numeric(value)) %>% 
    filter(!is.na(value)) %>% 
    select(date, value)
  
  colnames(data) <- c("date", series_name)
  
  file.remove("tmp.csv")
  
  data
}

euribor_3m <- download_from_bundesbank(
  "https://api.statistiken.bundesbank.de/rest/download/BBK01/ST0316?format=csv&lang=en",
  "3m"
)

euribor_6m <- download_from_bundesbank(
  "https://api.statistiken.bundesbank.de/rest/download/BBK01/ST0325?format=csv&lang=en",
  "6m"
)

eonia <- download_from_bundesbank(
  "https://api.statistiken.bundesbank.de/rest/download/BBK01/ST0304?format=csv&lang=en",
  "eonia"
)

ester <- download_from_bundesbank(
  "https://api.statistiken.bundesbank.de/rest/download/BBMMB/D.EU000A2X2A25.WT?format=csv&lang=en",
  "ester"
)

eonia %>% 
  full_join(ester, by="date") %>% 
  inner_join(euribor_3m, by="date") %>% 
  inner_join(euribor_6m, by="date") %>% 
  mutate(ester = if_else(is.na(ester), eonia - 0.085, ester)) %>% 
  filter(!is.na(ester)) %>% 
  write_csv("euro_benchmark.csv", na="")
