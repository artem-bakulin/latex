library(dplyr)
library(readr)
library(curl)

download_from_bundesbank <- function(series_url, series_name) {

  print(sprintf("Downloading %s from %s", series_name, series_url))
  curl_download(series_url, "tmp.csv")

  data <- read_csv("tmp.csv", skip=8, col_names=c("date", "value", "comment"))

  data <- data %>%
    mutate(
      date = as.Date(date, "%Y-%m-%d"),
      value = as.numeric(value)
    ) %>%
    filter(!is.na(value)) %>%
    select(date, value)

  colnames(data) <- c("date", series_name)

  file.remove("tmp.csv")

  data
}

euribor_3m <- download_from_bundesbank(
  "https://api.statistiken.bundesbank.de/rest/download/BBIG1/D.D0.EUR.MMKT.EURIBOR.M03.BID._Z?format=csv&lang=en",
  "3m"
)

euribor_6m <- download_from_bundesbank(
  "https://api.statistiken.bundesbank.de/rest/download/BBIG1/D.D0.EUR.MMKT.EURIBOR.M06.BID._Z?format=csv&lang=en",
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

# This code does not work as of October 2023 - the Spanish bank is not publishing these files anymore
# data_url <- "https://www.bde.es/webbde/es/estadis/infoest/series/ti_1_7.csv"
# temp_file_name <- "tmp.txt"
# curl_download(data_url, temp_file_name)
# 
# read_csv(temp_file_name) %>% 
#   transmute(
#     date = `NOMBRE DE LA SERIE`,
#     ester = D_DNBAA572,
#     eonia = D_DNBAA172,
#     `3m` = D_DNBAD172,
#     `6m` = D_DNBAE172
#   ) %>% 
#   tail(-5) %>% 
#   mutate(
#     date = sub(" ENE ", ".01.", date),
#     date = sub(" FEB ", ".02.", date),
#     date = sub(" MAR ", ".03.", date),
#     date = sub(" ABR ", ".04.", date),
#     date = sub(" MAY ", ".05.", date),
#     date = sub(" JUN ", ".06.", date),
#     date = sub(" JUL ", ".07.", date),
#     date = sub(" AGO ", ".08.", date),
#     date = sub(" AGO ", ".08.", date),
#     date = sub(" SEP ", ".09.", date),
#     date = sub(" OCT ", ".10.", date),
#     date = sub(" NOV ", ".11.", date),
#     date = sub(" DIC ", ".12.", date),
#     date = as.Date(date, "%d.%m.%Y"),
#     ester = as.numeric(ester),
#     eonia = as.numeric(eonia),
#     `3m` = as.numeric(`3m`),
#     `6m` = as.numeric(`6m`),
#     ester = if_else(is.na(ester), eonia - 0.085, ester)
#   ) %>% 
#   filter(!is.na(ester)) %>% 
#   write_csv("euro_benchmark.csv", na="")
# 
# file.remove(temp_file_name)