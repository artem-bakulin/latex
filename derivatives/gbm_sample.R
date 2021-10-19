library(dplyr)
library(readr)
library(ggplot2)

set.seed(1)

sample_gbm <- function(S, mu, sigma, T, n_steps) {
  delta_t <- 1 / n_steps
  data <- tibble(t=0, s=S, trend=S)
  S_trend <- S
  for (i in seq(delta_t, T, delta_t)) {
    S_trend <- S_trend + S*mu*delta_t
    S = S + S*mu*delta_t + S*sigma*sqrt(delta_t)*rnorm(1)
    data <- rbind(data, tibble(t=i, s=S, trend=S_trend)) 
  }

  data  
}

sample_gbm(100, 0.05, 0.1, 1, 100) %>% 
  write_csv("gbm_sample_100.csv")

sample_gbm(100, 0.1, 0.4, 1, 500) %>% 
  write_csv("gbm_sample_500.csv")
