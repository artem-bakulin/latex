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

black_scholes_price <- function(S, K, T, sigma, r, q) {
  
  d1 <- 1 / (sigma*sqrt(T)) * (log(S/K) + (r - q + sigma*sigma/2)*T)
  d2 <- 1 / (sigma*sqrt(T)) * (log(S/K) + (r - q - sigma*sigma/2)*T)
  
  S*exp(-q*T)*pnorm(d1) - K*exp(-r*T)*pnorm(d2)
}

tibble(
  S = seq(80, 120, 0.05)
) %>% 
  mutate(
    C_5 = black_scholes_price(S, 100, 0.25,  0.05, 0.05, 0),
    C_10 = black_scholes_price(S, 100, 0.25,  0.1,  0.05, 0),
    C_25 = black_scholes_price(S, 100, 0.25, 0.25, 0.05, 0)
  ) %>% 
  write_csv("call_price.csv")

tibble(
  x = seq(-5, 5, 0.01)
) %>% 
  mutate(
    norm_density = dnorm(x),
    lognorm_density = dlnorm(x)
  ) %>% 
  write_csv("norm_and_lognorm_density.csv")
