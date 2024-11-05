library(dplyr)
library(readr)
library(lubridate)
library(xlsx)
library(curl)
library(ggplot2)
library(tidyr)

black_scholes_d1 <- function(S, K, T, sigma, r, q) {
 
  1 / (sigma*sqrt(T)) * (log(S/K) + (r - q + sigma*sigma/2)*T) 
}

black_scholes_d2 <- function(S, K, T, sigma, r, q) {
  
  1 / (sigma*sqrt(T)) * (log(S/K) + (r - q - sigma*sigma/2)*T) 
}

black_scholes_delta <- function(S, K, T, sigma, r, q) {
  
  pnorm(black_scholes_d1(S, K, T, sigma, r, q))
}

black_scholes_vega <- function(S, K, T, sigma, r, q) {
  
  d1 <- 1 / (sigma*sqrt(T)) * (log(S/K) + (r - q + sigma*sigma/2)*T)
  S * dnorm(d1) * sqrt(T)
}

black_scholes_theta <- function(S, K, T, sigma, r, q) {

  d1 <- black_scholes_d1(S, K, T, sigma, r, q)
  d2 <- black_scholes_d2(S, K, T, sigma, r, q)
  
  -S * dnorm(d1) * sigma / (2*sqrt(T)) - r*K*exp(-r*T)*pnorm(d2)
}

black_scholes_gamma <- function(S, K, T, sigma, r, q) {
  
  d1 <- black_scholes_d1(S, K, T, sigma, r, q)
  
  dnorm(d1) / (S * sigma * sqrt(T))
}

black_scholes_сall <- function(S, K, T, sigma, r, q) {
  
  d1 <- black_scholes_d1(S, K, T, sigma, r, q)
  d2 <- black_scholes_d2(S, K, T, sigma, r, q)
  S*exp(-q*T)*pnorm(d1) - K*exp(-r*T)*pnorm(d2)
}

tibble(S = seq(0, 1, 0.01)) %>% 
  mutate(
    call_delta = black_scholes_delta(S, 0.5, 1, 0.25, 0, 0),
    put_delta = call_delta - 1
  ) %>% 
  write_csv("black_scholes_delta.csv")

tibble(S = seq(0, 1, 0.01)) %>% 
  mutate(
    vega = black_scholes_vega(S, 0.5, 1, 0.2, 0, 0)
  ) %>% 
  write_csv("black_scholes_vega.csv")

tibble(S = seq(0, 1, 0.01)) %>% 
  mutate(
    theta = black_scholes_theta(S, K=0.5, T=1, sigma=0.2, r=0, q=0)
  ) %>% 
  write_csv("black_scholes_theta.csv")

tibble(T = seq(0.01, 1, 0.01)) %>% 
  mutate(
    theta_itm = black_scholes_theta(S=0.55, K=0.5, T, sigma=0.3, r=0, q=0),
    theta_atm = black_scholes_theta(S=0.50, K=0.5, T, sigma=0.3, r=0, q=0),
    theta_otm = black_scholes_theta(S=0.45, K=0.5, T, sigma=0.3, r=0, q=0)
  ) %>% 
  write_csv("black_scholes_theta_by_money.csv")

tibble(S = seq(0, 1, 0.01)) %>% 
  mutate(
    gamma = black_scholes_gamma(S, 0.5, 1, 0.2, 0, 0)
  ) %>% 
  write_csv("black_scholes_gamma.csv", na = "")

tibble(T = seq(0.01, 1, 0.01)) %>% 
  mutate(
    gamma_itm = black_scholes_gamma(S=0.55, K=0.5, T, sigma=0.3, r=0, q=0),
    gamma_atm = black_scholes_gamma(S=0.50, K=0.5, T, sigma=0.3, r=0, q=0),
    gamma_otm = black_scholes_gamma(S=0.45, K=0.5, T, sigma=0.3, r=0, q=0)
  ) %>% 
  write_csv("black_scholes_gamma_by_money.csv")

S0 <- 100
K <- 100
T <- 0.25
sigma <- 0.2
r <- 0
q <- 0
dt <- 1/252

tibble(
  S = seq(95, 105, 0.01)
) %>% 
  mutate(
    original_call_pv = black_scholes_сall(S0, K, T, sigma, r, q),
    original_call_delta = black_scholes_delta(S0, K, T, sigma, r, q),
    original_loan = original_call_pv - original_call_delta * S0,
    option_pnl = original_call_pv - black_scholes_сall(S, K, T - dt, sigma, r, q),
    delta_pnl = original_call_delta * (S - S0),
    full_pnl = option_pnl + delta_pnl
  ) %>% 
  write_csv("gamma_pnl_example.csv")
