library(dplyr)
library(readr)
library(tidyr)
library(ggplot2)

black_scholes_d1 <- function(S, K, T, sigma, r, q) {
  
  1 / (sigma*sqrt(T)) * (log(S/K) + (r - q + sigma*sigma/2)*T) 
}

black_scholes_d2 <- function(S, K, T, sigma, r, q) {
  
  1 / (sigma*sqrt(T)) * (log(S/K) + (r - q - sigma*sigma/2)*T) 
}

black_scholes_price <- function(S, K, T, sigma, r, q) {
  
  d1 <- black_scholes_d1(S, K, T, sigma, r, q)
  d2 <- black_scholes_d2(S, K, T, sigma, r, q)
  S*exp(-q*T)*pnorm(d1) - K*exp(-r*T)*pnorm(d2)
}

black_scholes_delta <- function(S, K, T, sigma, r, q) {
  
  pnorm(black_scholes_d1(S, K, T, sigma, r, q))
}

simulate_stock_and_call <- function(S, mu, sigma, K, T, n_steps, n_scenarios, seed=0) {
  
  set.seed(0)
  
  r <- 0
  q <- 0
  
  tibble(step = seq(from=0, to=n_steps)) %>% 
    cross_join(
      tibble(scenario = seq(1:n_scenarios))
    ) %>% 
    mutate(
      random_norm = rnorm(nrow(.))
    ) %>% 
    group_by(scenario) %>% 
    arrange(step) %>% 
    mutate(
      t = step / n_steps,
      dt = t - lag(t, default=0),
      log_increment = (mu - sigma^2/2) * dt + sigma * random_norm * sqrt(dt),
      S = S_0 * exp(cumsum(log_increment)),
      bs_delta = black_scholes_delta(S, K, T - t, sigma, r, q),
      step_delta_pnl = if_else(step != 0, lag(bs_delta) * (S - lag(S)), 0),
      cumulative_pnl = cumsum(step_delta_pnl),
      balance =  black_scholes_price(S_0, K, T, sigma, r, q) + cumulative_pnl
    ) %>% 
    ungroup() 
  
}

S_0 <- 100
mu <- 0.05
sigma <- 0.2
K <- 100
T <- 1

simulate_stock_and_call(S_0, mu, sigma, K, T, n_steps=6, n_scenarios=20) %>% 
  select(scenario, t, S) %>% 
  pivot_wider(values_from = "S", names_from="scenario", names_prefix = "S_") %>% 
  write_csv("black_scholes_hedging_6_stock.csv")

simulate_stock_and_call(S_0, mu, sigma, K, T, n_steps=6, n_scenarios=20) %>% 
  select(scenario, t, bs_delta) %>% 
  pivot_wider(values_from = "bs_delta", names_from="scenario", names_prefix = "bs_delta_") %>% 
  write_csv("black_scholes_hedging_6_delta.csv")

simulate_stock_and_call(S_0, mu, sigma, K, T, n_steps=6, n_scenarios=20) %>% 
  select(scenario, t, balance) %>% 
  pivot_wider(values_from = "balance", names_from="scenario", names_prefix = "balance_") %>% 
  write_csv("black_scholes_hedging_6_balance.csv")

simulate_stock_and_call(S_0, mu, sigma, K, T, n_steps=6, n_scenarios=20) %>% 
  filter(step == max(step)) %>% 
  select(scenario, S, balance) %>% 
  pivot_wider(names_from="scenario", values_from=c("S", "balance")) %>% 
  write_csv("black_scholes_hedging_6_summary.csv")

simulate_stock_and_call(S_0, mu, sigma, K, T, 365*4, 200) %>%
  filter(step == max(step)) %>% 
  select(scenario, t, S, bs_delta, balance) %>% 
  write_csv("black_scholes_hedging_1420.csv")
