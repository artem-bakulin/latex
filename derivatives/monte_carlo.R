library(dplyr)
library(readr)
library(ggplot2)

set.seed(0)

n_points <- 1000

circle_data <- tibble(
  x = runif(n_points, 0, 1),
  y = runif(n_points, 0, 1)
) %>%
  mutate(
    in_circle = (x-0.5)^2 + (y-0.5)^2 <= 0.5^2
  )

monte_carlo_pi <- 4 * sum(circle_data["in_circle"]) / n_points
print(monte_carlo_pi)

circle_data %>% 
  filter(in_circle) %>% 
  write_csv("monte_carlo_pi_in_circle.csv")

circle_data %>% 
  filter(!in_circle) %>% 
  write_csv("monte_carlo_pi_not_in_circle.csv")

tibble(
  x = seq(-5, 5, 0.01),
  y = pnorm(x)
) %>% 
  write_csv("monte_carlo_norm_cdf.csv")

tibble(
  x = seq(0.01, 0.99, 0.01),
  y = qnorm(x)
) %>% 
  write_csv("monte_carlo_norm_inv_cdf.csv")


set.seed(0)

S_0 <- 100
sigma <- 0.15
r <- 0.02
q <- 0.00

tibble(
  path = as.character(seq(1, 10))
) %>% 
  inner_join(
    tibble(t = seq(0, 0.5, 0.05)),
    by=character()
  ) %>% 
  mutate(
    xi = rnorm(n())
  ) %>% 
  group_by(path) %>% 
  mutate(
    dt = t - lag(t),
    price_growth = exp((r - sigma^2/2)*dt + sigma*sqrt(dt)*xi),
    price_growth = if_else(is.na(price_growth), 1, price_growth),
    stock_price = S_0 * cumprod(price_growth)
  ) %>% 
  ungroup() %>% 
  select(path, t, stock_price) %>% 
  write_csv("monte_carlo_paths.csv")
