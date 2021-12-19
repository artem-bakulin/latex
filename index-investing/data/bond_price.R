library(dplyr)
library(ggplot2)

current_yield <- 0.01
yield_vol <- 0.0075
maturity <- 10
time_step <- 0.1

set.seed(0)
tibble(
  t = seq(0, maturity, time_step),
) %>% 
  mutate(
    yield_chg = rnorm(n(), 0, yield_vol * sqrt(time_step)),
    yield = current_yield + cumsum(yield_chg),
    price = 1 / (1 + yield) ^ (maturity - t)
  ) %>% 
  write_csv("bond_price_random_path.csv")
