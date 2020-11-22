library(dplyr)
library(readr)

N_SIMS <- 100

set.seed(1)

data <- tibble(
  step  = 1:N_SIMS,
  price = cumsum(rnorm(N_SIMS))
)

data %>% write_csv("price_random_walk.csv")
