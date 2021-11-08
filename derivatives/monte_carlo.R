library(dplyr)
library(readr)

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

circle_data %>% write_csv("monte_carlo_pi.csv")
