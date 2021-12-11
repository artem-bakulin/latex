library(MASS)
library(dplyr)
library(readr)
library(ggplot2)

mu <- 1
sigma <- 1
rho <- 0.5

tibble(
  x = seq(-mu-sigma*3, mu+sigma*3, 0.01)
) %>% 
  mutate(
    x1 = dnorm(x, -mu, sigma),
    x2 = dnorm(x, mu, sigma),
    avg = dnorm(x, 0, sqrt(0.25*sigma^2 + 0.25*sigma^2 + 2*0.25*rho*sigma*sigma))
  ) %>% 
#  ggplot(aes(x=x)) +
#  geom_line(aes(y=x1), color="red") +
#  geom_line(aes(y=x2), color="blue") +
#  geom_line(aes(y=avg), color="green")
  write_csv("sum_of_normal_density.csv")

set.seed(0)  

random_bivariate_std_normal <- function(correlation, sample_size=250) {
  
  mu <- c(0, 0)
  
  Sigma <- matrix(c(
    1, correlation,
    correlation, 1
  ), nrow=2)
  
  sample <- mvrnorm(mu, Sigma, n=sample_size)
  colnames(sample) <- c("x1", "x2")
  as_tibble(sample)
}

random_bivariate_std_normal(rho) %>% 
  mutate(
    x1 = -mu + sigma*x1,
    x2 = mu + sigma*x2
  ) %>% 
#  ggplot(aes(x1, x2)) + geom_point()
  write_csv("sum_of_normal_scatter.csv")
