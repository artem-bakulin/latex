# Prepare data for illustrative covariances plot
library(MASS)
library(dplyr)
library(readr)
library(ggplot2)

random_bivariate_std_normal <- function(correlation, sample_size=250) {
  
  mu <- c(0, 0)
  
  Sigma <- matrix(c(
    1, correlation,
    correlation, 1
  ), nrow=2)
  
  sample <- mvrnorm(mu, Sigma, n=sample_size)
  colnames(sample) <- c(sprintf("X%d", correlation*100), sprintf("Y%d",correlation*100))
  as_tibble(sample)
}

samples <- 
  random_bivariate_std_normal(0.0) %>% 
  bind_cols(
    random_bivariate_std_normal(0.25)
  ) %>% 
  bind_cols(
    random_bivariate_std_normal(0.75)
  ) %>% 
  bind_cols(
    random_bivariate_std_normal(1.0)
  )

samples %>% 
  ggplot(aes(X0, Y0)) + 
  geom_point()

samples %>% 
  ggplot(aes(X25, Y25)) + 
  geom_point()

samples %>% 
  ggplot(aes(X75, Y75)) + 
  geom_point()

samples %>% 
  ggplot(aes(X100, Y100)) + 
  geom_point()

samples %>% 
  write_csv("covariance_plot_random_samples.csv")

