library(MASS)
library(dplyr)
library(readr)
library(tidyr)
library(ggplot2)
library(moments)

tibble(
  x = seq(-8, 8, 0.05),
  norm_cdf = pnorm(x),
  norm_density = dnorm(x),
  mixture_density = 0.9501*dnorm(x, 2, 1) + (1 - 0.9501)*dnorm(x, -2.5, 0.25),
  norm_density_one_two = dnorm(x, 1, 2)
) %>% 
  write_csv("var_dist_example.csv")


# Find a mixture that has the same 5% quantile as std normal distribution
norm_mixture_cdf <- function(x, mu1, sigma1, mu2, sigma2, w) {
  
  w * pnorm(x, mu1, sigma1) + (1-w) * pnorm(x, mu2, sigma2)
}

my_cdf <- function(w) {
  
  norm_mixture_cdf(qnorm(0.05), mu1=2, sigma1=1, mu2=-2.5, sigma2=0.25, w)
}

my_cdf(0.9501)


random_bivariate_std_normal <- function(correlation, sample_size=150) {
  
  mu <- c(0, 0)
  
  Sigma <- matrix(c(
    1, correlation,
    correlation, 1
  ), nrow=2)
  
  sample <- mvrnorm(mu, Sigma, n=sample_size)
  colnames(sample) <- c(sprintf("X%d", correlation*100), sprintf("Y%d",correlation*1e00))
  as_tibble(sample)
}

set.seed(0)
samples <- 
  random_bivariate_std_normal(0.0) %>% 
  bind_cols(
    random_bivariate_std_normal(-0.75)
  ) %>% 
  bind_cols(
    random_bivariate_std_normal(0.75)
  )

samples %>% 
  write_csv("covariance_plot_random_samples.csv")



random_bivariate_normal <- function(mu1, sigma1, mu2, sigma2, rho, sample_size=150) {
  
  mu <- c(mu1, mu2)
  
  Sigma <- matrix(c(
    sigma1^2, rho*sigma1*sigma2,
    rho*sigma1*sigma2, sigma2^2
  ), nrow=2)
  
  sample <- mvrnorm(mu, Sigma, n=sample_size)
  colnames(sample) <- c("x", "y")
  as_tibble(sample)
}

set.seed(0)
random_bivariate_normal(0.20, 2.0, 0.25, 2.5, 0.9, 150) %>% 
  write_csv("var_example_yields.csv")

tibble(
  x = seq(-400, 400),
  y = dnorm(x, -5, 111.8)
) %>% 
  write_csv("var_example_portfolio_density.csv")

qnorm(0.05, -5, 111.8)
dnorm(-188.89, -5, 111.8)

vix <- read_csv("VXVCLS.csv") %>% 
  rename(
    date = DATE,
    vix = VXVCLS
  )

sp500 <- read_csv("SP500.csv") %>% 
  rename(
    date = DATE,
    sp500 = SP500
  )

libor <- read_csv("USD3MTD156N.csv") %>% 
  rename(
    date = DATE,
    libor = USD3MTD156N
  )

merged_data <- vix %>% 
  inner_join(sp500, by="date") %>% 
  inner_join(libor, by="date") %>% 
  transmute(
    date = date,
    vix = as.numeric(vix) / 100,
    sp500 = as.numeric(sp500),
    libor = as.numeric(libor) / 100
  ) %>% 
  filter(
    !is.na(vix),
    !is.na(sp500),
    !is.na(libor)
  ) %>% 
  mutate(
    sp500_chg = sp500 / lag(sp500) - 1
  )

merged_data %>% 
  arrange(desc(date)) %>% 
  head(501) %>%
  write_csv("var_sp500_data.csv")
  
sp500_changes <- merged_data %>% 
  tail(500) %>% 
  dplyr::select(sp500_chg) %>% 
  unlist() %>% 
  unname()

sample_mean <- mean(sp500_changes)
sample_std <- sd(sp500_changes)
sample_kurt <- kurtosis(sp500_changes) - 3

t_mu <- sample_mean
t_dof <- 6 / sample_kurt + 4
t_sigma <- sample_std * sqrt((t_dof - 2) / t_dof)

kernel_density <- density(sp500_changes, bw=0.002)

density_fun <- approxfun(kernel_density$x, kernel_density$y, yleft=0, yright=0)

tibble(
  x = seq(-0.15, 0.15, 0.0005),
  sp500_density = density_fun(x),
  norm_density = dnorm(x, sample_mean, sample_std),
  t_density = wiqid::dt2(x, location=t_mu, scale=t_sigma, df=t_dof)
) %>% 
  write_csv("var_sp500_changes_density.csv")

tibble(
  x = seq(-6, 6, 0.01),
  t2 = dt(x, 2),
  t5 = dt(x, 5),
  t100 = dt(x, 100)
) %>% 
  write_csv("var_t_dist_example.csv")
