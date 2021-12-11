# Data for efficient frontier plot

library(quadprog)
library(dplyr)
library(tibble)
library(ggplot2)
library(readr)

# Finds an optimal portfolio allocation for given target return
#     returns - a vector of expected returns of different assets
#     sigmas  - a vector of expected standard deviations
#     corrMat - correlation matrix
#     target  - required level of return
optimize_for_target_return <- function(returns, sigmas, corr_mat, target)
{
  # We are solving a quadratic programming problem:
  # x^T * H * x -> min subject to 
  # x * returns^T = target
  # sum(x) = 1
  # x >= 0
  
  n <- length(returns);
  
  # Variance-covariance matrix
  Dmat <- diag(as.vector(sigmas)) %*% corr_mat %*% diag(as.vector(sigmas));
  
  # Linear term is zero
  dvec <- rep(0, n);
  
  # Total weight is 1
  A1 <- matrix(rep(1, n), nrow=1);
  b1 <- 1;
  
  # Total return matches target
  A2 <- matrix(returns, nrow=1);
  b2 <- target;
  
  # x >= 0
  A3 <- diag(n);
  b3 <- matrix(rep(0, n), ncol=1);

  Amat <- rbind(A1, A2, A3);
  bvec <- rbind(b1, b2, b3);
  
  # Minimize 1/2*x^T*D*x - d^T*x subject to
  # A*x >= b
  # Where first 2 rows in A are equalities
  weights <- solve.QP(Dmat, dvec, t(Amat), bvec, meq=2)$solution;
  
  sigma <- sqrt(t(weights) %*% Dmat %*% weights);
  
  list(weights = unlist(weights), sigma=sigma[[1]]);
}


calculate_efficient_frontier <- function(returns, sigmas, corr_mat, min_return, max_return, step=0.05) {
   
  result <- c()
  
  for (return in seq(min_return, max_return, step)) {
    
    solution <- tryCatch({
      optimize_for_target_return(returns, sigmas, corr_mat, return);
    }, error = function(e) {
      NULL
    })
    
    if (is.null(solution)) {
      next;
    }
    
    weights <- solution$weights * 100
    names(weights) <- names(returns)
    
    sigma <- solution$sigma

    t <- tibble(
      "target_return" = return,
      "std_dev" = sigma
    ) %>% 
      bind_cols(as_tibble_row(weights))
    
    result <- bind_rows(result, t)
  }
  
  result
}

mu = c(
  "stocks" = 10.9,
  "bonds" = 5.2,
  "reit" = 10.8,
  "gold" = 7.0
)

sigmas <- c(
  "stocks" = 15.2, 
  "bonds" = 3.6,
  "reit" = 19.2,
  "gold" = 15.6
)

corr_mat <- matrix(c(
  1.00, 0.00, 0.59, 0.04,
  0.00, 1.00, 0.19, 0.28,
  0.59, 0.19, 1.00, 0.13,
  0.04, 0.28, 0.13, 1.00
), nrow=4)

frontier_data <- calculate_efficient_frontier(mu, sigmas, corr_mat, 0, 20)

return_with_min_std <- frontier_data %>% 
  filter(std_dev == min(std_dev)) %>% 
  select(target_return) %>% 
  unlist()

frontier_data <- frontier_data %>% 
  filter(target_return >= return_with_min_std)

frontier_data %>% 
  ggplot(aes(std_dev, target_return)) + 
  geom_line()

frontier_data %>% write_csv("efficient_frontier_plot_data.csv")

bonds_and_gold_sigma <- 12
gold_fraction = (bonds_and_gold_sigma - sigmas["bonds"]) / (sigmas["gold"] - sigmas["bonds"])
bonds_and_gold_return <- mu["bonds"] + gold_fraction * (mu["gold"] - mu["bonds"])

frontier_data %>% 
  filter(
    round(std_dev, 1) %in% c(bonds_and_gold_sigma)
    |
    target_return == round(bonds_and_gold_return, 1)
  )
  
frontier_data %>% 
  filter(round(std_dev, 1) == 9.3) %>% 
  mutate(
    stocks_mid = stocks / 2,
    bonds_mid = ((stocks+bonds) + (stocks)) / 2,
    reit_mid = ((stocks+bonds+reit) + (stocks+bonds)) / 2,
    gold_mid = (100 + (stocks+bonds+reit)) / 2
  )

###############################################################################
# Plot a smaller version with just two assets

mu <- mu %>% head(2)
sigmas <- sigmas %>% head(2)
corr_mat <- corr_mat[1:2, 1:2]

frontier_data_two_assets <- calculate_efficient_frontier(mu, sigmas, corr_mat, 0, 20)

frontier_data_two_assets %>%
  write_csv("efficient_frontier_plot_data_two_assets.csv")

lambda = 0.1
frontier_data %>% 
  mutate(utility = target_return - lambda/2 * std_dev^2) %>% 
  filter(utility == max(utility))
