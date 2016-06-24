rm(list = ls())

# Matrix algebra review in the pdf
# matrix multiplication in R
# R operator: A%*%B where A and B are two matrices
# Identity matrix: where the diagonal elements are 1 and all others are 0

?t # can be used to get the inverse of a matrix
?solve



# Covariance matrix
# X and Y are bivariate normally distributed
# suppose that the standard deviation of x and y is 0.1 and 0.05 respectively and correlation is 0.9

# Standard deviations and correlation
sig_x <- 0.10
sig_y <- 0.05
rho_xy <- 0.9

# Covariance between X and Y
sig_xy <- rho_xy * sig_y * sig_x
  
# Covariance matrix
Sigma_xy <- matrix()
  
