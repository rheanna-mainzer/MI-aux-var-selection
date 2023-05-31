# calc_FMI
#
# Estimate the fraction of missing information (FMI) in the mean of Y given a 
# set of one or more complete auxiliary variables (A) using the proxy pattern 
# mixture model 
# 
# Input:
# Y: Outcome with missing data
# A: Data frame containing auxiliary variables used to create proxy variable
#
# Details: 
# See https://doi.org/10.1111/insr.12091
# 
# Output:
# Estimate of the FMI
#
# Written by R Mainzer, May 2021

calc_FMI <- function(Y, A){
  
  # Create temporary data frame
  dat_tmp <- cbind(Y, A)
  dat_pred <- cbind(1, A)
  
  # Create proxy variable X, (X = A if p = 1)
  p <- ncol(A)
  
  if(p == 1){
      X <- A[, 1]
  } else {
    
    # Regression of Y on A to create proxy variable X
    mod_tmp <- lm(Y ~ ., data = dat_tmp)
    X <- predict(mod_tmp, newdata = dat_pred, type = "response")
    
  }
  
  # Missingness indicator and number of respondents
  my <- is.na(Y) * 1
  n <- nrow(A)
  r <- n - sum(my) 
  
  # Overall and respondent mean of X
  mean_x <- mean(X)
  mean_x_res <- mean(X[my == 0])

  # Respondent and nonrespondent variance and correlation
  var_x_res <- var(X[my == 0])
  var_x_nonres <- var(X[my == 1])
  var_y_res <- var(Y[my == 0])
  rho_xy_res <- cor(Y[my == 0], X[my == 0])
  
  # Calculate FMI
  DeltaHatx_sq <- (mean_x - mean_x_res)^2 / var_x_res 
  phiHatx <- (var_x_nonres - var_x_res) / var_x_res
  What <- (var_y_res / n) * (1 + rho_xy_res^2 * (r / (n - r) * DeltaHatx_sq + (n - r) / n * phiHatx))
  Bhat <- (var_y_res / r) * (1 - rho_xy_res^2) * ((n - r) / n + DeltaHatx_sq)
  FMI <- Bhat / (What + Bhat)
  FMI
  
} 