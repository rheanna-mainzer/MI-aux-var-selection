# Calculate the corresponding proportion of missing values for a grid of gamma_0 
# which are then used to calculate the value of gamma_0 used to generate data
# calc_gamma0
#
# Input:
# n: sample size
# p: number of auxiliary variables
# sig_a: covariance matrix of the auxiliary variables
# beta_0: intercept of data generating model for y
# beta_x: coefficient of x in data generating model for y
# beta_a: vector of coefficients for auxiliary variables in data generating model for y
# err_var: error variance for data generating model for y
# gamma_0vec: vector of values of gamma_0
# gamma_a: log(gamma_a) is the vector of coefficients of auxiliary variables in the missingness model for y
#
# Output:
# Data frame with columns gamma_0vec and corresponding percentage of missing values in y
#
# Written by R Mainzer, April 2021

calc_gamma0 <- function(n, p, sig_a, beta_0, beta_x, beta_a, err_var, gamma_0vec, gamma_a){
  
  # Parameters of the data generating model for x and a
  mu_xa <- rep(0, p + 1)
  cor_xa <- rep(0, p)
  var_x <- 1
  sig_xa <- rbind(cbind(var_x, t(cor_xa)), cbind(cor_xa, sig_a)) 
  
  # Generate a1, ..., a_p, x from mvn
  xa <- MASS::mvrnorm(n, mu_xa, sig_xa)
  
  # Generate yobs given a1, ..., a_p and x
  beta <- c(beta_0, beta_x, beta_a)
  yobs <- cbind(rep(1, n), xa) %*% beta + rnorm(n, 0, sd = sqrt(err_var))
 
  # Set up vector for results
  len_gam0 <- length(gamma_0vec)
  res <- rep(0, len_gam0)
  
  # Do the following for each value in gamma_0vec
  for(i in 1:len_gam0){
    
    # Generate Pr(My) given a1, ..., a_p and x (no relationship with x)
    logit_pMy <- gamma_0vec[i] + xa[, -1] %*% gamma_a 
    p_My <- exp(logit_pMy) / (1 + exp(logit_pMy))
    
    # Set data missing in y according to Pr(My)
    y <- yobs
    y[runif(n) < p_My] <- NA
    
    # Store the percentage of missing values in y
    res[i] <- sum(is.na(y)) / n 
    
  }
  
  out <- data.frame(gamma_0vec, res)
   
}

