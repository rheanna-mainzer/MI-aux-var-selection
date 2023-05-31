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
# Revised by R Mainzer, July 2022

################################################################################

calc_gamma0 <- function(n, p, cor, gamma_0vec, gam_coefs){
  
  # Generate data from MVN
  dat <- data.frame(MASS::mvrnorm(n, mu = rep(0, p + 3), Sigma = cor))
  colnames(dat) <- c("Ycom", "X", "Z", paste("A", 1:p, sep = ""))
 
  # Set up vector for results
  len_gam0 <- length(gamma_0vec)
  res <- rep(0, len_gam0)
  
  # Groups of auxiliary variables
  nblocks <- 3 # number of blocks of auxiliary variables in correlation matrix
  g <- p %/% nblocks # number of auxiliary variables in each group
  rem <- p %% nblocks # remainder (if p not exactly divisible by nblocks) These go into low correlation group
  
  # Gammas for missingess model
  gamma_x <- gam_coefs
  gamma_z <- gam_coefs
  gamma_a <- gam_coefs * c(rep(c(rep(0, floor(g/2)), rep(1, ceiling(g/2))), nblocks), rep(0, rem))
  
  # Do the following for each value in gamma_0vec
  for(i in 1:len_gam0){
    
    # Generate Pr(My) given (X, Z, A)
    logit_pMy <- gamma_0vec[i] + gamma_x * dat$X + gamma_z * dat$Z + 
      as.matrix(dplyr::select(dat, starts_with("A"))) %*% gamma_a 
    p_My <- exp(logit_pMy) / (1 + exp(logit_pMy))
    
    # Set data missing in y according to Pr(My)
    dat$y <- dat$Ycom
    dat$y[runif(n) < p_My] <- NA
    
    # Store the percentage of missing values in y
    res[i] <- sum(is.na(dat$y)) / n 
    
  }
  
  out <- data.frame(gamma_0vec, res)
   
}

################################################################################

# calc_gamma0_2
#
# Input:
# p: number of auxiliary variables
# cor: correlation matrix
# pmy: desired percentage of missing data
# gam_coefs: value of gamma coefficient for missingness model
# gamma0_vec:  vector of values used in inital search for gamma0

calc_gamma0_2 <- function(p, cor, pmy, gam_coefs, gamma_0vec){
  
  # First search to narrow down the value of gamma_0
  res <- calc_gamma0(n = 10000, p = p, cor = cor, gamma_0vec, gam_coefs)
  gam0_init <- res$gamma_0vec[which(abs(res$res - pmy) == min(abs(res$res - pmy)))]
  
  # Second search with new vector of values
  gamma_0vec <- seq(gam0_init - 1, gam0_init + 1, by = 0.1)
  res <- calc_gamma0(n = 10000, p, cor, gamma_0vec, gam_coefs)
  
  # Fit a linear model with gamma_0 and gamma_0^2 terms
  res <- cbind(res, gamma_0vecsq = res$gamma_0vec^2)
  gamma0_mod2 <- lm(res$res ~ res$gamma_0vec + res$gamma_0vecsq)
  
  # Search interval for gamma_0
  search_int <- c(min(gamma_0vec), max(gamma_0vec))
  
  # Find gamma_0 value using linear model with quadratic term
  gamma_0 <- uniroot(f = function(x){gamma0_mod2$coefficients[3] * x^2 + 
      gamma0_mod2$coefficients[2] * x + gamma0_mod2$coefficients[1] - pmy},
      interval = search_int)$root
  
  # Fitted model (store for supplementary figures)
  res$pred <- predict(gamma0_mod2)
  
  # Output 
  list(details = res, gamma0 = gamma_0, p = p)
  
}

