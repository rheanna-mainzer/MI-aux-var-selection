# calc_pwr
# Find rho_xy such that there is 80% power to estimate beta_x 
#
# Input:
# rho_xy: correlation between X and Y
# n: sample size
# M: number of simulation runs
# rho_xz: correlation between X and Z
# rho_yz: correlation between Y and Z
#
# Output: (list)
# Power estimate
# True value of beta_x for given correlations
# Correlation matrix 
#
# Written by R Mainzer, July 2022

calc_pwr <- function(rho_xy, n, p, M, rho_yz, rho_xz){
  
  # Specify mean and correlation matrix
  mu <- rep(0, p + 3)
  
  # Get correlation matrix
  Sig <- cr_corrmat(cor_xy = rho_xy, n = n, p = p)
  
  # Estimate power using simulation
  res <- rep(0, M)
  for(i in 1:M){
    
    # Generate complete data
    dat <- data.frame(MASS::mvrnorm(n, mu = mu, Sigma = Sig))
    colnames(dat) <- c("Y", "X", "Z")
    
    # Reject null hypothesis that beta_x = 0?
    res[i] <- 1 * (summary(lm(Y ~ X + Z, data = dat))$coef[2, 4] < 0.05)
    
  }
  
  # Power estimate
  pwr <- sum(res)/M
  
  # Calculate beta_x using conditional probability formula
  betax <- calc_betax(Sig)
  
  # Output power estimate and true beta_x value
  list(data.frame(Power = pwr, betax = betax), Sig)

}

# -------------------------------------------------

# calc_pwr2
# Find rho_xy such that there is 80% to estimate beta_x 
# using linear interpolation and "calc_pwr" program
#
# Input:
# seq: sequence of values (used to estimate rho_xy)
# n: sample size
# p: number of auxiliary variables
# M: number of simulation runs
# rho_xz: correlation between X and Z
# rho_yz: correlation between Y and Z
#
# Output: (list)
# Power estimate
# True value of beta_x for given correlations
# Value of rho_xy
# Correlation matrix 
#
# Written by R Mainzer

calc_pwr2 <- function(seq, n, p, M, rho_yz, rho_xz){
  
  # Set up results vector
  res <- rep(0, length(seq))
  
  # Estimate power for a vector of values
  for(i in 1:length(seq)){
    res[i] <- calc_pwr(rho_xy = seq[i], n, p, M, rho_yz, rho_xz)[[1]]$Power
  }
  
  # Fit a linear model with quadratic term
  seq_sq <- seq^2
  mod <- lm(res ~ seq + seq_sq)
  curve(mod$coefficients[1] + mod$coefficients[2]*x + mod$coefficients[3]*x^2, 
        col = "red", add = TRUE)
  
  # Obtain rho_xy 
  rho_xy <- uniroot(f = function(x){mod$coefficients[3] * x^2 + 
      mod$coefficients[2] * x + mod$coefficients[1] - 0.8},
      interval = c(min(seq), max(seq)))$root
  
  # Check power estimate for final value of rho_xy
  est <- calc_pwr(rho_xy = rho_xy, n, p, M, rho_yz, rho_xz)
  
  # Output pwr, beta_x, rho_xy
  list(data.frame(est[[1]], rho_xy = rho_xy), est[[2]])
  
}


