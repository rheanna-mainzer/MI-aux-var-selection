# Create simulation data 
# cr_simdata
#
# Input:
# scenario: simulation scenario
# n: sample size
# p: number of auxiliary variables
# sig_a: covariance matrix of the auxiliary variables
# beta_0: intercept of data generating model for y
# beta_x: coefficient of x in data generating model for y
# beta_a: vector of coefficients for auxiliary variables in data generating model for y
# err_var: error variance for data generating model for y
# gamma_0: the intercept of the missingness model for y (default = 1)
# gamma_a: the vector of coefficients of auxiliary variables in the missingness model for y
# nsim: number of simulated data sets to create
# sim_path: path where simulated data sets will be saved
#
# Output:
# nsim simulated data sets, saved in sim_path
#
# Written by R Mainzer, April 2021

cr_simdata <- function(scenario, n, p, sig_a, beta_0, beta_x, beta_a, err_var, gamma_0, gamma_a, nsim, sim_path){
  
  # Parameters of the data generating model for x and a
  mu_xa <- rep(0, p + 1)
  cor_xa <- rep(0, p)
  var_x <- 1
  sig_xa <- rbind(cbind(var_x, t(cor_xa)), cbind(cor_xa, sig_a))
  
  # Do the following nsim times
  for(j in 1:nsim){
    
    cat("Creating data set", j, "...")
    
    # Generate a1, ..., a_p, x from mvn
    xa <- MASS::mvrnorm(n, mu_xa, sig_xa)
    
    # Generate yobs given a1, ..., a_p and x
    beta <- c(beta_0, beta_x, beta_a)
    yobs <- cbind(rep(1, n), xa) %*% beta + rnorm(n, 0, sd = sqrt(err_var))
    
    # Generate Pr(My) given a1, ..., a_{n_aux} and x (no relationship with x)
    logit_pMy <- gamma_0 + xa[, -1] %*% gamma_a 
    p_My <- exp(logit_pMy) / (1 + exp(logit_pMy))
    
    # Set data missing in y according to Pr(My)
    y <- yobs
    y[runif(n) < p_My] <- NA
    
    # Save data set
    dat <- data.frame(y, xa)
    colnames(dat) <- c("y", "x", paste("a", 1:p, sep =""))
    file_name <- paste(sim_path, "simdat_", scenario, "_", 
                       paste(sprintf("%04d", j)), ".csv", sep = "")
    write.table(dat, file = file_name)
    
    cat("DONE", "\n")
    
  }
  
}