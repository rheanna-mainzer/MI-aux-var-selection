# Create simulation data 
# cr_simdata
#
# Input:
# scen_params: contains the DGP paramaters that change for each scenario
# nsim: number of simulated data sets to create
# sim_path: path where simulated data sets will be saved
#
# Output:
# nsim simulated data sets, saved in sim_path
#
# Written by R Mainzer, April 2021

cr_simdata <- function(scen_params, nsim, sim_path){
  
  # Get params
  n <- scen_params$n
  p <- scen_params$p
  cor <- as.matrix(scen_params$cor[[2]])
  gamma_0 <- scen_params$gam0$gamma0
  gam <- scen_params$gam_coefs
  
  # Gammas for missingess model
  gamma_x <- gam
  gamma_z <- gam
  gamma_a <- calc_gamma_a(p, gam)
  
  # Do the following nsim times
  for(j in 1:nsim){
    
    cat("Creating data set", j, "...")
    
    # Generate data from MVN
    dat <- data.frame(MASS::mvrnorm(n, mu = rep(0, p + 3), Sigma = cor))
    colnames(dat) <- c("y", "x", "z", paste("a", 1:p, sep = ""))
    
    # Generate Pr(My) given (X, Z, A)
    logit_pMy <- gamma_0 + gamma_x * dat$x + gamma_z * dat$z + 
      as.matrix(dplyr::select(dat, starts_with("a"))) %*% gamma_a 
    p_My <- exp(logit_pMy) / (1 + exp(logit_pMy))
    
    # Set data missing in y according to Pr(My)
    dat$y[runif(n) < p_My] <- NA
    
    # Save data set
    file_name <- paste(scen_params$name, "_",   
                       paste(sprintf("%04d", j)), ".csv", sep = "")
    write.table(dat, file = file.path(sim_path, file_name))
    
    cat("DONE", "\n")
    
  }
  
}