# cr_completedata
# Function used to create complete data
#
# Input:
# n: sample size
# scen_params: list containing scenario values for data generation
# including p, cor and gamma0
#
# Output:
# One complete simulated dataset
#
# Written by R Mainzer, July 2022

cr_completedata <- function(n, scen_params){
  
  # Get params
  p <- scen_params$p
  cor <- as.matrix(scen_params$cor[[2]])
  gamma_0 <- scen_params$gam0$gamma0
  
  # Generate data from MVN
  dat <- data.frame(MASS::mvrnorm(n, mu = rep(0, p + 3), Sigma = cor))
  colnames(dat) <- c("Ycom", "X", "Z", paste("A", 1:p, sep = ""))
  
  # Gammas for missingess model
  gam <- scen_params$gam_coefs
  gamma_x <- gam
  gamma_z <- gam
  gamma_a <- calc_gamma_a(p, gam)

  # Generate Pr(My) given (X, Z, A)
  logit_pMy <- gamma_0 + gamma_x * dat$X + gamma_z * dat$Z + 
    as.matrix(dplyr::select(dat, starts_with("A"))) %*% gamma_a 
  p_My <- exp(logit_pMy) / (1 + exp(logit_pMy))
  
  # Set data missing in y according to Pr(My)
  dat$Y <- dat$Ycom
  dat$Y[runif(n) < p_My] <- NA
  
  # Save missingness indicator variable
  dat$My <- 1*(is.na(dat$Y))
  
  # Output
  dat
   
}
  