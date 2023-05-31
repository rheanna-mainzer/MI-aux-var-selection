# Get and store parameters required for simulation study data generation
# 00_DGP_params
#
# This file will:
# 1. Generate a correlation matrix for each simulation scenario
# 2. Find the value of gamma_0 such that the desired proportion of 
#    missing values for y is achieved for each scenario
# 3. Save the parameters used for data generation for each scenario

# Load packages
source(file.path("..", "packages.R"), echo = TRUE)

# Load functions
source(file.path("functions", "simcor.r"), echo=TRUE)
source(file.path("functions", "calc_gamma0.R"), echo = TRUE)
source(file.path("functions", "calc_pwr.R"), echo = TRUE)
source(file.path("functions", "calc_betax.R"), echo = TRUE)
source(file.path("functions", "cr_corrmat.R"), echo = TRUE)
source(file.path("functions", "calc_gamma_a.R"), echo = TRUE)

# Set seed
set.seed(102)

# Obtain parameters for data generation for each scenario ---------------------

M <- 2000
rho_yz <- 0.3
rho_xz <- 0.3

# Generate correlation matrix for each n and p combination
cor_n250_p25 <- calc_pwr2(seq = seq(0.2, 0.3, by = 0.01), n = 250, p = 25, M, 
                          rho_yz, rho_xz)
cor_n250_p83 <- calc_pwr2(seq = seq(0.25, 0.35, by = 0.01), n = 250, p = 83, M, 
                          rho_yz, rho_xz)
cor_n1000_p100 <- calc_pwr2(seq = seq(0.17, 0.22, by = 0.005), n = 1000, 
                            p = 100, M, rho_yz, rho_xz)
cor_n1000_p333 <- calc_pwr2(seq = seq(0.18, 0.23, by = 0.005), n = 1000, 
                            p = 333, M, rho_yz, rho_xz)

# Scenarios 
n250_p25_pmy30_mod <- list(name = "n250_p25_pmy30_mod", n = 250, p = 25, pmy = 0.3, 
                       gam_coefs = log(1.2), cor = cor_n250_p25, 
                       gamma_0vec = seq(-10, 0, by = 0.1))
n250_p83_pmy30_mod <- list(name = "n250_p83_pmy30_mod", n = 250, p = 83, pmy = 0.3, 
                       gam_coefs = log(1.2), cor = cor_n250_p83, 
                       gamma_0vec = seq(-10, 0, by = 0.1))
n1000_p100_pmy30_mod <- list(name = "n1000_p100_pmy30_mod", n = 1000, p = 100, 
                         pmy = 0.3, gam_coefs = log(1.2), cor = cor_n1000_p100, 
                         gamma_0vec = seq(-10, 0, by = 0.1))
n1000_p333_pmy30_mod <- list(name = "n1000_p333_pmy30_mod", n = 1000, p = 333, 
                         pmy = 0.3, gam_coefs = log(1.2), cor = cor_n1000_p333, 
                         gamma_0vec = seq(-20, 0, by = 0.1))

# Scenarios - additional (increase pmy and gam)
n1000_p100_pmy50_mod <- list(name = "n1000_p100_pmy50_mod", n = 1000, p = 100, 
                             pmy = 0.5, gam_coefs = log(1.2), cor = cor_n1000_p100, 
                             gamma_0vec = seq(-20, 0, by = 0.1))
n1000_p100_pmy30_str <- list(name = "n1000_p100_pmy30_str", n = 1000, p = 100, 
                             pmy = 0.3, gam_coefs = log(2), cor = cor_n1000_p100, 
                             gamma_0vec = seq(-20, 0, by = 0.1))

# List all scenarios
scenarios <- list(n250_p25_pmy30_mod, 
                  n250_p83_pmy30_mod,
                  n1000_p100_pmy30_mod,
                  n1000_p333_pmy30_mod,
                  n1000_p100_pmy50_mod,
                  n1000_p100_pmy30_str)

# Calculate gamma_0 for each scenario
for(i in 1:6){
  
  # Calculate gamma0
  p <- scenarios[[i]]$p
  cor <- scenarios[[i]]$cor[[2]]
  pmy <- scenarios[[i]]$pmy
  gam_coefs <- scenarios[[i]]$gam_coefs
  gamma_0vec <- scenarios[[i]]$gamma_0vec
  gam0 <- calc_gamma0_2(p, cor, pmy, gam_coefs, gamma_0vec)
  
  # Update scenarios
  scenarios[[i]]$gam0 <- gam0
 
  # Save scenarios
  saveRDS(scenarios[[i]], file = file.path("data", scenarios[[i]]$name))
   
  # Print progress
  cat(scenarios[[i]]$name, "... complete", "\n")
  
}