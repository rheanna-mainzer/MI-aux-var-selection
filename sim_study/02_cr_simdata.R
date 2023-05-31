# Create simulation data
# 02_cr_simdata
# 
# This script will create 'nsim' simulated data sets for each scenario.

# Load packages
source(file.path("..", "packages.R"), echo = TRUE)

# Load functions
source(file.path("functions", "cr_simdata.R"), echo = TRUE)
source(file.path("functions", "calc_gamma_a.R"), echo = TRUE)

# Set seed
set.seed(10721)

# Create simulated data sets --------------------------------------------------

nsim <- 2000
scenarios <- list("n250_p25_pmy30_mod", 
                  "n250_p83_pmy30_mod",
                  "n1000_p100_pmy30_mod",
                  "n1000_p333_pmy30_mod",
                  "n1000_p100_pmy50_mod",
                  "n1000_p100_pmy30_str")

for(i in 1:6){
  
  # Select scenario
  scenario <- scenarios[[i]]
  
  # Load scenario parameters
  scen_params <- readRDS(file.path("data", scenario))
  
  # Create data
  cr_simdata(scen_params = scen_params, nsim = nsim, sim_path = "data")
  
}