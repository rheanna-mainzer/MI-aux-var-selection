# Tune parameters for analysis strategies
# 03_tuning_params_methods
# 
# This script is used to examine and illustrate the stopping rule for the 
# Forward-FMI strategy

# Load packages
source(file.path("..", "packages.R"), echo = TRUE)

# Load functions
source(file.path("functions", "calc_FMI.R"), echo = TRUE)
source(file.path("functions", "select_aux_forwardFMI.R"), echo = TRUE)
source(file.path("functions", "tune_FMI.R"), echo = TRUE)

# set seed
set.seed(1062021)

# ------------------------------------------------------------------------------
# Examine FMI for a random subset of data sets for each scenario to come up 
# with a data-based stopping rule
# ------------------------------------------------------------------------------

# Input
nsim <- 2000
scenarios <- list("n250_p25_pmy30_mod", 
                  "n250_p83_pmy30_mod",
                  "n1000_p100_pmy30_mod",
                  "n1000_p333_pmy30_mod",
                  "n1000_p100_pmy50_mod",
                  "n1000_p100_pmy30_str")


# Produce graphs of change in FMI for a random subset for each scenario to 
# examine the stopping rule
FMI_data <- list()
for(i in 1:6){
  
  subset <- sample(1:nsim, size = 4, replace = FALSE)
  
  for(j in 1:4){
    
    name <- paste(scenarios[[i]], "_", subset[[j]], sep = "") 
    FMI_df <- tune_FMI(subset[j], scenarios[[i]], name)
    
    if(j == 1){FMI_data[[i]] <- FMI_df}
    
  }
  
  # Save data
  saveRDS(FMI_data, file = file.path("results", "analysis", "FMI_data"))
  
}

# Forward-FMI stopping rule:
# n = 250, p = 25:  stop when the absolute change in FMI is less than 1% of the 
#                   percentage of missing data in Y
# n = 250, p = 83:  stop when the absolute change in FMI is less than 1% of the 
#                   percentage of missing data in Y
# n = 100, p = 100: stop when the absolute change in FMI is less than 1% of the 
#                   percentage of missing data in Y
# n = 333, p = 333: stop when the absolute change in FMI is less than 0.5% of 
#                   the percentage of missing data in Y
