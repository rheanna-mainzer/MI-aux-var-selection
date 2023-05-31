# Compile and present results from the simulation study
# 07_cr_simresults.R

# Load packages
source(file.path("..", "packages.R"), echo = TRUE)

# Load functions
source(file.path("functions", "cr_simresults.R"), echo = TRUE)
source(file.path("functions", "prop_aux_selected.R"), echo = TRUE)
source(file.path("functions", "calc_gamma_a.R"), echo = TRUE)


# Input ------------------------------------------------------------------------

strategy_vec <- c("cc", "mice-full", "quickpred-pt2", "quickpred-pt4", "pcaux", 
                  "forward", "forward-sw", "forward-fmi", "tests", "LASSO")
scenario_vec <- c("n250_p25_pmy30_mod", "n250_p83_pmy30_mod", 
                  "n1000_p100_pmy30_mod", "n1000_p333_pmy30_mod", 
                  "n1000_p100_pmy50_mod", "n1000_p100_pmy30_str")

# Combine results and calculate performance measures ---------------------------

sim_res <- list()
DGP <- list()
aux_vars <- list()

for(i in 1:6){
  cat("Scenario", i)
  
  # Define scenario
  scenario <- scenario_vec[i]
  
  # Read in first batch
  Rdat <- read.csv(file.path("results", "analysis", 
                             paste("R_", scenario, "_batch1", ".csv", sep = "")), 
                   sep = " ")
  Stata_dat <- data.frame(read_dta(file.path("results", "analysis", 
                                             paste("Stata_", scenario, "_batch1.dta", sep = ""))))
  
  for(j in 2:200){
  cat(".")
  
  # Read R results - batches 2 to 200
  R_file_name <- file.path("results", "analysis", paste("R_", scenario, "_batch", j, ".csv", sep = ""))
  Rdat_tmp <- read.csv(R_file_name, sep = " ")
  Rdat <- rbind(Rdat, Rdat_tmp)
  
  # Read Stata results - batches 2 to 200
  stata_file_name <- file.path("results", "analysis", paste("Stata_", scenario, "_batch", j, ".dta", 
                           sep = ""))
  stata_tmp <- data.frame(read_dta(stata_file_name))
  Stata_dat <- rbind(Stata_dat, stata_tmp)
  
  }
  
  # Combine R and Stata results
  nam <- paste0("res_", scenario)
  colnames(Stata_dat)[2] <- "nsim"
  res <- rbind(Rdat, Stata_dat)
  save(res, file = file.path("results", "analysis", paste("res_", scenario, ".RData", sep = "")))
  
  # Load true value of betax
  DGP[[i]] <- readRDS(file.path("data", paste(scenario)))
  betax <- DGP[[i]]$cor[[1]]$betax
  
  # Compile simulation results
  sim_res[[i]] <- cr_simresults(res, strategy_vec, 
                                betax = DGP[[i]]$cor[[1]]$betax, meany = 0)
  names(sim_res)[[i]] <- scenario
  
  # Examine selected auxiliary variables
  scenario <- DGP[[i]]$name
  p <- DGP[[i]]$p
  gam_coef <- exp(DGP[[i]]$gam_coefs)
  gamma_a <- calc_gamma_a(p = p, gam = gam_coef)
  cor_a <- DGP[[i]]$cor[[2]][1, 4:(p+3)]
  aux_vars[[i]] <- prop_aux_selected(res = res, cor_a = cor_a, gamma_a = gamma_a)
  names(aux_vars)[[i]] <- scenario

  cat("DONE", "\n")
  
}

# Save results
save(sim_res, file = file.path("results", "analysis", "sim_res_all"))
save(aux_vars, file = file.path("results", "analysis", "selected_aux_vars"))

