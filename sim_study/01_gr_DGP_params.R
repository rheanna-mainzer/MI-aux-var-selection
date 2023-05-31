# Create graphs illustrating the data generation procedure
# 01_gr_DGP_params
#
# Supplementary Figure 3: Correlation matrix for p = 25
# Supplementary Figure 4: Correlation matrix for p = 83
# Supplementary Figure 5: Method used to calculate gamma_0
# Supplementary Figure 6: Graphs of data for scenario = n250_p25_pmy30_mod
# Supplementary Figure 7: Graphs of data for scenario = n250_p83_pmy30_mod
# Supplementary Figure 8: Graphs of data for scenario = n1000_p100_pmy30_mod
# Supplementary Figure 9: Graphs of data for scenario = n1000_p333_pmy30_mod

# Load packages
source(file.path("..", "packages.R"), echo = TRUE)

# Load functions
source(file.path("functions", "cr_suppfig5.R"), echo = TRUE)
source(file.path("functions", "check_DGP.R"), echo = TRUE)
source(file.path("functions", "cr_corrmat.R"), echo = TRUE)
source(file.path("functions", "cr_completedata.R"), echo = TRUE)
source(file.path("functions", "calc_gamma_a.R"), echo = TRUE)

# ------------------------------------------------------------------------------

# Vector of scenario names
scenario_vec <- c("n250_p25_pmy30_mod", "n250_p83_pmy30_mod", 
                  "n1000_p100_pmy30_mod", "n1000_p333_pmy30_mod", 
                  "n1000_p100_pmy50_mod", "n1000_p100_pmy30_str")

# Read in scenario parameters
scenarios <- list()
for(i in 1:6){
  scenarios[[i]] <- readRDS(file.path("data", scenario_vec[i]))
}

# Create supplementary figure 3
gr_corrmat(cor_mat = scenarios[[1]]$cor[[2]], p = scenarios[[1]]$p, 
           print.labs = TRUE, print.cor = FALSE, save.fig = TRUE, 
           name = "suppfig3")

# Create supplementary figure 4
gr_corrmat(cor_mat = scenarios[[2]]$cor[[2]], p = scenarios[[2]]$p, print.labs = FALSE, 
           print.cor = FALSE, save.fig = TRUE, name = "suppfig4")

# Create supplementary figure 5
cr_suppfig5(gam0_p25 = scenarios[[1]]$gam0, 
            gam0_p83 = scenarios[[2]]$gam0, 
            gam0_p100 = scenarios[[3]]$gam0, 
            gam0_p333 = scenarios[[4]]$gam0,
            gam0_pmy50 = scenarios[[5]]$gam0,
            gam0_odds2 = scenarios[[6]]$gam0)

# Create supplementary figures 6 - 11
fig <- 6
for(i in 1:6){
  cat("scenario = ", scenarios[[i]]$name, "\n")
  dat <- cr_completedata(n = scenarios[[i]]$n, scen_params = scenarios[[i]])
  check_DGP(dat, graph = TRUE, save = TRUE, 
            name = paste("suppfig", fig, "_", scenarios[[i]]$name, sep =""))
  cat("\n")
  fig <- fig + 1
}
