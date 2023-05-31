# Analyse simulated data sets for each scenario using the "an_simdata" function
# 05_an_SimData

# Load packages
source(file.path("..", "packages.R"), echo = TRUE)

# Load functions
source(file.path("functions", "pred_ttest.R"), echo = TRUE)
source(file.path("functions", "calc_FMI.R"), echo = TRUE)
source(file.path("functions", "select_aux_forwardFMI_simstudy.R"), echo = TRUE)
source(file.path("functions", "an_simdata.R"), echo = TRUE)

#  ----------------------------------------------------------------------------

# Get argument from command line
args <- commandArgs(trailingOnly = TRUE)
N <- as.numeric(args[1]) # Batch number
scenario_vec <- c("n250_p25_pmy30_mod", "n250_p83_pmy30_mod", "n1000_p100_pmy30_mod",
                  "n1000_p333_pmy30_mod", "n1000_p100_pmy50_mod", "n1000_p100_pmy30_str")
k <- as.numeric(args[2]) # Scenario number
scenario <- scenario_vec[k]
cat("N = ", N, ", k = ", k, "\n")

# Set RNG using an adaption of the method available here: 
# https://thestatsgeek.com/2019/06/21/setting-seeds-when-running-r-simulations-in-parallel/
# Different starting seed used for each batch and scenario
RNGkind("L'Ecuyer-CMRG")
set.seed(375641)
batches <- 200
s <- .Random.seed
for(i in 1:((k - 1) * batches + N)){
  cat(i, "\n")
  s <- nextRNGStream(s)
  cat(s, "\n")
  # Send s to worker i as .Random.seed
}
.GlobalEnv$Random.seed <- s

# Other input
strategy_vec <- c("cc", "mice-full", "quickpred-pt2", "quickpred-pt4",
                  "pcaux", "forward-fmi", "tests", "LASSO")
m <- 30
maxit <- 10

# Do analysis
res <- an_simdata(scenario, N, strategy_vec, m, maxit)
write.table(res, file = file.path("results", "analysis", paste("R_", scenario, "_batch", N, ".csv", sep="")))

