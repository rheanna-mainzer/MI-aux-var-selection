# Create figure for Forward-FMI strategy
# 04_gr_FMI

# Supplementary Figure 10: Graphs for the Forward-FMI strategy

# Load packages
source(file.path("..", "packages.R"), echo = TRUE)

# Load functions 
source(file.path("functions", "gr_FMI.R"), echo = TRUE)

# ------------------------------------------------------------------------------

# Create supplementary figure 12
FMI_data <- readRDS(file = file.path("results", "analysis", "FMI_data"))
gr_FMI(FMI_data)

