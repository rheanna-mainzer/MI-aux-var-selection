# Produce figures and tables of results for paper
# 08_gr_tab_simresults

# Load packages
source(file.path("..", "packages.R"), echo = TRUE)

# Load functions
source(file.path("functions", "tab_simresults.R"), echo = TRUE)
source(file.path("functions", "cr_fig1.R"), echo = TRUE)
source(file.path("functions", "gr_prop_aux_selected_bygroup.R"), echo = TRUE)
source(file.path("functions", "gr_prop_aux_selected_total.R"), echo = TRUE)

# Create tables and figures ----------------------------------------------------

load(file.path("results", "analysis", "sim_res_all"))
load(file.path("results", "analysis", "selected_aux_vars"))

# Create Figures 1 and 2
fig1_res <- rbind(sim_res$n250_p25_pmy30_mod,
                  sim_res$n250_p83_pmy30_mod,
                  sim_res$n1000_p100_pmy30_mod,
                  sim_res$n1000_p333_pmy30_mod)
cr_fig1(fig1_res, save = TRUE)

# Create Supplementary Table 2
tab_simresults(rbind(sim_res$n250_p25_pmy30_mod, 
                     sim_res$n250_p83_pmy30_mod),
               tab_name = "SuppTable2")

# Create Supplementary Table 3
tab_simresults(rbind(sim_res$n1000_p100_pmy30_mod, 
                     sim_res$n1000_p333_pmy30_mod),
               tab_name = "SuppTable3")

# Create Supplementary Table 4
tab_simresults(rbind(sim_res$n1000_p100_pmy50_mod, 
                     sim_res$n1000_p100_pmy30_str),
               tab_name = "SuppTable4")

# Create Figure 3
gr_prop_aux_selected_total(
  dat = rbind(cbind(scenario = "n250_p25_pmy30_mod", aux_vars$n250_p25_pmy30_mod[[3]][1:7, c("strat", "total_n", "total_p")]),
              cbind(scenario = "n250_p83_pmy30_mod", aux_vars$n250_p83_pmy30_mod[[3]][1:7, c("strat", "total_n", "total_p")]),
              cbind(scenario = "n1000_p100_pmy30_mod", aux_vars$n1000_p100_pmy30_mod[[3]][1:7, c("strat", "total_n", "total_p")]),
              cbind(scenario = "n1000_p333_pmy30_mod", aux_vars$n1000_p333_pmy30_mod[[3]][1:7, c("strat", "total_n", "total_p")])),
  save = TRUE)

# Create Supplementary Figures 13 to 18
for(i in 1:6){
  
  gr_prop_aux_selected_bygroup(dat = select(aux_vars[[i]][[3]][1:7,], c("strat", starts_with("g") & ends_with("p"))), 
                               n_grp = aux_vars[[i]][[2]],
                               scenario = paste(i+12, "_", names(aux_vars)[i], sep = ""))
  
}

# ------------------------------------------------------------------------------

# Explore results

## mean ------------

# bias
lapply(sim_res, function(x){cbind(x[, c("strategy", "meany_bias")])})

# empSE
lapply(sim_res, function(x){cbind(x[, c("strategy", "meany_empSE")], 
                                  check = x$meany_empSE < x[1, c("meany_empSE")])})

# relSE
lapply(sim_res, function(x){cbind(x[, c("strategy", "meany_relerr")], 
                                  check = x$meany_relerr < x[1, c("meany_relerr")])})

# cov
lapply(sim_res, function(x){cbind(x[, c("strategy", "meany_covprob")])})

## beta ----------

# bias
lapply(sim_res, function(x){cbind(x[, c("strategy", "bx_bias")])})

# empSE
lapply(sim_res, function(x){cbind(x[, c("strategy", "bx_empSE")], 
                                  check = x$bx_empSE < x[1, c("bx_empSE")])})

# relSE
lapply(sim_res, function(x){cbind(x[, c("strategy", "bx_relerr")], 
                                  check = abs(x$bx_relerr) < 3.3)})

