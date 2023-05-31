tab_simresults <- function(dat, tab_name){
  
  # Create tables of results for the simulation study
  # tab_simresults
  #
  # Input: 
  # dat: data frame with results from all scenarios
  # tab_name: name of file created that contains table of results
  #
  # Written by R Mainzer, August 2021
  
  # Select columns
  dat <- dat[, c("scenario", "strategy", "bx_bias", "bx_empSE", 
                 "bx_relbias", "bx_zbias", "bx_avmodSE", "bx_relerr", 
                 "bx_covprob", "meany_bias", "meany_empSE", 
                 "meany_zbias", "meany_avmodSE", "meany_relerr", "meany_covprob")]
  
  # Round columns
  dat[, c("bx_bias")] <- round(dat[, c("bx_bias")], 3)
  dat[, c("bx_empSE")] <- round(dat[, c("bx_empSE")], 3)
  dat[, c("bx_relbias")] <- signif(dat[, c("bx_relbias"), 3])
  dat[, c("bx_zbias")] <- signif(dat[, c("bx_zbias"), 3])
  dat[, c("bx_avmodSE")] <- round(dat[, c("bx_avmodSE")], 3)
  dat[, c("bx_relerr")] <- round(dat[, c("bx_relerr")], 1)
  dat[, c("bx_covprob")] <- signif(dat[, c("bx_covprob")] * 100, 2)
  
  dat[, c("meany_bias")] <- round(dat[, c("meany_bias")], 3)
  dat[, c("meany_empSE")] <- round(dat[, c("meany_empSE")], 3)
  dat[, c("meany_zbias")] <- signif(dat[, c("meany_zbias"), 3])
  dat[, c("meany_avmodSE")] <- round(dat[, c("meany_avmodSE")], 3)
  dat[, c("meany_relerr")] <- round(dat[, c("meany_relerr")], 1)
  dat[, c("meany_covprob")] <- signif(dat[, c("meany_covprob")] * 100, 2)
  
  # Create table of results with spaces in between 
  space <- data.frame("bx_bias" = "", "bx_empSE" = "", 
                      "bx_relbias" = "", "bx_zbias" = "", "bx_avmodSE" = "", 
                      "bx_relerr" = "", "bx_covprob" = "",
                      "meany_bias" = "", "meany_empSE" = "", "meany_zbias" = "",
                      "meany_avmodSE" = "", "meany_relerr" = "", "meany_covprob" = "")
  colspace <- data.frame("Delete" = rep(" ", nrow(dat)))
  
  # Rename strategies
  dat$strategy <- gsub("cc", "CCA", dat$strategy)
  dat$strategy <- gsub("mice-full", "Full", dat$strategy)
  dat$strategy <- gsub("quickpred-pt2", "Quickpred-pt2", dat$strategy)
  dat$strategy <- gsub("quickpred-pt4", "Quickpred-pt4", dat$strategy)
  dat$strategy <- gsub("pcaux", "PcAux", dat$strategy)
  dat$strategy <- gsub("forward", "Forward", dat$strategy)
  dat$strategy <- gsub("Forward-fmi", "Forward-FMI", dat$strategy)
  dat$strategy <- gsub("tests", "PredMiss", dat$strategy)
  
  # Order according to scenario and strategies
  dat$strategy <- factor(dat$strategy, 
                             levels = c("CCA", "Full", "Quickpred-pt2", 
                                        "Quickpred-pt4", "PredMiss", "PcAux",
                                        "Forward", "Forward-sw", "Forward-FMI",
                                        "LASSO"))
  dat <- with(dat, dat[order(scenario, strategy),])
  
  # Data for the table
  tab_dat <- cbind(dat[, 2:9], colspace, dat[10:15])
  
  # Column headings for result tables
  col_header_l1 <- c(paste0(" & ", 
                            "\\multicolumn{7}{c}{\\textbf{$\\beta_{X}$}}",
                            " &  & ",
                            "\\multicolumn{6}{c}{\\textbf{$\\mu_{Y}$}}",
                            "\\\\", "\\cline{2-8} \\cline{10-15}"))
  col_header_l2 <- c(paste0("\\textbf{Strategy}"),
                     paste0("\\textbf{Bias}"),
                     paste0("\\textbf{EmpSE}"),
                     paste0("\\textbf{RelBias}"),
                     paste0("\\textbf{StdBias}"),
                     paste0("\\textbf{ModSE}"),
                     paste0("\\textbf{RelSE}"),
                     paste0("\\textbf{Cov}"),
                     paste0(" "),
                     paste0("\\textbf{Bias}"),
                     paste0("\\textbf{EmpSE}"),
                     paste0("\\textbf{StdBias}"),
                     paste0("\\textbf{ModSE}"),
                     paste0("\\textbf{RelSE}"),
                     paste0("\\textbf{Cov}"))
  colmns <- paste0("\\multicolumn{1}{l}{", col_header_l2, "}", collapse = " & ")
  colmns <- paste0(col_header_l1, colmns)
  colmns <- paste0(colmns, "\\\\ \\hline")
  
  # Insert rows with strategy names
  np_rws <- c(0, 10)
  if(dat$scenario[1] == "n250_p25_pmy30_mod"){r1 <- "$n = 250$, $p = 25$, missing = 30\\%, odds = 1.2"}
  if(dat$scenario[11] == "n250_p83_pmy30_mod"){r11 <- "$n = 250$, $p = 83$, missing = 30\\%, odds = 1.2"}
  if(dat$scenario[1] == "n1000_p100_pmy30_mod"){r1 <- "$n = 1000$, $p = 100$, missing = 30\\%, odds = 1.2"}
  if(dat$scenario[11] == "n1000_p333_pmy30_mod"){r11 <- "$n = 1000$, $p = 333$, missing = 30\\%, odds = 1.2"}
  if(dat$scenario[1] == "n1000_p100_pmy30_str"){
    dat$scenario <- factor(dat$scenario, levels = c("n1000_p100_pmy50_mod", "n1000_p100_pmy30_str"))
    dat <- with(dat, dat[order(scenario, strategy),])
  }
  if(dat$scenario[1] == "n1000_p100_pmy50_mod"){r1 <- "$n = 1000$, $p = 100$, missing = 50\\%, odds = 1.2"}
  if(dat$scenario[11] == "n1000_p100_pmy30_str"){r11 <- "$n = 1000$, $p = 100$, missing = 30\\%, odds = 2"}
  np_command <- c(paste("\\multicolumn{12}{l}{ ", r1, "} \\\\", sep = ""), 
                  paste("\\multicolumn{12}{l}{ ", r11, "} \\\\", sep = ""))
  
  # Create LaTeX table
  print(xtable(tab_dat, display= c("s", "s", rep("f", 7), "s", rep("f", 6)),
               caption = paste("Bias, empirical standard error (EmpSE), 
                               relative bias (RelBias), standardised bias (StdBias), 
                               average model standard error (ModSE), relative error 
                               in the model standard error (RelSE) and 95\\% confidence 
                               interval coverage (Cov) for $\\beta_X$ and $\\mu_Y$, 
                               for each analysis strategy and the simulation scenarios
                               where ", r1, " and ", r11,". 
                               MI estimates were successfully obtained for all simulated 
                               datasets (convergence = 1 for all strategies and scenarios).", sep = ""),
               digits = c(1, 1, 3, 3, 1, 1, 3, 1, 0, 2, 3, 3, 1, 3, 1, 0)),
        include.rownames = FALSE, 
        include.colnames = FALSE,
        add.to.row = list(pos = as.list(c(0, np_rws)), 
                          command = c(colmns, np_command)),
        hline.after = c(-1, nrow(tab_dat)),
        file = file.path("results", "tables", paste(tab_name)))
}
  