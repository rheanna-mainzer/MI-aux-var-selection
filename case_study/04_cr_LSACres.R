# Compile LSAC results
# 04_cr_LSACres
# 
# This script will compile the LSAC case study results and produce graphs for 
# the paper
#
# Written by R Mainzer, September 2021

# Load packages
source(file.path("..", "packages.R"), echo = TRUE)

# ------------------------------------------------------------------------------

# Load R data 
load(file.path("results", "LSAC_results_R.Rda"))

# Inspect results
res_out

# Time taken - convert to minutes
res_out$res$time_selec <- round(res_out$res$time_selec  / 60, 2)
res_out$res$time_mi <- round(res_out$res$time_mi / 60, 2)
res_out$res$time_tot <- round(res_out$res$time_tot / 60, 2)

# Check logged events
imp_full$loggedEvents
imp_qp2$loggedEvents
imp_qp4$loggedEvents
imp_PcAux$loggedEvents
imp_tests$loggedEvents
imp_lasso$loggedEvents

# Check predictor matrices and methods, and store names and number of 
# auxiliary variables
imp_full$pred
imp_full$meth
res_out$res$aux <- ifelse(res_out$res$strategy == "full", 
                          paste(names(which(imp_full$pred["PedsQL_tot_w4",] == 1)), collapse = " "),
                          NA)
res_out$res$n_aux <- ifelse(res_out$res$strategy == "full", 
                            sum(imp_full$pred["PedsQL_tot_w4",] == 1) - 6, NA)

imp_qp2$pred
imp_qp2$meth
res_out$res$aux <- ifelse(res_out$res$strategy == "quickpred-pt2",
                          paste(names(which(imp_qp2$pred["PedsQL_tot_w4",] == 1)), collapse = " "),
                          res_out$res$aux)
res_out$res$n_aux <- ifelse(res_out$res$strategy == "quickpred-pt2", 
                            sum(imp_qp2$pred["PedsQL_tot_w4", ] == 1) - 6, 
                            res_out$res$n_aux)

imp_qp4$pred
imp_qp4$meth
res_out$res$aux <- ifelse(res_out$res$strategy == "quickpred-pt4",
                          paste(names(which(imp_qp4$pred["PedsQL_tot_w4",] == 1)), collapse = " "),
                          res_out$res$aux)
res_out$res$n_aux <- ifelse(res_out$res$strategy == "quickpred-pt4",
                            sum(imp_qp4$pred["PedsQL_tot_w4", ] == 1) - 6,
                            res_out$res$n_aux)

imp_PcAux$pred
imp_PcAux$meth
res_out$res$aux <- ifelse(res_out$res$strategy == "PcAux", 
                          paste(names(which(imp_PcAux$pred["PedsQL_tot_w4",] == 1)), collapse = " "),
                          res_out$res$aux)
res_out$res$n_aux <- ifelse(res_out$res$strategy == "PcAux",
                            sum(imp_PcAux$pred["PedsQL_tot_w4", ] == 1) - 6,
                            res_out$res$n_aux)

imp_tests$pred
imp_tests$meth
res_out$res$aux <- ifelse(res_out$res$strategy == "tests", 
                          paste(names(which(imp_tests$pred["PedsQL_tot_w4",] == 1)), collapse = " "),
                          res_out$res$aux)
res_out$res$n_aux <- ifelse(res_out$res$strategy == "tests",
                            sum(imp_tests$pred["PedsQL_tot_w4",] == 1) - 6,
                            res_out$res$n_aux)

imp_lasso$pred
imp_lasso$meth
res_out$res$aux <- ifelse(res_out$res$strategy == "LASSO", 
                          paste(names(which(imp_lasso$pred["PedsQL_tot_w4",] == 1)), collapse = " "),
                          res_out$res$aux)
res_out$res$n_aux <- ifelse(res_out$res$strategy == "LASSO",
                            sum(imp_lasso$pred["PedsQL_tot_w4",] == 1) - 6,
                            res_out$res$n_aux)

# Remove exposure and covariates 
res_out$res$aux <- gsub(pattern = "bmiz_w1 female age_w1 IndStat NonEng sep_w1 ", 
                    replacement = "",
                    x = res_out$res$aux)

# Range of betax estimates
min(res_out$res$betax)
max(res_out$res$betax)
mean(res_out$res$betax)

# Range of meany estimates
min(res_out$res$muy)
max(res_out$res$muy)

# Label strategies
res_out$res$label[res_out$res$strategy == "CCA"] <- "CCA"
res_out$res$label[res_out$res$strategy == "full"] <- "Full"
res_out$res$label[res_out$res$strategy == "quickpred-pt2"] <- "Quickpred-pt2"
res_out$res$label[res_out$res$strategy == "quickpred-pt4"] <- "Quickpred-pt4"
res_out$res$label[res_out$res$strategy == "PcAux"] <- "PcAux"
res_out$res$label[res_out$res$strategy == "tests"] <- "PredMiss"
res_out$res$label[res_out$res$strategy == "LASSO"] <- "LASSO"

# Create Table 1 ---------------------------------------------------------------

tab1_dat <- res_out$res[, c("label", "aux", "n_aux", "time_selec", "time_mi", "time_tot")]
tab1_dat$order <- c(1, 2, 6, 3, 4, 5, 7)
tab1_dat <- dplyr::arrange(tab1_dat, tab1_dat$order)
tab1_dat <- subset(tab1_dat, select = -c(order))

print(xtable(tab1_dat, align = c("c", "c", "p{8cm}", rep("c", 4)),
             display = c("s", "s", "s", "d", rep("fg", 3))),
      include.rownames = FALSE,
      size= "\\fontsize{9pt}{10pt}\\selectfont",
      file = file.path("results", "Table1"))

# Create Figure 4 --------------------------------------------------------------

# Order strategies
res_out$res$label <- factor(res_out$res$label, order = TRUE, 
                            levels = c("LASSO", "PcAux", "PredMiss",
                                       "Quickpred-pt4", "Quickpred-pt2",
                                       "Full", "CCA"))

# Mean HRQoL
gg1 <- ggplot(data = res_out$res, aes(x = label, y = muy, ymin = muy_ll, ymax = muy_ul)) + 
  geom_point() + 
  geom_errorbar(width = 0.2) + 
  coord_flip() + 
  theme_bw() +
  labs(x = " ", y = "Estimate", title = "Mean HRQoL") +
  theme(plot.title = element_text(hjust = 0.5, size = 14))

# Regression coefficient
gg2 <- ggplot(data = res_out$res, aes(x = label, y = betax, ymin = betax_ll, ymax = betax_ul)) + 
  geom_point() + 
  geom_errorbar(width = 0.2) + 
  coord_flip() + 
  theme_bw() +
  labs(x = " ", y = "Estimate", title = "Effect of BMIz on HRQoL") + 
  theme(plot.title = element_text(hjust = 0.5, size = 14))

# Save figure
gg <- grid.arrange(gg1, gg2, ncol = 2)
ggsave(file.path("results", "fig_LSAC.pdf"), gg, 
       width = 7.53, height = 3.47, units = "in")
ggsave(file.path("results", "fig_LSAC.png"), gg, 
       width = 7.53, height = 3.47, units = "in")
