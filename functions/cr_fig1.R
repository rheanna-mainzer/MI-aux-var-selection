cr_fig1 <- function(dat, save = TRUE){
  
  # Create figure 1 for the paper

  # Update scenario vector
  dat$scenario_fact <- ifelse(dat$scenario == "n250_p25_pmy30_mod", "n = 250, p = 25", dat$scenario)
  dat$scenario_fact <- ifelse(dat$scenario == "n250_p83_pmy30_mod", "n = 250, p = 83", dat$scenario_fact)
  dat$scenario_fact <- ifelse(dat$scenario == "n1000_p100_pmy30_mod", "n = 1000, p = 100", dat$scenario_fact)
  dat$scenario_fact <- ifelse(dat$scenario == "n1000_p333_pmy30_mod", "n = 1000, p = 333", dat$scenario_fact)
  dat$scenario_fact <- factor(dat$scenario_fact, 
                              levels = c("n = 250, p = 25", "n = 250, p = 83",
                                         "n = 1000, p = 100", "n = 1000, p = 333"))
  
  # Update results with strategy numbers
  dat$strat_no <- ifelse(dat$strategy == "cc", 10, 0)
  dat$strat_no <- ifelse(dat$strategy == "mice-full", 9, dat$strat_no)
  dat$strat_no <- ifelse(dat$strategy == "quickpred-pt2", 8, dat$strat_no)
  dat$strat_no <- ifelse(dat$strategy == "quickpred-pt4", 7, dat$strat_no)
  dat$strat_no <- ifelse(dat$strategy == "tests", 6, dat$strat_no)
  dat$strat_no <- ifelse(dat$strategy == "pcaux", 5, dat$strat_no)
  dat$strat_no <- ifelse(dat$strategy == "forward", 4, dat$strat_no)
  dat$strat_no <- ifelse(dat$strategy == "forward-sw", 3, dat$strat_no)
  dat$strat_no <- ifelse(dat$strategy == "forward-fmi", 2, dat$strat_no)
  dat$strat_no <- ifelse(dat$strategy == "LASSO", 1, dat$strat_no)
  dat$strat_nop <- dat$strat_no + 0.2
  dat$strat_nom <- dat$strat_no - 0.2
  
  # Strategy labels
  dat$label[dat$strategy == "cc"] <- "CCA"
  dat$label[dat$strategy == "mice-full"] <- "Full"
  dat$label[dat$strategy == "quickpred-pt2"] <- "Quickpred-pt2"
  dat$label[dat$strategy == "quickpred-pt4"] <- "Quickpred-pt4"
  dat$label[dat$strategy == "tests"] <- "PredMiss"
  dat$label[dat$strategy == "pcaux"] <- "PcAux"
  dat$label[dat$strategy == "forward"] <- "Forward"
  dat$label[dat$strategy == "forward-sw"] <- "Forward-sw"
  dat$label[dat$strategy == "forward-fmi"] <- "Forward-FMI"
  dat$label[dat$strategy == "LASSO"] <- "LASSO"
  
  # mu_Y -----------------------------------------------------------------------
  
  # Facet plot - mean of y - bias
  gg_b <- ggplot(data = dat, aes(x = strat_no, y = meany_bias, ymin = meany_bias_MCll, ymax = meany_bias_MCul)) +
    geom_point() +
    geom_errorbar(width = 0.2) +
    geom_hline(yintercept = 0, linetype = 1, color = 2, size = 1) +
    scale_x_continuous(labels = dat$label, breaks = dat$strat_no) +
    theme_bw() + 
    coord_flip() +
    labs(x = " ", y = " ", title = "Bias") + 
    facet_wrap( ~ scenario_fact, ncol = 1, labeller = label_bquote(.(scenario_fact))) + 
    theme(plot.title = element_text(hjust = 0.5, size = 18),
          panel.grid.minor.y = element_blank()) 
  
  # Facet plot - mean of y - empirical SE
  gg_se <- ggplot(data = dat, aes(x = strat_no, y = meany_empSE, ymin = meany_empSE_MCll, ymax = meany_empSE_MCul)) +
    geom_point() +
    geom_errorbar(width = 0.2) +
    scale_x_continuous(labels = dat$label, breaks = dat$strat_no) +
    coord_flip() +
    theme_bw() + 
    labs(x = " ", y = " ", title = "Empirical SE") +
    facet_wrap( ~ scenario_fact, ncol = 1, labeller = label_bquote(.(scenario_fact))) + 
    theme(plot.title = element_text(hjust = 0.5, size = 18),
          panel.grid.minor.y = element_blank()) 
  
  # Combine facet plots
  print(grid.arrange(gg_b, gg_se, ncol = 2))
  g_muy <- grid.arrange(gg_b, gg_se, ncol = 2)
  g_muy
  
  # Save plot
  if(save){
    ggsave(file.path("results", "figures", "Fig1_meany.pdf"), plot = g_muy, 
           width = 20, height = 20, units = "cm")
    ggsave(file.path("results", "figures", "Fig1_meany.png"), 
           plot = g_muy, width = 20, height = 20, units = "cm")
  }
  
  # beta_x -------------------------------------------------------------
  
  # Facet plot - beta - bias
  gg_b <- ggplot(data = dat, aes(x = strat_no, y = bx_bias, ymin = bx_bias_MCll, ymax = bx_bias_MCul)) +
    geom_point() +
    geom_errorbar(width = 0.2) +
    geom_hline(yintercept = 0, linetype = 1, color = 2, size = 1) +
    scale_x_continuous(labels = dat$label, breaks = dat$strat_no) +
    coord_flip() +
    theme_bw() + 
    labs(x = " ", y = " ", title = "Bias") + 
    facet_wrap( ~ scenario_fact, ncol = 1, labeller = label_bquote(.(scenario_fact))) +
    theme(plot.title = element_text(hjust = 0.5, size = 18),
          panel.grid.minor.y = element_blank())
  
  # Facet plot - beta- empirical SE
  gg_se <- ggplot(data = dat, aes(x = strat_nop, y = bx_empSE, ymin = bx_empSE_MCll, ymax = bx_empSE_MCul)) +
    geom_point() +
    geom_errorbar(width = 0.2) +
    scale_x_continuous(labels = dat$label, breaks = dat$strat_no) +
    theme(panel.grid.minor.y = element_blank()) +
    coord_flip() +
    theme_bw() + 
    labs(x = " ", y = " ", title = "Empirical SE") +
    facet_wrap( ~ scenario_fact, ncol = 1, labeller = label_bquote(.(scenario_fact))) +
    theme(plot.title = element_text(hjust = 0.5, size = 18),
          panel.grid.minor.y = element_blank())
  
  # Combine facet plots
  print(grid.arrange(gg_b, gg_se, ncol = 2))
  g_beta <- grid.arrange(gg_b, gg_se, ncol = 2)
  g_beta
  
  # Save plot
  if(save){
    ggsave(file.path("results", "figures", "Fig2_betax.pdf"), 
           plot = g_beta, width = 20, height = 20, units = "cm")
    ggsave(file.path("results", "figures", "Fig2_betax.png"), 
           plot = g_beta, width = 20, height = 20, units = "cm")
  }
  
}
