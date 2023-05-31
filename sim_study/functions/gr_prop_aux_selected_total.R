gr_prop_aux_selected_total <- function(dat, save = TRUE){
  
  # This function will graph the selected variables for each analysis method
  # in total for each scenario
  #
  # Input
  # dat: data frame containing total proportion of auxiliary variables selected
  # for each scenario and strategy
  # save: save plot? TRUE/FALSE
  #
  # Written by R Mainzer, October 2022
  
  # Rename tests strategy
  dat$strat <- ifelse(dat$strat == "Tests", "PredMiss", dat$strat)
  
  # Order strategies and scenarios
  dat$strat <- factor(dat$strat, levels = c("LASSO", "Forward-FMI", "Forward-sw",
                                            "Forward", "PredMiss", "Quickpred-pt4", 
                                            "Quickpred-pt2"))
  dat$scenario <- factor(dat$scenario, levels = c("n250_p25_pmy30_mod", "n1000_p100_pmy30_mod", 
                                                  "n250_p83_pmy30_mod", "n1000_p333_pmy30_mod"),
                         labels = c(bquote("n = 250, p = 25"), 
                                    bquote("n = 1000, p = 100"),
                                    bquote("n = 250, p = 83"), 
                                    bquote("n = 1000, p = 333")))
  
  # Create graph
  gg <- ggplot(dat, aes(total_p, strat)) + 
    geom_col() +
    facet_wrap(vars(scenario), ncol = 2) +
    labs(y = "", x = "Average % of variables selected") +
    theme_bw() +
    theme(legend.position = "none",
          plot.margin = margin(10, 10, 10, 10, unit = "pt"))
  print(gg)
  
  # Save graph
  if(save == TRUE){
    
    ggsave(file.path("results", "figures", "Fig3_TotalAuxVars.pdf"), 
           plot = gg, width = 20, height = 15, units = "cm")
    ggsave(file.path("results", "figures", "Fig3_TotalAuxVars.png"), 
           plot = gg, width = 20, height = 15, units = "cm")
    
  }
  
}
