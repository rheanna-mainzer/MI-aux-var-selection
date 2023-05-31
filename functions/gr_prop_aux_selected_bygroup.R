gr_prop_aux_selected_bygroup <- function(dat, n_grp, scenario, save = TRUE){
  
  # This function will graph the selected variables for each analysis method, 
  # by group and scenario
  #
  # Input
  # dat: data frame 
  # n_grp: number of variables in each group
  # scenario: name of scenario (used for saving file)
  # save: save plot? TRUE/FALSE
  #
  # Written by R Mainzer, September 2022
  
  # Rename tests strategy
  dat$strat <- ifelse(dat$strat == "Tests", "PredMiss", dat$strat)
  
  # Change to long form and add group labels
  dat <- pivot_longer(dat, cols = ends_with("p"), names_to = "grp")
  dat$grp <- factor(dat$grp, levels = c("grp1_p", "grp3_p", "grp5_p", "grp7_p",
                                        "grp2_p", "grp4_p", "grp6_p", "grp8_p"),
                    labels = c(bquote("[0, 0.1)," ~ gamma != ~ "0, n =" ~ .(n_grp[1])), 
                               bquote("[0.1, 0.2)," ~ gamma != ~ "0, n =" ~ .(n_grp[3])),
                               bquote("[0.2, 0.4)," ~ gamma != ~ "0, n =" ~ .(n_grp[5])),
                               bquote("[0.4, 1]," ~ gamma != ~ "0, n =" ~ .(n_grp[7])),
                               bquote("[0, 0.1)," ~ gamma ~ "= 0, n =" ~ .(n_grp[2])),
                               bquote("[0.1, 0.2)," ~ gamma ~ "= 0, n =" ~ .(n_grp[4])),
                               bquote("[0.2, 0.4)," ~ gamma ~ "= 0, n =" ~ .(n_grp[6])),
                               bquote("[0.4, 1]," ~ gamma ~ "= 0, n =" ~ .(n_grp[8]))))
  
  # Add strategy order
  dat$strat <- factor(dat$strat, levels = c("Quickpred-pt2", "Quickpred-pt4", "PredMiss", 
                                            "Forward", "Forward-sw", "Forward-FMI", "LASSO"))
  
  # Graph
  gg <- ggplot(dat, aes(strat, value, fill = strat)) + 
    geom_col() + 
    facet_wrap(vars(grp), ncol = 4, labeller = label_parsed) +
    scale_fill_discrete(name = " ") +
    labs(y = "Average % of variables selected", x = "Strategy") +
    theme(legend.position = "bottom",
          legend.direction = "horizontal",
          legend.margin = margin(t = -0.1, r = 0, b = 0, l = 0, unit = "cm"),
          axis.text.x = element_blank(),
          axis.ticks = element_blank(),
          plot.margin = margin(10, 10, 10, 10, unit = "pt")) +
    guides(fill = guide_legend(byrow = TRUE, nrow = 1))

  
  print(gg)
  
  # Save graph
  if(save == TRUE){
    
    ggsave(file.path("results", "figures", paste("SuppFig", scenario, ".pdf", sep = "")), 
           plot = gg, width = 20, height = 12, units = "cm")
    ggsave(file.path("results", "figures", paste("SuppFig", scenario, ".png", sep = "")), 
           plot = gg, width = 20, height = 12, units = "cm")
    
  }
   
}
