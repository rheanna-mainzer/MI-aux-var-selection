cr_PcPlot <- function(PcAuxDat, hline, title, prop_var_print = FALSE, cum_var_print = TRUE){
  
  # cr_PcPlot
  # Create plots of principal components
  # 
  # Input:
  # PcAuxDat: Data obtained from createPcAux
  # hline: horizontal line to be drawn on cumulative proportion of variance graph
  # title: title of plot
  #
  # Written by R Mainzer, Nov 2020
  
  # Select colours
  cols <- brewer.pal(9, name = "Set1")  

  # Proportion of variance explained by principal components
  prop.var.expl <- c(PcAuxDat$rSquared$lin[1], diff(PcAuxDat$rSquared$lin))
  dat <- data.frame(PC = rep(1:length(prop.var.expl)), VarExp = prop.var.expl, CumVarExp = PcAuxDat$rSquared$lin)
  p1 <- ggplot(data = dat, aes(x = PC, y = VarExp)) + 
    geom_point(color = cols[2], size = 2) +
    labs(x = " ", y = " ",
         title = paste(title),
         subtitle = "Proportion of variance explained") +
    theme(plot.title = element_text(size = 10),
          plot.subtitle = element_text(size = 9))
  
  # Cumulative proportion of variance explained by principal components
  p2 <- ggplot(data = dat, aes(x = PC, y = CumVarExp)) + 
    geom_point(color = cols[2], size = 2) + 
    labs(x = " ", y = " ",
         subtitle = paste(title)) +
    geom_hline(yintercept = hline) +
    theme(plot.title = element_text(size = 10),
          plot.subtitle = element_text(size = 9))
  
  if(prop_var_print == TRUE & cum_var_print == FALSE){
    print(p1)
    out <- p1
  }
  
  if(prop_var_print == FALSE & cum_var_print == TRUE){
    print(p2)
    out <- p2
  }
    
  if(prop_var_print == TRUE & cum_var_print == TRUE){
    
    # Combine plots
    print(grid.arrange(p1, p2, nrow = 2))
    out <- c(p1, p2)
  }

  # Output result
  out
  
}