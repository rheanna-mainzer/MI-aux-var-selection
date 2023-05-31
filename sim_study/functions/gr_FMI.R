gr_FMI <- function(list){
  
  # n = 250, p = 25
  gg_1 <- ggplot(data = list[[1]], aes(x = Number, y = Change, color = Incl)) +
    geom_point(size = 2) +
    geom_hline(size = 1, yintercept = list[[1]]$frac[1], colour = "#619CFF") +
    scale_color_manual(values = c("black", "#F8766D")) + 
    theme(legend.position = "none") +
    labs(x = " ", 
         y = " ",
         subtitle = "n = 250, p = 25, missing = 30%, odds = 1.2") +
    theme(plot.subtitle = element_text(size = 9))
  
  # n = 250, p = 83
  gg_2 <- ggplot(data = list[[2]], aes(x = Number, y = Change, color = Incl)) +
    geom_point(size = 2) +
    geom_hline(size = 1, yintercept = list[[2]]$frac[1], colour = "#619CFF") +
    scale_color_manual(values = c("black", "#F8766D")) + 
    theme(legend.position = "none") +  
    labs(x = " ", 
         y = " ",
         subtitle = "n = 250, p = 83, missing = 30%, odds = 1.2") +
    theme(plot.subtitle = element_text(size = 9))
  
  # n = 1000, p = 100
  gg_3 <- ggplot(data = list[[3]], aes(x = Number, y = Change, color = Incl)) +
    geom_point(size = 2) +
    geom_hline(size = 1, yintercept = list[[3]]$frac[1], colour = "#619CFF") +
    scale_color_manual(values = c("black", "#F8766D")) + 
    theme(legend.position = "none") +  
    labs(x = " ", 
         y = " ",
         subtitle = "n = 1000, p = 100, missing = 30%, odds = 1.2") +
    theme(plot.subtitle = element_text(size = 9))
  
  # n = 1000, p = 333
  gg_4 <- ggplot(data = list[[4]], aes(x = Number, y = Change, color = Incl)) +
    geom_point(size = 2) +
    geom_hline(size = 1, yintercept = list[[4]]$frac[1], colour = "#619CFF") +
    scale_color_manual(values = c("black", "#F8766D")) + 
    theme(legend.position = "none") +  
    labs(x = " ", 
         y = " ",
         subtitle = "n = 1000, p = 333, missing = 30%, odds = 1.2") +
    theme(plot.subtitle = element_text(size = 9))
  
  # n = 1000, p = 100, pmy = 50, gam = 1.2
  gg_5 <- ggplot(data = list[[5]], aes(x = Number, y = Change, color = Incl)) +
    geom_point(size = 2) +
    geom_hline(size = 1, yintercept = list[[5]]$frac[1], colour = "#619CFF") +
    scale_color_manual(values = c("black", "#F8766D")) + 
    theme(legend.position = "none") +  
    labs(x = " ", 
         y = " ",
         subtitle = "n = 1000, p = 100, missing = 50%, odds = 1.2") +
    theme(plot.subtitle = element_text(size = 9))
  
  # n = 1000, p = 100, pmy = 30, gam = 2
  gg_6 <- ggplot(data = list[[6]], aes(x = Number, y = Change, color = Incl)) +
    geom_point(size = 2) +
    geom_hline(size = 1, yintercept = list[[6]]$frac[1], colour = "#619CFF") +
    scale_color_manual(values = c("black", "#F8766D")) + 
    theme(legend.position = "none") +  
    labs(x = " ", 
         y = " ",
         subtitle = "n = 1000, p = 100, missing = 30%, odds = 2") +
    theme(plot.subtitle = element_text(size = 9))
  
  # Final figure
  figure <- ggarrange(gg_1, gg_2, gg_3, gg_4, gg_5, gg_6, ncol = 2, nrow = 3)
  annotate_figure(figure, left = text_grob("Absolute change in FMI", rot = 90, 
                                           vjust = 1.5),
                  bottom = text_grob("Number of auxiliary variables included in imputation model", 
                                     vjust = -1))
  
  # Save figure
  ggsave(file.path("results", "figures", "SuppFig12.png"), 
         width = 7, height = 7, units = "in", plot = last_plot())
  ggsave(file.path("results", "figures", "SuppFig12.pdf"), 
         width = 7, height = 7, units = "in", plot = last_plot())
  
}