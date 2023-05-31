## Create graph for supplementary material

cr_suppfig5 <- function(gam0_p25, gam0_p83, gam0_p100, gam0_p333, gam0_pmy50, gam0_odds2){
  
  # p = 25
  dat25 <- gam0_p25$details
  gam025 <- gam0_p25$gamma0
  xlim25 <- c(min(dat25$gamma_0vec), max(dat25$gamma_0vec))
  ylim25 <- c(min(dat25$res), max(dat25$res))
  
  gg1 <- ggplot(data = dat25, aes(x = gamma_0vec, y = res)) +
    geom_point(size = 2) +
    geom_line(aes(y = pred), colour = "#619CFF", size = 1) + 
    geom_segment(aes(x = gam025, y = min(res), xend = gam025, yend = 0.3), colour = "#F8766D", 
                 size = 1, linetype = 2, inherit.aes = FALSE) + 
    geom_segment(aes(x = xlim25[1], y = 0.3, xend = gam025, yend = 0.3), colour = "#F8766D", 
                 size = 1, linetype = 2, inherit.aes = FALSE) +
    coord_cartesian(xlim = xlim25, ylim = ylim25) +
    labs(x = " ", y = " ", subtitle = "n = 250, p = 25, missing = 30%, odds = 1.2") + 
    annotate("text", x = xlim25[2], y = ylim25[1], label = list(bquote(gamma[0] == .(round(gam025, 2)))), 
             parse = TRUE, hjust = 1, vjust = 0) + 
    theme(plot.subtitle = element_text(size = 10))
  
  # p = 83
  dat83 <- gam0_p83$details
  gam083 <- gam0_p83$gamma0
  xlim83 <- c(min(dat83$gamma_0vec), max(dat83$gamma_0vec))
  ylim83 <- c(min(dat83$res), max(dat83$res))
  
  gg2 <- ggplot(data = dat83, aes(x = gamma_0vec, y = res)) +
    geom_point(size = 2) +
    geom_line(aes(y = pred), colour = "#619CFF", size = 1) + 
    geom_segment(aes(x = gam083, y = min(res), xend = gam083, yend = 0.3), colour = "#F8766D", 
                 size = 1, linetype = 2, inherit.aes = FALSE) + 
    geom_segment(aes(x = xlim83[1], y = 0.3, xend = gam083, yend = 0.3), colour = "#F8766D", 
                 size = 1, linetype = 2, inherit.aes = FALSE) +
    coord_cartesian(xlim = xlim83, ylim = ylim83) +
    labs(x = " ", 
         y = " ",
         subtitle = "n = 250, p = 83, missing = 30%, odds = 1.2") +
    annotate("text", x = xlim83[2], y = ylim83[1], label = list(bquote(gamma[0] == .(round(gam083, 2)))), 
             parse = TRUE, hjust = 1, vjust = 0) + 
    theme(plot.subtitle = element_text(size = 10))
  
  # p = 100
  dat100 <- gam0_p100$details
  gam0100 <- gam0_p100$gamma0
  xlim100 <- c(min(dat100$gamma_0vec), max(dat100$gamma_0vec))
  ylim100 <- c(min(dat100$res), max(dat100$res))
  
  gg3 <- ggplot(data = dat100, aes(x = gamma_0vec, y = res)) +
    geom_point(size = 2) +
    geom_line(aes(y = pred), colour = "#619CFF", size = 1) + 
    geom_segment(aes(x = gam0100, y = min(res), xend = gam0100, yend = 0.3), colour = "#F8766D", 
                 size = 1, linetype = 2, inherit.aes = FALSE) + 
    geom_segment(aes(x = xlim100[1], y = 0.3, xend = gam0100, yend = 0.3), colour = "#F8766D", 
                 size = 1, linetype = 2, inherit.aes = FALSE) +
    coord_cartesian(xlim = xlim100, ylim = ylim100) +
    labs(x = " ", 
         y = " ",
         subtitle = "n = 1000, p = 100, missing = 30%, odds = 1.2") +
    annotate("text", x = xlim100[2], y = ylim100[1], label = list(bquote(gamma[0] == .(round(gam0100, 2)))), 
             parse = TRUE, hjust = 1, vjust = 0) + 
    theme(plot.subtitle = element_text(size = 10))
  
  # p = 333
  dat333 <- gam0_p333$details
  gam0333 <- gam0_p333$gamma0
  xlim333 <- c(min(dat333$gamma_0vec), max(dat333$gamma_0vec))
  ylim333 <- c(min(dat333$res), max(dat333$res))
  
  gg4 <- ggplot(data = dat333, aes(x = gamma_0vec, y = res)) +
    geom_point(size = 2) +
    geom_line(aes(y = pred), colour = "#619CFF", size = 1) + 
    geom_segment(aes(x = gam0333, y = min(res), xend = gam0333, yend = 0.3), colour = "#F8766D", 
                 size = 1, linetype = 2, inherit.aes = FALSE) + 
    geom_segment(aes(x = xlim333[1], y = 0.3, xend = gam0333, yend = 0.3), colour = "#F8766D", 
                 size = 1, linetype = 2, inherit.aes = FALSE) +
    coord_cartesian(xlim = xlim333, ylim = ylim333) +
    labs(x = " ", 
         y = " ",
         subtitle = "n = 1000, p = 333, missing = 30%, odds = 1.2") +
    annotate("text", x = xlim333[2], y = ylim333[1], label = list(bquote(gamma[0] == .(round(gam0333, 2)))), 
             parse = TRUE, hjust = 1, vjust = 0) + 
    theme(plot.subtitle = element_text(size = 10))
  
  # 50% missing
  datpmy50 <- gam0_pmy50$details
  gam0pmy50 <- gam0_pmy50$gamma0
  xlimpmy50 <- c(min(datpmy50$gamma_0vec), max(datpmy50$gamma_0vec))
  ylimpmy50 <- c(min(datpmy50$res), max(datpmy50$res))
  
  gg5 <- ggplot(data = datpmy50, aes(x = gamma_0vec, y = res)) +
    geom_point(size = 2) +
    geom_line(aes(y = pred), colour = "#619CFF", size = 1) + 
    geom_segment(aes(x = gam0pmy50, y = min(res), xend = gam0pmy50, yend = 0.5), colour = "#F8766D", 
                 size = 1, linetype = 2, inherit.aes = FALSE) + 
    geom_segment(aes(x = xlimpmy50[1], y = 0.5, xend = gam0pmy50, yend = 0.5), colour = "#F8766D", 
                 size = 1, linetype = 2, inherit.aes = FALSE) +
    coord_cartesian(xlim = xlimpmy50, ylim = ylimpmy50) +
    labs(x = " ", 
         y = " ",
         subtitle = "n = 1000, p = 333, missing = 50%, odds = 1.2") +
    annotate("text", x = xlimpmy50[2], y = ylimpmy50[1], label = list(bquote(gamma[0] == .(round(gam0pmy50, 2)))), 
             parse = TRUE, hjust = 1, vjust = 0) + 
    theme(plot.subtitle = element_text(size = 10))
  
  # Odds = 2
  datodds2 <- gam0_odds2$details
  gam0odds2 <- gam0_odds2$gamma0
  xlimodds2 <- c(min(datodds2$gamma_0vec), max(datodds2$gamma_0vec))
  ylimodds2 <- c(min(datodds2$res), max(datodds2$res))
  
  gg6 <- ggplot(data = datodds2, aes(x = gamma_0vec, y = res)) +
    geom_point(size = 2) +
    geom_line(aes(y = pred), colour = "#619CFF", size = 1) + 
    geom_segment(aes(x = gam0odds2, y = min(res), xend = gam0odds2, yend = 0.3), colour = "#F8766D", 
                 size = 1, linetype = 2, inherit.aes = FALSE) + 
    geom_segment(aes(x = xlimodds2[1], y = 0.3, xend = gam0odds2, yend = 0.3), colour = "#F8766D", 
                 size = 1, linetype = 2, inherit.aes = FALSE) +
    coord_cartesian(xlim = xlimodds2, ylim = ylimodds2) +
    labs(x = " ", 
         y = " ",
         subtitle = "n = 1000, p = 333, missing = 30%, odds = 2") +
    annotate("text", x = xlimodds2[2], y = ylimodds2[1], label = list(bquote(gamma[0] == .(round(gam0odds2, 2)))), 
             parse = TRUE, hjust = 1, vjust = 0) + 
    theme(plot.subtitle = element_text(size = 10))
  
  # Combine graphs into 1 figure
  figure <- ggarrange(gg1, gg2, gg3, gg4, gg5, gg6, nrow = 3, ncol = 2)
  annotate_figure(figure, left = text_grob("Proportion of missing Y values", rot = 90, vjust = 1.5),
                  bottom = text_grob("Grid of values", vjust = -1))
  
  ggsave(file.path("results", "figures", "suppfig5.png"), width = 7, height = 7, units = "in", plot = last_plot())
  ggsave(file.path("results", "figures", "suppfig5.pdf"), width = 7, height = 7, units = "in", plot = last_plot())
  
}
