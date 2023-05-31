check_DGP <- function(dat, graph, save, name){
  
  # Check estimates in complete data
  cat("Estimated mean of y =", round(mean(dat$Ycom), 3), "\n",
      "Estimated betax = ", round(coef(lm(Ycom ~ X, data = dat))[2], 3), "\n",
      "Estimated variance of Y = ", round(var(dat$Ycom), 3), "\n",
      "Proportion of missing values = ", sum(is.na(dat$Y)) / nrow(dat), "\n")
  
  if(graph == TRUE){
    
    # Choose colours
    cols1 <- brewer.pal(9, name = "Set1") 
    cols2 <- brewer.pal(3, name = "Pastel1")
    
    # Graph correlations between Y and A 
    cor_fun <- function(x, y){
      cor(x, y, use = "complete.obs")
    }
    cors <- sapply(dplyr::select(dat, starts_with("A")), cor_fun, y = dat$Y)
    gg1 <- ggplot(data.frame(cors = cors), aes(x = cors)) +
      geom_histogram(aes(y = stat(count) / sum(count)),
                     breaks = seq(-0.1, 0.5, by = 0.1),
                     color = "black",
                     fill = cols1[2]) + 
      labs(x = "Correlation", y = "Relative frequency",
           title = "A")  +
      theme(plot.title = element_text(face = "bold"))
    
    # Graph odds ratios between M_Y and A 
    OR_fun2 <- function(x, My){
      mod <- glm(My ~ x, family = binomial(link = "logit"))
      out <- exp(as.numeric(coef(mod)["x"]))
    }
    ORs <- sapply(dplyr::select(dat, starts_with("A")), OR_fun2, My = dat$My)
    gg2 <- ggplot(data.frame(ORs = ORs), aes(x = ORs)) +
      geom_histogram(aes(y = stat(count) / sum(count)), 
                     breaks = seq(0.9, 1.4, by = 0.1),
                     color = "black",
                     fill = cols1[2]) + 
      labs(x = "Odds ratios", y = "Relative frequency",
           title = "B") +
      theme(plot.title = element_text(face = "bold"))
    
    #title = bquote("Odds ratios between"~M[Y]~"and A"))
    
    # Examine distributions of observed and missing data 
    
    # Generate data for plotting
    boxplot_dat <- data.frame(yobs = dat$Ycom, 
                              group = factor(ifelse(dat$My == 1, "Missing", "Observed"), 
                                             levels = c("All", "Observed", "Missing")))
    boxplot_dat <- rbind(boxplot_dat, data.frame(yobs = dat$Ycom, group = "All"))
    
    # Mean of observed and missing values
    mean(boxplot_dat[which(boxplot_dat$group == "Observed"), "yobs"])
    mean(boxplot_dat[which(boxplot_dat$group == "Missing"), "yobs"])
    
    gg3 <- ggplot(boxplot_dat, aes(y = yobs, x = group, fill = group)) + 
      geom_boxplot() + 
      scale_fill_manual(values=c(cols2[3], cols2[2], cols2[1])) +
      labs(x = " ", y = " ", title = "C") +
      theme(legend.position = "none",
            plot.title = element_text(face = "bold"))
    
    # Scatterplots showing observed and missing data 
    dat$My_fact <- as.factor(dat$My)
    levels(dat$My_fact) <- c("Observed", "Missing")
    gg4 <- ggplot(dat, aes(y = Ycom, x = X, color = My_fact)) +
      geom_point() +
      labs(x = "X", y = "Y", title = "D") +
      scale_fill_manual(name = " ") +
      scale_color_manual(values = c(cols1[2], cols1[1])) +
      theme(legend.position = c(0.9, 0.1), legend.title = element_blank(), 
            legend.key.size = unit(0.3, 'cm'),
            plot.title = element_text(face = "bold"))
    
    # Combine graphs
    gg_ba <- ggarrange(gg1, gg2, gg3, gg4, ncol = 2, nrow = 2)
    
    # Annotate graph
    p <- ncol(dplyr::select(dat, starts_with("A")))
    n <- nrow(dat)
    text <- bquote("n = " ~ .(n) ~ ", p = " ~ .(p))
    gg_ba <- annotate_figure(gg_ba, top = text_grob(text, size = 14, face = "bold"))
    print(gg_ba)
    
    if(save == TRUE){
      
      # Save figure
      ggsave(file.path("results", "figures", paste(name, ".png", sep = "")), 
             width = 7, height = 7, units = "in", plot = last_plot())
      ggsave(file.path("results", "figures", paste(name, ".pdf", sep = "")), 
             width = 7, height = 7, units = "in", plot = last_plot())
      
    }
    
  }
  
}

