tune_FMI <- function(subset, scenario, name){
  
  # Read data set
  dat <- read.table(file.path("data", paste(scenario, "_", paste(sprintf("%04d", subset)), ".csv", sep = "")))
  aux_dat <- select(dat, starts_with("a"))
  J <- ncol(aux_dat)
  
  # Calculate FMI at each step of forward selection 
  FMI <- select_aux_forwardFMI(dat$y, aux_dat, J = J, print = TRUE)
  
  # Proposed stopping rule - stop when the absolute change in FMI is less than
  # x% of the percentage of missing data
  pmy <- sum(is.na(dat$y))/length(dat$y)
  diffs <- c(NA, abs(diff(FMI$min_FMI)))
  if(J == 25){ frac <- 0.01 * pmy} 
  if(J == 83){ frac <- 0.01 * pmy} 
  if(J == 100){ frac <- 0.01 * pmy} 
  if(J == 333){ frac <- 0.005 * pmy} 
  last <- which(diffs < frac)[1] - 1

  # Create data frame to be used in plotting
  FMI_df <- data.frame(Number = 1:J, FMI = FMI$min_FMI, Change = diffs)
  FMI_df$Incl <- as.factor(c(rep(0, last - 1), 1, rep(0, J - last)))
  last_FMI <- FMI_df$FMI[last]
  FMI_df <- FMI_df[-1, ]
  
  # Plot absolute change in FMI vs number of auxiliary variables
  gg <- ggplot(data = FMI_df, aes(x = Number, y = Change, color = Incl)) +
    geom_point(size = 2) +
    geom_hline(yintercept = frac, col = "blue") +
    scale_color_manual(values = c("black", "#F8766D")) + 
    theme(legend.position = "none") +
    labs(x = " ", 
         y = " ",
         subtitle = bquote("Scenario:" ~.(scenario)~", Dataset:"~.(subset))) 
  
  # Save figure
  ggsave(file.path("results", "figures", paste("FMI_", name, ".png", sep = "")), 
         width = 7, height = 7, units = "in", plot = last_plot())
  ggsave(file.path("results", "figures", paste("FMI_", name, ".pdf", sep = "")), 
         width = 7, height = 7, units = "in", plot = last_plot())
  
  # Output data frame
  FMI_df <- data.frame(scenario = scenario, dataset = subset, frac = frac, FMI_df)
  out <- FMI_df
  
} 