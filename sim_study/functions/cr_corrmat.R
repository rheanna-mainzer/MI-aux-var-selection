# cr_corrmat
# Create correlation matrix used for data generation
#
# Input: 
# cor_xy: correlation between X and Y
# n: sample size
# p: number of auxiliary variables
#
# Written by RMainzer

cr_corrmat <- function(cor_xy, n, p){
  
  # Correlations between variables
  cor_xz <- 0.3
  cor_zy <- 0.3
  nblocks <- 3 # number of blocks of auxiliary variables in correlation matrix
  g <- p %/% nblocks # number of auxiliary variables in each group
  rem <- p %% nblocks # remainder (if p not exactly divisible by nblocks) These go into low correlation group
  cor_Ay <- c(rnorm(g, 0.4, 0.1), rnorm(g, 0.2, 0.05), rnorm(g + rem, 0.1, 0.05))
  
  # Correlation matrix of (X, Z, A)
  cor <- simcor.H(k = nblocks + 1, size = c(2, g, g, g + rem),
                  rho=rbind(c(0.3,0.2), c(0.4,0.4), c(0.2,0.2), c(0.1,0.1)),
                  power = 1, epsilon = 0.08, eidim = 2)
  cor[1, 2] <- cor_xz
  cor[2, 1] <- cor_xz
  
  # Correlation matrix of (Y, X, Z, A)
  sig_12 <- c(cor_xy, cor_zy, cor_Ay)
  cor2 <- rbind(t(c(1, t(sig_12))), cbind(sig_12, cor, deparse.level = 0))
  
  # Nearest positive definite matrix
  cor3 <- Matrix::forceSymmetric(Matrix::nearPD(cor2, corr = TRUE, keepDiag = TRUE)$mat)
  
  # Check that matrix positive definite
  det_cor3 <- det(cor3)
  if(det_cor3 > 0){
    cat("Correlation matrix is positive definite, determinant = ", det_cor3, "\n")
  } else {
    cat("Correlation matrix is not positive definite, determinant = ", det_cor3, "\n")
  }
  
  # Names for correlation matrix
  rownames(cor3) <- c("Y", "X", "Z", paste("A", 1:p, sep = ""))
  colnames(cor3) <- c("Y", "X", "Z", paste("A", 1:p, sep = ""))
  labs <- c("Y", "X", "Z", paste("A[", 1:p, "]", sep = ""))
  
  # Output correlation matrix
  cor3
  
}

# ------------------------------------------------------------------------------

# gr_corrmat
# Graph correlation matrix used for data generation
#
# Input: 
# cor_mat: correlation matrix used for data generation
# p: number of auxiliary variables
# print.labs: TRUE if labels are to be printed, FALSE otherwise
# print.cor: TRUE if correlation values are to be printed, FALSE otherwise
# save.fig: TRUE if figure is to be save, FALSE otherwise
# name: name of figure file for saving
#
# Written by RMainzer

gr_corrmat <- function(cor_mat, p, print.labs, print.cor, save.fig, name){
  
  # Labels
  labs <- c("Y", "X", "Z", paste("A[", 1:p, "]", sep = ""))
  
  # Graph correlation matrix
  melted_cormat <- melt(as.matrix(cor_mat), na.rm = TRUE)
  gg <- ggplot(data = melted_cormat, aes(x = Var1, y = Var2, fill = value)) + 
    geom_tile() +  
    xlab(" ") + 
    ylab(" ") 
  if(print.labs == TRUE){
    gg <- gg + 
      scale_x_discrete(labels = parse(text = labs)) + 
      scale_y_discrete(labels = parse(text = labs)) +
      theme(axis.text.x = element_text(size = 7),
            axis.text.y = element_text(size = 7))
  } else {
    gg <- gg +
      scale_x_discrete(labels = NULL) + 
      scale_y_discrete(labels = NULL)
  }
  if(print.cor == TRUE){
    gg <- gg + geom_text(aes(label = round(value, 2)), color = "white", size = 2)
  }
  print(gg)
  
  # Save graph?
  if(save.fig == TRUE){
    ggsave(file.path("results", "figures", paste(name, ".png", sep = "")), 
           width = 7, height = 5, units = "in", plot = last_plot())
    ggsave(file.path("results", "figures", paste(name, ".pdf", sep = "")), 
           width = 7, height = 5, units = "in", plot = last_plot())
  }
  
}
 