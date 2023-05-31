add_a <- function(x){
  # This function will add "a" to the start of each element in a vector
  
  for(i in 1:length(x)){
    x[i] <- paste("a", x[i], sep = "")
  }
  x
}

grp_assign <- function(x, DGP){
  # This function will group auxiliary variables
  
  if(length(which(DGP$group == x)) != 0){
    out <- add_a(which(DGP$group == x))
  } else {
    out <- NA
  }
  out
}

len_grps <- function(grp){
  # This function will return the number of auxiliary variables in a group
  
  if(anyNA(grp)){
    out <- 0
  } else {
    out <- length(grp)
  }
  out
}

summ_aux <- function(grpno, grp_lens, strategy, res2){
  # This function will return the average number and percentage (across simulations) 
  # of auxiliary variables in a group, for a given strategy
  
  if(grp_lens[grpno] != 0){
    a <- mean(res2[which(res2$strategy == strategy), grpno + 2])
    b <- a / grp_lens[grpno] * 100
    out <- paste0(round(a, 1), " (", round(b, 1), ")")
  } else {
    out <- paste0("-")
  }
  out
}

summ_aux2 <- function(grpno, grp_lens, strategy, res2){
  # This function will return the average number and percentage (across simulations)
  # of auxiliary variables in a group, for a given strategy, in numerical form
  
  if(grp_lens[grpno] != 0){
    a <- mean(res2[which(res2$strategy == strategy), grpno + 2])
    b <- a / grp_lens[grpno] * 100
    out <- c(a, b)
  } else {
    out <- c(NA, NA)
  }
  out
}

prop_aux_selected <- function(res, cor_a, gamma_a){
  
  # This function will calculate the average number (and proportion) of 
  # auxiliary variables in each of eights groups (see below details) that were 
  # selected by the analysis strategy, as well as the average number 
  # (proportion) in total
  #
  # Input:
  # res: data frame containing simulation study results for a given scenario
  # cor_a: vector of correlations between a and y used in DGP
  # gamma_a: vector of regression coefficients used in DGP
  #
  # Details: 
  # The groups are defined as follows:
  # grp1: 0 <= |correlation| < 0.1 and associated with missingness
  # grp2: 0 <= |correlation| < 0.1 and not associated with missingness
  # grp3: 0.1 <= |correlation| < 0.1 and associated with missingness
  # grp4: 0.1 <= |correlation| < 0.1 and not associated with missingness
  # grp5: 0.2 <= |correlation| < 0.1 and associated with missingness
  # grp6: 0.2 <= |correlation| < 0.1 and not associated with missingness
  # grp7: |correlation| >= 0.4 and associated with missingness
  # grp8: |correlation| >= 0.4 and not associated with missingness
  #
  # Written by R Mainzer, August 2021
  
  # Parameters used to define groups
  p <- length(cor_a)
  DGP <- data.frame(a = 1:p, cor_a = cor_a, gamma_a = gamma_a)
  
  # Filter to extract variable selection strategies
  res <- rbind(filter(res, str_detect(strategy, "quickpred-pt2")),
               filter(res, str_detect(strategy, "quickpred-pt4")),
               filter(res, str_detect(strategy, "forward")),
               filter(res, str_detect(strategy, "tests")),
               filter(res, str_detect(strategy, "LASSO")))
  
  # Group auxiliary variables using known correlation and association with missingness
  DGP$group <- ifelse(0 <= abs(DGP$cor_a) & abs(DGP$cor_a) < 0.1 & DGP$gamma_a != 0, 1, 0)
  DGP$group <- ifelse(0 <= abs(DGP$cor_a) & abs(DGP$cor_a) < 0.1 & DGP$gamma_a == 0, 2, DGP$group)
  DGP$group <- ifelse(0.1 <= abs(DGP$cor_a) & abs(DGP$cor_a) < 0.2 & DGP$gamma_a != 0, 3, DGP$group)
  DGP$group <- ifelse(0.1 <= abs(DGP$cor_a) & abs(DGP$cor_a) < 0.2 & DGP$gamma_a == 0, 4, DGP$group)
  DGP$group <- ifelse(0.2 <= abs(DGP$cor_a) & abs(DGP$cor_a) < 0.4 & DGP$gamma_a != 0, 5, DGP$group)
  DGP$group <- ifelse(0.2 <= abs(DGP$cor_a) & abs(DGP$cor_a) < 0.4 & DGP$gamma_a == 0, 6, DGP$group)
  DGP$group <- ifelse(abs(DGP$cor_a) >= 0.4 & DGP$gamma_a != 0, 7, DGP$group)
  DGP$group <- ifelse(abs(DGP$cor_a) >= 0.4 & DGP$gamma_a == 0, 8, DGP$group)

  # Variables in each group
  grp1 <- grp_assign(1, DGP)
  grp2 <- grp_assign(2, DGP)
  grp3 <- grp_assign(3, DGP)
  grp4 <- grp_assign(4, DGP)
  grp5 <- grp_assign(5, DGP)
  grp6 <- grp_assign(6, DGP)
  grp7 <- grp_assign(7, DGP)
  grp8 <- grp_assign(8, DGP)

  # Vector of group lengths
  grp_lens <- c(len_grps(grp1), len_grps(grp2), len_grps(grp3), len_grps(grp4), 
                len_grps(grp5), len_grps(grp6), len_grps(grp7), len_grps(grp8))
  grp_lens <- c(grp_lens, sum(grp_lens))

  # Set up data frame to store results  
  res2 <- data.frame(strategy = "NA", nsim = NA, 
                    grp1 = NA, grp2 = NA, grp3 = NA, grp4 = NA, 
                    grp5 = NA, grp6 = NA, grp7 = NA, grp8 = NA,
                    total = NA)
  
  # Count how many selected auxiliary variables were in each group for each 
  # dataset and analysis strategy
  for(i in 1:nrow(res)){
    
    x <- unlist(str_split(res$imp_vars[i], pattern = " "))
    grp1_len <- length(intersect(x, grp1))
    grp2_len <- length(intersect(x, grp2))
    grp3_len <- length(intersect(x, grp3))
    grp4_len <- length(intersect(x, grp4))
    grp5_len <- length(intersect(x, grp5))
    grp6_len <- length(intersect(x, grp6))
    grp7_len <- length(intersect(x, grp7))
    grp8_len <- length(intersect(x, grp8))
    total <- sum(grp1_len, grp2_len, grp3_len, grp4_len, 
                 grp5_len, grp6_len, grp7_len, grp8_len) 
    
    tmp <- data.frame(strategy = res$strategy[i], nsim = res$nsim[i],
                      grp1 = grp1_len, grp2 = grp2_len, grp3 = grp3_len, 
                      grp4 = grp4_len, grp5 = grp5_len, grp6 = grp6_len, 
                      grp7 = grp7_len, grp8 = grp8_len, total = total)
    res2 <- rbind(res2, tmp)
    
  }
  
  # Remove first row of NAs
  res2 <- res2[-1, ]
  
  # Calculate average number and percentage (across simulations) of auxiliary 
  # variables in a group for each analysis strategy
  out <- as.data.frame(
        rbind(unlist(lapply(1:9, summ_aux, grp_lens = grp_lens, strategy = "quickpred-pt2", res2 = res2)),
              unlist(lapply(1:9, summ_aux, grp_lens = grp_lens, strategy = "quickpred-pt4", res2 = res2)),
              unlist(lapply(1:9, summ_aux, grp_lens = grp_lens, strategy = "forward", res2 = res2)),
              unlist(lapply(1:9, summ_aux, grp_lens = grp_lens, strategy = "forward-sw", res2 = res2)),
              unlist(lapply(1:9, summ_aux, grp_lens = grp_lens, strategy = "forward-fmi", res2 = res2)),
              unlist(lapply(1:9, summ_aux, grp_lens = grp_lens, strategy = "tests", res2 = res2)),
              unlist(lapply(1:9, summ_aux, grp_lens = grp_lens, strategy = "LASSO", res2 = res2))))
  
  # Update row and column names
  colnames(out) <- c("grp1", "grp2", "grp3", "grp4", "grp5", "grp6", "grp7", "grp8", "total")
  out <- cbind(strategy = c("Quickpred-pt2", "Quickpred-pt4", "Forward", "Forward-sw", "Forward-fmi", "Tests", "LASSO"),
                         out)
  
  # Number of variables in each group:
  out <- rbind(c(res$scenario[1], paste("\\textbf{", grp_lens, "}", sep = "")),
                         out)
  
  # Results without rounding
  out2 <- as.data.frame(
          rbind(unlist(lapply(1:9, summ_aux2, grp_lens = grp_lens, strategy = "quickpred-pt2", res2 = res2)),
                unlist(lapply(1:9, summ_aux2, grp_lens = grp_lens, strategy = "quickpred-pt4", res2 = res2)),
                unlist(lapply(1:9, summ_aux2, grp_lens = grp_lens, strategy = "forward", res2 = res2)),
                unlist(lapply(1:9, summ_aux2, grp_lens = grp_lens, strategy = "forward-sw", res2 = res2)),
                unlist(lapply(1:9, summ_aux2, grp_lens = grp_lens, strategy = "forward-fmi", res2 = res2)),
                unlist(lapply(1:9, summ_aux2, grp_lens = grp_lens, strategy = "tests", res2 = res2)),
                unlist(lapply(1:9, summ_aux2, grp_lens = grp_lens, strategy = "LASSO", res2 = res2)))
  )
  colnames(out2) <- c("grp1_n", "grp1_p", "grp2_n", "grp2_p", "grp3_n", "grp3_p", 
                      "grp4_n", "grp4_p", "grp5_n", "grp5_p", "grp6_n", "grp6_p",
                      "grp7_n", "grp7_p", "grp8_n", "grp8_p", "total_n", "total_p")
  
  grp_lens2 <- rep(grp_lens, each = 2)
  grp_lens2[c(2, 4, 6, 8, 10, 12, 14, 16, 18)] <- 100
  out2 <- rbind(out2, grp_lens2)
  
  out2 <- cbind(strat = c("Quickpred-pt2", "Quickpred-pt4", "Forward", "Forward-sw", 
                          "Forward-FMI", "Tests", "LASSO", "Total"), out2)
  
  # Output results
  list(out, grp_lens, out2)
  
}

prop_aux_selected_byMyassoc <- function(res, gamma_a){
  
  # This function will calculate the average number (and proportion) of 
  # auxiliary variables that were selected by each analysis strategy, 
  # stratified by whether auxiliary variables appear in the missingness model.
  #
  # Input:
  # res: data frame containing simulation study results for a given scenario
  # cor_a: vector of correlations between a and y used in DGP
  # gamma_a: vector of regression coefficients used in DGP
  #
  # Details: 
  # Group 1: Auxiliary variables in group 1 appear in the missingness model
  # Group 2: Auxiliary variables in group 2 are omitted from the missingess model
  #
  # Written by R Mainzer, October 2022
  
  # Parameters used to define groups
  p <- length(gamma_a)
  DGP <- data.frame(a = 1:p, gamma_a = gamma_a)
  
  # Filter to extract variable selection strategies
  res <- rbind(filter(res, str_detect(strategy, "quickpred-pt2")),
               filter(res, str_detect(strategy, "quickpred-pt4")),
               filter(res, str_detect(strategy, "forward")),
               filter(res, str_detect(strategy, "tests")),
               filter(res, str_detect(strategy, "LASSO")))
  
  # Group auxiliary variables by whether they appear in the missingness model
  DGP$group <- ifelse(DGP$gamma_a != 0, 1, 2)
  
  # Variables in each group
  grp1 <- grp_assign(1, DGP)
  grp2 <- grp_assign(2, DGP)
  
  # Vector of group lengths
  grp_lens <- c(len_grps(grp1), len_grps(grp2))

  # Set up data frame to store results  
  res2 <- data.frame(strategy = "NA", nsim = NA, grp1 = NA, grp2 = NA)
  
  # Count how many selected auxiliary variables were in each group for each 
  # dataset and analysis strategy
  for(i in 1:nrow(res)){
    
    x <- unlist(str_split(res$imp_vars[i], pattern = " "))
    grp1_len <- length(intersect(x, grp1))
    grp2_len <- length(intersect(x, grp2))
    total <- sum(grp1_len, grp2_len) 
    
    tmp <- data.frame(strategy = res$strategy[i], nsim = res$nsim[i],
                      grp1 = grp1_len, grp2 = grp2_len)
    res2 <- rbind(res2, tmp)
    
  }
  
  # Remove first row of NAs
  res2 <- res2[-1, ]
  
  # Calculate average number and percentage (across simulations) of auxiliary 
  # variables in a group for each analysis strategy
  out <- as.data.frame(
    rbind(unlist(lapply(1:2, summ_aux, grp_lens = grp_lens, strategy = "quickpred-pt2", res2 = res2)),
          unlist(lapply(1:2, summ_aux, grp_lens = grp_lens, strategy = "quickpred-pt4", res2 = res2)),
          unlist(lapply(1:2, summ_aux, grp_lens = grp_lens, strategy = "forward", res2 = res2)),
          unlist(lapply(1:2, summ_aux, grp_lens = grp_lens, strategy = "forward-sw", res2 = res2)),
          unlist(lapply(1:2, summ_aux, grp_lens = grp_lens, strategy = "forward-fmi", res2 = res2)),
          unlist(lapply(1:2, summ_aux, grp_lens = grp_lens, strategy = "tests", res2 = res2)),
          unlist(lapply(1:2, summ_aux, grp_lens = grp_lens, strategy = "LASSO", res2 = res2))))
  
  # Update row and column names
  colnames(out) <- c("in_miss_model", "not_in_miss_mod")
  out <- cbind(strategy = c("Quickpred-pt2", "Quickpred-pt4", "Forward", "Forward-sw", "Forward-fmi", "Tests", "LASSO"),
               out)
  
  # Number of variables in each group:
  out <- rbind(c(res$scenario[1], paste("\\textbf{", grp_lens, "}", sep = "")),
               out)
  
  # Results without rounding
  out2 <- as.data.frame(
    rbind(unlist(lapply(1:2, summ_aux2, grp_lens = grp_lens, strategy = "quickpred-pt2", res2 = res2)),
          unlist(lapply(1:2, summ_aux2, grp_lens = grp_lens, strategy = "quickpred-pt4", res2 = res2)),
          unlist(lapply(1:2, summ_aux2, grp_lens = grp_lens, strategy = "forward", res2 = res2)),
          unlist(lapply(1:2, summ_aux2, grp_lens = grp_lens, strategy = "forward-sw", res2 = res2)),
          unlist(lapply(1:2, summ_aux2, grp_lens = grp_lens, strategy = "forward-fmi", res2 = res2)),
          unlist(lapply(1:2, summ_aux2, grp_lens = grp_lens, strategy = "tests", res2 = res2)),
          unlist(lapply(1:2, summ_aux2, grp_lens = grp_lens, strategy = "LASSO", res2 = res2)))
  )
  colnames(out2) <- c("grp1_n", "grp1_p", "grp2_n", "grp2_p")
  
  grp_lens2 <- rep(grp_lens, each = 2)
  out2 <- rbind(out2, grp_lens2)
  
  out2 <- cbind(strat = c("Quickpred-pt2", "Quickpred-pt4", "Forward", "Forward-sw", 
                          "Forward-FMI", "Tests", "LASSO", "Total"), out2)
  
  # Output results
  list(table = out, group_length = grp_lens, dat = out2)
  
}
