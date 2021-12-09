# select_aux_forwardFMI_simstudy
#
# Forward selection using fraction of missing information (FMI) 
# Select for inclusion variables that produces the smallest FMI
# 
# Input:
# Y: outcome with missing data
# aux_dat: data frame of auxiliary variables
# scenario: simulation scenario
# print: print progress of forward selection?
#
# Output:
# A list containing the estimated FMI and selected auxiliary variables
#
# Written by R Mainzer, May 2021

select_aux_forwardFMI_simstudy <- function(Y, aux_dat, scenario, print = FALSE){
  
  # Proportion of missing values in Y
  pmy <- sum(is.na(Y))/length(Y)
  
  # Initialise with no auxiliary variables in the set
  A_set <- data.frame()
  
  # Number of auxiliary variables in aux_dat
  p_auxdat <- ncol(aux_dat)
  
  # Potential auxiliary variables (to be added to A_set)
  A_potential <- aux_dat
  
  # Set up vector to store results
  min_FMI <- NULL
  
  # Print progress
  if(print == TRUE){cat("Variables selected:", "\n")}
  
  # Count the number of iterations of the algorithm
  j <- 1
  
  # Scenario-specific stopping rule
  last_diff <- 100
  if(scenario == "basic"){ frac <- 0.01 * pmy} 
  if(scenario == "extreme"){ frac <- 0.01 * pmy}
  if(scenario == "realistic"){ frac <- 0.001 * pmy}
      
  # Continue algorithm until the decrease in FMI from an additional auxiliary
  # variable is less than the pre-specified percentage of the proportion of 
  # missing data in Y
  while(last_diff > frac){
    
    # Size of set of potential auxiliary variables
    p_auxpot <- ncol(A_potential)
    p_auxset <- ncol(A_set)
    
    if(p_auxpot + p_auxset != p_auxdat) stop("auxiliary variable not accounted for")
    
    # Temporary variable to store FMI
    FMI_tmp <- rep(0, p_auxpot)
    
    # Find variable with lowest FMI and add this to selected set 
    for(i in 1:p_auxpot){
      
      if(j == 1){
        A_tmp <- A_potential[i]
      } else {
        A_tmp <- cbind(A_set, A_potential[i])
      }
      
      FMI_tmp[i] <- calc_FMI(Y, A_tmp)
      
    }
    
    # Add variable with the lowest FMI to A_set
    min_FMI[j] <- min(FMI_tmp)
    FMI_pos_tmp <- which(FMI_tmp == min_FMI[j])
    add_var_tmp <- A_potential[FMI_pos_tmp] 
    
    if(j == 1){
      A_set <- rbind(A_set, add_var_tmp)
    } else {
      A_set <- cbind(A_set, add_var_tmp)
    }
    
    # Remove selected variable from set of potential auxiliary variables
    A_potential <- subset(A_potential, select = -c(FMI_pos_tmp))
    
    # Create arbitrary large value for "diffs" if j = 1
    if(j == 1){
      last_diff <- 100
    }
    
    if(j > 1){
      # Calculate the absolute differences in min_FMI
      diffs <- c(abs(diff(min_FMI)))
      last_diff <- diffs[j - 1]
    }
    
    # Update progress
    if(print == TRUE){ cat(j, "... ")}

    # Update j
    j <- j + 1
    
  }

  if(print == TRUE){ cat("DONE")}
  
  # Remove the last variable added (will not be included in imputation model) and last minimum FMI
  min_FMI <- min_FMI[1:(j - 2)]
  A_set <- A_set[, 1:(j-2)]
    
  # Output list of min_FMI and selected auxiliary variables
  out <- list(min_FMI = min_FMI, A_set = A_set)
  
}