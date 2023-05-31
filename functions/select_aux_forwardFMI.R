# Forward selection with FMI 
# Select variable that produces the smallest FMI
# Stopping criteria - estimate the variance of the change in FMI at each step and 
# only add subsequent auxiliary variables if they significantly decrease the FMI?
select_aux_forwardFMI <- function(Y, aux_dat, J, print = FALSE){
  
  # Input:
  # Y: outcome with missing data
  # aux_dat: data frame of auxiliary variables
  # n: sample size
  # J: number of auxiliary variables to include in set - will be updated as 
  #    decision rule changes
  # print: print progress of forward selection?
  
  # Initially no auxiliary variables in the set
  A_set <- data.frame()
  
  # Numer of auxiliary variables in aux dat
  p_auxdat <- ncol(aux_dat)
  
  # Potential auxiliary variables
  A_potential <- aux_dat
  
  # Set up vector to store results
  min_FMI <- rep(0, 3)
  
  if(print == TRUE){ cat("Variables selected:", "\n")}
  
  for(j in 1:J){
    
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
   
    # Update progress
    if(print == TRUE){ cat(j, "... ")}
     
  }
  
  if(print == TRUE){ cat("DONE", "\n")}
  
  # Output data frame of selected auxiliary variables
  out <- list(min_FMI = min_FMI, A_set = A_set)
  
}