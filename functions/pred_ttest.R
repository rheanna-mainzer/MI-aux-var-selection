# pred_ttest
#
# This function will return a 1 if the p value from a t test is <= 0.05 and a 0 
# otherwise. Used to construct the predictor matrix to be used by mice for the 
# tests strategy.
#
# Input:
# x: auxiliary variable
# group: grouping for t test (missingness indicator)
#
# Output:
# 1 if p-value <= 0.05 (auxiliary variable is included in imputation model); 
# 0 if p-value > 0.05 or number of observations in each group is less than 20 
#
# Written by R Mainzer, Jan 2021

pred_ttest <- function(x, group){
  
  # Check if there are enough observations for the t test
  n_group <- colSums(table(x, group))
  if(n_group[1] < 20 | n_group[2] < 20){
    out <- 0
  } else {
    
    # Extract the p value 
    df <- data.frame(x, group)
    p_val <- t.test(x ~ group, na.rm = TRUE)$p.value
    
    # Output a 1 if auxiliary variable is to be included in imputation model, 
    # otherwise output a 0
    if(p_val > 0.05){
      out <- 0
    } else {
      out <- 1
    }
    
    out
    
  }
  
}
