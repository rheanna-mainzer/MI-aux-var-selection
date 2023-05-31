# This function will return a 1 if the prop.test p value is <= 0.05 and a 0 otherwise
pred_proptest <- function(x, group){
  
  # x is the categorical variable
  # group is the grouping for the t test
  
  # Combine categories 4 and 5 if there are 5 categories
  if(length(table(x)) == 5){
    x <- as.numeric(levels(x))[as.integer(x)]
    x[which(x == 5)] <- 4
    x <- factor(x, ordered = TRUE)
  } 
  
  # Calculate frequencies for each group and store in a table
  grp1 <- x[which(group == 0)]
  grp2 <- x[which(group == 1)]
  freq_grp1 <- table(grp1)
  freq_grp2 <- table(grp2)
  tab <- cbind(as.vector(freq_grp1), as.vector(freq_grp2))

  # Chi-squared test
  suppressWarnings(Xsq <- chisq.test(tab))
  
  # If any expected counts are too low, don't use the variable as a predictor
  if(any(as.vector(Xsq$expected) < 5)){
    out <- 0
    
  } else {
    
    # Calculate p value
    p_val <- Xsq$p.value

    # Output a 1 if predictor is to be included in model for outcome, 
    # otherwise output a 0
    if(p_val > 0.05){
      out <- 0
    } else {
      out <- 1
    }
    
  }
  
  # Remove x
  rm(x)
  
  # Output result
  out
  
}