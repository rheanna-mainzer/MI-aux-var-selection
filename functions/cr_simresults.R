# Create simulation study results
# cr_simresults
# 
# This function will create a data frame containing performance measures with 
# associated Monte Carlo Standard Errors (MCSEs) after obtaining estimates 
# from each simulated data set.
# 
# Input:
# dat: A data frame containing estimates from each simulated data set and 
#      analysis strategy for a given scenario
# strategy_vec: vector of analysis strategies (strings)
# betax: true regression coefficient known from DGP
# meany: true mean of y known from DGP
#
# Output:
# A data frame containing estimated performance measures and MCSEs.
#
# Written by RMainzer, June 2021
#
# Updates:
# 2022-01-06: Added additional performance measures: standardised bias, 
# relative bias, relative error in the model SE

cr_simresults <- function(dat, strategy_vec, betax, meany){
  
  # Scenario and nsim
  scenario <- dat$scenario[1]
  n_strat <- length(strategy_vec)
  nsim <- nrow(dat) / n_strat
  
  # Set up data frame to store results
  res <- data.frame(scenario = rep(scenario, n_strat),
                    strategy = "NA",
                    bx_bias = NA,
                    bx_bias_MCSE = NA,
                    bx_bias_MCll = NA,
                    bx_bias_MCul = NA,
                    bx_empSE = NA,
                    bx_empSE_MCSE = NA,
                    bx_empSE_MCll = NA,
                    bx_empSE_MCul = NA,
                    bx_zbias = NA,
                    bx_relbias = NA,
                    bx_avmodSE = NA,
                    bx_avmodSE_MCSE = NA,
                    bx_avmodSE_MCll = NA,
                    bx_avmodSE_MCul = NA,
                    bx_relerr = NA,
                    bx_covprob = NA,
                    bx_covprob_MCSE = NA,
                    bx_covprob_MCll = NA,
                    bx_covprob_MCul = NA,
                    bx_conver = NA,
                    meany_bias = NA,
                    meany_bias_MCSE = NA,
                    meany_bias_MCll = NA,
                    meany_bias_MCul = NA,
                    meany_empSE = NA,
                    meany_empSE_MCSE = NA,
                    meany_empSE_MCll = NA,
                    meany_empSE_MCul = NA,
                    meany_zbias = NA,
                    meany_avmodSE = NA,
                    meany_avmodSE_MCSE = NA,
                    meany_avmodSE_MCll = NA,
                    meany_avmodSE_MCul = NA,
                    meany_relerr = NA,
                    meany_covprob = NA,
                    meany_covprob_MCSE = NA,
                    meany_covprob_MCll = NA,
                    meany_covprob_MCul = NA,
                    meany_conver = NA)
  
  for(i in 1:n_strat){
    
    # Subset of data containing estimates from ith strategy in strategy_vec
    strat_tmp <- strategy_vec[i]
    dat_tmp <- dat[dat$strategy == strat_tmp, ]
    
    ## Performance measures for beta_x --------------------------------------------
    
    # Convergence rate for beta_x 
    bx_conver_rate_tmp <- (1 - sum(is.na(dat_tmp$bs)) / nsim) * 100
    
    # Subset of data where bx was successfully estimated
    if(bx_conver_rate_tmp != 100){
      bx_nonconver_id <- which(is.na(dat_tmp$bx))
      bx_dat_tmp <- dat_tmp[-c(bx_nonconver_id),]
    } else {
      bx_dat_tmp <- dat_tmp
    }
    
    # Calculate bias estimate
    bx_bar_tmp <- mean(bx_dat_tmp$bx)
    bx_bias_tmp <- bx_bar_tmp - betax
    sumsq_bx_minus_bxbar <- sum((bx_dat_tmp$bx - bx_bar_tmp)^2)
    bx_bias_MCSE_tmp <- sqrt(1/(nsim * (nsim - 1)) * sumsq_bx_minus_bxbar)
    bx_bias_MCll_tmp <- bx_bias_tmp - 1.96 * bx_bias_MCSE_tmp
    bx_bias_MCul_tmp <- bx_bias_tmp + 1.96 * bx_bias_MCSE_tmp
    
    # Calculate empirical SE estimate
    bx_empSE_tmp <- sqrt(1/(nsim - 1) * sumsq_bx_minus_bxbar)
    bx_empSE_MCSE_tmp <- bx_empSE_tmp / sqrt(2 * (nsim - 1))
    bx_empSE_MCll_tmp <- bx_empSE_tmp - 1.96 * bx_empSE_MCSE_tmp
    bx_empSE_MCul_tmp <- bx_empSE_tmp + 1.96 * bx_empSE_MCSE_tmp
    
    # Calculate standardised bias
    bx_zbias_tmp <- bx_bias_tmp / bx_empSE_tmp * 100
    
    # Calculate relative bias
    bx_relbias_tmp <- bx_bias_tmp / betax * 100
    
    # Calculate average model SE estimate
    bx_avmodSE_tmp <- sqrt(mean(bx_dat_tmp$bx_se^2))
    var_var_bx <- (1 / (nsim - 1)) * sum((bx_dat_tmp$bx_se^2 - bx_avmodSE_tmp^2)^2)
    bx_avmodSE_MCSE_tmp <- sqrt(var_var_bx / (4 * nsim * bx_avmodSE_tmp^2))
    bx_avmodSE_MCll_tmp <- bx_avmodSE_tmp - 1.96 * bx_avmodSE_MCSE_tmp
    bx_avmodSE_MCul_tmp <- bx_avmodSE_tmp + 1.96 * bx_avmodSE_MCSE_tmp
    
    # Calculate relative error in the model SE
    bx_relerr_tmp <- (bx_avmodSE_tmp / bx_empSE_tmp - 1) * 100
    
    # Calculate coverage probability estimate
    bx_covprob_tmp <- mean(ifelse(betax >= bx_dat_tmp$bx_ll & betax <= bx_dat_tmp$bx_ul, 1, 0))
    bx_covprob_MCSE_tmp <- sqrt(bx_covprob_tmp * (1 - bx_covprob_tmp) / nsim)
    bx_covprob_MCll_tmp <- bx_covprob_tmp - 1.96 * bx_covprob_MCSE_tmp
    bx_covprob_MCul_tmp <- bx_covprob_tmp + 1.96 * bx_covprob_MCSE_tmp
    
    ## Performance measures for mean of Y -----------------------------------------
    
    # Convergence rate for mean of y
    ybar_conver_rate_tmp <- (1 - sum(is.na(dat_tmp$ybar)) / nsim) * 100
    
    # Subset of data where mean of y was successfully estimated
    if(ybar_conver_rate_tmp != 100){
      ybar_nonconver_id <- which(is.na(dat_tmp$ybar))
      ybar_dat_tmp <- dat_tmp[-c(ybar_nonconver_id), ]
    } else {
      ybar_dat_tmp <- dat_tmp
    }
    
    # Calculate bias estimate
    ybar_bar_tmp <- mean(ybar_dat_tmp$ybar)
    ybar_bias_tmp <- ybar_bar_tmp - meany
    sumsq_ybar_minus_ybarbar <- sum((ybar_dat_tmp$ybar - ybar_bar_tmp)^2)
    ybar_bias_MCSE_tmp <- sqrt(1/(nsim * (nsim - 1)) * sumsq_ybar_minus_ybarbar)
    ybar_bias_MCll_tmp <- ybar_bias_tmp - 1.96 * ybar_bias_MCSE_tmp
    ybar_bias_MCul_tmp <- ybar_bias_tmp + 1.96 * ybar_bias_MCSE_tmp
    
    # Calculate empirical SE estimate
    ybar_empSE_tmp <- sqrt(1/(nsim - 1) * sumsq_ybar_minus_ybarbar)
    ybar_empSE_MCSE_tmp <- ybar_empSE_tmp / sqrt(2 * (nsim - 1))
    ybar_empSE_MCll_tmp <- ybar_empSE_tmp - 1.96 * ybar_empSE_MCSE_tmp
    ybar_empSE_MCul_tmp <- ybar_empSE_tmp + 1.96 * ybar_empSE_MCSE_tmp
    
    # Calculate standardised bias
    ybar_zbias_tmp <- ybar_bias_tmp / ybar_empSE_tmp * 100
    
    # Calculate average model SE estimate
    ybar_avmodSE_tmp <- sqrt(mean(ybar_dat_tmp$ybar_se^2))
    var_var_ybar <- (1 / (nsim - 1)) * sum((ybar_dat_tmp$ybar_se^2 - ybar_avmodSE_tmp^2)^2)
    ybar_avmodSE_MCSE_tmp <- sqrt(var_var_ybar / (4 * nsim * ybar_avmodSE_tmp^2))
    ybar_avmodSE_MCll_tmp <- ybar_avmodSE_tmp - 1.96 * ybar_avmodSE_MCSE_tmp
    ybar_avmodSE_MCul_tmp <- ybar_avmodSE_tmp + 1.96 * ybar_avmodSE_MCSE_tmp
    
    # Calculate relative error in the model SE
    ybar_relerr_tmp <- (ybar_avmodSE_tmp / ybar_empSE_tmp - 1) * 100
    
    # Calculate coverage probability estimate
    ybar_covprob_tmp <- mean(ifelse(meany >= ybar_dat_tmp$ybar_ll & meany <= ybar_dat_tmp$ybar_ul, 1, 0))
    ybar_covprob_MCSE_tmp <- sqrt(ybar_covprob_tmp * (1 - ybar_covprob_tmp) / nsim)
    ybar_covprob_MCll_tmp <- ybar_covprob_tmp - 1.96 * ybar_covprob_MCSE_tmp
    ybar_covprob_MCul_tmp <- ybar_covprob_tmp + 1.96 * ybar_covprob_MCSE_tmp
    
    ## Update results ---------------------------------------------------------

    res[i, "strategy"] <- strat_tmp
    res[i, "bx_bias"] <- bx_bias_tmp
    res[i, "bx_bias_MCSE"] <- bx_bias_MCSE_tmp
    res[i, "bx_bias_MCll"] <- bx_bias_MCll_tmp
    res[i, "bx_bias_MCul"] <- bx_bias_MCul_tmp
    res[i, "bx_empSE"] <- bx_empSE_tmp
    res[i, "bx_empSE_MCSE"] <- bx_empSE_MCSE_tmp
    res[i, "bx_empSE_MCll"] <- bx_empSE_MCll_tmp
    res[i, "bx_empSE_MCul"] <- bx_empSE_MCul_tmp
    res[i, "bx_zbias"] <- bx_zbias_tmp
    res[i, "bx_relbias"] <- bx_relbias_tmp
    res[i, "bx_avmodSE"] <- bx_avmodSE_tmp
    res[i, "bx_avmodSE_MCSE"] <- bx_avmodSE_MCSE_tmp
    res[i, "bx_avmodSE_MCll"] <- bx_avmodSE_MCll_tmp
    res[i, "bx_avmodSE_MCul"] <- bx_avmodSE_MCul_tmp
    res[i, "bx_relerr"] <- bx_relerr_tmp
    res[i, "bx_covprob"] <- bx_covprob_tmp
    res[i, "bx_covprob_MCSE"] <- bx_covprob_MCSE_tmp
    res[i, "bx_covprob_MCll"] <- bx_covprob_MCll_tmp
    res[i, "bx_covprob_MCul"] <- bx_covprob_MCul_tmp
    res[i, "bx_conver"] <- bx_conver_rate_tmp
    res[i, "meany_bias"] <- ybar_bias_tmp
    res[i, "meany_bias_MCSE"] <- ybar_bias_MCSE_tmp
    res[i, "meany_bias_MCll"] <- ybar_bias_MCll_tmp
    res[i, "meany_bias_MCul"] <- ybar_bias_MCul_tmp
    res[i, "meany_empSE"] <- ybar_empSE_tmp
    res[i, "meany_empSE_MCSE"] <- ybar_empSE_MCSE_tmp
    res[i, "meany_empSE_MCll"] <- ybar_empSE_MCll_tmp
    res[i, "meany_empSE_MCul"] <- ybar_empSE_MCul_tmp
    res[i, "meany_zbias"] <- ybar_zbias_tmp
    res[i, "meany_avmodSE"] <- ybar_avmodSE_tmp
    res[i, "meany_avmodSE_MCSE"] <- ybar_avmodSE_MCSE_tmp
    res[i, "meany_avmodSE_MCll"] <- ybar_avmodSE_MCll_tmp
    res[i, "meany_avmodSE_MCul"] <- ybar_avmodSE_MCul_tmp
    res[i, "meany_relerr"] <- ybar_relerr_tmp
    res[i, "meany_covprob"] <- ybar_covprob_tmp
    res[i, "meany_covprob_MCSE"] <- ybar_covprob_MCSE_tmp
    res[i, "meany_covprob_MCll"] <- ybar_covprob_MCll_tmp
    res[i, "meany_covprob_MCul"] <- ybar_covprob_MCul_tmp
    res[i, "meany_conver"] <- ybar_conver_rate_tmp
    
  }
  
  # Output results
  out <- res
  
}