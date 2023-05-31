# Analyse simulated data
# an_simdata
#
# This function will apply the analysis strategies in strategy_vec to one batch 
# of simulated datasets (10 datasets per batch) and output the desired estimates
# 
# Input:
# scenario: "n250_p25_pmy30_mod", "n250_p83_pmy30_mod", "n1000_p100_pmy30_mod",
#           "n1000_p333_pmy30_mod", "n1000_p100_pmy50_mod", "n1000_p100_pmy30_str",
# N: batch number (1 - 200)
# strategy_vec: vector of analysis strategies (strings):
#   "cc": complete case analysis
#   "mice-full": all auxiliary variables included in the imputation model
#   "quickpred-pt2": mice quickpred strategy with mincor = 0.2
#   "quickpred-pt4": mice quickpred strategy with mincor = 0.4
#   "pcaux": principal components as auxiliary variables
#   "forward-fmi": forward selection based on the FMI
#   "tests": auxiliary variables selection using t tests
#   "LASSO": auxiliary variables selection using the LASSO
# m: number of imputations
# maxit: number of iterations for mice (not important for univariate missingness)
#
# Functions:
# pred_ttest.R: select auxiliary variables using t tests
# calc_FMI.R: calculate FMI
# select_aux_forwardFMI_simstudy.R: forward selection of auxiliary variables using FMI
# 
# Output:
# Data frame containing the following columns:
#   1. scenario: scenario name
#   2. nsim: simulated dataset number
#   3. straetgy: analysis strategy
#   4. bx: estimate of the regression coefficient (y ~ x)
#   5. bx_se: standard error of bx
#   6. bx_ll: lower limit of 95% confidence interval for beta x
#   7. bx_ul: upper limit of 95% confidence interval for beta x
#   8. ybar: estimate of mean of y
#   9. ybar_se: standard error of ybar
#   10. ybar_ll: lower limit of the 95% confidence interval for mean of y
#   11. ybar_ul: upper limit of the 95% confidence interval for mean of y
#   12. imp_vars: variables selected for the imputation model
#
# Notes: 
# Forward and Forward-sw are implemented in separate functions in Stata
#
# Written by R Mainzer, Jan 2021
# Revised by R Mainzer, July 2022
# Revised by R Mainzer, Feb 2023

an_simdata <- function(scenario, N, strategy_vec, m, maxit){
  
  # Set up data frame to store results
  n_strat <- length(strategy_vec)
  n_row <- 10 * n_strat
  res <- data.frame(scenario = scenario,
                    nsim = NA,
                    strategy = rep("NA", n_row), 
                    bx = NA, 
                    bx_se = NA, 
                    bx_ll = NA, 
                    bx_ul = NA,
                    ybar = NA, 
                    ybar_se = NA, 
                    ybar_ll = NA, 
                    ybar_ul = NA,
                    imp_vars = "NA")
  
  # Row for updating results
  r <- 1 
  
  # Run analysis in 200 batches of 10 each (2000 total)
  for(k in ((N - 1) * 10 + 1):(N * 10)){
    
    # Read in data set
    dat <- read.table(file.path("data", paste(scenario, "_", sprintf("%04d", k), 
                                                    ".csv", sep = "")))
    
    # Sample size
    n <- nrow(dat) 
    
    # Loop over strategies
    for(j in 1:n_strat){
      
      strategy <- strategy_vec[j]
      
      # Complete case analysis
      if(strategy == "cc"){
        
        # No imputation model
        imp_vars <- NA
        
        # Estimate of beta_x
        mod1 <- lm(y ~ x + z, data = dat)
        summ_mod1 <- summary(mod1)
        bx <- summ_mod1$coefficients[2, "Estimate"]
        bx_se <- summ_mod1$coefficients[2, "Std. Error"]
        bx_ll <- confint(mod1)[2, "2.5 %"]
        bx_ul <- confint(mod1)[2, "97.5 %"]
        
        # Estimate of mu_Y
        mod2 <- lm(y ~ 1, data = dat)
        summ_mod2 <- summary(mod2)
        ybar <- summ_mod2$coefficients[1, "Estimate"]
        ybar_se <- summ_mod2$coefficients[1, "Std. Error"]
        ybar_ll <- confint(mod2)[1, "2.5 %"]
        ybar_ul <- confint(mod2)[1, "97.5 %"]
        
      }
      
      # Full imputation model using mice
      if(strategy == "mice-full"){
        
        # Do MI
        imp_dat <- dat
        imp <- mice(dat, m = m, maxit = maxit, defaultMethod = c("norm", "logreg", "polyreg", "polr"),
                    print = FALSE)
        
        # Variables included in the imputation model for y
        imp_vars <- names(which(imp$predictorMatrix["y",] == 1))
        
        # Estimate of beta_x
        fit1 <- with(imp, lm(y ~ x + z))
        pool1 <- summary(pool(fit1), conf.int = TRUE)
        bx <- pool1[2, "estimate"]
        bx_se <- pool1[2, "std.error"]
        bx_ll <- pool1[2, "2.5 %"]
        bx_ul <- pool1[2, "97.5 %"]
        
        # Estimate of mu_y
        fit2 <- with(imp, lm(y ~ 1))
        pool2 <- summary(pool(fit2), conf.int = TRUE)
        ybar <- pool2[1, "estimate"]
        ybar_se <- pool2[1, "std.error"]
        ybar_ll <- pool2[1, "2.5 %"]
        ybar_ul <- pool2[1, "97.5 %"]
        
      }
      
      # Auxiliary variable selection using quickpred with mincor = 0.2
      if(strategy == "quickpred-pt2"){
        
        # Do MI
        imp_dat <- dat
        imp <- mice(imp_dat, m = m, maxit = maxit, defaultMethod = c("norm", "logreg", "polyreg", "polr"),
                    pred = quickpred(dat, mincor = 0.2, minpuc = 0, include = c("x", "z")),
                    print = FALSE)
        
        # Variables included in the imputation model for y
        imp_vars <- names(which(imp$predictorMatrix["y",] == 1))
        
        # Estimate of beta_x
        fit1 <- with(imp, lm(y ~ x + z))
        pool1 <- summary(pool(fit1), conf.int = TRUE)
        bx <- pool1[2, "estimate"]
        bx_se <- pool1[2, "std.error"]
        bx_ll <- pool1[2, "2.5 %"]
        bx_ul <- pool1[2, "97.5 %"]
        
        # Estimate of mu_y
        fit2 <- with(imp, lm(y ~ 1))
        pool2 <- summary(pool(fit2), conf.int = TRUE)
        ybar <- pool2[1, "estimate"]
        ybar_se <- pool2[1, "std.error"]
        ybar_ll <- pool2[1, "2.5 %"]
        ybar_ul <- pool2[1, "97.5 %"]
        
      }
      
      # Auxiliary variable selection using quickpred with mincor = 0.4
      if(strategy == "quickpred-pt4"){
        
        # Do MI
        imp_dat <- dat
        imp <- mice(imp_dat, m = m, maxit = maxit, defaultMethod = c("norm", "logreg", "polyreg", "polr"),
                    pred = quickpred(dat, mincor = 0.4, minpuc = 0, include = c("x", "z")),
                    print = FALSE)
        
        # Variables included in the imputation model for y
        imp_vars <- names(which(imp$predictorMatrix["y",] == 1))
        
        # Estimate of beta_x
        fit1 <- with(imp, lm(y ~ x + z))
        pool1 <- summary(pool(fit1), conf.int = TRUE)
        bx <- pool1[2, "estimate"]
        bx_se <- pool1[2, "std.error"]
        bx_ll <- pool1[2, "2.5 %"]
        bx_ul <- pool1[2, "97.5 %"]
        
        # Estimate of mu_y
        fit2 <- with(imp, lm(y ~ 1))
        pool2 <- summary(pool(fit2), conf.int = TRUE)
        ybar <- pool2[1, "estimate"]
        ybar_se <- pool2[1, "std.error"]
        ybar_ll <- pool2[1, "2.5 %"]
        ybar_ul <- pool2[1, "97.5 %"]
        
      }
      
      # Principal components of auxiliary variables
      if(strategy == "pcaux"){
        
        # Prepare the data
        prep_dat <- cbind(id = 1:n, dat)
        aux_dat <- select(prep_dat, c("id", starts_with("a")))
        prep_aux_dat <- prepData(rawData = aux_dat, idVars = "id", verbose = 0)
        
        # Extract principal component scores
        pc_aux_dat <- createPcAux(pcAuxData = prep_aux_dat, nComps = c(0.4, 0), doImputation = FALSE,
                                  verbose = 0)
        all_dat <- mergePcAux(pcAuxData = pc_aux_dat, rawData = prep_dat)
        imp_dat <- select(all_dat, c("y", "x", "z", starts_with("lin")))
        
        # Do MI
        imp <- mice(imp_dat, m = m, maxit = maxit, defaultMethod = c("norm", "logreg", "polyreg", "polr"),
                    print = FALSE)
        
        # Variables included in the imputation model for y
        imp_vars <- names(imp$predictorMatrix["y",])
        
        # Estimate of beta_x
        fit1 <- with(imp, lm(y ~ x + z))
        pool1 <- summary(pool(fit1), conf.int = TRUE)
        bx <- pool1[2, "estimate"]
        bx_se <- pool1[2, "std.error"]
        bx_ll <- pool1[2, "2.5 %"]
        bx_ul <- pool1[2, "97.5 %"]
        
        # Estimate of mu_y
        fit2 <- with(imp, lm(y ~ 1))
        pool2 <- summary(pool(fit2), conf.int = TRUE)
        ybar <- pool2[1, "estimate"]
        ybar_se <- pool2[1, "std.error"]
        ybar_ll <- pool2[1, "2.5 %"]
        ybar_ul <- pool2[1, "97.5 %"]
        
      }
      
      # Forward selection based on fraction of missing information
      if(strategy == "forward-fmi"){
        
        # Prepare the data
        aux_dat <- select(dat, starts_with("a"))
        aux_dat_selected <- select_aux_forwardFMI_simstudy(dat$y, aux_dat, scenario)
        imp_dat <- cbind(y = dat$y, x = dat$x, z = dat$z, aux_dat_selected$A_set)
        
        # Do MI
        imp <- mice(imp_dat, m = m, maxit = maxit, defaultMethod = c("norm", "logreg", "polyreg", "polr"),
                    print = FALSE)
        
        # Variables included in the imputation model for y
        imp_vars <- names(imp$predictorMatrix["y",])
        
        # Estimate of beta_x
        fit1 <- with(imp, lm(y ~ x + z))
        pool1 <- summary(pool(fit1), conf.int = TRUE)
        bx <- pool1[2, "estimate"]
        bx_se <- pool1[2, "std.error"]
        bx_ll <- pool1[2, "2.5 %"]
        bx_ul <- pool1[2, "97.5 %"]
        
        # Estimate of mu_y
        fit2 <- with(imp, lm(y ~ 1))
        pool2 <- summary(pool(fit2), conf.int = TRUE)
        ybar <- pool2[1, "estimate"]
        ybar_se <- pool2[1, "std.error"]
        ybar_ll <- pool2[1, "2.5 %"]
        ybar_ul <- pool2[1, "97.5 %"]
        
      }
      
      # Auxiliary variable selection using t tests
      if(strategy == "tests"){
        
        # Auxiliary variable selection
        my <- ifelse(is.na(dat$y), 1, 0)
        aux_dat <- select(dat, starts_with("a"))
        row1_pred <- sapply(aux_dat, FUN = pred_ttest, group = my, simplify = "array")
        
        # Make predictor matrix
        imp_dat <- dat
        pred <- make.predictorMatrix(imp_dat)
        pred["y", names(row1_pred)] <- row1_pred
        imp <- mice(imp_dat, m = m, maxit = maxit, defaultMethod = c("norm", "logreg", "polyreg", "polr"),
                    predictorMatrix = pred, print = FALSE)
        
        # Variables included in the imputation model for y
        imp_vars <- names(which(imp$predictorMatrix["y", ] == 1))
        
        # Estimate of beta_x
        fit1 <- with(imp, lm(y ~ x + z))
        pool1 <- summary(pool(fit1), conf.int = TRUE)
        bx <- pool1[2, "estimate"]
        bx_se <- pool1[2, "std.error"]
        bx_ll <- pool1[2, "2.5 %"]
        bx_ul <- pool1[2, "97.5 %"]
        
        # Estimate of mu_y
        fit2 <- with(imp, lm(y ~ 1))
        pool2 <- summary(pool(fit2), conf.int = TRUE)
        ybar <- pool2[1, "estimate"]
        ybar_se <- pool2[1, "std.error"]
        ybar_ll <- pool2[1, "2.5 %"]
        ybar_ul <- pool2[1, "97.5 %"]
        
      }
      
      # LASSO
      if(strategy == "LASSO"){
        
        # LASSO requires complete predictors in matrix form
        complete_dat <- dat[!is.na(dat$y), ]
        y <- complete_dat$y
        X <- as.matrix(complete_dat[, 2:ncol(dat)])
        
        # Choose model using cross validation
        cvfit <- cv.glmnet(X, y)

        # Create the row of the predictor matrix used to impute y
        pred_dat <- data.frame(var = coef(cvfit)@Dimnames[[1]], coef = matrix(coef(cvfit, s = "lambda.1se")))
        pred_dat <- pred_dat[-1,]
        
        # First entry is 0 as y is not used on the RHS of the imputation model for y
        pred_row <- c(0, ifelse(pred_dat$coef != 0, 1, 0))
        
        # Get predictor matrix and update
        imp_dat <- dat
        imp <- mice(imp_dat, maxit = 0, defaultMethod = c("norm", "logreg", "polyreg", "polr"),
                    print = FALSE)
        pred <- imp$predictorMatrix
        pred[1, ] <- pred_row
        pred[1, c("x", "z")] <- 1
        
        # Do MI
        imp <- mice(imp_dat, m = m, maxit = maxit, defaultMethod = c("norm", "logreg", "polyreg", "polr"),
                    predictorMatrix = pred, print = FALSE)
        
        # Variables included in the imputation model for y
        imp_vars <- names(which(imp$predictorMatrix["y",] == 1))
        
        # Estimate of beta_x
        fit1 <- with(imp, lm(y ~ x + z))
        pool1 <- summary(pool(fit1), conf.int = TRUE)
        bx <- pool1[2, "estimate"]
        bx_se <- pool1[2, "std.error"]
        bx_ll <- pool1[2, "2.5 %"]
        bx_ul <- pool1[2, "97.5 %"]
        
        # Estimate of mu_y
        fit2 <- with(imp, lm(y ~ 1))
        pool2 <- summary(pool(fit2), conf.int = TRUE)
        ybar <- pool2[1, "estimate"]
        ybar_se <- pool2[1, "std.error"]
        ybar_ll <- pool2[1, "2.5 %"]
        ybar_ul <- pool2[1, "97.5 %"]
        
      }
      
      # Update results
      res[r, 2] <- k
      res[r, 3] <- strategy
      res[r, 4] <- bx
      res[r, 5] <- bx_se
      res[r, 6] <- bx_ll
      res[r, 7] <- bx_ul
      res[r, 8] <- ybar
      res[r, 9] <- ybar_se
      res[r, 10] <- ybar_ll
      res[r, 11] <- ybar_ul
      res[r, 12] <- paste(imp_vars, collapse = ' ')
      
      # Update row
      r <- r + 1
      
    }
    
  }

  # Return results
  out <- res
  
}