# Analysis case study
# 02_an_LSACdat
# 
# This script will apply each analysis strategy to the LSAC case study to
# estimate the effect of BMI z-score on HRQoL
#
# Written by R Mainzer, Aug 2021

# Load packages
source(file.path("..", "packages.R"), echo = TRUE)

# Load functions
source(file.path("functions", "cr_PcPlot.R"))
source(file.path("functions", "pred_ttest.R"))
source(file.path("functions", "pred_proptest.R"))

# Set seed
set.seed(31082021)

# Set MI inputs
m <- 20
maxit <- 10

# Read data --------------------------------------------------------------------

# Read in LSAC data
load(file.path("data", "LSAC_clean"))
LSAC <- LSAC_subset

# Group PedsQL items and totals
PedsQL <- grep("PedsQL", names(LSAC), value = TRUE)
PedsQL_tot <- grep("tot", PedsQL, value = TRUE)
PedsQL_item <- setdiff(PedsQL, PedsQL_tot)
PedsQL_item_w1 <- grep("w1", PedsQL_item, value = TRUE)
PedsQL_item_w2 <- grep("w2", PedsQL_item, value = TRUE)
PedsQL_item_w3 <- grep("w3", PedsQL_item, value = TRUE)
PedsQL_item_w4 <- grep("w4", PedsQL_item, value = TRUE)

# Change variable types
facts <- c("female", "IndStat", "NonEng", grep("shcn", names(LSAC), value = TRUE))
ords <- PedsQL_item
LSAC[facts] <- lapply(LSAC[facts], factor)
LSAC[ords] <- lapply(LSAC[ords], factor, ordered = TRUE)

# Group analysis variables
ana_vars <- c("PedsQL_tot_w4", "bmiz_w1", "female",  "age_w1", "IndStat", "NonEng", "sep_w1")
ana_vars_miss <- c("PedsQL_tot_w4", "bmiz_w1", "IndStat", "NonEng", "sep_w1")
ana_vars_nomiss <- c("female", "age_w1")

# Add ID variable
LSAC$id <- 1:nrow(LSAC)

# Number of observations
n <- nrow(LSAC)

# Number of individuals with data on PedsQL_tot_w4
r_Y <- n - sum(is.na(LSAC$PedsQL_tot_w4))

# Number of missing values for each analysis variable
colSums(is.na(LSAC[, ana_vars]))

# Group auxiliary variables
ghm <- grep("ghm", names(LSAC), value = TRUE)
shcn <- grep("shcn", names(LSAC), value = TRUE)
sdq <- grep("sdq", names(LSAC), value = TRUE)
mr <- grep("mr", names(LSAC), value = TRUE)
ppvt <- grep("ppvt", names(LSAC), value = TRUE)
aux_vars <- c(PedsQL_item_w1, PedsQL_item_w2, PedsQL_item_w3, ghm, shcn, sdq, mr, ppvt)

# Analyse data using each strategy ---------------------------------------------

# Set up data frame for results
res <- data.frame(strategy = rep("", 7), 
                  betax = rep(NA, 7), 
                  betax_se = rep(NA, 7), 
                  betax_ll = rep(NA, 7), 
                  betax_ul = rep(NA, 7), 
                  betax_p = rep(NA, 7),
                  muy = rep(NA, 7),
                  muy_se = rep(NA, 7),
                  muy_ll = rep(NA, 7),
                  muy_ul = rep(NA, 7),
                  muy_p = rep(NA, 7),
                  time_selec = rep(NA, 7),
                  time_mi = rep(NA, 7),
                  time_tot = rep(NA, 7),
                  stringsAsFactors = FALSE) 

# Set up list for storing predictor matrices
res_pred <- vector(mode = "list", length = 7)

# Complete case analysis -------------------------------------------------------

# Start time
start_time <- proc.time()

# Complete case analysis
fit_CCA <- lm(PedsQL_tot_w4 ~ bmiz_w1 + female + age_w1 + IndStat + NonEng + sep_w1, data = LSAC)

mean_CCA <- lm(PedsQL_tot_w4 ~ 1, data = LSAC)

# Stop time
stop_time <- proc.time() - start_time

# Update results
res[1, ] <- list(strategy = "CCA", 
                 betax = summary(fit_CCA)$coefficients[2, 1],
                 betax_se = summary(fit_CCA)$coefficients[2, 2],
                 betax_ll = confint(fit_CCA)[2, 1],
                 betax_ul = confint(fit_CCA)[2, 2],
                 betax_p = summary(fit_CCA)$coefficients[2, 4],
                 muy = summary(mean_CCA)$coefficients[1],
                 muy_se = summary(mean_CCA)$coefficients[2],
                 muy_ll = confint(mean_CCA)[1],
                 muy_ul = confint(mean_CCA)[2],
                 muy_p = summary(mean_CCA)$coefficients[4],
                 time_selec = NA,
                 time_mi = NA,
                 time_tot = stop_time)

# Remove variables
rm("start_time", "stop_time")

# Full model -------------------------------------------------------------------

# Start time
start_time <- proc.time()

# Prepare data
impData <- subset(LSAC, select = c(ana_vars, aux_vars))

# Update methods
imp <- mice(impData, maxit = 0)
pred <- imp$predictorMatrix
meth <- imp$meth
meth[c(PedsQL_item_w1, PedsQL_item_w2, PedsQL_item_w3)] <- "polr"
meth[c("bmiz_w1", "sep_w1", mr, sdq, ppvt, ghm)] <- "norm"
meth[c("IndStat", "NonEng", shcn)] <- "logreg"
meth[c("PedsQL_tot_w4")] <- "pmm"

# Do multiple imputation
# * added beside variable name indicates a ridge penalty was added
imp_full <- mice(impData, m = m, maxit = maxit, method = meth, predictorMatrix = pred)

# Fit model to each MI data set
fit_full <- with(imp_full, lm(PedsQL_tot_w4 ~ bmiz_w1 + female + age_w1 + IndStat + NonEng + sep_w1))
mean_full <- with(imp_full, lm(PedsQL_tot_w4 ~ 1))

# Pool results
pool_full <- pool(fit_full)
pool_mean_full <- pool(mean_full)

# Stop time
stop_time <- proc.time() - start_time

# Update results data frame
sum_pool_full <- summary(pool_full, conf.int = TRUE)
sum_pool_mean_full <- summary(pool_mean_full, conf.int = TRUE)
res[2, ] <- list(strategy = "full", 
                 sum_pool_full[2, "estimate"], 
                 sum_pool_full[2, "std.error"], 
                 sum_pool_full[2, "2.5 %"], 
                 sum_pool_full[2, "97.5 %"],
                 sum_pool_full[2, "p.value"],
                 sum_pool_mean_full[1, "estimate"],
                 sum_pool_mean_full[1, "std.error"],
                 sum_pool_mean_full[1, "2.5 %"],
                 sum_pool_mean_full[1, "97.5 %"],
                 sum_pool_mean_full[1, "p.value"],
                 time_selec = NA,
                 time_mi = NA,
                 time_tot = stop_time[3])

# Store predictor matrix
res_pred[[1]]$full <- pred

# Remove variables
rm("start_time", "stop_time")

# PcAux ------------------------------------------------------------------------

# Start timer
start_time1 <- proc.time()

# Prepare data
cleanData <- PcAux::prepData(rawData = data.frame(LSAC[c("id", aux_vars)]),
                             nomVars = shcn,
                             ordVars = c(PedsQL_item_w1, PedsQL_item_w2, PedsQL_item_w3),
                             idVars = "id",
                             simMode = TRUE)

# Obtain principal components
pcAuxOut <- createPcAux(pcAuxData = cleanData, nComps = c(10, 0))

# Create data for imputation
# Stick with 8 principal components when items at waves 1 - 3 are used as auxiliary variables
impData <- data.frame(LSAC[ana_vars], 
                      pcAuxOut$pcAux$lin[c("linPC1", "linPC2", "linPC3", "linPC4", 
                                           "linPC5", "linPC6", "linPC7", "linPC8")])

# Stop timer
stop_time1 <- proc.time() - start_time1

# Start timer
start_time2 <- proc.time()

# Get predictor matrix
imp <- mice(impData, maxit = 0)
meth <- imp$meth
meth[c("bmiz_w1", "sep_w1")] <- "norm"
meth[c("IndStat", "NonEng")] <- "logreg"
meth[c("PedsQL_tot_w4")] <- "pmm"
pred <- imp$pred

# Do multiple imputation 
imp_PcAux <- mice(impData, maxit = maxit, m = m, predictorMatrix = pred, method = meth)

# Fit model to each MI data set
fit_PcAux <- with(imp_PcAux, lm(PedsQL_tot_w4 ~ bmiz_w1 + female + age_w1 + IndStat + NonEng + sep_w1))
mean_PcAux <- with(imp_PcAux, lm(PedsQL_tot_w4 ~ 1))

# Pool results
pool_PcAux <- pool(fit_PcAux)
pool_mean_PcAux <- pool(mean_PcAux)

# Stop timer
stop_time2 <- proc.time() - start_time2

# Update results data frame
sum_pool_PcAux <- summary(pool_PcAux, conf.int = TRUE)
sum_pool_mean_PcAux <- summary(pool_mean_PcAux, conf.int = TRUE)
res[3, ] <- list(strategy = "PcAux", 
                 sum_pool_PcAux[2, "estimate"], 
                 sum_pool_PcAux[2, "std.error"], 
                 sum_pool_PcAux[2, "2.5 %"],
                 sum_pool_PcAux[2, "97.5 %"],
                 sum_pool_PcAux[2, "p.value"],
                 sum_pool_mean_PcAux[1, "estimate"],
                 sum_pool_mean_PcAux[1, "std.error"],
                 sum_pool_mean_PcAux[1, "2.5 %"],
                 sum_pool_mean_PcAux[1, "97.5 %"],
                 sum_pool_mean_PcAux[1, "p.value"],
                 time_selec = stop_time1[3],
                 time_mi = stop_time2[3],
                 time_tot = stop_time1[3] + stop_time2[3])

# Store predictor matrix
res_pred[[2]]$PcAux <- pred

# Remove variables
rm("start_time1", "start_time2", "stop_time1", "stop_time2")

# Quickpred Pt2 ----------------------------------------------------------------

# Start timer
start_time1 <- proc.time()

# Create data for imputation
impData <-  LSAC[c(ana_vars, aux_vars)]

# Select variables
pred <- quickpred(impData, mincor = 0.2, minpuc = 0, include = c(ana_vars))

# Stop timer
stop_time1 <- proc.time() - start_time1

# Start timer
start_time2 <- proc.time()
  
# Update methods
imp <- mice(impData, maxit = 0, predictorMatrix = pred)
meth <- imp$meth
meth[c(PedsQL_item_w1, PedsQL_item_w2, PedsQL_item_w3)] <- "polr"
meth[c("bmiz_w1", "sep_w1", mr, sdq, ppvt, ghm)] <- "norm"
meth[c("IndStat", "NonEng", shcn)] <- "logreg"
meth[c("PedsQL_tot_w4")] <- "pmm"
  
# Do multiple imputation
imp_qp2 <- mice(impData, m = m, maxit = maxit, method = meth, predictorMatrix = pred)
  
# Fit model to each MI data set
fit_qp2 <- with(imp_qp2, lm(PedsQL_tot_w4 ~ bmiz_w1 + female + age_w1 + IndStat + NonEng + sep_w1))
mean_qp2 <- with(imp_qp2, lm(PedsQL_tot_w4 ~ 1))
  
# Pool results
pool_qp2 <- pool(fit_qp2)
pool_mean_qp2 <- pool(mean_qp2)
  
# Stop time
stop_time2 <- proc.time() - start_time2
  
# Update results
sum_pool_qp2 <- summary(pool_qp2, conf.int = TRUE)
sum_pool_mean_qp2 <- summary(pool_mean_qp2, conf.int = TRUE)
res[4, ] <- list(strategy = "quickpred-pt2", 
                 sum_pool_qp2[2, "estimate"], 
                 sum_pool_qp2[2, "std.error"], 
                 sum_pool_qp2[2, "2.5 %"],
                 sum_pool_qp2[2, "97.5 %"],
                 sum_pool_qp2[2, "p.value"],
                 sum_pool_mean_qp2[1, "estimate"],
                 sum_pool_mean_qp2[1, "std.error"],
                 sum_pool_mean_qp2[1, "2.5 %"],
                 sum_pool_mean_qp2[1, "97.5 %"],
                 sum_pool_mean_qp2[1, "p.value"],
                 time_selec = stop_time1[3],
                 time_mi = stop_time2[3],
                 time_tot = stop_time1[3] + stop_time2[3])

# Store predictor matrix
res_pred[[3]]$qp2 <- pred

# Remove variables
rm("start_time1", "start_time2", "stop_time1", "stop_time2")

# Quickpred Pt4 ----------------------------------------------------------------

# Start timer
start_time1 <- proc.time()

# Create data for imputation
impData <-  LSAC[c(ana_vars, aux_vars)]

# Select variables
pred <- quickpred(impData, mincor = 0.4, minpuc = 0, include = c(ana_vars))

# Stop timer
stop_time1 <- proc.time() - start_time1

# Start timer
start_time2 <- proc.time()

# Update methods
imp <- mice(impData, maxit = 0, predictorMatrix = pred)
meth <- imp$meth
meth[c(PedsQL_item_w1, PedsQL_item_w2, PedsQL_item_w3)] <- "polr"
meth[c("bmiz_w1", "sep_w1", mr, sdq, ppvt, ghm)] <- "norm"
meth[c("IndStat", "NonEng", shcn)] <- "logreg"
meth[c("PedsQL_tot_w4")] <- "pmm"

# Do multiple imputation
imp_qp4 <- mice(impData, m = m, maxit = maxit, method = meth, predictorMatrix = pred)

# Fit model to each MI data set
fit_qp4 <- with(imp_qp4, lm(PedsQL_tot_w4 ~ bmiz_w1 + female + age_w1 + IndStat + NonEng + sep_w1))
mean_qp4 <- with(imp_qp4, lm(PedsQL_tot_w4 ~ 1))

# Pool results
pool_qp4 <- pool(fit_qp4)
pool_mean_qp4 <- pool(mean_qp4)

# Stop timer
stop_time2 <- proc.time() - start_time2

# Update results
sum_pool_qp4 <- summary(pool_qp4, conf.int = TRUE)
sum_pool_mean_qp4 <- summary(pool_mean_qp4, conf.int = TRUE)
res[5, ] <- list(strategy = "quickpred-pt4", 
                 sum_pool_qp4[2, "estimate"], 
                 sum_pool_qp4[2, "std.error"], 
                 sum_pool_qp4[2, "2.5 %"],
                 sum_pool_qp4[2, "97.5 %"],
                 sum_pool_qp4[2, "p.value"],
                 sum_pool_mean_qp4[1, "estimate"],
                 sum_pool_mean_qp4[1, "std.error"],
                 sum_pool_mean_qp4[1, "2.5 %"],
                 sum_pool_mean_qp4[1, "97.5 %"],
                 sum_pool_mean_qp4[1, "p.value"],
                 time_selec = stop_time1[3],
                 time_mi = stop_time2[3],
                 time_tot = stop_time1[3] + stop_time2[3])

# Store predictor matrix
res_pred[[4]]$qp4 <- pred

# Remove variables
rm("start_time1", "start_time2", "stop_time1", "stop_time2")

### T tests --------------------------------------------------------------------

# Start timer
start_time1 <- proc.time()

# Separate auxiliary variables into continuous and categorical
cont_aux_vars <- c(ghm, sdq, mr, ppvt)
cat_aux_vars <- c(PedsQL_item_w1, PedsQL_item_w2, PedsQL_item_w3, shcn)

# Create data for imputation
impData <- as.data.frame(LSAC[c(ana_vars, cont_aux_vars, cat_aux_vars)])
#n_aux <- length(aux_vars)

# Get default method and predictor matrix
imp <- mice(impData, maxit = 0)
meth <- imp$meth
pred <- imp$pred

# Get the new predictor matrix:
# For each outcome variable, use hypothesis tests for each auxiliary variable to test
# the "MCAR" assumption
outcomes <- c(ana_vars_miss, cont_aux_vars, cat_aux_vars)
for(i in 1:length(outcomes)){
  
  outcomes <- c(ana_vars_miss, cont_aux_vars, cat_aux_vars)
  cat("Variable = ", outcomes[i], "\n")
  
  # Temporary grouping based on missing data in variable i
  group_tmp <- is.na(impData[outcomes[i]]) * 1
  colnames(group_tmp) <- NULL
  dat_tmp <- cbind(group_tmp = group_tmp, impData[c(cont_aux_vars, cat_aux_vars)])
  
  # t tests for continuous variables
  pred_cont_tmp <- sapply(dat_tmp[c(cont_aux_vars)], pred_ttest, group = dat_tmp$group_tmp)
  
  # chi-squared tests for ordinal and binary variables
  pred_cat_tmp <- sapply(dat_tmp[c(cat_aux_vars)], pred_proptest, group = dat_tmp$group_tmp)
  
  # Update predictor matrix row
  pred[outcomes[i], cont_aux_vars] <- pred_cont_tmp
  pred[outcomes[i], cat_aux_vars] <- pred_cat_tmp
  
  # Remove dat_tmp
  rm(dat_tmp)
  
}

# Stop timer
stop_time1 <- proc.time() - start_time1

# Start timer
start_time2 <- proc.time()

# Update method
meth[c(PedsQL_item_w1, PedsQL_item_w2, PedsQL_item_w3)] <- "polr"
meth[c("bmiz_w1", "sep_w1", mr, sdq, ppvt, ghm)] <- "norm"
meth[c("IndStat", "NonEng", shcn)] <- "logreg"
meth[c("PedsQL_tot_w4")] <- "pmm"

# Do multiple imputation
imp_tests <- mice(impData, m = m, maxit = maxit, method = meth, predictorMatrix = pred)

# Fit model to each MI data set
fit_tests <- with(imp_tests, lm(PedsQL_tot_w4 ~ bmiz_w1 + female + age_w1 + IndStat + NonEng + sep_w1))
mean_tests <- with(imp_tests, lm(PedsQL_tot_w4 ~ 1))

# Pool results
pool_tests <- pool(fit_tests)
pool_mean_tests <- pool(mean_tests)

# Stop timer
stop_time2 <- proc.time() - start_time2

# Update results data frame
sum_pool_tests <- summary(pool_tests, conf.int = TRUE)
sum_pool_mean_tests <- summary(pool_mean_tests, conf.int = TRUE)
res[6, ] <- list(strategy = "tests", 
                 sum_pool_tests[2, "estimate"], 
                 sum_pool_tests[2, "std.error"], 
                 sum_pool_tests[2, "2.5 %"],
                 sum_pool_tests[2, "97.5 %"],
                 sum_pool_tests[2, "p.value"],
                 sum_pool_mean_tests[1, "estimate"],
                 sum_pool_mean_tests[1, "std.error"],
                 sum_pool_mean_tests[1, "2.5 %"],
                 sum_pool_mean_tests[1, "97.5 %"],
                 sum_pool_mean_tests[1, "p.value"],
                 time_selec = stop_time1[3],
                 time_mi = stop_time2[3],
                 time_tot = stop_time1[3] + stop_time2[3])

# Store predictor matrix
res_pred[[5]]$tests <- pred

# Remove variables
rm("start_time1", "start_time2", "stop_time1", "stop_time2")

# LASSO  -----------------------------------------------------------------------

# Start timer
start_time1 <- proc.time()

# Prepare data
impData <- subset(LSAC, select = c(ana_vars, aux_vars))

# Set default contrasts for unordered and ordered factors
options(contrasts=c('contr.sum','contr.treatment')) 

# LASSO will only work on complete data
complete_dat <- impData[complete.cases(impData), ]
target <- "PedsQL_tot_w4"
  
# LASSO requires predictors to be in a matrix form
y <- complete_dat[[target]]
X_df = complete_dat[, !(names(complete_dat) %in% target)]
X_mat <- model.matrix(~., data = X_df)[, -1]
  
# Set up groups for grouped lasso
df <- data.frame(name = colnames(X_mat))
df$ind <- ifelse(startsWith(df$name, "PedsQL") & !grepl("tot", df$name), 0, 1)
df$ind <- ifelse(startsWith(df$name, "PedsQL") & endsWith(df$name, "2"), 1, df$ind)
df$grp <- cumsum(df$ind)
  
# Fit model using group lasso with 10-fold cross validation
cvfit <- cv.gglasso(x = X_mat, y = y, group = df$grp, nfolds = 10)
s <- cvfit$lambda.1se
df$coef <- coef(cvfit, s = cvfit$lambda.1se)[-1, ]

# Find variables to include in imputation model
df$pred <- ifelse(df$coef != 0, 1, 0)
df2 <- df[df$ind == 1,]
df2$names <- colnames(X_df)
df3 <- df2[df2$pred == 1, ]

# Update imp data: include all variables in analysis model and those chosen by group lasso
impData2 <- impData[, union(ana_vars, df3$names)]

# Get default method and predictor matrix
imp <- mice(impData2, maxit = 0)
meth <- imp$meth
pred <- imp$pred
# The auxiliary variables won't be the same each time, but polr is default for ordinal variables:
# Check of the methods after running shows the appropriate methods have been used
meth[c("PedsQL_b2b_w2", "PedsQL_b2d_w2", "PedsQL_a4_w3", "PedsQL_b3c_w3")] <- "polr"
meth[c("bmiz_w1", "sep_w1", "mr_w2", "mr_w3", "sdq_w1", "sdq_w2", "sdq_w3", 
       "sdq_w4", "ppvt_w1", "ppvt_w2", "ppvt_w3", "ghm_w4")] <- "norm"
meth[c("IndStat", "NonEng")] <- "logreg"
meth[c("PedsQL_tot_w4")] <- "pmm"

# Stop timer
stop_time1 <- proc.time() - start_time1

# Start timer
start_time2 <- proc.time()

# Do imputation
imp_lasso <- mice(impData2, m = m, maxit = maxit, method = meth, predictorMatrix = pred)

# Fit model to each MI data set
fit_lasso <- with(imp_lasso, lm(PedsQL_tot_w4 ~ bmiz_w1 + female + age_w1 + IndStat + NonEng + sep_w1))
mean_lasso <- with(imp_lasso, lm(PedsQL_tot_w4 ~ 1))

# Pool results
pool_lasso <- pool(fit_lasso)
pool_mean_lasso <- pool(mean_lasso)

# Stop time
stop_time2 <- proc.time() - start_time2

# Update results data frame
sum_pool_lasso <- summary(pool_lasso, conf.int = TRUE)
sum_pool_mean_lasso <- summary(pool_mean_lasso, conf.int = TRUE)
res[7, ] <- list(strategy = "LASSO", 
                 sum_pool_lasso[2, "estimate"], 
                 sum_pool_lasso[2, "std.error"], 
                 sum_pool_lasso[2, "2.5 %"],
                 sum_pool_lasso[2, "97.5 %"],
                 sum_pool_lasso[2, "p.value"],
                 sum_pool_mean_lasso[1, "estimate"],
                 sum_pool_mean_lasso[1, "std.error"],
                 sum_pool_mean_lasso[1, "2.5 %"],
                 sum_pool_mean_lasso[1, "97.5 %"],
                 sum_pool_mean_lasso[1, "p.value"],
                 time_selec = stop_time1[3],
                 time_mi = stop_time2[3],
                 time_tot = stop_time1[3] + stop_time2[3])

# Store predictor matrix
res_pred[[6]]$LASSO <- pred

# Remove variables
rm("start_time1", "start_time2", "stop_time1", "stop_time2")

# Save results -----------------------------------------------------------------

res_out <- list(maxit = maxit, m = m, res = res)

# Save results as a data frame 
save(res_out, imp_full, imp_PcAux, imp_qp2, imp_qp4, imp_tests, imp_lasso,
     res_pred, file = file.path("results", "LSAC_results_R.Rda"))
