# Description of variables from the Longitudinal Study of Australian Children 
# case study (n = 4938)
# 01_summ_LSAC
# 
# This script will produce:
# Supplementary Table 1: summary of variables for case study
# SuppFig_corrs: Histogram of correlations between HRQoL and auxiliary variables
# SuppFig_ORs: Histogram of odds ratios between HRQoL and auxiliary variables
#
# Updated by R Mainzer, July 2022

# Load packages
source(file.path("..", "packages.R"), echo = TRUE)

# Read data --------------------------------------------------------------------

#load(file.path("data", "LSAC_clean"))
load(file.path("data", "LSAC_synth"))
LSAC_subset <- LSAC_synth

# Group variables --------------------------------------------------------------

# Group PedsQL items and totals
PedsQL <- grep("PedsQL", names(LSAC_subset), value = TRUE)
PedsQL_tot <- grep("tot", PedsQL, value = TRUE)
PedsQL_item <- setdiff(PedsQL, PedsQL_tot)
PedsQL_item_w1 <- grep("w1", PedsQL_item, value = TRUE)
PedsQL_item_w2 <- grep("w2", PedsQL_item, value = TRUE)
PedsQL_item_w3 <- grep("w3", PedsQL_item, value = TRUE)
PedsQL_item_w4 <- grep("w4", PedsQL_item, value = TRUE)

# Group auxiliary variables
ghm <- grep("ghm", names(LSAC_subset), value = TRUE)
shcn <- grep("shcn", names(LSAC_subset), value = TRUE)
sdq <- grep("sdq", names(LSAC_subset), value = TRUE)
mr <- grep("mr", names(LSAC_subset), value = TRUE)
ppvt <- grep("ppvt", names(LSAC_subset), value = TRUE)

# Group variables
Y <- "PedsQL_tot_w4"
X <- "bmiz_w1"
Z_cont <- c("age_w1", "sep_w1")
Z_bin <- c("female", "IndStat", "NonEng")
A_cont <- c(PedsQL_item_w1, PedsQL_item_w2, PedsQL_item_w3, ghm, sdq, mr, ppvt)
A_bin <- shcn

# Calculate summaries ----------------------------------------------------------

# n and p 
n <- nrow(LSAC_subset)
p <- ncol(LSAC_subset)

# Missingness indicator for Y
M_Y <- is.na(LSAC_subset["PedsQL_tot_w4"]) * 1

# Set up data frame to store results
df_miss <- data.frame(variable = "", 
                      miss_n = NA, 
                      miss_percent = NA,
                      miss_tab_res = "",
                      cor = NA, 
                      or = NA,
                      or_ll = NA,
                      or_ul = NA,
                      or_tab_res = "")

# Data summaries
for(i in 1:p){
  
  a <- names(LSAC_subset[i])
  b <- sum(is.na(LSAC_subset[i]))
  c <- round(sum(is.na(LSAC_subset[i])) / n * 100, 1)
  d <- paste0(b, " (", c, ")")
  e <- cor(LSAC_subset$PedsQL_tot_w4, LSAC_subset[i], use = "complete.obs")
  
  if(i == 1){
    f <- NA
    g <- NA
    h <- NA
    j <- "-"
  }
  
  if(i != 1){
    
    # Standardise variable
    X <- LSAC_subset[i]
    X <- X[, 1]
    X.mean <- mean(X, na.rm = TRUE)
    X.sd <- sd(X, na.rm = TRUE)
    Xz <- (X - X.mean) / X.sd
    
    # Fit logistic regression model
    mod <- glm(M_Y ~ Xz, family = binomial(link='logit'))
    
    f <- exp(summary(mod)$coefficients["Xz", 1])
    g <- round(exp(summary(mod)$coefficients["Xz", 1] - 
                     qnorm(0.975) *  summary(mod)$coefficients["Xz", 2]), 2)
    h <- round(exp(summary(mod)$coefficients["Xz", 1] + 
                     qnorm(0.975) *  summary(mod)$coefficients["Xz", 2]), 2)
    j <- paste0(round(f, 2), " (", g, ",", h, ")")
    
  }
  
  df_miss[i, ] <- list(variable = a,
                       miss_n = b,
                       miss_percent = c,
                       tab_res = d,
                       cor = e,
                       or = f,
                       or_ll = g,
                       or_ul = h,
                       or_tab_res = j)
}

# Graph correlations -----------------------------------------------------------

# Create figure
cols <- brewer.pal(9, name = "Set1") 
corr_Y <- data.frame(Y = abs(df_miss[8:92, c("cor")]))
gg1 <- ggplot(corr_Y, aes(x = Y)) +
  geom_histogram(aes(y = stat(count) / sum(count)), 
                 color = "black", fill = cols[2], 
                 breaks = seq(0, 0.7, by = 0.05)) +
  labs(x = "Correlation", y = "Relative frequency")
gg1

# Save figure
ggsave(file.path("results", "SuppFig_corrs.png"), 
       width = 7, height = 5, units = "in", plot = last_plot())
ggsave(file.path("results", "SuppFig_corrs.pdf"), 
       width = 7, height = 5, units = "in", plot = last_plot())

# Graph odds ratios ------------------------------------------------------------

# histogram
OR_Yaux <- data.frame(OR = df_miss[8:92, c("or")])
  gg1 <- ggplot(OR_Yaux, aes(x = OR)) +
  geom_histogram(aes(y = stat(count) / sum(count)), 
                 color = "black", fill = cols[2], 
                 breaks = seq(0.55, 1.35, by = 0.05)) +
  labs(x = "Odds ratio", y = "Relative frequency")
gg1

# Save figure
ggsave(file.path("results", "SuppFig_ORs.png"), 
                 width = 7, height = 5, units = "in", plot = last_plot())
ggsave(file.path("results", "SuppFig_ORs.pdf"), 
                 width = 7, height = 5, units = "in", plot = last_plot())

# Create Supplementary Table 1 -------------------------------------------------

# HRQOL, BMIz, Female, Age, IndStat, NonEng, SEP
col5 <- df_miss[c(1:7), "miss_tab_res"]
col6 <- "-"
col6 <- c(col6, df_miss[c(2:7), "cor"])
col7 <- df_miss[c(1:7), "or_tab_res"]

# PedsQL, wave 1
df_Pedsql_w1 <- df_miss[grep("^PedsQL.+w1", df_miss$variable), ]
length(df_Pedsql_w1$variable)
col5 <- c(col5, paste0(min(df_Pedsql_w1$miss_n), 
                       " (", min(df_Pedsql_w1$miss_percent), "), ",
                      max(df_Pedsql_w1$miss_n), 
                      " (", max(df_Pedsql_w1$miss_percent), ")"))
col6 <- c(col6, paste0(min(df_Pedsql_w1$cor), ", ", max(df_Pedsql_w1$cor)))
col7 <- c(col7, paste0(df_Pedsql_w1[which(df_Pedsql_w1$or == min(df_Pedsql_w1$or)), 
                                    "or_tab_res"], 
                       ", ", df_Pedsql_w1[which(df_Pedsql_w1$or == max(df_Pedsql_w1$or)), 
                                          "or_tab_res"]))

# PedsQL, wave 2
df_Pedsql_w2 <- df_miss[grep("^PedsQL.+w2", df_miss$variable), ]
length(df_Pedsql_w2$variable)
col5 <- c(col5, paste0(min(df_Pedsql_w2$miss_n), " (", min(df_Pedsql_w2$miss_percent), "), ",
                       max(df_Pedsql_w2$miss_n), " (", max(df_Pedsql_w2$miss_percent), ")"))
col6 <- c(col6, paste0(min(df_Pedsql_w2$cor), ", ", max(df_Pedsql_w2$cor)))
col7 <- c(col7, paste0(df_Pedsql_w2[which(df_Pedsql_w2$or == min(df_Pedsql_w2$or)), "or_tab_res"], 
                       ", ", df_Pedsql_w2[which(df_Pedsql_w2$or == max(df_Pedsql_w2$or)), "or_tab_res"]))

# PedsQL, wave 3
# Two variables with the maximum OR = 1.29
df_Pedsql_w3 <- df_miss[grep("^PedsQL.+w3", df_miss$variable), ]
length(df_Pedsql_w3$variable)
col5 <- c(col5, paste0(min(df_Pedsql_w3$miss_n), " (", min(df_Pedsql_w3$miss_percent), "), ",
                       max(df_Pedsql_w3$miss_n), " (", max(df_Pedsql_w3$miss_percent), ")"))
col6 <- c(col6, paste0(min(df_Pedsql_w3$cor), ", ", max(df_Pedsql_w3$cor)))
col7 <- c(col7, paste0(df_Pedsql_w3[which(df_Pedsql_w3$or == min(df_Pedsql_w3$or)), "or_tab_res"], 
                       ", ", df_Pedsql_w3[which(df_Pedsql_w3$or == max(df_Pedsql_w3$or))[2], "or_tab_res"]))

# GHM
df_ghm <- df_miss[grep("ghm", df_miss$variable), ]
col5 <- c(col5, df_ghm$miss_tab_res)
col6 <- c(col6, df_ghm$cor)
col7 <- c(col7, df_ghm$or_tab_res)

# SHCN 
df_shcn <- df_miss[grep("shcn", df_miss$variable), ]
col5 <- c(col5, df_shcn$miss_tab_res)
col6 <- c(col6, df_shcn$cor)
col7 <- c(col7, df_shcn$or_tab_res)

# SDQ
df_sdq <- df_miss[grep("sdq", df_miss$variable), ]
col5 <- c(col5, df_sdq$miss_tab_res)
col6 <- c(col6, df_sdq$cor)
col7 <- c(col7, df_sdq$or_tab_res)

# MR 
df_mr <- df_miss[grep("mr", df_miss$variable), ]
col5 <- c(col5, df_mr$miss_tab_res)
col6 <- c(col6, df_mr$cor)
col7 <- c(col7, df_mr$or_tab_res)

# PPVT
df_ppvt <- df_miss[grep("ppvt", df_miss$variable), ]
col5 <- c(col5, df_ppvt$miss_tab_res)
col6 <- c(col6, df_ppvt$cor)
col7 <- c(col7, df_ppvt$or_tab_res)

begtab <- "\\begin{tabular}[c]{@{}l@{}}"
endtab <- "\\end{tabular}"

col1 <- c("HRQoL", "BMIz", "Female", "Age", "IndStat", "NonEng", "SEP", 
          "PedsQL items", " ", " ", "GHM", " ", " ", " ", "SHCN", " ", 
          " ", " ", "SDQ", " ", " ", " ", "MR", " ", " ", "PPVT", " ", " ")
col2 <- c(4, 1, "-", 1, "-", "-", 1, 
          1, 2, 3, 
          1, 2, 3, 4, 
          1, 2, 3, 4, 
          1, 2, 3, 4,
          2, 3, 4, 
          1, 2, 3)
col3 <- c("Outcome", "Exposure", "Covariate", "Covariate", "Covariate", "Covariate", "Covariate", 
          "Auxiliary", " ", " ", 
          "Auxiliary", " ", " ", " ", 
          "Auxiliary", " ", " ", " ", 
          "Auxiliary", " ", " ", " ", 
          "Auxiliary", " ", " ", 
          "Auxiliary", " ", " ")
col4 <- c(paste0(begtab, "Continuous variable derived from PedsQL items; \\\\ range 18.75 to 100; higher scores indicate better \\\\ HRQoL", endtab),
          paste0(begtab, "BMI, standardized by age and gender; \\\\ range -4.6 to 4.9", endtab),
          "0 = male, 1 = female",
          "Child age in months; range 48 to 67",
          "0 = non-indigenous, 1 = indigenous",
          paste0(begtab, "0 = english speaking background, \\\\ 1 = non-english speaking background", endtab),
          paste0(begtab, "Standardized socioeconomic position of the \\\\ child's family; range -3.7 to 3; higher scores \\\\ indicate less disadvantage", endtab),
          paste0("5-point Likert scale response, ranges from"),
          paste0("1 = \"never a problem\" to 5 = \"always a problem\""),
          " ",
          paste0("Rating of child's current health, ranges from"),
          paste0("1 = \"Excellent\" to 5 = \"Poor\""),
          " ",
          " ",
          paste0("0 = child does not have a health condition that"), 
          paste0("requires special care, 1 = child has a health"), 
          paste0("condition that requires special care"),
          " ", 
          paste0("Scale score providing information on child"),
          paste0("behaviour; range 0 - 35; higher scores represent"),
          paste0("worse problems"),
          " ",
          paste0("Measure of child's non-verbal intelligence"),
          paste0("range 1 to 19; higher scores indicate"),
          paste0("better performance"),
          paste0("\\multirow{2}{*}{", begtab, "Measure of child's vocabulary; range 28 - 106; \\\\ higher scores indicate better performance", endtab, "}"),
          " ",
          " ")

# Store all results in a data frame
df <- data.frame("Variable" = col1, Wave = col2, Type = col3, Detail = col4, 
                 "Missing" = col5, "Cor" = col6,
                 "OR" = col7)

# Column headings
col_header_text <- c(paste0("\\textbf{Variable}"),
                     paste0("\\textbf{Wave}"),
                     paste0("\\textbf{Type}"),
                     paste0("\\textbf{Details}"),
                     paste0("\\textbf{\\begin{tabular}[c]{@{}c@{}}Missing data,\\\\ n (\\%)\\end{tabular}}"),
                     paste0("\\textbf{\\begin{tabular}[c]{@{}c@{}}Correlation with \\\\ HRQoL\\end{tabular}}"),
                     paste0("\\textbf{\\begin{tabular}[c]{@{}c@{}}OR for missingness in \\\\ HRQoL (95\\% CI)\\end{tabular}}"))
colmns <- paste0("\\multicolumn{1}{l}{", col_header_text, "}", collapse = " & ")
colmns <- paste0( colmns, "\\\\")

# Colour alternate rows
rws <- c(1, 3, 5, 7:9, 14:17, 22:24)
col <- rep("\\rowcolor[gray]{0.95}", length(rws))

# Print LaTeX code for table
out <- print(xtable(df, align = "llllllll",
             label = c("tab:LSAC"),
             caption =  c("Description of variables in the LSAC case study (n = 4983) and summary of associations with HRQoL. Correlations are pairwise Pearson correlation coefficients for the available cases. Odds ratios (ORs) are from univariate logistic regressions of the missingness indicator of HRQoL, where predictor variables are scaled to have mean 0 and variance 1")), 
      include.rownames = FALSE, 
      include.colnames = FALSE,
      booktabs = TRUE, 
      sanitize.text.function = identity, 
      add.to.row = list(pos = as.list(c(0, rws)), command = c(colmns, col)),
      file = file.path("results", "SupTable1"))

# Percentage of complete cases in all variables --------------------------------

p1 <- mice::md.pattern(LSAC_subset[, c("PedsQL_tot_w4", "bmiz_w1", "female", 
                                       "age_w1", "IndStat", "NonEng", "sep_w1")])

p2 <- mice::md.pattern(LSAC_subset, plot = FALSE)



