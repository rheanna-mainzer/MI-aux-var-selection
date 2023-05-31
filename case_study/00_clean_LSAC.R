# 00_clean_LSAC
# Clean LSAC data

# Load packages
source(file.path("..", "packages.R"), echo = TRUE)

# Read data -------------------------------------------------------------------

# Read in LSAC data
LSAC <- read_dta(file.path("data", "LSAC.dta"))
LSAC <- remove_labels(LSAC)
LSAC <- data.frame(LSAC)
attach(LSAC)

# Group PedsQL items and totals
PedsQL <- grep("PedsQL", names(LSAC), value = TRUE)
PedsQL_tot <- grep("tot", PedsQL, value = TRUE)
PedsQL_item <- setdiff(PedsQL, PedsQL_tot)
PedsQL_item_w1 <- grep("w1", PedsQL_item, value = TRUE)
PedsQL_item_w2 <- grep("w2", PedsQL_item, value = TRUE)
PedsQL_item_w3 <- grep("w3", PedsQL_item, value = TRUE)

# Group main variables
Y <- "PedsQL_tot_w4"
X <- "bmiz_w1"
Z_cont <- c("age_w1", "sep_w1")
Z_bin <- c("female", "IndStat", "NonEng")

# Group auxiliary variables
ghm <- grep("ghm", names(LSAC), value = TRUE)
shcn <- grep("shcn", names(LSAC), value = TRUE)
sdq <- grep("sdq", names(LSAC), value = TRUE)
mr <- grep("mr", names(LSAC), value = TRUE)
ppvt <- grep("ppvt", names(LSAC), value = TRUE)

# Group continuous auxiliary variables
A_cont <- c(PedsQL_item_w1, PedsQL_item_w2, PedsQL_item_w3, ghm, shcn, sdq, mr, ppvt)

# Subset LSAC data
LSAC_subset <- LSAC[c(Y, X, Z_cont, Z_bin, A_cont)]
save(LSAC_subset, file = file.path("data", "LSAC_clean"))
#write_dta(data = LSAC_subset, path = file.path("data", "LSAC_clean"))
