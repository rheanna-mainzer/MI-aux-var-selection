# MI-aux-var-selection

R and Stata code to carry out the simulation study reported in the following paper:

Mainzer, R.M., Nguyen, C.D., Carlin, J.B., Moreno-Betancur, M., White, I.R., Lee, K.J. A comparison of strategies for selecting auxiliary variables for multiple imputation. 

Files and folder structure:
---------------------------

This project comprises of two main folders: one for the simulation study (sim_study) and one for the case study (case_study). Below is a summary of the folder and file structure with descriptions.

- sim_study: 
    - data: folder containing simulation datasets and stored data generation parameters
    - functions: folder containing all user-written functions required for the simulation study
    - mim_tmp: contains folders to store temporary files produced by mim, required to prevent Stata errors when running code in parallel
    - results: folder containing simulation study results
        - analysis: folder containing analysis results 
        - figures: folder containing figures for the manuscript
        - tables: folder containing tables for the manuscript
    - 00_DGP_params.R: R script to calculate and store data generation parameters
    - 01_gr_DGP_params.R: R script to produce supplementary figures 3 - 11
    - 02_cr_simdata.R: R script to create simulation datasets
    - 03_tuning_params_methods.R: R script to investigate the FMI strategy
    - 04_gr_FMI.R: R script to produce supplementary figure 12 (FMI figure)
    - 05_an_simdata.R: R script to analyse the simulated datasets
    - 06_an_simdata_stata.do: Stata script to analyse the simulated datasets
    - 07_cr_simresults.R: R script to compile the results from the simulation study
    - 08_gr_tab_simresults.R: R script to produce figures 1 - 3, supplementary tables 2 - 4 and supplementary figures 13 - 18
- case_study 
    - analysis_data: data folder, currently empty.* 
    - functions: folder containing user-written functions for the case study
    - results:
    - 00_cr_SuppFig_cors.R
    - 01_cr_SuppFig_ORs.R
    - 02_cr_SuppTab_summary.R
    - 03_an_LSACdat.R
    - 04_an_LSACdat.do
    - 05_cr_LSACres.R

*The data that support the findings of this study are available from the Australian Government Department of Social Services (DSS). Restrictions apply to the availability of these data, which were used under license for this study. Data are available at https://dataverse.ada.edu.au with the permission of the DSS. A synthetic dataset comparable to the case study data in size and structure is available on request. 

Reproducing the results:
------------------------

The following scripts need to be run, in order, to reproduce the entire simulation study:
  - 00_DGP_params.R
  - 01_gr_DGP_params.R
  - 02_cr_simdata.R
  - 03_tuning_params_methods.R
  - 04_gr_FMI.R
  - 05_an_simdata.R
  - 06_an_simdata_stata.do
  - 07_cr_simresults.R
  - 08_gr_tab_simresults.R

To reproduce part of the simulation study at a reduced run time, the following parameters can be changed: 
  - nsim: number of simulation datasets, line 18 of "02_cr_simdata.R"
  - m: the number of imputations, line 42 of "05_an_simdata.R" and line 55 of "06_an_simdata_stata.do"

Given all intermediate results are stored, the following scripts need to be run, in any order, to reproduce figures and tables for the manuscript:
  - 01_gr_DGP_params.R
  - 04_gr_FMI.R
  - 08_gr_tab_simresults.R

The following scripts need to be run, in any order, to reproduce the case study results:
  - 00_cr_SuppFig_cors.R
  - 01_cr_SuppFig_ORs.R
  - 02_cr_SuppTab_summary.R
  - 03_an_LSACdat.R
  - 04_an_LSACdat.do
  - 05_cr_LSACres.R

Software version information: 
-----------------------------

R:

> sessionInfo()
R version 4.1.2 (2021-11-01)
Platform: x86_64-pc-linux-gnu (64-bit)
Running under: CentOS Linux 7 (Core)

Matrix products: default
BLAS:   /hpc/software/installed/R/4.1.2/lib64/R/lib/libRblas.so
LAPACK: /hpc/software/installed/R/4.1.2/lib64/R/lib/libRlapack.so

locale:
 [1] LC_CTYPE=en_US.UTF-8       LC_NUMERIC=C
 [3] LC_TIME=en_US.UTF-8        LC_COLLATE=en_US.UTF-8
 [5] LC_MONETARY=en_US.UTF-8    LC_MESSAGES=en_US.UTF-8
 [7] LC_PAPER=en_US.UTF-8       LC_NAME=C
 [9] LC_ADDRESS=C               LC_TELEPHONE=C
[11] LC_MEASUREMENT=en_US.UTF-8 LC_IDENTIFICATION=C

attached base packages:
[1] parallel  tools     stats     graphics  grDevices utils     datasets
[8] methods   base

other attached packages:
 [1] xtable_1.8-4       tidyr_1.3.0        stringr_1.5.0      reshape2_1.4.4
 [5] RColorBrewer_1.1-2 PcAux_0.0.0.9014   mitools_2.4        miceadds_3.11-6
 [9] mice_3.15.0        matrixStats_0.61.0 lattice_0.20-45    labelled_2.8.0
[13] htmlTable_2.2.1    haven_2.4.1        gridExtra_2.3      dplyr_1.1.0
[17] glmnet_4.1-2       Matrix_1.4-0       ggpubr_0.6.0       ggplot2_3.4.1
[21] gglasso_1.5

loaded via a namespace (and not attached):
 [1] splines_4.1.2     foreach_1.5.2     carData_3.0-4     cellranger_1.1.0
 [5] pillar_1.7.0      backports_1.4.1   glue_1.6.2        digest_0.6.29
 [9] ggsignif_0.6.2    checkmate_2.0.0   colorspace_2.0-2  htmltools_0.5.2
[13] plyr_1.8.6        pkgconfig_2.0.3   broom_0.7.8       purrr_1.0.1
[17] scales_1.2.1      openxlsx_4.2.4    rio_0.5.27        rlecuyer_0.3-5
[21] tibble_3.1.6      generics_0.1.2    car_3.0-11        ellipsis_0.3.2
[25] withr_2.5.0       cli_3.6.0         survival_3.2-13   magrittr_2.0.2
[29] crayon_1.5.0      readxl_1.3.1      fansi_1.0.2       rstatix_0.7.2
[33] forcats_0.5.1     foreign_0.8-82    data.table_1.14.2 hms_1.1.1
[37] lifecycle_1.0.3   munsell_0.5.0     zip_2.2.0         compiler_4.1.2
[41] rlang_1.0.6       grid_4.1.2        iterators_1.0.14  rstudioapi_0.13
[45] htmlwidgets_1.5.4 gtable_0.3.0      codetools_0.2-18  abind_1.4-5
[49] DBI_1.1.2         curl_4.3.2        R6_2.5.1          knitr_1.37
[53] fastmap_1.1.0     utf8_1.2.2        shape_1.4.6       stringi_1.7.6
[57] Rcpp_1.0.7        vctrs_0.5.2       tidyselect_1.2.0  xfun_0.29

Stata: 

. about

Stata/BE 17.0 for Unix (Linux 64-bit x86-64)
Revision 10 May 2022
Copyright 1985-2021 StataCorp LLC

Total usable memory: 377.69 GB

Stata license: Unlimited-user network, expiring 23 Aug 2023
Serial number: 401709398075
  Licensed to: MCRI User
               Murdoch Children's Research Institute
