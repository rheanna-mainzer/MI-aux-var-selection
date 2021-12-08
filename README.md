# MI-aux-var-selection

R and Stata code to carry out the simulation study reported in the following paper:

Mainzer, R.M., Nguyen, C.D., Carlin, J.B., Moreno-Betancur, M., White, I.R., Lee, K.J. A comparison of strategies for selecting auxiliary variables for multiple imputation. 

**Parameters of the data generation process (DGP):**
- basic_DGP_params
- extreme_DGP_params
- realistic_DGP_params

_These files contain an external representation of the R objects used for data generation. They can be read into R using the "load" function._

**Functions written to carry out simulation study:**
- ``calc_gamma0.R``: calculate the proportion of missing values in Y for a grid of values of gamma_0
- ``cr_simdata.R``: create and store simulated datasets
- ``an_simdata.R``: apply analysis strategies implemented in R to simulated datasets
- ``an_simdata.do``: apply analysis strategies implemented in Stata to simulated datasets
- ``select_aux_forwardFMI_simstudy.R``: implements the forward selection algorithm for the "Forward-FMI" strategy
- ``calc_FMI.R``:
- ``pred_ttest.R``: used to create the predictor matrix for ``mice`` for the "tests" analysis strategy
- ``cr_simresults.R``: calculate performance measures (bias, empirical SE, average model SE, coverage) and associated Monte Carlo SEs
- ``prop_aux_selected.R``: calculate average number (and proportion) of selected auxiliary variables in each subgroup and in total across simulation runs
