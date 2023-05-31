/*______________________________________________________________________________

	Do file: 03_an_LSACdat.do

	ANALYSIS MODEL
	Analyis based on a logistic regression of PEDSQL on BMI at wave 4
	http://pediatrics.aappublications.org/content/128/4/677
	
	* THE ANALYSIS MODEL USES THE FOLLOWING VARIABLES
	
	PedsQL_tot_w4: PEDSQL total scale score at 12/13 years (HRQoL outcome) 
	bmiz_w1: BMI z-score at 4/5 years (exposure) 
	
	Covariates:
	female: gender (female = 1) 
	age_w1: child's age at wave 1
	IndStat: child's indigenous status
	NonEng: Mother's main language is not English
	sep_w1: Socieoeconomic status at wave 1
	
	Execute using "do [...], nostop" to allow all errors to be recorded in the 
	log file
_______________________________________________________________________________*/

* Set up do file 
capture log close
version 17
clear all
set more off
log using "03_an_LSACdat.log", text replace

* Set up postfile
*capture postfile results
*if _rc {
	postfile results str20 (strategy) OR SE ll ul p using "results/LSAC_results_stata", replace
*}

* Set seed
set seed 3032189

/*______________________________________________________________________________
Variable summaries
_______________________________________________________________________________*/
*use "data/LSAC.dta", clear
use "data/LSAC_synth.dta", clear
	
codebook PedsQL_tot_w4 bmiz_w1 female age_w1 IndStat NonEng sep_w1


/*______________________________________________________________________________
* COMPLETE CASE ANALYSIS
_______________________________________________________________________________*/
*use "data/LSAC.dta", clear
use "data/LSAC_synth.dta", clear
	
regress PedsQL_tot_w4 bmiz_w1 i.female age_w1 i.IndStat i.NonEng sep_w1

matrix res = r(table)
local OR = res["b", "bmiz_w1"]
local SE = res["se", "bmiz_w1"]
local ll = res["ll", "bmiz_w1"]
local ul = res["ul", "bmiz_w1"]
local p = res["pvalue", "bmiz_w1"]

post results ("CCA") (`OR') (`SE') (`ll') (`ul') (`p') 


/*______________________________________________________________________________
FULL MODEL
_______________________________________________________________________________*/
*use "data/LSAC.dta", clear
use "data/LSAC_synth.dta", clear

mi set flong
mi register imputed PedsQL_tot_w4 bmiz_w1 IndStat NonEng sep_w1 ///
					PedsQL_a1_w1-PedsQL_b3c_w1 ///
					PedsQL_b1a_w2-PedsQL_b3c_w2 ///
					PedsQL_a1_w3-PedsQL_b3c_w3 ///
					ghm_w* shcn_w* sdq_w* mr_w* ppvt_w*
mi register regular female age_w1

mi impute chained (regress) bmiz_w1 sep_w1 mr_w* sdq_w* ppvt_w* ghm_w*  ///
					  (logit) IndStat NonEng shcn_w* /// 
					  (ologit) PedsQL_a1_w1-PedsQL_b3c_w1  ///
							   PedsQL_b1a_w2-PedsQL_b3c_w2 ///
							   PedsQL_a1_w3-PedsQL_b3c_w3 ///
					  (pmm, knn(5)) PedsQL_tot_w4 /// 
					  = female age_w1, add(20) noisily augment

/*
ERROR MESSAGE:

Note: 5 observations completely determined.  Standard errors questionable.
convergence not achieved
ologit failed to converge on observed data
error occurred during imputation of bmiz_w1 sep_w1 mr_w2 mr_w3 mr_w4 sdq_w1 sdq_w2 sdq_w3 sdq_w4 ppvt_w1
ppvt_w2 ppvt_w3 ghm_w1 ghm_w2 ghm_w3 ghm_w4 IndStat NonEng shcn_w1 shcn_w2 shcn_w3 shcn_w4 PedsQL_a1_w1
PedsQL_a2_w1 PedsQL_a3_w1 PedsQL_a4_w1 PedsQL_a5_w1 PedsQL_a6_w1 PedsQL_a7_w1 PedsQL_a8_w1 PedsQL_b1a_w1
PedsQL_b1b_w1 PedsQL_b1c_w1 PedsQL_b1d_w1 PedsQL_b1e_w1 PedsQL_b2a_w1 PedsQL_b2b_w1 PedsQL_b2c_w1
PedsQL_b2d_w1 PedsQL_b2e_w1 PedsQL_b3a_w1 PedsQL_b3b_w1 PedsQL_b3c_w1 PedsQL_b1a_w2 PedsQL_b1b_w2
PedsQL_b1c_w2 PedsQL_b1d_w2 PedsQL_b1e_w2 PedsQL_b2a_w2 PedsQL_b2b_w2 PedsQL_b2c_w2 PedsQL_b2d_w2
PedsQL_b2e_w2 PedsQL_b3e_w2 PedsQL_b3f_w2 PedsQL_b3g_w2 PedsQL_b3b_w2 PedsQL_b3c_w2 PedsQL_a1_w3
PedsQL_a2_w3 PedsQL_a3_w3 PedsQL_a4_w3 PedsQL_a5_w3 PedsQL_a6_w3 PedsQL_a7_w3 PedsQL_a8_w3 PedsQL_b1a_w3
PedsQL_b1b_w3 PedsQL_b1c_w3 PedsQL_b1d_w3 PedsQL_b1e_w3 PedsQL_b2a_w3 PedsQL_b2b_w3 PedsQL_b2c_w3
PedsQL_b2d_w3 PedsQL_b2e_w3 PedsQL_b3e_w3 PedsQL_b3f_w3 PedsQL_b3g_w3 PedsQL_b3b_w3 PedsQL_b3c_w3
PedsQL_tot_w4 on m = 1
r(430);	

*/				  
		  

/*______________________________________________________________________________
FORWARD SELECTION WITH ICE
  - cmd( : regress) and match() combined indicate PMM
  - group PedsQL items to include all dummies in selected models
_______________________________________________________________________________*/
*use "data/LSAC.dta", clear
use "data/LSAC_synth.dta", clear

* Shorten variable names to avoid ice problem with including groups
rename PedsQL_a*_w* PQL_a*_*
rename PedsQL_b*_w* PQL_b*_*

ice PedsQL_tot_w4 bmiz_w1 IndStat NonEng sep_w1 i.female age_w1 ///
	o.PQL_a1_1 o.PQL_a2_1 o.PQL_a3_1 o.PQL_a4_1 o.PQL_a5_1 ///
	o.PQL_a6_1 o.PQL_a7_1 o.PQL_a8_1 o.PQL_b1a_1 o.PQL_b1b_1 ///
	o.PQL_b1c_1 o.PQL_b1d_1 o.PQL_b1e_1 o.PQL_b2a_1 o.PQL_b2b_1 ///
	o.PQL_b2c_1 o.PQL_b2d_1 o.PQL_b2e_1 o.PQL_b3a_1 o.PQL_b3b_1 ///
	o.PQL_b3c_1 ///
	o.PQL_a1_2 o.PQL_a2_2 o.PQL_a3_2 o.PQL_a4_2 o.PQL_a5_2 ///
	o.PQL_a6_2 o.PQL_a7_2 o.PQL_a8_2 o.PQL_b1a_2 o.PQL_b1b_2 ///
	o.PQL_b1c_2 o.PQL_b1d_2 o.PQL_b1e_2 o.PQL_b2a_2 o.PQL_b2b_2 ///
	o.PQL_b2c_2 o.PQL_b2d_2 o.PQL_b2e_2 o.PQL_b3e_2 o.PQL_b3f_2 ///
	o.PQL_b3g_2 o.PQL_b3b_2 o.PQL_b3c_2 ///
	o.PQL_a1_3 o.PQL_a2_3 o.PQL_a3_3 o.PQL_a4_3 o.PQL_a5_3 o.PQL_a6_3 ///
	o.PQL_a7_3 o.PQL_a8_3 o.PQL_b1a_3 o.PQL_b1b_3 o.PQL_b1c_3 o.PQL_b1d_3 ///
	o.PQL_b1e_3 o.PQL_b2a_3 o.PQL_b2b_3 o.PQL_b2c_3 o.PQL_b2d_3 o.PQL_b2e_3 ///
	o.PQL_b3e_3 o.PQL_b3f_3 o.PQL_b3g_3 o.PQL_b3b_3 o.PQL_b3c_3 ///
	ghm_w* shcn_w* sdq_w* mr_w* ppvt_w*, ///
	stepwise ///
	m(20) ///
	match(PedsQL_tot_w4) ///
	swopts(lock(PedsQL_tot_w4 bmiz_w1 IndStat NonEng sep_w1 female age_w1) ///
		   group(i.PQL_a1_1, i.PQL_a2_1, i.PQL_a3_1, i.PQL_a4_1, ///
				 i.PQL_a5_1, i.PQL_a6_1, i.PQL_a7_1, i.PQL_a8_1, ///
				 i.PQL_b1a_1, i.PQL_b1b_1, i.PQL_b1c_1, i.PQL_b1d_1, ///
				 i.PQL_b1e_1, i.PQL_b2a_1, i.PQL_b2b_1, i.PQL_b2c_1, ///
				 i.PQL_b2d_1, i.PQL_b2e_1, i.PQL_b3a_1, i.PQL_b3b_1, ///
				 i.PQL_b3c_1, /// 
				 i.PQL_a1_2, i.PQL_a2_2, i.PQL_a3_2, i.PQL_a4_2, i.PQL_a5_2, ///
				 i.PQL_a6_2, i.PQL_a7_2, i.PQL_a8_2, i.PQL_b1a_2, i.PQL_b1b_2, ///
				 i.PQL_b1c_2, i.PQL_b1d_2, i.PQL_b1e_2, i.PQL_b2a_2, i.PQL_b2b_2, ///
				 i.PQL_b2c_2, i.PQL_b2d_2, i.PQL_b2e_2, i.PQL_b3e_2, i.PQL_b3f_2, ///
				 i.PQL_b3g_2, i.PQL_b3b_2, i.PQL_b3c_2, ///
				 i.PQL_a1_3, i.PQL_a2_3, i.PQL_a3_3, i.PQL_a4_3, i.PQL_a5_3, ///
				 i.PQL_a6_3, i.PQL_a7_3, i.PQL_a8_3, i.PQL_b1a_3, i.PQL_b1b_3, ///
				 i.PQL_b1c_3, i.PQL_b1d_3, i.PQL_b1e_3, i.PQL_b2a_3, i.PQL_b2b_3, ///
				 i.PQL_b2c_3, i.PQL_b2d_3, i.PQL_b2e_3, i.PQL_b3e_3, i.PQL_b3f_3, ///
				 i.PQL_b3g_3, i.PQL_b3b_3, i.PQL_b3c_3) ///
		   pe(0.05) show) ///
	saving("MIdat_forward") replace 

/* 
ERROR: occured during selection of prediction equations

Return code 430 (convergence not achieved)

From documentation:
  You have estimated a maximum likelihood model, and Stata's
  maximization procedure failed to converge to a solution;
  see help maximize.  Check if the model is identified.
*/

	
/*______________________________________________________________________________
FORWARD STEPWISE SELECTION WITH ICE
  - cmd( : regress) and match() combined indicated PMM for items
  - group PedsQL items to include all dummies in selected models
  - pr() specifies the significance level for removal from the model
_______________________________________________________________________________*/
*use "data/LSAC.dta", clear
use "data/LSAC_synth.dta", clear

* Shorten variable names to avoid ice problem with including groups
rename PedsQL_a*_w* PQL_a*_*
rename PedsQL_b*_w* PQL_b*_*

ice PedsQL_tot_w4 bmiz_w1 IndStat NonEng sep_w1 i.female age_w1 ///
	o.PQL_a1_1 o.PQL_a2_1 o.PQL_a3_1 o.PQL_a4_1 o.PQL_a5_1 ///
	o.PQL_a6_1 o.PQL_a7_1 o.PQL_a8_1 o.PQL_b1a_1 o.PQL_b1b_1 ///
	o.PQL_b1c_1 o.PQL_b1d_1 o.PQL_b1e_1 o.PQL_b2a_1 o.PQL_b2b_1 ///
	o.PQL_b2c_1 o.PQL_b2d_1 o.PQL_b2e_1 o.PQL_b3a_1 o.PQL_b3b_1 ///
	o.PQL_b3c_1 ///
	o.PQL_a1_2 o.PQL_a2_2 o.PQL_a3_2 o.PQL_a4_2 o.PQL_a5_2 ///
	o.PQL_a6_2 o.PQL_a7_2 o.PQL_a8_2 o.PQL_b1a_2 o.PQL_b1b_2 ///
	o.PQL_b1c_2 o.PQL_b1d_2 o.PQL_b1e_2 o.PQL_b2a_2 o.PQL_b2b_2 ///
	o.PQL_b2c_2 o.PQL_b2d_2 o.PQL_b2e_2 o.PQL_b3e_2 o.PQL_b3f_2 ///
	o.PQL_b3g_2 o.PQL_b3b_2 o.PQL_b3c_2 ///
	o.PQL_a1_3 o.PQL_a2_3 o.PQL_a3_3 o.PQL_a4_3 o.PQL_a5_3 o.PQL_a6_3 ///
	o.PQL_a7_3 o.PQL_a8_3 o.PQL_b1a_3 o.PQL_b1b_3 o.PQL_b1c_3 o.PQL_b1d_3 ///
	o.PQL_b1e_3 o.PQL_b2a_3 o.PQL_b2b_3 o.PQL_b2c_3 o.PQL_b2d_3 o.PQL_b2e_3 ///
	o.PQL_b3e_3 o.PQL_b3f_3 o.PQL_b3g_3 o.PQL_b3b_3 o.PQL_b3c_3 ///
	ghm_w* shcn_w* sdq_w* mr_w* ppvt_w*, ///
	stepwise ///
	m(2) ///
	match(PedsQL_tot_w4) ///
	swopts(lock(PedsQL_tot_w4 bmiz_w1 IndStat NonEng sep_w1 female age_w1) ///
		   group(i.PQL_a1_1, i.PQL_a2_1, i.PQL_a3_1, i.PQL_a4_1, ///
				 i.PQL_a5_1, i.PQL_a6_1, i.PQL_a7_1, i.PQL_a8_1, ///
				 i.PQL_b1a_1, i.PQL_b1b_1, i.PQL_b1c_1, i.PQL_b1d_1, ///
				 i.PQL_b1e_1, i.PQL_b2a_1, i.PQL_b2b_1, i.PQL_b2c_1, ///
				 i.PQL_b2d_1, i.PQL_b2e_1, i.PQL_b3a_1, i.PQL_b3b_1, ///
				 i.PQL_b3c_1, /// 
				 i.PQL_a1_2, i.PQL_a2_2, i.PQL_a3_2, i.PQL_a4_2, i.PQL_a5_2, ///
				 i.PQL_a6_2, i.PQL_a7_2, i.PQL_a8_2, i.PQL_b1a_2, i.PQL_b1b_2, ///
				 i.PQL_b1c_2, i.PQL_b1d_2, i.PQL_b1e_2, i.PQL_b2a_2, i.PQL_b2b_2, ///
				 i.PQL_b2c_2, i.PQL_b2d_2, i.PQL_b2e_2, i.PQL_b3e_2, i.PQL_b3f_2, ///
				 i.PQL_b3g_2, i.PQL_b3b_2, i.PQL_b3c_2, ///
				 i.PQL_a1_3, i.PQL_a2_3, i.PQL_a3_3, i.PQL_a4_3, i.PQL_a5_3, ///
				 i.PQL_a6_3, i.PQL_a7_3, i.PQL_a8_3, i.PQL_b1a_3, i.PQL_b1b_3, ///
				 i.PQL_b1c_3, i.PQL_b1d_3, i.PQL_b1e_3, i.PQL_b2a_3, i.PQL_b2b_3, ///
				 i.PQL_b2c_3, i.PQL_b2d_3, i.PQL_b2e_3, i.PQL_b3e_3, i.PQL_b3f_3, ///
				 i.PQL_b3g_3, i.PQL_b3b_3, i.PQL_b3c_3) ///
		   pe(0.05) pr(0.05001) show) ///
	saving("MIdat_forward") replace 

/* 
ERROR: occured during selection of prediction equations

Return code 430 (convergence not achieved)

From documentation:
  You have estimated a maximum likelihood model, and Stata's
  maximization procedure failed to converge to a solution;
  see help maximize.  Check if the model is identified.
*/


*______________________________________________________________________________


* Close postfile
postclose results
	
* Close log
log close
	
exit	