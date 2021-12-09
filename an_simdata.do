/* 
 an_simata.do
 Program used to analyse simulated datasets using "forward" and "forward-sw" 
 auxiliary variable selection strategies

 Input:
 scenario: string, one of "basic", "extreme" or "realistic"
 batch: batch number (250 datasets = 1 batch)
 m: number of imputations
 
 Output:
 Data set containing the following columns:
	1. scenario: scenario name
	2. N: simulation number
	3. strategy: analysis strategy
	4. bx: estimate of the regression coefficient 
	5. bx_se: standard error of bx
	6. bx_ll: lower limit of 95% confidence interval for beta x
	7. bx_ul: upper limit of 95% confidence interval for beta x
	8. ybar: estimate of mean of y
	9. ybar_se: standard error of ybar
	10. ybar_ll: lower limit of the 95% confidence interval for mean of y
	11. ybar_ul: upper limit of the 95% confidence interval for mean of y	
	12. imp_vars: variables selected for the imputation model
 
 Written by R Mainzer, May 2021
*/

capture program drop an_simdata
cap snapshot erase _all

program define an_simdata

	syntax, scenario(string) batch(integer) m(integer) 

	* Change directory so _mim_[m].dta files don't cause trouble
	cd "mim_tmp/`scenario'_`batch'"
		
	* Set up postfile for results 
	capture postclose results
	postfile results str20 scenario N str20 strategy ///
		bx bx_se bx_ll bx_ul ybar ybar_se ybar_ll ybar_ul str100 imp_vars ///
		using "../../results/Stata_`scenario'_batch`batch'", replace
		
	local start = (`batch' - 1) * 250 + 1
	local stop = (`batch' * 250)
	
	forvalues k = `start'(1)`stop' {
	
		* Print progress
		di "k = " `k'
	
		* Update row for results
		local r = `r' + 1

		*cap {
		
		* Read in data set
		local k_p_10000 = 10000 + `k'
		local nsim_string = substr("`k_p_10000'", -4, .)
		di "../../data/simdat_`scenario'_`nsim_string'.csv"
		import delimited "../../data/simdat_`scenario'_`nsim_string'.csv", ///
		delimiter(space, collapse) varnames(nonames) encoding(ISO-8859-2) clear

		* Name variables correctly
		rename (v1 v2 v3) (index y x)
		rename v# a#, renumber

		* Drop first column of incorrect labels
		drop if _n == 1

		* Drop index column
		drop index
		
		* Number of auxiliary variables
		local p = c(k) - 2

		* Change variables to numeric
		destring y x a1-a`p', ignore("NA") replace
	
		* Save snapshot 
		snapshot save

		* Do MI with forward selection 
		ice y x a1-a`p', stepwise m(`m') swopts(lock(x) pe(0.05)) clear
		local vars = r(eq1)
		
		* Store estimates from mim in memory instead of on disk
		 char _dta[mim_ests] "memory"
				
		* Estimates for beta x
		mim, storebv : regress y x
				
		matrix A = e(b)
		local bx = A[1, 1] 
		
		matrix B = e(V)
		local bx_se = sqrt(B[1, 1])
		
		matrix C = e(MIM_dfvec)
		local bx_df = C[1, 1]		

		local bx_ll = `bx' - invt(`bx_df', 0.975) * `bx_se' 
		local bx_ul = `bx' + invt(`bx_df', 0.975) * `bx_se' 
		
		* Estimates for the mean of y
		mim, storebv: mean y
		
		matrix D = e(b)
		local ybar = D[1,1]
		
		matrix E = e(V)
		local ybar_se = sqrt(E[1, 1])
		
		matrix F = e(MIM_dfvec)
		local ybar_df = F[1,1]
		
		local ybar_ll = `ybar' - invt(`ybar_df', 0.975) * `ybar_se'
		local ybar_ul = `ybar' + invt(`ybar_df', 0.975) * `ybar_se'
	
		* Update results
		post results ("`scenario'") (`k') ("forward") (`bx') (`bx_se') ///
			(`bx_ll') (`bx_ul') (`ybar') (`ybar_se') (`ybar_ll') (`ybar_ul') ///
			("`vars'")

		* Restore snapshot
		snapshot restore 1
		
		* Do MI with forward-sw selection
		ice y x a1-a`p', stepwise m(`m') swopts(lock(x) forward pe(0.05) pr(0.051)) clear
		local vars = r(eq1)
	
		* Store estimates from mim in memory instead of on disk
		char _dta[mim_ests] "memory"
		 
		* Estimates for beta x
		mim, storebv : regress y x
		
		matrix A = e(b)
		local bx = A[1, 1]
		
		matrix B = e(V)
		local bx_se = sqrt(B[1,1])
		
		matrix C = e(MIM_dfvec)
		local bx_df = C[1, 1]
		
		local bx_ll = `bx' - invt(`bx_df', 0.975) * `bx_se'
		local bx_ul = `bx' + invt(`bx_df', 0.975) * `bx_se'
		
		* Estimates for the mean of y
		mim, storebv: mean y
		
		matrix D = e(b)
		local ybar = D[1,1]
		
		matrix E = e(V)
		local ybar_se = sqrt(E[1, 1])
		
		matrix F = e(MIM_dfvec)
		local ybar_df = F[1,1]
		
		local ybar_ll = `ybar' - invt(`ybar_df', 0.975) * `ybar_se'
		local ybar_ul = `ybar' + invt(`ybar_df', 0.975) * `ybar_se'

		* Update results
		post results ("`scenario'") (`k') ("forward-sw") (`bx') (`bx_se') ///
			(`bx_ll') (`bx_ul') (`ybar') (`ybar_se') (`ybar_ll') (`ybar_ul') ///
			("`vars'")
		
		* Erase snapshot
		snapshot erase 1
		
		*}
	
	}

	* Close postfile
	postclose results

end

