* 06_an_simdata_stata.do
* 
* Analyse simulated data sets for a given scenario
* Analysis strategies applied in Stata are forward and forward-sw in the ice function
*
* Written by R Mainzer, May 2021
* ______________________________________________________________________________

* Set working directory
* cd ...

* Load function do file
do "functions/an_simdata.do"

* Read inputs from command line
args N k
* N = batch number (200 batches)
* k = scenario number (6 scenarios)

* Set seed
* Use the method described here: 
* https://www.stata.com/features/overview/stream-random-number-generator/
local batches = 200
local stream_stop = ((`k' - 1) * `batches' + `N')
forvalues s = 1(1)`stream_stop' {
	di `s'
	* For each batch and scenario, use a different rngstream
	set rngstream `s' 
}
set seed 375641
*local state = c(rngstate) 
*di "`state'"

* Inputs to function
*local scenario = "basic"
if `k' == 1 {
	local scenario = "n250_p25_pmy30_mod"
}
if `k' == 2 {
	local scenario = "n250_p83_pmy30_mod"
}
if `k' == 3 {
	local scenario = "n1000_p100_pmy30_mod"
}
if `k' == 4 {
	local scenario = "n1000_p333_pmy30_mod"
}
if `k' == 5 {
	local scenario = "n1000_p100_pmy50_mod"
} 
if `k' == 6 {
	local scenario = "n1000_p100_pmy30_str"
}

local m = 30

* Run function
an_simdata, scenario(`scenario') batch(`N') m(`m')

