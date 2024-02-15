***
/*
Evaluation of Plan S

A joint project of scidecode GbR and W. Benedikt Schmal
WP2 - Counterfactual Impact Evaluation
Author: W. Benedikt Schmal

*/


**** SETTINGS
set more off
cap log close
clear all
set seed 1896

global data  = "YOUR PATH\data"
global plots = "YOUR PATH\plots"


**** DATA PREPARATION ****
* Import datasets
cd "$data"
* Prepare individual datasets
local funders = "anid china dfg fwf fwo_flanders hhmi MBIE ncn nwo ukri nci" // additional funder NCI (National Cancer Institute added). Jan 5, 2024
	* follow self-assigned labels of the downloaded datasets
foreach k of local funders {
	if ("`k'" == "china"){
		forval j = 13/23 { // remove 24 for "all time"
			di "current funder: `k' and year = 20`j'"
			import delim `k'_`j', clear
			gen funder = "`k'"
			ren published_date date
			split date, parse(-)
				destring date1, gen(year)
				destring date2, gen(month)
				destring date3, gen(day)
			drop date*
			drop conceptsdisplay_name // for the moment for easier data handling
			compress
			save `k'_`j'.dta, replace
		}
		use `k'_13, clear
		forval m = 14/23 {
			append using `k'_`m', force
		}
	}	
	else {
		di "current funder: `k'"
		import delim `k', clear
		gen funder = "`k'"
		ren published_date date
		split date, parse(-)
			destring date1, gen(year)
			destring date2, gen(month)
			destring date3, gen(day)
		drop date*
		drop conceptsdisplay_name // for the moment for easier data handling
	}
	compress
	save `k'.dta, replace
	di "current funder: `k' DONE"
}


* aggregate datasets

local fnd2 = "china anid dfg fwf fwo_flanders hhmi MBIE ncn nwo ukri nci"

foreach k of local fnd2 {
	if ("`k'" == "china"){
		use `k', clear
		gen hv_n = 1
	}
	else {
		append using `k', force
		replace hv_n = hv_n+1
		mean hv_n
		di ">>> NOW WE HAVE FUNDER `k' ADDED <<<"
	}	
}

save plan_s_aggr_raw, replace

