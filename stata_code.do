***
/*
Counterfactual Impact Evaluation of Plan S by cOAlition S
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


**** Part I - DATA PREPARATION/AGGREGATION ****
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


******* PART II - Further preprocessing

cd "$data"
use plan_s_aggr_raw, clear

*** preprocess dataset
ren openalxopen_accessoa_status oa_status
ren openalxauthors_count auth_count
ren authorshipsinstitutionscountry_c auth_countries

* remove years 2013 and 2014 due to data anomalies
drop if inlist(year, 2013, 2014) // 91,281 observations deleted

gen oa_gold = 0
replace oa_gold = 1 if strpos(oa_status,"gold") == 1
gen oa_all = 0
replace oa_all = 1 if oa_status != "closed"
gen oa_hybrid = 0
replace oa_hybrid = 1 if oa_status =="hybrid"

gen oa_green = 0
replace oa_green = 1 if oa_status =="green"

* Solve the problem of duplicates in the dataset
bys doi: gen count = _N // search for duplicates and identify them
tab count
drop count


bys doi funder: gen hv_count = _N
tab hv_count

drop hv_count 
bys doi funder: gen hv_count = _n
tab hv_count
drop if hv_count == 2 // 236 duplicates deleted
drop hv_count


gen publ2 = publisher
bys publ2: gen count = _N
tab count
replace publ2 = "other" if count < 10 // cpuld be varied

* generate encoded vars for factor variables in regs
encode funder, gen(fund)
encode publ2, gen(publ_2)


* gen treatment variables
* announcement date Plan S: September 2018
* Launch date for implementing  Plan S-aligned OA policy: 1 January 2021
	*Problem: UKRI only joined on 1 April 2022
	* See https://www.coalition-s.org/plan-s-funders-implementation/
* add NCI and adjust HHMI

cap drop T treat t_treat excl
gen T = 1 if year >= 2021 
replace T = 0 if T ==.
replace T = 0 if year == 2021 & (funder == "ukri" | funder == "hhmi")
replace T = 0 if year == 2022 & month <= 3 & funder == "ukri"

*replace T = 0 if year == 2018 & month <= 9  // 1 month in addition >> may be varied

gen treat = .
foreach fund in "fwf" "nwo" "ncn" "ukri" "hhmi" {  // treatment group
    replace treat = 1 if funder == "`fund'"
}
foreach fund in "anid" "MBIE" "china" { 	// control group
    replace treat = 0 if funder == "`fund'"
}
gen excl = 0
foreach fund in "dfg" "fwo_flanders" "nci" {
	replace excl = 1 if funder == "`fund'" // DOUBLE CHECK
}	
replace treat = . if excl == 1

gen t_treat = T * treat	 // 22,215 missing values (Dec 7)
	
* verify
tab funder if treat == 0
tab funder if treat == 1
tab funder if treat == .	
	
*** descriptive statistics ***
tab funder, sort // 85% China 	
tab year	
tab oa_status, sort
tab oa_all
unique issn // 15,177
unique publisher // 427
unique publ2  // 194
su auth_count // very few obs identified, may harm regressions >> leave out
tab t_treat
tab t_treat year
tab treat year
tab treat t_treat

tab orgs

gen treat_comp = treat
replace treat_comp = 10 if treat_comp == .
replace treat_comp = .  if treat_comp == 0
replace treat_comp = 0  if treat_comp == 10
gen t_treat_comp = T * treat_comp	 //

* Finalize and save final dataset to run regressions with
drop authorshipsauthordisplay_name corresponding_authorsauthordispl
drop corresponding_authorsinstitution auth_countries

drop orgs
compress

count // 2,575,427 obs observations in toal 
cd "$data"	
save plan_s.dta, replace	

tab t_treat if funder == "ukri"
tab t_treat year if funder == "ukri"
tab t_treat year if funder == "hhmi"
tab t_treat year
tab t_treat funder
tab t_treat_comp  funder


*********************************************************

* * *    R E G R E S S I O N     A N A L Y S I S    * * *

*********************************************************
cd "$data"
use plan_s, clear

su oa_all, d
su oa_gold, d
tab funder if treat_comp == 0
tab funder treat_comp
tab funder treat
tab funder, gen(fd)
tab funder year

tab funder if treat_comp !=.
tab funder if treat !=.
encode funder, gen(fund_en)
tab funder fund_en

foreach type in "green" "gold" "hybrid" {
	gen excl_non_`type' = 0
	replace excl_non_`type' = 1 if oa_all == 1 & oa_`type' == 0
	tab oa_status  if excl_non_`type' != 1
}

**
/*
On which level should we cluster?
journal (often done) or funder: point of treatment
BUT low number of clusters: Distortion!!
*/

***  POOLED REGRESSION (NOT PAIRWISE)  ***

*** Std Errors clustered on journal level
gen treat_cont = treat
gen t_treat_cont = t_treat				
	
foreach type in "all" "gold" "hybrid" {
	mat results = J(1, 5, .)
	foreach k in "cont" "comp" {
			reg oa_`type' T treat_`k' t_treat_`k', vce(cluster issn)
		
			scalar coef = _b[t_treat_`k']
			scalar se = _se[t_treat_`k']
			
			// Calculate 95% confidence intervals
			scalar lower_ci_95 = coef - invttail(e(df_r), 0.025) * se
			scalar upper_ci_95 = coef + invttail(e(df_r), 0.025) * se
			
			// Calculate 99% confidence intervals
			scalar lower_ci_99 = coef - invttail(e(df_r), 0.005) * se
			scalar upper_ci_99 = coef + invttail(e(df_r), 0.005) * se
			
			// Append coefficients and confidence intervals to the matrix
			matrix new_results = coef, lower_ci_95, upper_ci_95, lower_ci_99, upper_ci_99
			matrix results = results \ new_results
	}
	mat li results	
}
	
*** for exclusive Gold OA vs CA comparison
mat results = J(1, 5, .)
foreach k in "cont" "comp" {
	reg oa_gold T treat_`k' t_treat_`k', vce(cluster issn), if excl_non_gold != 1

	scalar coef = _b[t_treat_`k']
	scalar se = _se[t_treat_`k']
	
	// Calculate 95% confidence intervals
	scalar lower_ci_95 = coef - invttail(e(df_r), 0.025) * se
	scalar upper_ci_95 = coef + invttail(e(df_r), 0.025) * se
	
	// Calculate 99% confidence intervals
	scalar lower_ci_99 = coef - invttail(e(df_r), 0.005) * se
	scalar upper_ci_99 = coef + invttail(e(df_r), 0.005) * se
	
	// Append coefficients and confidence intervals to the matrix
	matrix new_results = coef, lower_ci_95, upper_ci_95, lower_ci_99, upper_ci_99
	matrix results = results \ new_results
}
mat li results	


*** for exclusive Hybrid OA vs CA comparison
mat results = J(1, 5, .)
foreach k in "cont" "comp" {
	reg oa_hybrid T treat_`k' t_treat_`k', vce(cluster issn), if excl_non_hybrid != 1

	scalar coef = _b[t_treat_`k']
	scalar se = _se[t_treat_`k']
	
	// Calculate 95% confidence intervals
	scalar lower_ci_95 = coef - invttail(e(df_r), 0.025) * se
	scalar upper_ci_95 = coef + invttail(e(df_r), 0.025) * se
	
	// Calculate 99% confidence intervals
	scalar lower_ci_99 = coef - invttail(e(df_r), 0.005) * se
	scalar upper_ci_99 = coef + invttail(e(df_r), 0.005) * se
	
	// Append coefficients and confidence intervals to the matrix
	matrix new_results = coef, lower_ci_95, upper_ci_95, lower_ci_99, upper_ci_99
	matrix results = results \ new_results
}
mat li results	



*** for exclusive Green OA vs CA comparison
mat results = J(1, 5, .)
foreach k in "cont" "comp" {
	reg oa_green T treat_`k' t_treat_`k', vce(cluster issn), if excl_non_green != 1

	scalar coef = _b[t_treat_`k']
	scalar se = _se[t_treat_`k']
	
	// Calculate 95% confidence intervals
	scalar lower_ci_95 = coef - invttail(e(df_r), 0.025) * se
	scalar upper_ci_95 = coef + invttail(e(df_r), 0.025) * se
	
	// Calculate 99% confidence intervals
	scalar lower_ci_99 = coef - invttail(e(df_r), 0.005) * se
	scalar upper_ci_99 = coef + invttail(e(df_r), 0.005) * se
	
	// Append coefficients and confidence intervals to the matrix
	matrix new_results = coef, lower_ci_95, upper_ci_95, lower_ci_99, upper_ci_99
	matrix results = results \ new_results
}
mat li results	

*********************************************************

*				 Pairwise Comparisons					*

*********************************************************
* always excluding other types of open access (except for "oa_all")
gen treat_pair = .
gen t_treat_pair = .


*** ALL OA TYPES AGAINST CLOSED ACCESS
*** Main control Group
// Create an empty matrix to store regression results for t_treat_pair

mat results = J(1, 5, .)
foreach treat_fund in "fwf" "hhmi" "ncn" "nwo" "ukri" {  // treatment group
    foreach control_fund in "anid" "MBIE" "china" {  // control group
        drop treat_pair t_treat_pair
        gen treat_pair = .
        replace treat_pair = 1 if funder == "`treat_fund'"
        replace treat_pair = 0 if funder == "`control_fund'"
        gen t_treat_pair = T * treat_pair
        qui reg oa_all T treat_pair t_treat_pair, vce(cluster issn)
        
        // Store coefficient for t_treat_pair in the scalar
        scalar coef = _b[t_treat_pair]
        scalar se = _se[t_treat_pair]
        
        // Calculate 95% confidence intervals
        scalar lower_ci_95 = coef - invttail(e(df_r), 0.025) * se
        scalar upper_ci_95 = coef + invttail(e(df_r), 0.025) * se
        
        // Calculate 99% confidence intervals
        scalar lower_ci_99 = coef - invttail(e(df_r), 0.005) * se
        scalar upper_ci_99 = coef + invttail(e(df_r), 0.005) * se
        
        // Append coefficients and confidence intervals to the matrix
        matrix new_results = coef, lower_ci_95, upper_ci_95, lower_ci_99, upper_ci_99
        matrix results = results \ new_results
    }
}

// Display the matrix of coefficients and confidence intervals
mat li results


****
* Comparison Group
mat results = J(1, 5, .)
foreach treat_fund in "fwf" "hhmi" "ncn" "nwo" "ukri" {  // treatment group
    foreach control_fund in "dfg" "fwo_flanders" "nci" {  // control group
        drop treat_pair t_treat_pair
        gen treat_pair = .
        replace treat_pair = 1 if funder == "`treat_fund'"
        replace treat_pair = 0 if funder == "`control_fund'"
        gen t_treat_pair = T * treat_pair
        reg oa_all T treat_pair t_treat_pair, vce(cluster issn)
        
        // Store coefficient and confidence intervals for t_treat_pair in the matrix
        scalar coef = _b[t_treat_pair]
        scalar se = _se[t_treat_pair]
		
        // Calculate 95% confidence intervals
        scalar lower_ci_95 = coef - invttail(e(df_r), 0.025) * se
        scalar upper_ci_95 = coef + invttail(e(df_r), 0.025) * se
        
        // Calculate 99% confidence intervals
        scalar lower_ci_99 = coef - invttail(e(df_r), 0.005) * se
        scalar upper_ci_99 = coef + invttail(e(df_r), 0.005) * se
        
        // Append coefficients and confidence intervals to the matrix
        matrix new_results = coef, lower_ci_95, upper_ci_95, lower_ci_99, upper_ci_99
        matrix results = results \ new_results
    }
}

// Display coefficients and confidence intervals for t_treat_pair for each fund vs fund_control pair
mat li results


*** ONLY >gold OA< AGAINST CLOSED ACCESS

cap drop treat_pair t_treat_pair

gen treat_pair = .
gen t_treat_pair = .

*** Main control Group
mat results = J(1, 5, .)
foreach treat_fund in "fwf" "hhmi" "ncn" "nwo" "ukri" {  // treatment group
    foreach control_fund in "anid" "MBIE" "china" {  // control group
        drop treat_pair t_treat_pair
        gen treat_pair = .
        replace treat_pair = 1 if funder == "`treat_fund'"
        replace treat_pair = 0 if funder == "`control_fund'"
        gen t_treat_pair = T * treat_pair
        reg oa_gold T treat_pair t_treat_pair, vce(cluster issn), if excl_non_gold != 1
        
        // Store coefficient for t_treat_pair in the scalar
        scalar coef = _b[t_treat_pair]
        scalar se = _se[t_treat_pair]
        
        // Calculate 95% confidence intervals
        scalar lower_ci_95 = coef - invttail(e(df_r), 0.025) * se
        scalar upper_ci_95 = coef + invttail(e(df_r), 0.025) * se
        
        // Calculate 99% confidence intervals
        scalar lower_ci_99 = coef - invttail(e(df_r), 0.005) * se
        scalar upper_ci_99 = coef + invttail(e(df_r), 0.005) * se
        
        // Append coefficients and confidence intervals to the matrix
        matrix new_results = coef, lower_ci_95, upper_ci_95, lower_ci_99, upper_ci_99
        matrix results = results \ new_results
    }
}
mat li results


**** Comparison Group
mat results = J(1, 5, .)
foreach treat_fund in "fwf" "hhmi" "ncn" "nwo" "ukri" {  // treatment
    foreach control_fund in "dfg" "fwo_flanders" "nci" {  // comparison
        drop treat_pair t_treat_pair
        gen treat_pair = .
        replace treat_pair = 1 if funder == "`treat_fund'"
        replace treat_pair = 0 if funder == "`control_fund'"
        gen t_treat_pair = T * treat_pair
        qui reg oa_gold T treat_pair t_treat_pair, vce(cluster issn), if excl_non_gold != 1
        
        // Store coefficient and confidence intervals for t_treat_pair in the matrix
        scalar coef = _b[t_treat_pair]
        scalar se = _se[t_treat_pair]
		
        // Calculate 95% confidence intervals
        scalar lower_ci_95 = coef - invttail(e(df_r), 0.025) * se
        scalar upper_ci_95 = coef + invttail(e(df_r), 0.025) * se
        
        // Calculate 99% confidence intervals
        scalar lower_ci_99 = coef - invttail(e(df_r), 0.005) * se
        scalar upper_ci_99 = coef + invttail(e(df_r), 0.005) * se
        
        // Append coefficients and confidence intervals to the matrix
        matrix new_results = coef, lower_ci_95, upper_ci_95, lower_ci_99, upper_ci_99
        matrix results = results \ new_results
    }
}
mat li results



*** ONLY >hybrid OA< AGAINST CLOSED ACCESS

cap drop treat_pair t_treat_pair

gen treat_pair = .
gen t_treat_pair = .


*** Main control Group
mat results = J(1, 5, .)
foreach treat_fund in "fwf" "hhmi" "ncn" "nwo" "ukri" {  // treatment group
    foreach control_fund in "anid" "MBIE" "china" {  // control group
        drop treat_pair t_treat_pair
        gen treat_pair = .
        replace treat_pair = 1 if funder == "`treat_fund'"
        replace treat_pair = 0 if funder == "`control_fund'"
        gen t_treat_pair = T * treat_pair
        reg oa_hybrid T treat_pair t_treat_pair, vce(cluster issn), if excl_non_hybrid != 1
        
        // Store coefficient for t_treat_pair in the scalar
        scalar coef = _b[t_treat_pair]
        scalar se = _se[t_treat_pair]
        
        // Calculate 95% confidence intervals
        scalar lower_ci_95 = coef - invttail(e(df_r), 0.025) * se
        scalar upper_ci_95 = coef + invttail(e(df_r), 0.025) * se
        
        // Calculate 99% confidence intervals
        scalar lower_ci_99 = coef - invttail(e(df_r), 0.005) * se
        scalar upper_ci_99 = coef + invttail(e(df_r), 0.005) * se
        
        // Append coefficients and confidence intervals to the matrix
        matrix new_results = coef, lower_ci_95, upper_ci_95, lower_ci_99, upper_ci_99
        matrix results = results \ new_results
    }
}
mat li results


**** Comparison Group
mat results = J(1, 5, .)
foreach treat_fund in "fwf" "hhmi" "ncn" "nwo" "ukri" {  // treatment
    foreach control_fund in "dfg" "fwo_flanders" "nci" {  // comparison
        drop treat_pair t_treat_pair
        gen treat_pair = .
        replace treat_pair = 1 if funder == "`treat_fund'"
        replace treat_pair = 0 if funder == "`control_fund'"
        gen t_treat_pair = T * treat_pair
        qui reg oa_hybrid T treat_pair t_treat_pair, vce(cluster issn), if excl_non_hybrid != 1
        
        // Store coefficient and confidence intervals for t_treat_pair in the matrix
        scalar coef = _b[t_treat_pair]
        scalar se = _se[t_treat_pair]
		
        // Calculate 95% confidence intervals
        scalar lower_ci_95 = coef - invttail(e(df_r), 0.025) * se
        scalar upper_ci_95 = coef + invttail(e(df_r), 0.025) * se
        
        // Calculate 99% confidence intervals
        scalar lower_ci_99 = coef - invttail(e(df_r), 0.005) * se
        scalar upper_ci_99 = coef + invttail(e(df_r), 0.005) * se
        
        // Append coefficients and confidence intervals to the matrix
        matrix new_results = coef, lower_ci_95, upper_ci_95, lower_ci_99, upper_ci_99
        matrix results = results \ new_results
    }
}
mat li results


****
*** ONLY >green OA< AGAINST CLOSED ACCESS
****

cap drop treat_pair t_treat_pair

gen treat_pair = .
gen t_treat_pair = .


*** Main control Group
mat results = J(1, 5, .)

foreach treat_fund in "fwf" "hhmi" "ncn" "nwo" "ukri" {  // treatment group
    foreach control_fund in "anid" "MBIE" "china" {  // control group
        drop treat_pair t_treat_pair
        gen treat_pair = .
        replace treat_pair = 1 if funder == "`treat_fund'"
        replace treat_pair = 0 if funder == "`control_fund'"
        gen t_treat_pair = T * treat_pair
        reg oa_green T treat_pair t_treat_pair, vce(cluster issn), if excl_non_green != 1
        
        // Store coefficient for t_treat_pair in the scalar
        scalar coef = _b[t_treat_pair]
        scalar se = _se[t_treat_pair]
        
        // Calculate 95% confidence intervals
        scalar lower_ci_95 = coef - invttail(e(df_r), 0.025) * se
        scalar upper_ci_95 = coef + invttail(e(df_r), 0.025) * se
        
        // Calculate 99% confidence intervals
        scalar lower_ci_99 = coef - invttail(e(df_r), 0.005) * se
        scalar upper_ci_99 = coef + invttail(e(df_r), 0.005) * se
        
        // Append coefficients and confidence intervals to the matrix
        matrix new_results = coef, lower_ci_95, upper_ci_95, lower_ci_99, upper_ci_99
        matrix results = results \ new_results
    }
}
mat li results


**** Comparison Group
mat results = J(1, 5, .)
foreach treat_fund in "fwf" "hhmi" "ncn" "nwo" "ukri" {  // treatment
    foreach control_fund in "dfg" "fwo_flanders" "nci" {  // comparison
        drop treat_pair t_treat_pair
        gen treat_pair = .
        replace treat_pair = 1 if funder == "`treat_fund'"
        replace treat_pair = 0 if funder == "`control_fund'"
        gen t_treat_pair = T * treat_pair
        qui reg oa_green T treat_pair t_treat_pair, vce(cluster issn), if excl_non_green != 1
        
        // Store coefficient and confidence intervals for t_treat_pair in the matrix
        scalar coef = _b[t_treat_pair]
        scalar se = _se[t_treat_pair]
		
        // Calculate 95% confidence intervals
        scalar lower_ci_95 = coef - invttail(e(df_r), 0.025) * se
        scalar upper_ci_95 = coef + invttail(e(df_r), 0.025) * se
        
        // Calculate 99% confidence intervals
        scalar lower_ci_99 = coef - invttail(e(df_r), 0.005) * se
        scalar upper_ci_99 = coef + invttail(e(df_r), 0.005) * se
        
        // Append coefficients and confidence intervals to the matrix
        matrix new_results = coef, lower_ci_95, upper_ci_95, lower_ci_99, upper_ci_99
        matrix results = results \ new_results
    }
}
mat li results


*********************************************************

* * *      	 Additional Descriptive Statistics      * * *

*********************************************************


* pre/post lists by OA status and funder


replace funder = "mbie" if funder == "MBIE"
replace funder = "nsfc" if funder == "china"
replace funder = "fwo" if funder == "fwo_flanders"


replace oa_status = "z_closed" if oa_status == "closed"
local funders = "anid dfg fwf fwo hhmi mbie nci ncn nsfc nwo ukri"
foreach k of local funders {
	di "This is funder `k'"
	tab oa_status T if funder == "`k'"
	di "This was funder `k'"
}

tab oa_status T // all funders together


