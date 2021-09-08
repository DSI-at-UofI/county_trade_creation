clear all
/*
Noé J Nava (noejn2@illinois.edu)
https://noejn2.github.io/

Script checks that the formula I am using in R brings the same results as the command predict in Stata.

Yes, it does.


*/

cd "C:\Users\noejn2\OneDrive - University of Illinois - Urbana\Drive\Projects\county_trade_creation"

/* Panel regression Set-up*/
frame copy default df_2017
frame change df_2017
use "data\dyadic_state_2017"
rename head_mayer_2017 head_mayer_2012

* Mergingg with inter-state trade
merge 1:1 orig_ini dest_ini using "data\cfs_cleaned_data\cfs_2017_sctg02_collapsed", keepusing(value notrade)
drop if _merge == 2 
drop _merge
rename value trade
replace trade = trade/1000000
gen year = 2017

frame create df_2012
frame change df_2012
use "data\dyadic_state_2012", clear

merge 1:1 orig_ini dest_ini using "data\cfs_cleaned_data\cfs_2012_sctg02_collapsed", keepusing(value)
drop if _merge == 2 
drop _merge
rename value trade
replace trade = trade/1000000
gen year = 2012

frameappend df_2017, drop
/* End: Panel regression Set-up*/

* Estimation part:
**** IMPORTANT: If climate region is not present, then run R script -- ~/code/clm_region.R in the trade_interpolation repo ---
egen orig_id = group(orig)		// Numerical IDs for orig_state
egen dest_id = group(dest)		// Numerical IDs for dest_state
egen pair_id = group(orig dest) // Numerical IDs for dyadic
egen time_id = group(year)  // Numerical IDs for years
egen clm_orig_time_id = group(clm_region_i year) // Numerical IDs for climate region at origin for each year
egen clm_dest_time_id = group(clm_region_j year) // Numerical IDs for climate region at destination for each year

gen one = 1 // This is used when absorbing the constant
gen intra = orig == dest	                        // Intra flow indicator
replace distance = distance + head_mayer_2012       // Including internal distance
replace distance = log(distance)
gen intra_distance = intra*distance

* I will drop all non-sctg02
drop gdp_id001_i emp_id001_i gdp_id001_j emp_id001_j gdp_id069_i emp_id069_i gdp_id069_j emp_id069_j gdp_id100_i emp_id100_i gdp_id100_j emp_id100_j gdp_id103_i emp_id103_i gdp_id103_j emp_id103_j gdp_id107_i emp_id107_i gdp_id107_j emp_id107_j gdp_id003_i
*** Variables will be re-scalled by 1,000,000 such that coefficients are large ****
global vars gdp_id011_j gdp_id163_j gdp_id002_j gdp_id063_j gdp_id064_j gdp_id068_j gdp_id106_j gdp_id065_j gdp_id066_j gdp_id078_j gdp_id093_j
gen gdp_j = 0
foreach var in $vars { /* IMPORTANT: The adjustment by /1000000 is done here, not need to do it in other scripts. */
	replace `var' = `var'/1000000
	replace gdp_j = gdp_j + `var'
}

replace sales_i = log(sales_i)
replace gdp_j = log(gdp_j)

*** Variables now have labesl as suggested by Bill ***
label var sales_i "Sales"
label var acres_i "Acreage"

label var distance "Distance"
label var contiguity "Contiguity"

label var gdp_id011_j "Ranching (GDP)"
label var gdp_id163_j "Chemicals manufacturing (GDP)"
label var gdp_id002_j "Grain farming (GDP)"
label var gdp_id063_j "Dog and cat food manufacturing (GDP)"
label var gdp_id064_j "Other animal food manufacturing (GDP)"
label var gdp_id068_j "Corn milling  (GDP)"
label var gdp_id106_j "Breweries  (GDP)"
label var gdp_id065_j "Flour milling (GDP)"
label var gdp_id066_j "Rice milling (GDP)"
label var gdp_id078_j "Specialties manufacturing (GDP)"
label var gdp_id093_j "Bread manufacturing (GDP)"
label var gdp_j "GDP"

* distance and contiguity are also included
* FE structire is climate zone with time [exporter and importer] plus intra plus time
*tab clm_orig_time_id, gen(clm_orig_time_FE)
*tab clm_dest_time_id, gen(clm_dest_time_FE)
*tab intra, gen(intra_FE)
ppmlhdfe trade distance sales_i gdp_j, a(one clm_orig_time_id clm_dest_time_id intra contiguity) cl(orig_id dest_id) d(sum_FE)

predict y_predict
gen y_hat = exp(-.4714657*distance  +  .6398341 *sales_i + .6620246 *gdp_j + sum_FE - 11.27216)
* end