*------------------------------------------------------------------------------
* STATS506 Problem Set 4 Question 2
* 
* Author: Ningyuan Wang
* Date: November 25, 2019
* 
* Explore which census division has the largest disparity between urban and rural 
* areas in terms of the proportion of homes with internet access.
*
* Make a summary table to present the point estimate and confidence interval for 
* the disparity between urban and rural areas in each census.
*
* Data source: 
* https://www.eia.gov/consumption/residential/data/2015/csv/recs2015_public_v4.csv

version 16.0 
*cd  D:\506PS4\q2
log using ps4_q2.log, text replace

// import data
import delimited https://www.eia.gov/consumption/residential/data/2015/csv/recs2015_public_v4.csv, clear 
save recs2015_public_v4.dta, replace 
*----------------------------------------------------------------------- *
* Which census division has the largest disparity between urban and rural
* in terms of the proportion of homes with internet access? *
*----------------------------------------------------------------------- *

// create "internet" dataset with useful variables
use recs2015_public_v4.dta, clear
keep doeid nweight division uatyp10 internet
generate x = 100 * internet // generate new variable

generate urban = uatyp10
replace urban  = "U" if uatyp10 == "C"
drop uatyp10 // recode urban

label define div 1 "New England" 2 "Middle Atlantic" 3 "East North Central" ///
4 "West North Central" 5 "South Atlantic" 6 "East South Central" ///
7 "West South Central" 8 "Mountaion North" 9 "Mountaion South" 10 "Pacific"
label values division div // recode division

save internet.dta, replace // 

// build weights_long data
use recs2015_public_v4.dta, clear
keep doeid brrwt1-brrwt96
reshape long brrwt, i(doeid) j(repl)
save weights_long.dta, replace // 
* datasets set up -------------------------------------------------------------

// compute point estimates for difference
use internet.dta, clear
generate winternet = nweight * x
collapse (sum) winternet nweight, by(division urban) 
generate est = winternet/nweight // proportion for each unique comb

drop winternet nweight
reshape wide est, i(division) j(urban) string // point estimate for each division
generate diff = abs(estU - estR) 
format diff %3.2f 
drop estR estU
save pe.dta, replace
gsort -diff 

// compute replicate estimates for difference
use internet.dta, clear
keep doeid x division urban

merge 1:m doeid using weights_long.dta
generate rinternet = brrwt * x
collapse (sum) rinternet brrwt, by(division urban repl)
generate est_r = rinternet/brrwt 

drop rinternet brrwt
reshape wide est_r, i(division repl) j(urban) string
generate diff_r = abs(est_rU - est_rR )
format diff_r %3.2f 
drop est_rU est_rR 
save pe_r.dta, replace

// compute standard error and confidence interval
use pe_r.dta, clear
merge m:1 division using pe.dta
generate s = (diff_r - diff)^2
collapse (mean) s diff, by(division)
generate se = sqrt(s) * 2
format se %3.2f 
drop s
generate lower = diff - 1.96*se
generate upper = diff + 1.96*se
format lower upper %3.2f
save se_ci.dta, replace

export delimited division diff se lower upper using "q2_summary", replace 


log close
* conclusion: the census division of Mountaion North has the largest disparity 
* between urban and rural areas in terms of the proportion of homes with 
* internet access.



















