*------------------------------------------------------------------------------
* STATS506 Problem Set 4 Question 3
* 
* Author: Ningyuan Wang
* Date: November 21, 2019
* 
* This is a soultion to the question "are people in the US more likely to drink
* water on a weekday than a weekend day?"

*
* Data sources: 2005-2006 NHANES survey
* Day 1
* https://wwwn.cdc.gov/Nchs/Nhanes/2005-2006/DR1TOT_D.XPT
* Day 2
* https://wwwn.cdc.gov/Nchs/Nhanes/2005-2006/DR2TOT_D.XPT
* Demographic data
* https://wwwn.cdc.gov/Nchs/Nhanes/2005-2006/DEMO_D.XPT

version 16.0 
*cd  D:\506PS4\q3
log using ps4_q3.log, text replace

**a---------------------------------------------------------------------------**
// import the data
fdause https://wwwn.cdc.gov/Nchs/Nhanes/2005-2006/DEMO_D.XPT, clear
quietly compress
gsort +seqn
keep seqn riagendr ridageyr indfmpir ridexmon
save DEMO_D.dta, replace

fdause https://wwwn.cdc.gov/Nchs/Nhanes/2005-2006/DR1TOT_D.XPT, clear
quietly compress
gsort +seqn
generate byte sd = 1
save DR1TOT_D.dta, replace


fdause https://wwwn.cdc.gov/Nchs/Nhanes/2005-2006/DR2TOT_D.XPT, clear
quietly compress
gsort +seqn
generate byte sd = 2
save DR2TOT_D.dta, replace

// organize the data
// reduce variables
use DR2TOT_D.dta, clear
keep seqn sd dr2day dr2_320z dr2_330z dr2bwatz 
save DR2TOT_D.dta, replace

use DR1TOT_D.dta, clear
keep seqn sd dr1day dr1_320z dr1_330z dr1bwatz 
save DR1TOT_D.dta, replace

//merge two datasets in one
rename dr1day dr2day
rename dr1_320z dr2_320z
rename dr1_330z dr2_330z
rename dr1bwatz dr2bwatz
append using DR2TOT_D.dta
merge m:1 seqn using DEMO_D.dta
keep if _merge==3 // reduce to matched data
drop _merge


//clean up variable names and value labels
// weekday: 0-No, 1-Yes
generate weekday = 1
replace weekday = 0 if dr2day == 7 | dr2day == 1
replace weekday = . if dr2day == .
rename dr2day intake

// water and water_drank: 0 - no drank, 1-drank
generate water = dr2_320z + dr2_330z + dr2bwatz
drop dr2_320z dr2_330z dr2bwatz
generate water_drank = 1
replace water_drank = 0 if water==0
replace water_drank = . if water==.

// gender: 1-male, 2-female
*label define labgender 1 male 2 female, replace
*label values riagendr labegender
rename riagendr gender

// age
rename ridageyr age

// pir
rename indfmpir pir

// winter: 1-Yes, 2-No
*label define labwinter 1 yes 2 no, replace
*label values ridexmon labwinter
rename ridexmon winter
save df.dta, replace


**b---------------------------------------------------------------------------**
* Centering the continuous variables age and pir, around the mean value for 
* those with no missing data on any of the varibales 
* what are the mean age and pir around which our regression variables are 
* centered?
use df.dta, clear

// missing: 1-missing, 0-otherwise
generate missing = 0
local myvars "seqn intake sd winter gender age pir weekday water"
foreach var of varlist `myvars'{
	replace missing = 1 if `var'==.
	}
	
drop if missing == 1
egen age_bar = mean(age)//mean age = 27.97578

egen pir_bar = mean(pir) // mean pir = 2.395235

generate age_center = (age - age_bar) / 10
generate pir_center = pir - pir_bar
drop age pir age_bar pir_bar missing
save df_nomissing.dta, replace
collapse (mean) (age_center-pir_center)
save summarydata

* Conclusion: Before centering the variables age and pir, the mean age of the 
* participants are 27.98, and the mean pir of the participants is 2.40. After
* centering age and pir, the mean for age is 3.86e-08, for pir is -7.27e-08. 

**c---------------------------------------------------------------------------**
* Use Day 1 data to fit a logisitc model investing how the odds of drinking 
* water changes for weekdays relative to weekends, while controlling for winter,
* age, age squared, gender and pir. 
use df_nomissing.dta, clear
drop if sd==2
// logistic regression
logistic water_drank i.winter c.age_center##c.age_center i.gender pir_center ///
i.weekday, nolog

// store the results
matrix t =r(table)
putexcel set ps4_q3_c_coef.xls, replace
putexcel A3= "or" A4 = "se" A5 ="z" A6="pvalue" A7="lwr" A8= "upr" A9= "df" ///
A10 = "crit" A11="eform"
putexcel B1 = matrix(t), colnames

* Determine the average marginal effect on the probability scale
margins, dydx(*) post 

matrix b = e(b)
matrix V = e(V)
mata:
 b = st_matrix("b")
 v = st_matrix("V")
 se = sqrt( diagonal(v) )
 se = se'
 st_matrix("se", se) 
end

putexcel set ps4_q3_c_ame.xls, replace
putexcel B2=matrix(e(b))
putexcel B1="winter_1" C1="winter_2" D1="age_center" E1="gender_m" ///
F1="gender_f" G1="pir_center" H1="weekday_0" I1="weekday_1"
putexcel A1="variable" A2="est" A3="se"
putexcel B3 = matrix(se)


**d---------------------------------------------------------------------------**
use df_nomissing.dta, clear

// mixed logistic model
meglm water_drank i.winter c.age_center##c.age_center i.gender pir_center ///
i.weekday ||_all:R.seqn, family(binomial) link(logit)

// store the results
matrix b = e(b)
matrix V = e(V)
mata:
 b = st_matrix("b")
 v = st_matrix("V")
 se = sqrt( diagonal(v) )
 se = se'
 st_matrix("se", se) 
end

putexcel set ps4_q3_d_coef.xls, replace
putexcel B2=matrix(e(b))
putexcel B1="winter_1" C1="winter_2" D1="age_center" E1="age_squared" ///
F1="gender_m" G1="gender_f" H1="pir_center" I1="weekday_0" J1="weekday_1" ///
K1=" _cons" L1="var_cons" 
putexcel A1="term" A2="est" A3="se"
putexcel B3 = matrix(se) // need exponential values later in the table


// Determine the average marginal effect on the probability scale
margins, dydx(*) post

matrix b = e(b)
matrix V = e(V)
mata:
 b = st_matrix("b")
 v = st_matrix("V")
 se = sqrt( diagonal(v) )
 se = se'
 st_matrix("se", se) 
end

putexcel set ps4_q3_d_ame.xls, replace
putexcel B2=matrix(e(b))
putexcel B1="winter_1" C1="winter_2" D1="age_center" E1="gender_m" ///
F1="gender_f" G1="pir_center" H1="weekday_0" I1="weekday_1" 
putexcel A1="term" A2="est" A3="se"
putexcel B3 = matrix(se)

log close
