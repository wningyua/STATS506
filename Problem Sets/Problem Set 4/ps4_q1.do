*------------------------------------------------------------------------------
* STATS506 Problem Set 4 Question 1
* 
* Author: Ningyuan Wang
* Date: November 21, 2019
* 
* Fit linear mixed effect models exploring how each curvature meaasure 
* differs by condition. 
* Fit one model per curvature measure with "Condition" as the only fixed effect
* and include a random intercept for each subject and additional random 
* intercepts for each "Exemplar".
* Use a log transformed curvature measure as the response
* Compare the relative coeeficients between models

// Set up
version 16.0 
*cd  D:\506PS4\q1
log using ps4_q1.log, text replace
import delimited "D:\506PS4\q1\measure_df.csv", clear 
preserve

// encode condition to a factor variable
egen condition_fac = group(condition) 
replace condition_fac = 0 if condition_fac ==2 // atypical is 1 and typical is 0

// log transformation for responses
generate log_distance = log(totaldistance)
generate log_max = log(maxabsdev) 
generate log_avg = log(avgabsdev)
generate log_auc = log(auc)
save measure_df.dta, replace


// fit mixed-effect models and store the est and se, seperately
// distance
mixed log_distance i.condition_fac || _all:R.subject || _all:R.exemplar, reml
matrix b = e(b)
matrix V = e(V)
mata:
 b = st_matrix("b")
 v = st_matrix("V")
 se = sqrt( diagonal(v) )
 se = se'
 st_matrix("se", se) 
end

//max
mixed log_max i.condition_fac || _all:R.subject || _all:R.exemplar, reml 
matrix b = e(b)
matrix V = e(V)
mata:
 b = st_matrix("b")
 v = st_matrix("V")
 se = sqrt( diagonal(v) )
 se = se'
 st_matrix("se", se) 
end

// avg
mixed log_avg i.condition_fac || _all:R.subject || _all:R.exemplar, reml
matrix b = e(b)
matrix V = e(V)
mata:
 b = st_matrix("b")
 v = st_matrix("V")
 se = sqrt( diagonal(v) )
 se = se'
 st_matrix("se", se) 
end

// auc
mixed log_auc i.condition_fac || _all:R.subject || _all:R.exemplar, reml
matrix b = e(b)
matrix V = e(V)
mata:
 b = st_matrix("b")
 v = st_matrix("V")
 se = sqrt( diagonal(v) )
 se = se'
 st_matrix("se", se) 
end


// create a list to store above results 
clear
input curvature est_log se  
1 .40578999 .1011734   
2 .6511469  .13561998 
3 .16134899 .03883614  
4 .51007945 .11802741  
end


// format the output as a list and export as a csv file 
generate est = exp(est_log)
generate lwr = exp(est_log - 1.96 * se)
generate upr = exp(est_log + 1.96 * se)
generate measure = "s"
replace  measure = "auc" if curvature == 1
replace measure = "avg" if curvature == 2
replace measure = "dis" if curvature == 3
replace measure = "max" if curvature == 4
keep measure est lwr upr
format est lwr upr %3.2f
list, abbreviate(18) clean noobs
gsort -est
rename est relative_effect
export delimited using "D:\506PS4\q1\ps4_q1_tb.csv", replace

log close
* conclusion: in above four models, we see that the measure of average absolute
* deviation has the largest relative effect in terms of the condition of atypical.






