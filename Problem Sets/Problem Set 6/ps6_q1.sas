/* -------------------------------------------------------------------------- *
 * The solution of Problem Set 6 Question 1 for Stats 506
 *
 * Repeat question 1 from problem set 4, originally question 2 part d of problem
 * set 2. 
 * Fit linear mixed models exploring how each curvature measure differs by 
 * condition.
 *
 * Author: Ningyuan Wang
 * Updated: Dec 11, 2019
 * -------------------------------------------------------------------------- *
 */
/* 80: ---------------------------------------------------------------------- */

/* Set working directory of the program */
%let user = %sysget(USERPROFILE);
%let rc = %sysfunc(dlgcdir("&user./Desktop"));

/* sas library: ------------------------------------------------------------- */
libname mylib './data';

/* import delimited data with proc import: ---------------------------------- */
proc import 
datafile='./data/measure_df.csv' 
out=work.measure;


/* use a data step and a set statement to save: ----------------------------- */
data mylib.df1;
 set measure;
	log_dist=log(abs(distance));
	log_max=log(abs(max));
	log_avg=log(abs(avg));
	log_auc=log(abs(auc));
run;


/* view the contents of this file: ------------------------------------------ */
proc contents data=mylib.df1;
run;


proc print data=mylib.df1(obs=10);
run;


/* fit mixed model for curvatures*/

/* total distance */
Ods output SolutionF = dist_coef;

proc mixed data=mylib.df1;
class condition subject exemplar;
model log_dist = condition / cl;
random intercept  / type = vc subject = subject;
random intercept / type = vc subject = exemplar;
run;



/* Max_Abs_Dev */
Ods output SolutionF = max_coef;

proc mixed data=mylib.df1;
class condition subject exemplar;
model log_max = condition / cl;
random intercept / type = vc subject = subject;
random intercept / type = vc subject = exemplar;
run;


/* Avg_Abs_Dev */
Ods output SolutionF = avg_coef;

proc mixed data=mylib.df1;
class condition subject exemplar;
model log_avg = condition / cl;
random intercept / type = vc subject = subject;
random intercept / type = vc subject = exemplar;
run;


/* AUC */
Ods output SolutionF = auc_coef;

proc mixed data=mylib.df1;
class condition subject exemplar;
model log_auc = condition / cl;
random intercept / type = vc subject = subject;
random intercept / type = vc subject = exemplar;
run;
