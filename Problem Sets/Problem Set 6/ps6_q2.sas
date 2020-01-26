/* -------------------------------------------------------------------------- *
 * The solution of Problem Set 6 Question 2 for Stats 506
 *
 * Revise question 1 from problem set 2, part a, c-d. 
 *
 * This file imports RECS data from:
 * ./data/2015/csv/recs2015_public_v4.csv
 * https://www.eia.gov/consumption/residential
 *
 * Author: Ningyuan Wang
 *
 * Updated: Dec 11, 2019
 * -------------------------------------------------------------------------- *
 */
/* 80: ---------------------------------------------------------------------- */

/* Set working directory of the program */
%let user = %sysget(USERPROFILE);
%let rc = %sysfunc(dlgcdir("&user./Desktop"));

/* sas library */
libname mylib './data';

/* import delimited data with proc import */
proc import datafile='./data/recs2015_public_v4.csv' out=work.recs;

/* use a data step and a set statement to save*/
data mylib.recs2015;
 set recs;

/* view the contents of this file */
proc contents data=mylib.recs2015;
run;

/* a. reshape the replicate weights to a logner fromat ----------------------*/
data mylib.weights;
set recs;
keep doeid brrwt1-brrwt96;
run;


proc contents data=mylib.weights;


proc transpose data=mylib.weights out=weights_long;
by doeid;
run;

proc print data=weights_long(obs=5);

/* save the replicate weights data as brrwt_long file*/
data mylib.brrwt_long;
  set weights_long(rename =(col1 = w)) ;
  repl = substr(_name_, 6, 2);
  drop _name_;
  run; 



proc print data=mylib.brrwt_long(obs=5);
run;

/* b. estimate the national average home temperature at night, amonng homes that
use space heating -----------------------------------------------------------*/

/* point estimate */
data point_est_q2;
set recs;
n_ht2=0;
if heathome=1 then n_ht2=nweight;
if heathome=0 then delete;
if tempnite=-2 then delete;
keep n_ht2 heathome tempnite nweight doeid;
run;

proc print data=point_est_q2;
run;

/* compute point estimate */
proc means data=point_est_q2;
 var tempnite;
 weight nweight;
 output out=out1(drop = _type_ _freq_) mean=avg;
 run;



proc print data=out1;


/* standard error with replicate estimates */
data repl_est_q2;
merge point_est_q2(keep= doeid tempnite) mylib.brrwt_long;
by doeid;
run;


proc contents data=repl_est_q2;

/* compute replicate estimates */
proc means data=repl_est_q2;
class repl;	
var tempnite;
weight w;
output out=out2(drop = _type_ _freq_) mean=avg_repl;
run;

/* compute standard error */
data out_3;
merge out2 out1;
retain _avg;
if not missing(avg) then _avg=avg;
else avg=_avg;
drop _avg;
v = (avg_repl - avg)**2;
proc means;
var v;
output out=out4(drop = _type_ _freq_) mean=v
run;

data out5;
set out4;
se = 2*sqrt(v);

data out_2b;
merge out1 out5;
drop v;
run;

/* Print the result*/
proc print data=out_2b;
format avg 10.3 se 10.3;
title "National average home temperature at night among homes using space heating (2015 RECS)."; 
run;

/* Export to csv */
proc export data=out_2b
  outfile = 'national_home_temp.csv'
  dbms=dlm replace; 
  delimiter  = ",";
run; 



/* c.estimate the average winter home temperatures at night, during the day 
with someone at home, and during the day with no one home (when applicable)--*/

/* organize data*/
data q2_c;
set recs;
if tempnite=-2 then delete;
if tempgone=-2 then delete;
if temphome=-2 then delete;
n_wt2=nweight;
keep n_wt2 doeid nweight division tempnite temphome tempgone;
run;

/* reshape to long data*/
/*proc transpose data=q2_c out=temps_long(rename=(_NAME_= type COL1=temp));
by doeid division nweight n_wt2;
var temphome tempnite tempgone;
run;*/

proc transpose data=q2_c out=temps_long(rename=(_name_ = TYPE COL1=TEMP));
by doeid division nweight;
var temphome tempnite tempgone;
run;

proc contents data=temps_long;


/* format statement for divisions */
proc format library=mylib.recs_formats;
 value division
       1="New England"
       2="Middle Atlantic"
       3="East North Central"
       4="West North Central"
       5="South Atlantic"
       6="East South Central"
       7="West South Central"
       8="Mountain North"
       9="Mountain South"
       10="Pacific";
      
/* Tell SAS where to find this format later */
options fmtsearch=( mylib.recs_formats work );



/* compute point estimates: -------------------------------------------- */
/*proc summary data=temps_long;
 class division type;
 var temp;
 weight nweight;
 output out=temps_pe(_TYPE_=3) mean=pe; 
 run;*/

proc summary data=q2_c;
class division;
var temphome tempgone tempnite;
weight nweight;
output out=temps_pe mean=home_pe gone_pe nite_pe;
run;

data temps_pe;
set temps_pe;
if _type_ =0 then delete;
drop _type_ _freq_;
run;


/* standard error with replicate estimates */
data temps_repl;
merge q2_c mylib.brrwt_long;
by doeid;
run;


/* compute replicate estimates */
proc summary data=temps_repl;
 class division repl;
 var temphome tempgone tempnite ;
 weight w;
 output out=temps_re mean=home_re gone_re nite_re ; 
 run;

data temps_re;
set temps_re;
if _type_ <3 then delete;
drop _type_ _freq_;
run;

/* compute standard error */
data temps_ee;
merge temps_pe temps_re;
by division;
drop repl;
v_home = (home_re - home_pe)**2;
v_nite =(nite_re - nite_pe)**2;
v_gone = (gone_re - gone_pe)**2;
run;

proc summary data=temps_ee;
class division;
var v_home v_nite v_gone;
output out=temps_se mean = home_v nite_v gone_v;
run; 

data temps_se;
set temps_se;
se_home = 2*sqrt(home_v);
se_nite = 2*sqrt(nite_v);
se_gone = 2*sqrt(gone_v);
if _type_=0 then delete;
drop _freq_ _type_ home_v nite_v gone_v;
run;

/* merge point estimate and standard error*/
data temps_out;
merge temps_pe temps_se;
run;

/* save the results as a csv file*/
proc print data=temps_out;
format home_pe 10.3 gone_pe 10.3 nite_pe 10.3 se_home 10.3 se_nite 10.3 se_gone 10.3 division division. ;
title "Average winter home temperature at night, during the day with someone home, and during the day no one home (2015 RECS)."; 
run;


/* Export to csv */
proc export data=temps_out
  outfile = 'average_temps.csv'
  dbms=dlm replace; 
  delimiter  = ",";
run; 





