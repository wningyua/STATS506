/* -------------------------------------------------------------------------- *
 * The solution of Problem Set 6 Question 3 for Stats 506
 *
 * Repeat the second two parts of question 2 above, using proc SQL
 *
 * This file imports RECS data from:
 * ./data/2015/csv/recs2015_public_v4.csv
 * https://www.eia.gov/consumption/residential
 *
 * Author: Ningyuan Wang
 *
 * Updated: Dec 12, 2019
 * -------------------------------------------------------------------------- *
 */

/* Set working directory of the program */
%let user = %sysget(USERPROFILE);
%let rc = %sysfunc(dlgcdir("&user./Desktop"));

/* sas library: ------------------------------------------------------------- */
libname mylib './data';
/*%let path= ./data;*/

/* Tell sas where to find formats: ------------------------------------------ */ 
options fmtsearch=( mylib.recs_formats work ); 
run;




/* b. estimate the national average home temperature at night, amonng homes that
use space heating -----------------------------------------------------------*/
/* point estimate */
proc sql;
	create table temp_pe as 
		select heathome, doeid, tempnite, sum(tempnite*nweight)/sum(nweight) as temp_pe
		from mylib.recs2015
        where heathome=1; 
		quit;
		run;


/* merge weights to the temp_pe table*/
proc sql;
	create table temp_re1 as 
	select *
	from mylib.brrwt_long b
	join temp_pe t
	on b.doeid = t.doeid;
	quit;
	run;


/* compute replicate estimate*/
proc sql;
create table temp_re2 as
select temp_pe, sum(tempnite*w)/sum(w) as temp_re, repl
from temp_re1
group by repl;
quit;
run;

/* compute standard error*/
proc sql;
create table temp_re3 as
select distinct  repl, temp_pe, temp_re, 2*sqrt(mean((temp_re - temp_pe)**2)) as se
from temp_re2
order by repl;
quit;
run;

/* save the result*/
proc sql;
create table temp_re4 as
select distinct temp_pe, se
from temp_re3;
quit;
run;

proc print data=temp_re4;
format avg 10.3 se 10.3;
title "national average home temperature (2015 RECS)."; 
run;


proc export data=temp_re4
  outfile = 'national_temp_avg(sql).csv'
  dbms=dlm replace; 
  delimiter  = ",";
run; 


/* c.estimate the average winter home temperatures at night, during the day 
with someone at home, and during the day with no one home (when applicable)--*/

/* point estimate by division*/
proc sql;
create table all_pe as
select  doeid, division,tempnite, temphome, tempgone,
		sum(tempnite*nweight)/sum(nweight) as nite_pe,
		sum(temphome*nweight)/sum(nweight) as home_pe,
		sum(tempgone*nweight)/sum(nweight) as gone_pe
from mylib.recs2015
where tempnite>0 and temphome>0 and tempgone>0
group by division;
quit;
run;

/* replicate estimates by division*/

/* merge the replicate weights table*/
proc sql;
create table all_re1 as
select *
from mylib.brrwt_long b
join all_pe a
on b.doeid = a.doeid;
quit;
run;

/* compute replicate estimate*/
proc sql;
create table all_re2 as
select division, nite_pe, home_pe, gone_pe, repl,
	sum(tempnite*w)/sum(w) as nite_re,
	sum(temphome*w)/sum(w) as home_re,
	sum(tempgone*w)/sum(w) as gone_re
from all_re1
group by division, repl;
quit;
run;


/* compute standard error*/
proc sql;
create table all_re3 as
select distinct  division, nite_pe, home_pe, gone_pe,
2*sqrt(mean((nite_pe - nite_re)**2)) as nite_se,
2*sqrt(mean((home_pe - home_re)**2)) as home_se,
2*sqrt(mean((gone_pe - gone_re)**2)) as gone_se
from all_re2
group by division;
quit;
run;

/* save the results as a csv file*/
proc print data=all_re3;
format home_pe 10.3 gone_pe 10.3 nite_pe 10.3 nite_se 10.3 home_se 10.3 gone_se 10.3 division division. ;
title "Average winter home temperature at night, during the day with someone home, and during the day no one home (2015 RECS)."; 
run;



/* Export to csv */
proc export data=all_re3
  outfile = 'average_temps(sql).csv'
  dbms=dlm replace; 
  format home_pe 10.3 gone_pe 10.3 nite_pe 10.3 nite_se 10.3 home_se 10.3 gone_se 10.3 division division. ;
  delimiter  = ",";
run; 






