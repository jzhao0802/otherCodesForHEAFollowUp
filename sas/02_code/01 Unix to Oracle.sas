options remote=psc02 comamid=tcp;
filename rlink "C:\Users\zywang\Desktop\tem work\script\psc02.txt";   
signon psc02;

rsubmit;
options mprint nosymbolgen mlogic obs = max;
libname tempwork "/fs219.3/zywang/Shire"; 
libname mydblib oracle user=B1003325_SHIRE password=B1003325 path=PNDWAA01 schema="B1003325_SHIRE";
libname subwork "/fs219.3/zywang/Shire/splitData";
options user = tempwork;
endrsubmit;

libname tempwork slibref = tempwork server = psc02;
libname subwork slibref = subwork server = psc02;
libname mydblib slibref = mydblib server = psc02;

/*Import the pos hae data and obtain the lookback days sequence*/
libname outdir "F:\shire_followup\data";
proc import datafile="C:\Projects\2016\Shire\165M_data_selection\dat_hae_1111_rf_nov26_flag.csv"
			out = outdir.pos_clean_data;
run;

data outdir.pos_clean_data;
	set outdir.pos_clean_data;
	if FLAG_=1 then delete;
run;

proc sql;
	select count(*) from  outdir.pos_clean_data;
quit;


proc sql;
create table hae_unique_lookbackdays as
	select distinct lookback_days from outdir.pos_clean_data;
quit;

/*Identify the days*/
%let date_period = 90;

data hae_unique_lookbackdays_period;
	set hae_unique_lookbackdays;
	by lookback_days;
	do lookback_days_period=lookback_days-90 to lookback_days+90;
		output;
	end;
run;

/*QC*/
proc sql;
select distinct cnt from
(
	select lookback_days, count(*) as cnt from hae_unique_lookbackdays_period
	group by lookback_days
);
quit;
/*181*/

proc sql;
	create table outdir.hae_unique_lookbackdays_period as
		select distinct lookback_days_period from hae_unique_lookbackdays_period;
quit;


*rsubmit;
proc upload data=outdir.pos_clean_data out = tempwork.pos_clean_data;
run;

*rsubmit;
proc upload data=outdir.hae_unique_lookbackdays_period out = tempwork.hae_unique_lookbackdays_period;
run;


/*Download the 165M data from ORACLE*/
*rsubmit;
data tempwork.nonhae_165M;
	set mydblib.scoring_predictor4_sub3;
run;

*rsubmit;
proc sql;
create table qc_ck1 as
	select count(*) from tempwork.nonhae_165M;
quit;
/*166012077*/

/*Select variabels to calculate the envents*/
*rsubmit;
proc contents data=tempwork.nonhae_165M;
	ods output Variables = tempwork.variables;
run;

*rsubmit;
data tempwork.flag_variables;
	set tempwork.variables;
	if kindex(Variable, "_FLAG") > 0;
	keep Variable;
run;

*rsubmit;
proc sql noprint;
	select Variable into :allvar separated " " from tempwork.flag_variables;
quit;
%put &allvar;

/*Data for part I*/
/*For I.1*/
*rsubmit;
data tempwork.datset_I1_temp tempwork.datset_I2_temp;
	set tempwork.nonhae_165M(where=(age le 12));
	_flag_cnt = sum(of &allvar);
	if _flag_cnt < 3 and _flag_cnt > 0 then output tempwork.datset_I1_temp;
	if _flag_cnt ge 3 then output tempwork.datset_I2_temp;
	drop _flag_cnt;
run;

*rsubmit;
proc sql;
create table tempwork.datset_I1 as
	select a.* from tempwork.datset_I1_temp a
	inner join tempwork.Hae_unique_lookbackdays_period b
	on a.lookback_days = b.lookback_days_period;
quit;

*rsubmit;
proc sql;
create table tempwork.datset_I2 as
	select a.* from tempwork.datset_I2_temp a
	inner join tempwork.Hae_unique_lookbackdays_period b
	on a.lookback_days = b.lookback_days_period;
quit;


*rsubmit;
data tempwork.datset_I3_temp;
	set tempwork.nonhae_165M(where=(age gt 12));
	_flag_cnt = sum(of &allvar);
	if _flag_cnt <3 and  _flag_cnt >0;
	drop _flag_cnt;
run;

*rsubmit;
proc sql;
create table tempwork.datset_I3 as
	select a.* from tempwork.datset_I3_temp a
	inner join tempwork.Hae_unique_lookbackdays_period b
	on a.lookback_days = b.lookback_days_period;
quit;


/*Some QC*/
*rsubmit;
proc sql;
create table qc_ck2 as
	select count(*) from tempwork.nonhae_165M where age le 12
	union all
	select count(*) from tempwork.datset_I1_temp
	union all
	select count(*) from tempwork.datset_I2_temp
	union all
	select count(*) from tempwork.datset_I0_temp;
quit;
/*
 27,078,809 
  6,839,010 
 19,543,250 
    696,549 
*/

/*Split the data*/
*rsubmit;
/*%let parts = 20;*/
/*%let i = 1;*/
/*%let intable = tempwork.datset_I1;*/
/*%let outable = subwork.datset_I1;*/


*rsubmit;
%macro splitdata(intable, outable, parts);
*rsubmit;
proc sql noprint;
	select count(*) into :cnt from &intable.;
quit;
%put &cnt;
%do i=1 %to %eval(&parts+1);
	*rsubmit;
	data &outable._&i.;
		set &intable.;
		if _n_ ge %eval((&i.-1)*%eval(&cnt./&parts.)) and _n_ lt %eval(&i.*%eval(&cnt./&parts.));
	run;
%end;
%mend;
*rsubmit;
%splitdata(tempwork.datset_I1,subwork.datset_I1, 20);

*rsubmit;
%splitdata(tempwork.datset_I2,subwork.datset_I2, 60);

*rsubmit;
%splitdata(tempwork.datset_I3,subwork.datset_I3, 100);



/*Upedated 2016/05/05, extract the data with event = 0*/
*rsubmit;
data tempwork.datset_I0_temp;
	set tempwork.nonhae_165M;
	_flag_cnt = sum(of &allvar);
	if _flag_cnt = 0;
	drop _flag_cnt;
run;

*rsubmit;
proc sql;
create table tempwork.datset_I0 as
	select a.* from tempwork.datset_I0_temp a
	inner join tempwork.Hae_unique_lookbackdays_period b
	on a.lookback_days = b.lookback_days_period;
quit;

*rsubmit;
data tempwork.datset_I0;
	set tempwork.datset_I0;
	if age < 0 then delete;
run;


*rsubmit;
proc sql;
	select count(*) from tempwork.datset_I0_temp
	union all
	select count(*) from tempwork.datset_I0;
quit;

*rsubmit;
proc sql;
	select max(age) as max, min(age) as min from tempwork.datset_I0;
quit;

*rsubmit;
%splitdata(tempwork.datset_I0,subwork.datset_I0, 20);

/*END*/
