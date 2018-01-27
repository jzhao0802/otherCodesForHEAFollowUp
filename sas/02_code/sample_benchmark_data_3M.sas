libname outdir "D:\shire_followup\data"; 

/*data outdir.final_data_pt;*/
/*	set outdir.final_data;*/
/*	keep patient_id lookback_days;*/
/*	if age > 12;*/
/*run;*/
/*123187702*/
/**/

proc sql;
create table outdir.freq_days as
select
	lookback_days, count(*) as cnt
from
	outdir.final_data_pt
group by 
	lookback_days;

quit;
/*1361
*/

data outdir.freq_days;
	set outdir.freq_days;
	cnt_in3m = round((cnt*3)/123);
	id = _n_;
run;

proc sql;
	select count(*) into :cnt from outdir.freq_days;
quit;

%put &cnt;



data outdir.nohae_3M_index;
	set outdir.final_data_pt;
	if 1=2;
run;

%macro sample_data;
%do i = 1 %to &cnt;
	proc sql noprint;
		select cnt_in3m into :cnt_in3m from outdir.freq_days where id = &i;
		select lookback_days into :lookback_days from outdir.freq_days where id = &i;
	quit;

	%put &cnt_in3m.;
	%put &lookback_days.;

	proc surveyselect data=outdir.final_data_pt(where=(lookback_days=&lookback_days.))	
				out=final_data_pt_sample method=srs seed=1234 sampsize = &cnt_in3m.;
	run;

	proc append data = final_data_pt_sample base = outdir.nohae_3M_index;
	run;
%end;

proc sql;
	create table nonhae_3M as
		select a.* from outdir.final_data a inner join outdir.nonhae_3M_index b
		on a.patient_id = b.patient_id;
	quit;
%mend;

%sample_data;

data outdir.nonhae_3M;
set nonhae_3M;
run;

proc export data=outdir.nonhae_3M
outfile='D:\shire_followup\data\nonhae_3M.csv'
dbms=csv replace;
run;
proc export data=outdir.nonhae_200K
outfile='D:\shire_followup\data\nonhae_200K.csv'
dbms=csv replace;
run;

data nonhae_3m_miss;
	set outdir.nonhae_3M;
	n_miss = cats(nmiss(of _numeric_));
run;

proc sql;
select count(*) from nonhae_3m_miss where n_miss >1;
quit;


/*check the overlapped patient_id in 200K and 3M datasets*/
proc sql;
create table outdir.nonhae_overlap_200K_3M as
select
	a.*
from
	outdir.nonhae_200K a inner join outdir.nonhae_3M b
on
	a.patient_id = b.patient_id;
quit;

proc sql;
select count(*) from outdir.nonhae_overlap_200K_3M;
quit;
