libname outdir "D:\shire_followup\data"; 

/*data outdir.final_data_pt;*/
/*	set outdir.final_data;*/
/*	keep patient_id lookback_days;*/
/*	if age > 12;*/
/*run;*/
/*123187702*/
/**/
proc import out=outdir.nonhae_3m_clean2335697
datafile='D:\shire_followup\data\neg_3M_clean2335697.csv'
dbms=csv repalce;
run;

proc sql;
	select count(*) from outdir.nonhae_3m_clean2335697;
quit;

proc sql;
create table outdir.nonhae_3m_pt as
select patient_id, lookback_days
from outdir.nonhae_3m_clean2335697;
quit;

/*proc sql;*/
/*select count(*) from outdir.final_data_pt;*/
/*quit;*/
/*95788592*/
proc sql;
create table outdir.freq_days as
select
	lookback_days, count(*) as cnt
from
	outdir.nonhae_3m_pt
group by 
	lookback_days;

quit;
/*1361*/

data outdir.freq_days;
	set outdir.freq_days;
	cnt_in300k = round((cnt*300000)/2335697);
	id = _n_;
run;

proc sql;
	select count(*) into :cnt from outdir.freq_days;
quit;

%put &cnt;

data outdir.nohae_300K_index;
	set outdir.nonhae_3m_pt;
	if 1=2;
run;

%macro sample_data;
%do i = 1 %to &cnt;
	proc sql noprint;
		select cnt_in300k into :cnt_in300k from outdir.freq_days where id = &i;
		select lookback_days into :lookback_days from outdir.freq_days where id = &i;
	quit;

	%put &cnt_in300k.;
	%put &lookback_days.;

	proc surveyselect data=outdir.nonhae_3m_pt(where=(lookback_days=&lookback_days.))	
				out=nonhae_3m_pt_sample method=srs seed=1234 sampsize = &cnt_in300k.;
	run;

	proc append data = nonhae_3m_pt_sample base = outdir.nohae_300K_index;
	run;
%end;
proc sql;
	create table outdir.nonhae_300k_v2 as
		select a.* from outdir.nonhae_3m_clean2335697 a inner join outdir.nohae_300K_index b
		on a.patient_id = b.patient_id;
quit;
%mend;
%sample_data;

/*QC*/
proc sql;
	select count(*) from outdir.nonhae_300k_v2
	union all
	select sum(cnt_in300k) from outdir.freq_days;
quit;

data outdir.nonhae_300k_v2;
	set outdir.nonhae_300k_v2;
	drop VAR1;
run;

proc export data=outdir.nonhae_300k_v2
	outfile='D:\shire_followup\data\nonhae_300k_v2.csv'
	dbms=csv replace;
run;

/**/
/*proc export data=outdir.nonhae_200K*/
/*	outfile='D:\shire_followup\data\nonhae_200K.csv'*/
/*	dbms=csv replace;*/
/*run;*/

data nonhae_3m_miss;
	set outdir.nonhae_3M;
	n_miss = cats(nmiss(of _numeric_));
run;

proc sql;
	select count(*) from nonhae_3m_miss where n_miss >1;
quit;


/*check the overlapped patient_id in 300K and 2.3M datasets*/
/*proc sql;*/
/*create table outdir.nonhae_overlap_300K_3M as*/
/*	select a.**/
/*	from outdir.nonhae_300K_v2 a inner join outdir.nonhae_3m_clean2335697 b on a.patient_id = b.patient_id;*/
/*quit;*/

proc sql;
create table outdir.rm_overlap_tb as
	select a.*, b.patient_id as flag_in_300K
	from outdir.nonhae_3m_clean2335697 a left join outdir.nonhae_300K_v2 b
	on a.patient_id=b.patient_id;
quit;

data outdir.nonhae_3m_v2_f;
set outdir.rm_overlap_tb;
if flag_in_300K = .;
run;

/*2035717
*/

proc export data=outdir.nonhae_3m_v2_f
outfile='D:\shire_followup\data\nonhae_2d3m_2m.csv'
dbms=csv replace;
run;


/*remove the new sampled 200K( with evetn <3 removed) from 2.3M */
proc import out=outdir.nonhae_200K_v2
datafile='D:\shire_followup\data\nonhae_200K_v2.csv'
dbms=csv replace;
run;
/*217400*/

proc sql;
create table rm_200K_v2_tb as
	select a.*, b.patient_id as in_200K_v2_flag
	from
		outdir.Nonhae_3m_clean2335697 a left join outdir.nonhae_200K_v2 b
	on
		a.patient_id=b.patient_id;
quit;

data outdir.nonhae_2d3m_2d1m_f;
set rm_200K_v2_tb;
if in_200K_v2_flag=.;
run;
