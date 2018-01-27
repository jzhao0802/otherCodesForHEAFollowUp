libname indir "F:\shire_followup\data"; 
libname outdir "F:\shire_followup\data\200K_2.3M\95M_I2";

option symbolgen mprint;
/*data outdir.final_data_pt;*/
/*	set outdir.final_data;*/
/*	keep patient_id lookback_days;*/
/*	if age > 12;*/
/*run;*/
/*123187702*/
/**/
%let type = I2;
%let type2="I2";
%put &type2;

proc sql;
create table outdir.freq_days_95M_&type. as
select
	lookback_days, count(*) as cnt
from
	indir.dataset_ii where upcase(datasource) in ("95M", &type2.)
group by 
	lookback_days;
quit;
/*1361
*/

proc sql noprint;
select sum(cnt) into :totalcnt from indir.Qc_dataset_ii
where upcase(datasource) in ("95M", &type2.);
quit;
%put &totalcnt;


data outdir.freq_days_95M_&type.;
	set outdir.freq_days_95M_&type.;
	cnt_in3m = round((cnt*2300000)/&totalcnt)+1;
	id = _n_;
run;

proc sql noprint;
	select count(*) into :cnt from outdir.freq_days_95M_&type.;
quit;

%put &cnt;

data outdir.final_data_pt_95M_&type.;
/*	set test;*/
	set indir.dataset_ii;
	where datasource in ("95M", &type2.);
	keep patient_id lookback_days;
run;

data outdir.nonhae_3M_index_95M_&type.;
	set outdir.final_data_pt_95M_&type.;
	if 1=2;
run;

%macro sample_data;
%do i = 1 %to &cnt;
	proc sql noprint;
		select cnt_in3m into :cnt_in3m from outdir.freq_days_95M_&type. where id = &i;
		select lookback_days into :lookback_days from outdir.freq_days_95M_&type. where id = &i;
	quit;

	%put &cnt_in3m.;
	%put &lookback_days.;

	proc surveyselect noprint data=outdir.final_data_pt_95M_&type.(where=(lookback_days=&lookback_days.))	
				out=final_data_pt_sample method=srs seed=1234 sampsize = &cnt_in3m.;
	run;

	proc append data = final_data_pt_sample base = outdir.nonhae_3M_index_95M_&type.;
	run;
%end;

proc sql;
	create table nonhae_3M as
		select a.* from 
		(select * from indir.dataset_ii where datasource in ("95M", &type2.))a inner join outdir.nonhae_3M_index_95M_&type. b
		on a.patient_id = b.patient_id;
	quit;
%mend;

%sample_data;

data outdir.nonhae_3M_95M_&type.;
set nonhae_3M;
run;

proc export data=outdir.nonhae_3M_95M_&type.
outfile="&outdir/nonhae_3M_95M_&type..csv"
dbms=csv replace;
run;
/*proc export data=outdir.nonhae_200K*/
/*outfile='D:\shire_followup\data\nonhae_200K.csv'*/
/*dbms=csv replace;*/
/*run;*/

data nonhae_3m_miss;
	set outdir.nonhae_3M;
	n_miss = cats(nmiss(of _numeric_));
run;

proc sql;
select count(*) from nonhae_3m_miss where n_miss >1;
quit;

