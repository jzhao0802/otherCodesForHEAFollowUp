libname indir "F:\shire_followup\data"; 
libname outdir "F:\shire_followup\data\200K_2.3M\2.5M_from_123M";

%let num_samp=2500000;
%let num_samp_flag =2d5;

proc sql;
select count(*) from indir.final_data_pt;
quit;
data indir.final_data_pt;
	set indir.final_data;
	keep patient_id lookback_days;
/*	if age > 12;*/
run;
/*123187702*/


proc sql;
select count(*) into :cnt from indir.final_data_pt;
quit;
%put &cnt.;

proc  import out=indir.dong200K
datafile="F:\shire_followup\data\dong200K.csv"
dbms=csv replace;
run;
/*there is no nonhae patient id so no way to exclude them from 123M*/


proc sql;
select count(*) from indir.LHAE_ptid
union
select count(*) from indir.dong200K;
quit;
/*exclue the 9692 LHAE and Dong 200K*/
/*proc sql;*/
/*create table outdir.lhae_Dong200K as*/
/*select patient_id from indir.LHAE_ptid*/
/*union*/
/*select patient_id from indir.dong200K;*/
/*quit;*/

proc sql;
create table outdir.final_data_pt_excLHAE as
select a.*, b.patient_id as flag_in
from
  indir.final_data_pt a left join indir.LHAE_ptid b
on a.patient_id=b.patient_id
where flag_in=.;
quit;
/*123181267
*/

proc sql;
select count(*) into :cnt_all from outdir.final_data_pt_excLHAE;
quit;

proc sql;
create table outdir.freq_days as
select
	lookback_days, count(*) as cnt
from
	outdir.final_data_pt_excLHAE
group by 
	lookback_days;

quit;
/*1361
*/

data outdir.freq_days;
	set outdir.freq_days;
	cnt_insmp = round((cnt*&num_samp.)/&cnt_all.);
	id = _n_;
run;

proc sql;
	select count(*) into :cnt from outdir.freq_days;
quit;

%put &cnt;



data outdir.nonhae_&num_samp_flag.M_index;
	set outdir.final_data_pt_excLHAE;
	if 1=2;
run;


%macro sample_data(cnt);
%do i = 1 %to &cnt;
	proc sql noprint;
		select cnt_insmp into :cnt_insmp from outdir.freq_days where id = &i;
		select lookback_days into :lookback_days from outdir.freq_days where id = &i;
	quit;

	%put &cnt_insmp.;
	%put &lookback_days.;
	
	%if &cnt>0  %then %do;
		proc surveyselect data=outdir.final_data_pt_excLHAE(where=(lookback_days=&lookback_days.))	
					out=final_data_pt_sample method=srs seed=1234 sampsize = &cnt_insmp.;
		run;

		proc append data = final_data_pt_sample base = outdir.nonhae_&num_samp_flag.M_index;
		run;
	%end;
%end;

proc sql;
	create table outdir.nonhae_&num_samp_flag.M as
		select a.* from indir.final_data a inner join outdir.nonhae_&num_samp_flag.M_index b
		on a.patient_id = b.patient_id;
	quit;
%mend;

%sample_data(&cnt.);


proc export data=outdir.nonhae_&num_samp_flag.M
outfile='F:\shire_followup\data\200K_2.3M\2.5M_from_123M\nonhae_&num_samp_flag.M.csv'
dbms=csv replace;
run;
proc sql;
select count(*) from outdir.nonhae_&num_samp_flag.M;
quit;

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
