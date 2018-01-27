
option symbolgen mprint;

%let type = I3;
%let type2 = "I3";

libname outdir "F:\shire_followup\data\200K_2.3M\another_200K\95M_&type."; 
libname indir "F:\shire_followup\data\200K_2.3M\95M_&type.";
libname indir2 "F:\shire_followup\data";



proc sql;
create table outdir.freq_days_95M_&type. as
select
	lookback_days, count(*) as cnt
from
	indir.Nonhae_pt_for_exc3m
group by 
	lookback_days;
quit;
/*1361*/


/*use the dataset after removing the extracted 2.3M from dataset_II.b*/
proc sql;
select count(*) into : totalcnt from indir.Nonhae_pt_for_exc3m;
quit;

%put &totalcnt;

data outdir.freq_days_95M_&type.;
	set outdir.freq_days_95M_&type.;
	cnt_in3m = round((cnt*200000)/&totalcnt);
	id = _n_;
run;

proc sql noprint;
	select count(*) into :cnt from outdir.freq_days_95M_&type.;
quit;

%put &cnt;

data outdir.final_data_pt_95M_&type.;
	set indir.Nonhae_pt_for_exc3m;
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

	%if &cnt_in3m. ne 0 %then %do;
		proc surveyselect noprint data=outdir.final_data_pt_95M_&type.(where=(lookback_days=&lookback_days.))	
					out=final_data_pt_sample method=srs seed=1234 sampsize = &cnt_in3m.;
		run;

		proc append data = final_data_pt_sample base = outdir.nonhae_3M_index_95M_&type.;
		run;
	%end;
%end;

proc sql;
	create table outdir.nonhae_200K_95M_&type. as
		select a.* from 
		(select * from indir2.dataset_ii where datasource in ("95M", &type2.))a inner join outdir.nonhae_3M_index_95M_&type. b
		on a.patient_id = b.patient_id;
	quit;
%mend;

%sample_data;
/**/
/*data outdir.nonhae_3M_95M_&type.;*/
/*	set nonhae_3M;*/
/*run;*/

proc export data=outdir.nonhae_200K_95M_&type.
outfile="F:\shire_followup\data\200K_2.3M\another_200K\95M_&type.\nonhae_200K_95M_&type..csv"
dbms=csv replace;
run;
/*proc export data=outdir.nonhae_200K*/
/*outfile='D:\shire_followup\data\nonhae_200K.csv'*/
/*dbms=csv replace;*/
/*run;*/

data nonhae_3m_miss;
	set outdir.nonhae_200K_95M_&type.;
	n_miss = cats(nmiss(of _numeric_));
run;

proc sql;
	select count(*) from nonhae_3m_miss where n_miss >1;
quit;

/*QC frequence of lookback_days*/
proc sql;
	create table outdir.freq_days_200K_95M_&type._QC as
	select
		a.lookback_days
		, a.cnt
		, a.cnt_in3M
		, b.freq_200K
	from
		outdir.freq_days_95m_&type a 
		left join
		(select lookback_days 
				, count(*) as freq_200K
		from outdir.nonhae_200K_95M_&type.
		group by lookback_days
		) b
	on a.lookback_days=b.lookback_days
	;
/*	where a.cnt_in3M ^= b.freq_200K or b.freq_200K=. ;*/
quit;


