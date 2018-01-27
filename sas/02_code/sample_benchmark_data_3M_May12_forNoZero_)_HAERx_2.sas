libname indir "F:\shire_followup\data"; 
libname outdir "F:\shire_followup\data\200K_2.3M\95M_I1_I2_I3_I4(event=0)";
libname outdir1 "F:\shire_followup\data\200K_2.3M\95M_I1_I2_I3";
option symbolgen mprint;
%let type = I1_I2_I3_I4;
%let type2="I1", "I2", "I3", "I4";
%put &type2;


data outdir.final_data_pt_95M_&type._1;
/*  set test;*/
    set outdir1.final_data_pt_95M_i1_i2_i3 outdir.pt_i4;
run;

/*exclude the Likely HAE patients*/
proc sql;
create table outdir.final_data_pt_95M_&type. as
select 
	a.*, b.patient_id as flat_lhae
from
	outdir.final_data_pt_95M_&type._1 a left join indir.lhae_ptid b
on
	a.patient_id=b.patient_id
where flat_lhae=.;

quit;
/*152514686
*/

proc sql ;
select count(*) into :totalcnt from outdir.final_data_pt_95M_&type.;
/*where upcase(datasource) in ("95M", &type2.);*/
quit;

proc sql;
create table outdir.freq_days as
select lookback_days, count(*) as cnt
from outdir.final_data_pt_95M_&type.
group by lookback_days;
quit;



data outdir.freq_days;
	set outdir.freq_days;
	cnt_in3m = round((cnt*2300000)/&totalcnt);
	id = _n_;
run;

proc sql noprint;
	select count(*) into :cnt from outdir.freq_days;
quit;

%put &cnt;


data outdir.nonhae_3M_index_95M_&type.;
	set outdir.final_data_pt_95M_&type.(obs=10);
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
	%if &cnt_in3m. > 0 %then %do;
		proc surveyselect noprint data=outdir.final_data_pt_95M_&type.(where=(lookback_days=&lookback_days.))	
					out=outdir.final_data_pt_sample method=srs seed=1234 sampsize = &cnt_in3m.;
		run;

		proc append data = outdir.final_data_pt_sample base = outdir.nonhae_3M_index_95M_&type.;
		run;
	%end;
%end;

/*proc sql;*/
/*	create table outdir.nonhae_3M_95M_&type. as*/
/*		select a.* from */
/*		(select * from indir.dataset_ii where datasource in ("95M", &type2.))a inner join outdir.nonhae_3M_index_95M_&type. b*/
/*		on a.patient_id = b.patient_id;*/
/*	quit;*/
%mend;

%sample_data;

proc sql;
select count(*) from outdir.nonhae_3M_index_95M_&type.;
quit;

/*exclude the 2.3M patid from outdir.final_data_pt_95M_&type.*/
proc sql;
create table outdir.final_data_pt_95M_1234_exc3m(drop=flag_in3m and drop=flat_lhae) as
select a.*, b.patient_id as flag_in3m
from outdir.final_data_pt_95M_&type. a left join outdir.nonhae_3M_index_95M_&type. b
on a.patient_id=b.patient_id
where flag_in3m = .;
quit;

proc sql;
select count(*) as cnt_all from outdir.final_data_pt_95M_&type.
union
select count(*) as cnt_exc3m from outdir.final_data_pt_95M_1234_exc3m;
quit;

proc sql;
select * from outdir.freq_days
where lookback_days in (1100, 1113);
quit;

/*check the continous pannel of lookback_days in HAE dataset*/


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

