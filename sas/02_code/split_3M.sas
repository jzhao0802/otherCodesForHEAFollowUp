/*split 3M dataset into n separated files*/

libname indir "F:\shire_followup\data"; 
libname outdir "F:\shire_followup\data\200K_2.3M\95M_I1_I2_I3";

%let n=10;
option symbolgen mprint;
/*data outdir.final_data_pt;*/
/*	set outdir.final_data;*/
/*	keep patient_id lookback_days;*/
/*	if age > 12;*/
/*run;*/
/*123187702*/
/**/
%let type = I1_I2_I3;

data for_nonhae_3M_split;
set outdir.nonhae_3M_95M_&type.;
num=mod(_N_, &n.);
keep patient_id num;
run;

proc freq data=for_nonhae_3M_split;
table num;
run;

%macro extract_data();
	%do i= 1 %to &n.;
		proc sql;
			create table outdir.nonhae_3M_split_&i. as
				select a.*, b.num
				from outdir.nonhae_3M_95M_&type. a inner join for_nonhae_3M_split b
				on a.patient_id=b.patient_id and b.num=&i.-1;
		quit;
		
		proc export data=outdir.nonhae_3M_split_&i.
		outfile="F:\shire_followup\data\200K_2.3M\95M_&type.\nonhae_3M_split_&i..csv"
		dbms=csv replace;
		run;
	%end;
%mend;
%extract_data();
