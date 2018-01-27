/*split 3M dataset into n separated files*/

libname indir "F:\shire_followup\data\200K_2.3M\2.5M_from_123M"; 
libname outdir "F:\shire_followup\data\200K_2.3M\2.5M_from_123M\split";

%let n=10;
option symbolgen mprint;
/*data outdir.final_data_pt;*/
/*	set outdir.final_data;*/
/*	keep patient_id lookback_days;*/
/*	if age > 12;*/
/*run;*/
/*123187702*/
/**/
%let type = 2d5;

data for_nonhae_&type.M_split;
set indir.nonhae_&type.m;
num=mod(_N_, &n.);
keep patient_id num;
run;

proc freq data=for_nonhae_&type.M_split;
table num;
run;

%macro extract_data();
	%do i= 1 %to &n.;
		proc sql;
			create table outdir.nonhae_&type.M_split_&i. as
				select a.*, b.num
				from indir.nonhae_&type.M a inner join for_nonhae_&type.M_split b
				on a.patient_id=b.patient_id and b.num=&i.-1;
		quit;
		
		proc export data=outdir.nonhae_&type.M_split_&i.
		outfile="F:\shire_followup\data\200K_2.3M\2.5M_from_123M\split\nonhae_&type.M_split_&i..csv"
		dbms=csv replace;
		run;
	%end;
%mend;
%extract_data();
