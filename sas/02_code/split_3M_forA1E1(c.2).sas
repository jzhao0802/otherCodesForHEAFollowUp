/*split 3M dataset into n separated files*/

libname indir "F:\shire_followup\data\200K_2.3M\2.3M_A1E1"; 
libname outdir "F:\shire_followup\data\200K_2.3M\2.3M_A1E1\split";



%let n=10;
option symbolgen mprint;

proc import out=indir.nonhae_3m
datafile="F:\shire_followup\data\200K_2.3M\2.3M_A1E1\neg_3M_clean2335697.csv"
dbms=csv replace;
run;
/*2335697
*/

%let type = A1E1;

data for_nonhae_3M_split;
set indir.nonhae_3m;
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
				from indir.nonhae_3M a inner join for_nonhae_3M_split b
				on a.patient_id=b.patient_id and b.num=&i.-1;
		quit;
		
		proc export data=outdir.nonhae_3M_split_&i.
		outfile="F:\shire_followup\data\200K_2.3M\2.3M_A1E1\split\nonhae_3M_split_&i..csv"
		dbms=csv replace;
		run;
	%end;
%mend;
%extract_data();


