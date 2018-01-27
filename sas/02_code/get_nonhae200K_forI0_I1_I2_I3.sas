%let type1 = I1_I2_I3_I4(event=0);
%let type2 = I1_I2_I3;
options symbolgen mprint;


libname outdir "F:\shire_followup\data\200K_2.3M\95M_&type1.";
libname tempdir "D:\shire_followup\data\95M_&type1.";
libname dir123 "F:\shire_followup\data\200K_2.3M\95M_&type2.";
proc export data=indir.lhae_ptid_days
outfile='F:\shire_followup\data\lhae_ptid.csv'
dbms=csv replace;
run;

proc import out=ptid
datafile="F:\shire_followup\data\lhae_ptid.csv"
dbms=csv replace;
run;

proc sql;
/*select count(*)  from dir123.nonhae_200k_index;*/
/*union*/
/*select count(*)  from dir123.nonhae_200K*/
/*union*/
select count(*)  from dir123.nonhae_200K_95m_i1_i2_i3;
quit;

proc sql;
create table dir123.nonhae_200K_index as
select patient_id, lookback_days
from dir123.nonhae_200K_95m_i1_i2_i3;
quit;

proc sql;
create table dir123.overlap_ptid_200K_3m as
select a.patient_id
from dir123.nonhae_200K_index a inner  join outdir.nonhae_3m_index_95m_i1_i2_i3_i4 b
on a.patient_id=b.patient_id;
quit;

proc sql;
create table ck as
select count(*) as n_123 from dir123.nonhae_200K_index 
union
select (count(*)/2668587) as pct from dir123.final_data_pt_95m_i1_i2_i3 ;
quit;

proc freq data=dir123.nonhae_200K_95m_i1_i2_i3;
table datasource;
run;

proc sql;
create table dir123.nonhae_200K_index_haeid as
select patient_id, lookback_days, hae_patient_id
from dir123.nonhae_200K_95m_i1_i2_i3;
quit;
proc export data=dir123.nonhae_200K_index_haeid
outfile="F:\shire_followup\data\200K_2.3M\95M_&type2.\nonhae_200K_index_haeid_95M_&type2..csv"
dbms=csv replace;
run;

proc import out=outdir.nonhae_200K_i0
datafile="F:\shire_followup\data\200K_2.3M\95M_&type1.\nonhae_200K_i0.csv"
dbms=csv replace;
run;

data outdir.nonhae_200K;
set outdir.nonhae_200K_i0 dir123.nonhae_200K_95m_i1_i2_i3(drop=datasource);
run;

