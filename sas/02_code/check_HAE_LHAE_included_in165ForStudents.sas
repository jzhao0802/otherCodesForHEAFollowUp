libname indir "F:\shire_followup\data"; 
libname outdir "F:\shire_followup\data\200K_2.3M\95M_I1_I2_I3_I4(event=0)";
libname outdir1 "F:\shire_followup\data\200K_2.3M\95M_I1_I2_I3";
option symbolgen mprint;
%let type = I1_I2_I3_I4;
%let type2="I1", "I2", "I3", "I4";
%put &type2;


proc import out=indir.hae_1233
datafile="F:\shire_followup\data\dat_hae_1111_rf_nov26_flag.csv"
dbms=csv replace;
run;

proc sql;
create table outdir.lhae_in_95m1234 as
select a.patient_id
from indir.lhae_ptid a inner join outdir.final_data_pt_95m_&type._1 b
on a.patient_id=b.patient_id;
quit;
/*6699
*/

proc sql;
create table outdir.hae_in_95m1234 as
select a.patient_id
from indir.hae_1233 a inner join outdir.lhae_in_95m1234 b
on a.patient_id=b.patient_id;
quit;
/*850*/

proc sql;
create table outdir.num_check as
select count(*) from outdir.final_data_pt_95m_&type._1
union
select count(*) from outdir.lhae_in_95m1234
union
select count(*) from outdir.hae_in_95m1234;
quit;

