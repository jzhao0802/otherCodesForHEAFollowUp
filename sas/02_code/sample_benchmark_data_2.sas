libname outdir "D:\shire_followup\data"; 

data outdir.final_data_pt;
	set outdir.final_data_clean;
	keep patient_id lookback_days;
	if age > 12;
run;
/*123187702
*/
proc sql;
select count(*) as cnt from outdir.final_data_pt;
quit;
/*get the pure nonHae after remove the patid in LHAE_9692*/
proc import out=outdir.HAE_ptid_days
datafile='D:\shire_followup\data\HAE_ptid_days_1087.csv'
dbms=csv replace;
run;

proc import out=outdir.LHAE_ptid
datafile='D:\shire_followup\data\patient_id_9692.csv'
dbms=csv replace;
run;
data LHAE_ptid;
set outdir.LHAE_ptid;
lhae_flag = 1;
run;

proc sql;
create table outdir.tb_clean as
select a.patient_id, a.lookback_days, b.lhae_flag
from outdir.final_data_pt a left join LHAE_ptid b
on a.patient_id=b.patient_id;
quit;
/*123187702
*/
data outdir.nonhae_pt;
set outdir.tb_clean;
if lhae_flag=.;
run;
/*123181267
*/

/*exclude the 3M patient_id from the outdir.nonhae_pt*/
proc sql;
create table outdir.nonhae_pt_for_exc3M as
select
	a.patient_id as patient_id
	, a.lookback_days as lookback_days
	, b.patient_id as excl_flag
from
	outdir.nonhae_pt a left join outdir.nonhae_3M_index b
on
	a.patient_id = b.patient_id;

quit;

data outdir.nonhae_pt_exc3M(drop = excl_flag);
	set outdir.nonhae_pt_for_exc3M;
	if excl_flag = .;
run;
/*120176882
*/
/*93446887
*/
proc sql;
select count(*) as cnt from outdir.nonhae_pt_exc3M;
quit;

proc sort data=outdir.hae_ptid_days out=hae_ptid_days_ord;
by lookback_days;
run;
data hae_ptid_days_ord;
set hae_ptid_days_ord;
by lookback_days;
retain count;
if first.lookback_days then count=0;
count+1;
run;

data outdir.hae_ptid_days_u outdir.hae_ptid_days_d;
set hae_ptid_days_ord;
if count=1 then output outdir.hae_ptid_days_u;/*717*/
if count > 1 then output outdir.hae_ptid_days_d;/*370*/
run;

proc sql;
create table nonhae_matched_t1 as
select 
		a.patient_id as patient_id_nonhae
		, b.patient_id as patient_id
		, b.lookback_days as lookback_days
from 
	outdir.nonhae_pt_exc3M a left join outdir.hae_ptid_days_u b
on a.lookback_days = b.lookback_days;
quit;
/*123181267
*/

data nonhae_matched_u;
set nonhae_matched_t1;
where patient_id ^=.;
run;
/*54387029
*/
/*sampling 200k*/
proc sort data=nonhae_matched_u;
by patient_id;
run;

proc surveyselect data=nonhae_matched_u out=smp_nonhae_u method=srs seed=1234 samplesize = 200;
strata patient_id;
run;

proc freq data=smp_nonhae_u;
	table patient_id;
run;

proc sql;
create table tb3 as
select
	a.patient_id as patient_id_nonhae
	,a.lookback_days as lookback_days
	, b.patient_id as matched_hae_ptid
from	
	outdir.nonhae_pt_exc3M a left join smp_nonhae_u b
on
	a.patient_id = b.patient_id_nonhae;
quit;
/*123181267
*/

data nonhae_matched_for_d ;
set tb3;
where matched_hae_ptid =.;
run;
/*123037867
*/

proc sql;
create table nonhae_matched_t2 as
select
	a.patient_id_nonhae as patient_id_nonhae
	, b.patient_id as patient_id
	, b.lookback_days as lookback_days
from
	nonhae_matched_for_d a left join outdir.hae_ptid_days_d b
on
	a.lookback_days=b.lookback_days;
quit;
/*129020427

*/

data nonhae_matched_d;
set nonhae_matched_t2;
where patient_id ^=.;
run;
/*24735773
*/

/*sampling 200k*/
proc sort data=nonhae_matched_d;
by patient_id;
run;

proc surveyselect data=nonhae_matched_d out=smp_nonhae_d method=srs seed=1234 samplesize = 200;
strata patient_id;
run;

proc freq data=smp_nonhae_d;
	table patient_id;
run;

/*check*/
proc sql;
create table qc1 as
select
	a.patient_id
from
	smp_nonhae_u a inner join smp_nonhae_d b
on
	a.patient_id_nonhae=b.patient_id_nonhae;
quit;

data outdir.smp_nonhae_ptid;
set smp_nonhae_u smp_nonhae_d;
keep patient_id patient_id_nonhae;
run;
/*217400
*/

proc sql;
create table outdir.nonhae_200K as
select
	a.patient_id as hae_patient_id
	, b.*
from
	outdir.smp_nonhae_ptid a inner join outdir.final_data b
on
	a.patient_id_nonhae=b.patient_id;
quit;
/*217400
*/

proc export data=outdir.nonhae_200K
outfile='D:\shire_followup\data\nonhae_200K_v2.csv'
dbms=csv replace;
run;

/*for likely hae patients , calculate the patient unmber after removing the event < 3*/
proc sql;
create table outdir.lhae_tb as
select a.*
from outdir.final_data a inner join outdir.LHAE_ptid b
on a.patient_id=b.patient_id;
quit;
/*6435
*/
