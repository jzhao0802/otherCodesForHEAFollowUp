libname indir "F:\shire_followup\data";
libname outdir 'F:\shire_followup\data\distr_lookback_days';

proc sql;
create table lookback_days_2d3M as
select lookback_days, count(*)/&pt_num. as freq from indir.nonhae_3m_clean2335697
group by lookback_days
;
quit;

proc sql;
select count(*) into :pt_num1 from indir.final_data_pt_clean;
quit;

proc sql;
create table lookback_days_95M as
select lookback_days, count(*)/&pt_num1. as freq_2 from indir.final_data_pt_clean
group by lookback_days;
quit;

%put &pt_num1;

proc sql;
create table outdir.lookback_days_comp as
select a.lookback_days,
		b.freq as freq_2d3M,
		a.freq_2 as freq_95M,
		(abs(b.freq-a.freq_2))/a.freq_2 as diff_rate
from lookback_days_95M a left join lookback_days_2d3M b
on a.lookback_days=b.lookback_days;
quit;

proc sql;
select max(abs(diff_rate)) into :max_rate from outdir.lookback_days_comp;
quit;
%put &max_rate;

proc sort data=outdir.lookback_days_comp;
by descending  diff_rate;
run;

proc export data=outdir.lookback_days_comp
outfile="F:\shire_followup\data\distr_lookback_days\lookback_days_comp.csv"
dbms=csv repalce;
run;
