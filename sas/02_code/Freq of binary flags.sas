libname outdir "D:\shire_followup\data"; 

proc sql;
	select count(*) as total_cnt from outdir.nonhae_3m;
quit;

proc contents data=outdir.nonhae_3m;
	ods output Variables = variables;
run;

data flag_variables;
	set variables;
	if kindex(Variable, "_FLAG") > 0;
	keep Variable;
run;

data flag_variables;
	set flag_variables;
	id = _n_;
run;

proc sql noprint;
	select count(*) into :t_cnt from flag_variables;
quit;

%put &t_cnt;

%macro com_freq;
proc sql;
create table freq_brinary as 
	select * from freq_brinary_temp where 1=2;
quit;

%do i = 1 %to &t_cnt;
	proc sql noprint;
		select compress(Variable) into :varname from flag_variables where id = &i;
	quit;
	%put &varname;

	proc sql;
		create table gp_cnt as
			select &varname as flag, count(*) as cnt from outdir.nonhae_3m
			group by &varname;
	quit;
	
	proc sort data=gp_cnt;
		by flag;
	run;

	proc transpose data=gp_cnt out=gp_cnt_tran(drop = _NAME_) prefix = cnt;
		var cnt;
		id flag;
	quit;

	data gp_cnt_tran1;
		format variable $20.;
		set gp_cnt_tran;
		variable = "&varname";
	run;

	proc append data=gp_cnt_tran1 base = freq_brinary;
	run;

	proc datasets noprint;
		delete gp_cnt Gp_cnt_tran Gp_cnt_tran1;
	quit;
	%put &varname. done;
%end;
%mend;

%com_freq;

data Flag_freq_brinary;
	set freq_brinary;
	totalcnt = cnt0 + cnt1;
	flag1_pct = cnt1/totalcnt;
run;

proc sql;
	select distinct totalcnt from Flag_freq_brinary
	union all
	select count(*) as total_cnt from outdir.nonhae_3m;
quit;

data outdir.Flag_freq_brinary;
	set Flag_freq_brinary;
	drop totalcnt;
run;

proc sort data=outdir.Flag_freq_brinary;
	by flag1_pct;
run;

proc export data=outdir.Flag_freq_brinary
			outfile="D:\shire_followup\data\Flag_freq_brinary.csv"
			dbms=csv replace;
run;
