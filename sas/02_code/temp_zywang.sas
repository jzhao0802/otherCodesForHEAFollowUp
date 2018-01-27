libname dir1 "F:\shire_followup\data\200K_2.3M\95M_I2";

data dir1.Nonhae_pt_for_exc3m_I2;
	set dir1.Nonhae_pt_for_exc3m;
	keep patient_id lookback_days;
run;


proc sql;
create table unique_lookbackdays as
	select distinct lookback_days from dir1.Hae_ptid_days_ord_i2;
quit;

%let date_period = 90;

data hae_unique_lookbackdays_period;
	set unique_lookbackdays;
	by lookback_days;
	do lookback_days_period=lookback_days-90 to lookback_days+90;
		output;
	end;
run;

proc sql;
	create table nonhae_lookbackdays as
		select distinct lookback_days_period from hae_unique_lookbackdays_period;
quit;

proc sql;
	create table dir1.Nonhae_pt_for_exc3m_I2 as
		select a.patient_id
		from dir1.Nonhae_pt_for_exc3m a 
		inner join nonhae_lookbackdays  b on a.lookback_days = b.lookback_days_period;
quit;




libname dir2 "F:\shire_followup\data\200K_2.3M\95M_I1_I2_I3";
data dir2.Nonhae_pt_for_exc3m_I1_I2_I3;
	set dir2.Nonhae_pt_for_exc3m;
	keep patient_id lookback_days;
run;




proc sql;
create table unique_lookbackdays as
	select distinct lookback_days from dir2.hae_ptid_days_ord_i1_i2_i3;
quit;

%let date_period = 90;

data hae_unique_lookbackdays_period;
	set unique_lookbackdays;
	by lookback_days;
	do lookback_days_period=lookback_days-90 to lookback_days+90;
		output;
	end;
run;

proc sql;
	create table nonhae_lookbackdays as
		select distinct lookback_days_period from hae_unique_lookbackdays_period;
quit;

proc sql;
	create table dir2.Nonhae_pt_for_exc3m_I1_I2_I3 as
		select a.patient_id
		from dir2.Nonhae_pt_for_exc3m a 
		inner join nonhae_lookbackdays  b on a.lookback_days = b.lookback_days_period;
quit;


libname dir3 "F:\shire_followup\data\200K_2.3M\95M_I3";
data dir3.Nonhae_pt_for_exc3m_I3;
	set dir3.Nonhae_pt_for_exc3m;
	keep patient_id lookback_days;
run;


proc sql;
create table unique_lookbackdays as
	select distinct lookback_days from dir3.Hae_ptid_days_ord_i3;
quit;

%let date_period = 90;

data hae_unique_lookbackdays_period;
	set unique_lookbackdays;
	by lookback_days;
	do lookback_days_period=lookback_days-90 to lookback_days+90;
		output;
	end;
run;

proc sql;
	create table nonhae_lookbackdays as
		select distinct lookback_days_period from hae_unique_lookbackdays_period;
quit;

proc sql;
	create table dir3.Nonhae_pt_for_exc3m_I3 as
		select a.patient_id
		from dir3.Nonhae_pt_for_exc3m a 
		inner join nonhae_lookbackdays  b on a.lookback_days = b.lookback_days_period;
quit;


data dir3.temp;
	set dir3.Nonhae_pt_for_exc3m_I3(obs=10);
run;
