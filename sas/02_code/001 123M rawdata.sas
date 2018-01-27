

data filename;
	infile "d:\shire_followup\filname.txt" missover dsd;
	input filename $;
run;
data filename;
	set filename;
	id = _n_;
run;

proc sql;
	select count(*) into :cnt from filename;
quit;

%put &cnt;

libname outdir "F:\shire_followup\data";

data sectaa;
	infile "Y:\Hui\Project_2016\Shire_followup\01_data\split_data_ptid\sectaa" missover dsd delimiter = "," obs=10;
	input v1-v241;
run;

data outdir.final_data;
	set sectaa;
	if 1=2;
run;

%macro read_data;
%do i = 1 %to &cnt;
	proc sql noprint;
		select filename into :filename from filename where id = &i;
	quit;

	%put &filename.;

	data outdir.&filename.;
		infile "Y:\Hui\Project_2016\Shire_followup\01_data\split_data_ptid\&filename." missover dsd delimiter = ",";
		input v1-v241;
	run;

	proc append data = outdir.&filename. base = outdir.final_data;
	run;
%end;
%mend;

%read_data;

proc sql;
create table cnt_ck as
	select count(*) as cnt from outdir.final_data;
quit;

proc sql;
create table outdir.final_data as
	select 
		v1 as patient_id,
		v2 as DIAG_1_FLAG,
		v3 as DIAG_2_FLAG,
		v4 as DIAG_3_FLAG,
		v5 as DIAG_4_FLAG,
		v6 as DIAG_5_FLAG,
		v7 as DIAG_6_FLAG,
		v8 as DIAG_7_FLAG,
		v9 as DIAG_8_FLAG,
		v10 as DIAG_9_FLAG,
		v11 as DIAG_10_FLAG,
		v12 as DIAG_11_FLAG,
		v13 as DIAG_12_FLAG,
		v14 as DIAG_13_FLAG,
		v15 as DIAG_14_FLAG,
		v16 as DIAG_15_FLAG,
		v17 as DIAG_16_FLAG,
		v18 as DIAG_17_FLAG,
		v19 as DIAG_18_FLAG,
		v20 as DIAG_19_FLAG,
		v21 as DIAG_20_FLAG,
		v22 as DIAG_21_FLAG,
		v23 as DIAG_22_FLAG,
		v24 as DIAG_23_FLAG,
		v25 as DIAG_24_FLAG,
		v26 as DIAG_25_FLAG,
		v27 as DIAG_26_FLAG,
		v28 as DIAG_27_FLAG,
		v29 as DIAG_28_FLAG,
		v30 as DIAG_29_FLAG,
		v31 as DIAG_30_FLAG,
		v32 as DIAG_31_FLAG,
		v33 as DIAG_32_FLAG,
		v34 as DIAG_33_FLAG,
		v35 as DIAG_34_FLAG,
		v36 as DIAG_35_FLAG,
		v37 as DIAG_36_FLAG,
		v38 as DIAG_37_FLAG,
		v39 as DIAG_38_FLAG,
		v40 as DIAG_39_FLAG,
		v41 as DIAG_40_FLAG,
		v42 as DIAG_41_FLAG,
		v43 as DIAG_42_FLAG,
		v44 as DIAG_43_FLAG,
		v45 as DIAG_44_FLAG,
		v46 as DIAG_45_FLAG,
		v47 as DIAG_46_FLAG,
		v48 as DIAG_47_FLAG,
		v49 as DIAG_48_FLAG,
		v50 as DIAG_49_FLAG,
		v51 as DIAG_50_FLAG,
		v52 as DIAG_51_FLAG,
		v53 as DIAG_52_FLAG,
		v54 as DIAG_53_FLAG,
		v55 as DIAG_54_FLAG,
		v56 as DIAG_55_FLAG,
		v57 as DIAG_56_FLAG,
		v58 as DIAG_57_FLAG,
		v59 as DIAG_58_FLAG,
		v60 as PRC_59_FLAG,
		v61 as PRC_60_FLAG,
		v62 as PRC_61_FLAG,
		v63 as PRC_62_FLAG,
		v64 as PRC_63_FLAG,
		v65 as PRC_64_FLAG,
		v66 as PRC_65_FLAG,
		v67 as PRC_66_FLAG,
		v68 as RX_67_FLAG,
		v69 as RX_68_FLAG,
		v70 as RX_69_FLAG,
		v71 as RX_70_FLAG,
		v72 as RX_71_FLAG,
		v73 as RX_72_FLAG,
		v74 as RX_73_FLAG,
		v75 as RX_74_FLAG,
		v76 as RX_75_FLAG,
		v77 as RX_76_FLAG,
		v78 as RX_77_FLAG,
		v79 as RX_78_FLAG,
		v80 as DIAG_1_FREQ,
		v81 as DIAG_2_FREQ,
		v82 as DIAG_3_FREQ,
		v83 as DIAG_4_FREQ,
		v84 as DIAG_5_FREQ,
		v85 as DIAG_6_FREQ,
		v86 as DIAG_7_FREQ,
		v87 as DIAG_8_FREQ,
		v88 as DIAG_9_FREQ,
		v89 as DIAG_10_FREQ,
		v90 as DIAG_11_FREQ,
		v91 as DIAG_12_FREQ,
		v92 as DIAG_13_FREQ,
		v93 as DIAG_14_FREQ,
		v94 as DIAG_15_FREQ,
		v95 as DIAG_16_FREQ,
		v96 as DIAG_17_FREQ,
		v97 as DIAG_18_FREQ,
		v98 as DIAG_19_FREQ,
		v99 as DIAG_20_FREQ,
		v100 as DIAG_21_FREQ,
		v101 as DIAG_22_FREQ,
		v102 as DIAG_23_FREQ,
		v103 as DIAG_24_FREQ,
		v104 as DIAG_25_FREQ,
		v105 as DIAG_26_FREQ,
		v106 as DIAG_27_FREQ,
		v107 as DIAG_28_FREQ,
		v108 as DIAG_29_FREQ,
		v109 as DIAG_30_FREQ,
		v110 as DIAG_31_FREQ,
		v111 as DIAG_32_FREQ,
		v112 as DIAG_33_FREQ,
		v113 as DIAG_34_FREQ,
		v114 as DIAG_35_FREQ,
		v115 as DIAG_36_FREQ,
		v116 as DIAG_37_FREQ,
		v117 as DIAG_38_FREQ,
		v118 as DIAG_39_FREQ,
		v119 as DIAG_40_FREQ,
		v120 as DIAG_41_FREQ,
		v121 as DIAG_42_FREQ,
		v122 as DIAG_43_FREQ,
		v123 as DIAG_44_FREQ,
		v124 as DIAG_45_FREQ,
		v125 as DIAG_46_FREQ,
		v126 as DIAG_47_FREQ,
		v127 as DIAG_48_FREQ,
		v128 as DIAG_49_FREQ,
		v129 as DIAG_50_FREQ,
		v130 as DIAG_51_FREQ,
		v131 as DIAG_52_FREQ,
		v132 as DIAG_53_FREQ,
		v133 as DIAG_54_FREQ,
		v134 as DIAG_55_FREQ,
		v135 as DIAG_56_FREQ,
		v136 as DIAG_57_FREQ,
		v137 as DIAG_58_FREQ,
		v138 as PRC_59_FREQ,
		v139 as PRC_60_FREQ,
		v140 as PRC_61_FREQ,
		v141 as PRC_62_FREQ,
		v142 as PRC_63_FREQ,
		v143 as PRC_64_FREQ,
		v144 as PRC_65_FREQ,
		v145 as PRC_66_FREQ,
		v146 as RX_67_FREQ,
		v147 as RX_68_FREQ,
		v148 as RX_69_FREQ,
		v149 as RX_70_FREQ,
		v150 as RX_71_FREQ,
		v151 as RX_72_FREQ,
		v152 as RX_73_FREQ,
		v153 as RX_74_FREQ,
		v154 as RX_75_FREQ,
		v155 as RX_76_FREQ,
		v156 as RX_77_FREQ,
		v157 as RX_78_FREQ,
		v158 as DIAG_1_AFREQ,
		v159 as DIAG_2_AFREQ,
		v160 as DIAG_3_AFREQ,
		v161 as DIAG_4_AFREQ,
		v162 as DIAG_5_AFREQ,
		v163 as DIAG_6_AFREQ,
		v164 as DIAG_7_AFREQ,
		v165 as DIAG_8_AFREQ,
		v166 as DIAG_9_AFREQ,
		v167 as DIAG_10_AFREQ,
		v168 as DIAG_11_AFREQ,
		v169 as DIAG_12_AFREQ,
		v170 as DIAG_13_AFREQ,
		v171 as DIAG_14_AFREQ,
		v172 as DIAG_15_AFREQ,
		v173 as DIAG_16_AFREQ,
		v174 as DIAG_17_AFREQ,
		v175 as DIAG_18_AFREQ,
		v176 as DIAG_19_AFREQ,
		v177 as DIAG_20_AFREQ,
		v178 as DIAG_21_AFREQ,
		v179 as DIAG_22_AFREQ,
		v180 as DIAG_23_AFREQ,
		v181 as DIAG_24_AFREQ,
		v182 as DIAG_25_AFREQ,
		v183 as DIAG_26_AFREQ,
		v184 as DIAG_27_AFREQ,
		v185 as DIAG_28_AFREQ,
		v186 as DIAG_29_AFREQ,
		v187 as DIAG_30_AFREQ,
		v188 as DIAG_31_AFREQ,
		v189 as DIAG_32_AFREQ,
		v190 as DIAG_33_AFREQ,
		v191 as DIAG_34_AFREQ,
		v192 as DIAG_35_AFREQ,
		v193 as DIAG_36_AFREQ,
		v194 as DIAG_37_AFREQ,
		v195 as DIAG_38_AFREQ,
		v196 as DIAG_39_AFREQ,
		v197 as DIAG_40_AFREQ,
		v198 as DIAG_41_AFREQ,
		v199 as DIAG_42_AFREQ,
		v200 as DIAG_43_AFREQ,
		v201 as DIAG_44_AFREQ,
		v202 as DIAG_45_AFREQ,
		v203 as DIAG_46_AFREQ,
		v204 as DIAG_47_AFREQ,
		v205 as DIAG_48_AFREQ,
		v206 as DIAG_49_AFREQ,
		v207 as DIAG_50_AFREQ,
		v208 as DIAG_51_AFREQ,
		v209 as DIAG_52_AFREQ,
		v210 as DIAG_53_AFREQ,
		v211 as DIAG_54_AFREQ,
		v212 as DIAG_55_AFREQ,
		v213 as DIAG_56_AFREQ,
		v214 as DIAG_57_AFREQ,
		v215 as DIAG_58_AFREQ,
		v216 as PRC_59_AFREQ,
		v217 as PRC_60_AFREQ,
		v218 as PRC_61_AFREQ,
		v219 as PRC_62_AFREQ,
		v220 as PRC_63_AFREQ,
		v221 as PRC_64_AFREQ,
		v222 as PRC_65_AFREQ,
		v223 as PRC_66_AFREQ,
		v224 as RX_67_AFREQ,
		v225 as RX_68_AFREQ,
		v226 as RX_69_AFREQ,
		v227 as RX_70_AFREQ,
		v228 as RX_71_AFREQ,
		v229 as RX_72_AFREQ,
		v230 as RX_73_AFREQ,
		v231 as RX_74_AFREQ,
		v232 as RX_75_AFREQ,
		v233 as RX_76_AFREQ,
		v234 as RX_77_AFREQ,
		v235 as RX_78_AFREQ,
		v236 as GENDERM,
		v237 as patient_age as AGE,
		v238 as lookback_days,
		v239 as ER_FREQ,
		v240 as ER_AFREQ,
		v241 as ER_FLAG
	from outdir.final_data;
quit;


proc export data=outdir.final_data
			outfile="D:\shire_followup\data\final_data.txt"
			dbms=dlm;
			delimiter=",";
run;

proc sql;
	select count(*) from outdir.final_data_pt;
quit;

/*Clean the final data*/

proc sql;
	select count(*) as cnt1, count(distinct patient_id) as cnt2 from outdir.final_data;
quit;

proc import datafile="D:\shire_followup\data\pos_clean.csv"
			out = outdir.pos_clean_data;
run;

/*HAE patients lookback days */
proc sql;
create table pos_lookbackdays as
	select distinct LOOKBACK_DAYS from outdir.Pos_clean_data;
quit;

proc sql noprint;
	select count(*) into :poscnt from pos_lookbackdays;
quit;
%put &poscnt.;

%let DaysOneSide = 29;

data pos_lookbackdays;
	set pos_lookbackdays;
	minday = LOOKBACK_DAYS - &DaysOneSide.;
	maxday = LOOKBACK_DAYS + &DaysOneSide.;
	id = _n_;
run;

proc sql;
create table neg_lookbackdays as
	select distinct lookback_days from outdir.final_data;
quit;

proc sql;
	select count(*) from neg_lookbackdays;
quit;

%macro lookbackdays;
	data neg_lookbackdays;
		set neg_lookbackdays;
		flag = 0;
	run;

	%do i = 1 %to &poscnt.;
		proc sql noprint;
			select minday, maxday into :minday, :maxday from pos_lookbackdays where id = &i;
		quit;
		%put &minday &maxday;

		data neg_lookbackdays;
			set neg_lookbackdays;
			if lookback_days ge &minday. and lookback_days le &maxday. then flag = flag+1;
		run;
	%end;

	data neg_lookbackdays_final;
		set neg_lookbackdays;
		if flag = 0 then delete;
	run;
%mend;
%lookbackdays;

proc sql;
	select count(*) from neg_lookbackdays
	union
	select count(*) from neg_lookbackdays_final;
quit;

proc sql noprint;
	select lookback_days into :dropDays separated "," from neg_lookbackdays where lookback_days not in
	(select lookback_days from neg_lookbackdays_final);
quit;

%put &dropDays.;

/*Select variabels to remove patents whose events < 3*/
libname outdir2 "D:\shire_followup\data";
proc sql;	
create table ck1 as
	select count(*) from outdir.final_data
	union all
	select count(*) from outdir2.final_data;
quit;

proc contents data=outdir.final_data;
	ods output Variables = variables;
run;

data flag_variables;
	set variables;
	if kindex(Variable, "_FLAG") > 0;
	keep Variable;
run;

proc sql noprint;
	select Variable into :allvar separated " " from flag_variables;
quit;

%put &allvar;

/*Data Clean*/
data outdir.final_data_Clean;
	set outdir.final_data;
	_flag_cnt = sum(of &allvar);
	if _flag_cnt <3 or age le 12 
/*		or lookback_days in (&dropDays.) */
		then delete;
/*	GENDER = GENDERM;*/
	drop  _flag_cnt;
run;

proc sql;
create table pats_summary as
	select "After Cleaning" as type, count(*) as cnt from outdir.final_data_Clean
	union all
	select "Before Cleaning" as type, count(*) as cnt from  outdir.final_data;
quit;

data outdir.final_data_Clean_ck;
	set outdir.final_data_Clean(obs=100);
	array x _numeric_;
	do i = 1 to dim(x);
		if x(i) lt 0 then j+1;
	end;
	keep j;
run;

data outdir.final_data_ck;
	set outdir.final_data(obs=100);
	array x _numeric_;
	do i = 1 to dim(x);
		if x(i) lt 0 then j+1;
	end;
	keep j;
run;

proc sql;
create table neg_value_ck as
	select 1 as type,count(*) as cnt from outdir.final_data_ck where j>0
	union
	select 2 as type,  count(*) as cnt from outdir.final_data_Clean_ck where j>0;
quit;



/*200k and 3M check*/

proc sql;
	select count(*), count(distinct patient_id) from outdir.Nonhae_200k
	union all
	select count(*), count(distinct patient_id) from outdir.Nonhae_200k_v2;
quit;

proc sql;
create table ck1 as
	select a.* from outdir.Nonhae_200k_v2 a
	inner join
	(select patient_id, count(*) as cnt from outdir.Nonhae_200k_v2 group by patient_id having count(*) >1) b
	on a.patient_id = b.patient_id
	order by a.patient_id;
quit;

proc sql;
create table _200k_3M_match as
	select a.patient_id as patient_id_200k,
			b.patient_id as patient_id_3M,
			case 
				when b.patient_id = . then 0
				else 1
			end as match_flag
			from (select distinct patient_id from outdir.Nonhae_200k) a
			left join (select distinct patient_id from outdir.Nonhae_200k_v2) b
			on a.patient_id = b.patient_id
	;
quit;

proc sql;
	select match_flag, count(*) as cnt from _200k_3M_match
	group by match_flag;
quit;

