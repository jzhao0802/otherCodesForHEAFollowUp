%let outdir = X:\Temp_zywang\165M2Csv;
libname datain "F:\shire_followup\data";
libname dataout "&outdir.";
libname datae "F:\shire_followup\data\dataset_165M";

data datain.dataset_event0;
	set datae.datset_i0_1 - datae.datset_i0_21;
	datasource = "EV0";
run;


%macro splitdata(intable, outable, parts, outfile);
proc sql ;
	create table totalcnt as
		select count(*) as cnt from datain.&intable.;
quit;

data null;
	set totalcnt;
	call symput("cnt", strip(cnt));
run;
%put &cnt;

%do i=1 %to %eval(&parts+1);
	data dataout.&outable._&i.;
		set datain.&intable.;
		if _n_ ge %eval((&i.-1)*%eval(&cnt./&parts.)) and _n_ lt %eval(&i.*%eval(&cnt./&parts.));
	run;

	proc export data= dataout.&outable._&i.
            outfile= "&outdir.\&outfile._&i..csv" 
            dbms=csv replace;
     		putnames=yes;
	run;

	proc datasets library = dataout noprint;
		delete &outable._&i.;
	run;
	quit;
%end;
%mend;

%splitdata(dataset_event0,dataset_event0, 10, out_165M_event0);

%splitdata(Dataset_ii,Dataset_ii, 500, out_165M_eventGT0);


%let intable=Dataset_ii;
