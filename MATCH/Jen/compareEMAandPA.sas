libname match "D:\REACH\MATCH\Dataset\MATCH_EMA\Combined\V21";
libname jen "Y:\Jing\Jen";

data jen.emaPAcomparison20170315;
set match.Match_ema_v21mini (keep = id wave weekend comply promptStart promptEnd cDone_sports cDone_tv valid_120_before nonvalid_120_before mvpa_120_before sed_120_before light_120_before);
if wave = 1;
run;


 
