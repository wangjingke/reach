libname match "D:\REACH\MATCH\Dataset\MATCH_EMA\Combined";
libname jen "Y:\Jing\Jen";

data jen.emaPAcomparison20160831;
set match.Match_ema_v20 (keep = subjectid wave weekend comply prompt_start prompt_end child_done_sports child_done_tv mvpa_120_before sed_120_before);
if wave = 1;
run;
