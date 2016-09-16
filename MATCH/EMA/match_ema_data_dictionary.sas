
ods trace on/label;
proc contents data = tmp1.match_ema_v21mini varnum;
run;
ods trace off;

ods pdf file="D:\REACH\MATCH\Dataset\MATCH_EMA\Combined\V21\MATCH_EMA_V21mini_Dictionary.pdf";
ods listing;
ods select position;
title "MATCH_EMA_V21 mini Dictionary";
proc contents data = tmp1.match_ema_v21mini varnum;
run;
ods pdf close;

ods pdf file="D:\REACH\MATCH\Dataset\MATCH_EMA\Combined\V21\MATCH_EMA_V21_Dictionary.pdf";
ods listing;
ods select position;
title "MATCH_EMA_V21 Dictionary";
proc contents data = tmp1.match_ema_v21 varnum;
run;
ods pdf close;
