libname match 'C:\Users\wangjink\Documents\REACH\MATCH\Project MATCH';
run;

data activity;
set match.Match_ema_V14 (keep=ID WAVE DID DATE MOTHER COMPLY MOTHER_WITHCHILD MOTHER_WITHCHILD_0C MOTHER_STRESSED CHILD_STRESSED MOTHER_STRESSED_0C CHILD_STRESSED_0C MVPA_120_BEFORE SED_120_BEFORE OMVPA_120_BEFORE OSED_120_BEFORE MVPA_120_AFTER OMVPA_120_AFTER SED_120_AFTER OSED_120_AFTER);
if COMPLY^=1 then delete;
if MOTHER_WITHCHILD^=1 & MOTHER_WITHCHILD_0C ^=1 then delete;

logMVPA_120_BEFORE=log(MVPA_120_BEFORE);
logOMVPA_120_BEFORE=log(OMVPA_120_BEFORE);
logMVPA_120_AFTER=log(MVPA_120_AFTER);
logOMVPA_120_AFTER=log(OMVPA_120_AFTER);
run;

data mother;
set activity;
if mother=0 then delete;
run;

data child;
set activity;
if mother=1 then delete;
run;

proc mixed data=child;
class DID;
model SED_120_AFTER=MOTHER_STRESSED_0C CHILD_STRESSED OSED_120_AFTER / s;
repeated / type=un subject=DID hlm;
run;

proc mixed data=mother;
class DID CHILD_STRESSED_0C MOTHER_STRESSED;
model SED_120_AFTER=CHILD_STRESSED_0C MOTHER_STRESSED OSED_120_AFTER / s;
repeated / type=un subject=DID hlm;
run;









