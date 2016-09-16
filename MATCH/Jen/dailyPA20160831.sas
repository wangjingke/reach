libname match "D:\REACH\MATCH\Dataset\MATCH_EMA\Combined";

data wave1;
set match.Match_ema_v20 (keep = subjectID date did wave day_nonvalid day_valid day_sed day_light day_mod day_vig day_mvpa);
if wave=1;
if subjectid>12000 then
    do;
        nonvalid_c = day_nonvalid;
        valid_c = day_valid;
        sed_c = day_sed;
        light_c = day_light;
        mod_c = day_mod;
        vig_c = day_vig;
        mvpa_c = day_mvpa;
    end;
else
    do;
        nonvalid_m = day_nonvalid;
        valid_m = day_valid;
        sed_m = day_sed;
        light_m = day_light;
        mod_m = day_mod;
        vig_m = day_vig;
        mvpa_m = day_mvpa;
    end;
run;

proc sort data=wave1 noduprecs;
by _all_;
run;

proc sort data=wave1;
by did date subjectid;
run;

data _temp1_;
set wave1;
by did date subjectid;
array mmvpa{7} nonvalid_m valid_m sed_m light_m mod_m vig_m mvpa_m;
array tempmvpa {7} temp_nonvalid temp_valid temp_sed temp_light temp_mod temp_vig temp_mvpa;
array daymvpa {7} day_nonvalid day_valid day_sed day_light day_mod day_vig day_mvpa;
retain temp_did temp_date temp_nonvalid temp_valid temp_sed temp_light temp_mod temp_vig temp_mvpa;
if first.did then temp_did=did;
if first.date then
    do;
        temp_date=date;
        do i = 1 to dim(tempmvpa);
            tempmvpa{i}=mmvpa{i};
        end;
        drop i;
    end;
do i = 1 to dim(mmvpa);
    if did=temp_did & date=temp_date & last.date then mmvpa{i}=tempmvpa{i};
end;

drop temp_:;
run;

proc sort data=_temp1_;
by did date descending subjectid;
run;

data _temp2_;
set _temp1_;
by did date descending subjectid;
array cmvpa{7} nonvalid_c valid_c sed_c light_c mod_c vig_c mvpa_c;
array tempmvpa {7} temp_nonvalid temp_valid temp_sed temp_light temp_mod temp_vig temp_mvpa;
array daymvpa {7} day_nonvalid day_valid day_sed day_light day_mod day_vig day_mvpa;
retain temp_did temp_date temp_nonvalid temp_valid temp_sed temp_light temp_mod temp_vig temp_mvpa;
if first.did then temp_did=did;
if first.date then
    do;
        temp_date=date;
        do i = 1 to dim(tempmvpa);
            tempmvpa{i}=cmvpa{i};
        end;
        drop i;
    end;
do i = 1 to dim(cmvpa);
    if did=temp_did & date=temp_date & last.date then cmvpa{i}=tempmvpa{i};
end;
drop wave subjectid day_: temp_:;
run;

libname jen "Y:\Jing\Jen";
proc sort data=_temp2_ noduprecs out=jen.dailypa20160831;
by _ALL_;
run;

proc sort data=jen.dailypa20160831;
by did date;
run;
