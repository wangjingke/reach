# madres ema responses format
scale4=rbind(
    scale=c("1. Not at all", "2. A little", "3. Quite a bit", "4. Extremely"),
    level=c(1:4)
)

scaleWU=rbind(
    scale=c("1. 0", "2. 1", "3. 2", "4. 3", "5. 4", "6. 5-8", "7. 9+"),
    level=c(1:7)
)

scaleWell=rbind(
    scale=c("1. Much worse than usual", "2. A little worse than usual", "3. About the same as usual", "4. A little better than usual", "5. Much better than usual"),
    level=c(1:5)
)

scaleWhere=rbind(
    scale=c("1. Home (Indoors)", "2. Home (Outdoors)", "3. Work (Indoors)", "4. Outdoors (not at home)", "5. Car/Bus/Train", "6. Other"),
    level=c(1:6)
)

scaleSafe=rbind(
    scale=c("1. Very unsafe", "2. Somewhat unsafe", "3. Somewhat safe", "4. Very safe"),
    level=c(1:4)
)


# this script assembles EMA data from MADRES pilot study with the accelerometer measurements
work_dir="X:/Maternal and Child Health Study/Pilot/Jing/processed"
setwd(work_dir)

ema_dir="../EMA Data"
# scan for EMA files
ema.list=list.files(path=ema_dir, all.files=TRUE, include.dirs = FALSE, recursive = TRUE, full.names = FALSE)

# load library to read xlsx files
library(XLConnect)
#library for imperfect match rbind
library(plyr)
ema=c()
for (i in 1:length(ema.list)) {
    segmentX = strsplit(ema.list[i], "\\.|_|/")[[1]] # split by dot or underscore
    idX = unique(grep("^.*N[[:digit:]]{4}.*$", segmentX, value = TRUE))
    extenX = tail(segmentX, n = 1)
    # reading in EMA file based on its extension
    if (extenX == "xlsx") {
        fileX = readWorksheetFromFile(paste0(ema_dir, "/", ema.list[i]), sheet = 1, header = TRUE, colTypes="character")
    } else {
        fileX = read.csv(paste0(ema_dir, "/", ema.list[i]), stringsAsFactors = FALSE)
    }
    questX = fileX[grepl("^.*Random Time", fileX$Trigger), ] # extract only the questionaires

    # align answers from morning questionnaires with daily ones
    morning=grep("MADRES_Morning", questX$Form)
    am.q=grep("am", names(questX), value = TRUE)
    questX[morning, gsub("_am", "", am.q)]= questX[morning, am.q]
    # drop morning questions
    questX=questX[,!names(questX) %in% am.q]
    questX$N_id=idX
    # the answer to timeuse_done has changed from a scale of 6 to 9 in the last two, so use the new scale as standard, and convert the old ones to new
    if (!idX %in% c("N0236", "N0237")) {
        colnames(questX)[colnames(questX)=="timeuse_done_6"] = "timeuse_done_9"
        colnames(questX)[colnames(questX)=="timeuse_done_5"] = "timeuse_done_7"
        colnames(questX)[colnames(questX)=="timeuse_done_4"] = "timeuse_done_6"
        colnames(questX)[colnames(questX)=="timeuse_done_3"] = "timeuse_done_5"
    }

    ema=rbind.fill(ema, questX)
}

# rearange the columns
pos_id=grep("N_id", names(ema))
ema=ema[, c(pos_id, (1:ncol(ema))[-pos_id])]
ema=ema[, !grepl("am", names(ema))]
ema$sleep_waketime=sapply(strsplit(ema$sleep_waketime, " "), tail, 1)
ema$sleep_sleeptime=sapply(strsplit(ema$sleep_sleeptime, " "), tail, 1)

# merge the five sleep quality variables into one
for (i in 1:nrow(ema)) {
    quality=grep("1", ema[i, c("sleep_quality_1", "sleep_quality_2", "sleep_quality_3", "sleep_quality_4", "sleep_quality_5")])
    ema[i, "sleep_quality"]=ifelse(length(quality)>0, quality, ema[i, "sleep_quality"])
}
# drop excessive sleep quality variables
ema=subset(ema, select=-c(sleep_quality_1, sleep_quality_2, sleep_quality_3, sleep_quality_4, sleep_quality_5))

# uniform date
dateSplit=function(x) {
    segments=strsplit(x, " ")[[1]][1]
    
    if (grepl("/", segments)) {
        year=strsplit(segments, "/")[[1]][3]
        mon=strsplit(segments, "/")[[1]][1]
        if(nchar(mon)==1) {mon=paste0("0", mon)}
        day=strsplit(segments, "/")[[1]][2]
        if(nchar(day)==1) {day=paste0("0", day)}
        return(paste(year, mon, day, sep="-"))
    } else {
        return(segments)
    }
}
ema$Form_start_date=sapply(ema$Form_start_date, dateSplit)
ema$Form_finish_date=sapply(ema$Form_finish_date, dateSplit)

# uniform time
timeSplit=function(x) {segments=tail(strsplit(x, " ")[[1]], 1)}
ema$Form_start_time=sapply(ema$Form_start_time, timeSplit)
ema$Form_finish_time=sapply(ema$Form_finish_time, timeSplit)

# convert variables to factors
keys=readWorksheetFromFile("D:/GitHub/reach/MADRES/pilot/madres_pilot_ema_keys.xlsx", sheet = 1, header = TRUE)
for (i in 1:nrow(keys)) {
    # change to factor first
    if (!is.na(keys[i, "Scale"])) {
        scaleX=get(keys$Scale[i])
        ema[,keys$SurveyName[i]]=factor(ema[,keys$SurveyName[i]], levels=scaleX["level",], labels=scaleX["scale",])
    }
    # change variable name
    if (!is.na(keys[i, "EmaName"])) {
        colnames(ema)[colnames(ema)==keys$SurveyName[i]]=keys$EmaName[i]
    }
}

# creating time stamp for the survey start point
ema$timestamp=strptime(paste0(ema$Form_start_date, " ", ema$Form_start_time), format="%Y-%m-%d %H:%M:%S", tz="America/Los_Angeles")

# drop greetings
ema$greeting=NULL
ema$greeting_end=NULL
ema$cortisol_wake=NULL
ema$cortisol_wake30=NULL
ema$cortisol_afternoon=NULL
ema$cortisol_bedtime=NULL

#### EMA compliance ####
compliance = ema[,c("N_id", "Missing")]
compliance$Missing = ifelse(is.na(compliance$Missing) | compliance$Missing=="", "done", compliance$Missing)
compRate = table(compliance$N_id, compliance$Missing, useNA = "ifany")
write.table(compRate, "clipboard", row.names = TRUE, col.names = TRUE, sep = "\t")

# adding the ACC data
acc_dir="../Accelerometer Data"
# scan for ACC files, search csv files containing the accelerometer data ending with "sec.csv"
acc.list=list.files(path=acc_dir, pattern="^.*(EMAP|sec)\\.csv$", all.files=TRUE, include.dirs = FALSE, recursive = TRUE, full.names = FALSE)
# loading functions for processing ACC files
source("D:/GitHub/reach/common/functions_acc_process.R")

#### accelerometer compliance ####
accSummary=c()
accBrief=c()
for (i in 1:length(acc.list)) {
    accX = acc.sum.mp(acc.read(paste0("X:/Maternal and Child Health Study/Pilot/Jing/Accelerometer Data/", acc.list[i])), id=substr(acc.list[i], 1, 5), age=18)
    accSummary = rbind(accSummary, cbind(subjectID = accX$abstract$ID, accX$details))
    accBrief = rbind(accBrief, accX$abstract)
}
write.table(accSummary, "clipboard", row.names = FALSE, sep = "\t")
write.table(accBrief, "clipboard", row.names = FALSE, sep = "\t")

# acc measurements
acc_point=c(outer(outer(c("valid", "nonvalid", "sed", "light", "mod", "vig", "mvpa"), c("_15", "_30", "_60", "_120"), paste0), c("_before", "_after"), paste0))
acc_win=paste0(outer(c("valid", "nonvalid", "sed", "light", "mod", "vig", "mvpa"), c("_30", "_60", "_120", "_240"), paste0), "_window")
acc=data.frame(matrix(NA, nrow=nrow(ema), ncol=length(acc_point)+length(acc_win), dimnames = list(c(), c(acc_point, acc_win))))
ema=cbind(ema, acc)

MinToSec=function(u) {return(u*60)}

indiv=unique(ema$N_id)
MVPA=rbind(
    category=c("nonwear", "sedentary", "light", "moderate", "vigorous"),
    var=c("nonvalid", "sed", "light", "mod", "vig")
)
for (i in 1:length(indiv)) {
    accX=acc.sum.mp(acc.read(paste0(acc_dir,"/",grep(indiv[i], acc.list, value=TRUE))), age=18, id=indiv[i])
    # locate the row num of indiv
    loc=grep(indiv[i], ema$N_id)
    for (j in 1:length(loc)) {
        anchor1=ema[loc[j], "timestamp"]
        # loop through all MVPA var
        for (k in c(15, 30, 60, 120)) {
            anchor0=anchor1-MinToSec(k)
            anchor2=anchor1+MinToSec(k)
            
            # before prompt
            mvpaX=acc.mvpa(accX, start=anchor0, end=anchor1)
            for (m in 1:nrow(mvpaX)) {
                type=MVPA["var", grep(mvpaX$MVPA[m], MVPA["category",])]
                ema[loc[j], paste0(type, "_", k, "_before")]=mvpaX[m, "min"]
            }
            # after prompt
            mvpaX=acc.mvpa(accX, start=anchor1, end=anchor2)
            for (m in 1:nrow(mvpaX)) {
                type=MVPA["var", grep(mvpaX$MVPA[m], MVPA["category",])]
                ema[loc[j], paste0(type, "_", k, "_after")]=mvpaX[m, "min"]
            }
        }
    }
}

for (k in c(15, 30, 60, 120)) {
    ema[,paste0("valid_", k, "_before")]=apply(ema[,paste0(c("sed_", "light_", "mod_", "vig_"), paste0(k, "_before"))], 1, sum, na.rm=TRUE)
    ema[,paste0("mvpa_", k, "_before")]=apply(ema[,paste0(c("mod_", "vig_"), paste0(k, "_before"))], 1, sum, na.rm=TRUE)
    ema[,paste0("valid_", k, "_after")]=apply(ema[,paste0(c("sed_", "light_", "mod_", "vig_"), paste0(k, "_after"))], 1, sum, na.rm=TRUE)
    ema[,paste0("mvpa_", k, "_after")]=apply(ema[,paste0(c("mod_", "vig_"), paste0(k, "_after"))], 1, sum, na.rm=TRUE)
    for (m in c("valid", "nonvalid", "sed", "light", "mod", "vig", "mvpa")) {
        ema[,paste0(m, "_", 2*k, "_window")]=apply(ema[,paste0(paste0(m, "_", k), c("_before", "_after"))], 1, sum, na.rm=TRUE)
    }
}

# output data for StatTransfer
save(ema, list="ema", file = "madres_pilot_ema_20161122.RData")

# creating STATA script for labeling variables
StataLabel=file("madres_pilot_ema_20161122.do", "w")
for (i in 1:nrow(keys)) {
    varnameX=ifelse(is.na(keys$EmaName[i]), keys$SurveyName[i], keys$EmaName[i])
    if (!is.na(keys$Label[i])) {
        commandX=paste0('label variable ', varnameX, ' "', keys$Label[i], ' ', ifelse(is.na(keys$SubLabel[i]), "", keys$SubLabel[i]), '"')
        write(commandX, file=StataLabel, append = TRUE)
    }
}
close(StataLabel)





















