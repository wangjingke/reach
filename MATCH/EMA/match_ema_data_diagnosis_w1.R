work_dir="D:/REACH/MATCH/Dataset/MATCH_EMA/Wave1/Beta20160825"
setwd(work_dir)

# load necessary functions
source("D:/GitHub/reach/MATCH/EMA/backend/match_ema_diagnosis.R")

w1raw = readRDS("MATCH_EMA_W1_2016-08-26.rds")
##################
### unprompted ###
##################
w1noComply = w1raw[is.na(w1raw$COMPLY), 1:19]
w1noComply = ema.diagnosis(w1noComply, emaDir = "D:/REACH/MATCH/EMA_Clean_RDS/W1", emaList = "MATCH_EMA_List_Clean_2016-08-24.csv")

compliance = readRDS("MATCH_W1_EMA_Compliance_2016-08-25.rds")
compliance$attention[compliance$attention$Missing==100,]
# remove indivs with no EMA data in wave 1 (11032, 11049, 12032, 12116, 12154, 12163, 12166, 12184, 12197)
sum(w1noComply$SubjectID %in% c("11032", "11049", "12032", "12116", "12154", "12163", "12166", "12184", "12197"))
sum(w1noComply$SubjectID %in% c("11032", "11049"))
sum(w1noComply$SubjectID %in% c("12032", "12116", "12154", "12163", "12166", "12184", "12197"))
w1noComply = w1noComply[!w1noComply$SubjectID %in% c("11032", "11049", "12032", "12116", "12154", "12163", "12166", "12184", "12197"),]

w1noComply = ema.reason.automatic(w1noComply)
table(w1noComply$primary, useNA = "ifany")
table(w1noComply$primary, w1noComply$mother, useNA = "ifany")

# missing prompts that need manual check
w1noComply = ema.battery(ema = w1noComply, emaDir = "D:/REACH/MATCH/EMA_Clean_RDS/W1")
w1noComply = ema.inWakeWindow(ema = w1noComply, emaDir = "D:/REACH/MATCH/EMA_Clean_RDS/W1", emaList = "MATCH_EMA_List_Clean_2016-08-24.csv")

# summarize the causes
w1noComply = ema.causeOfMiss(w1noComply)
table(w1noComply$causeOfMiss, useNA = "ifany")
table(w1noComply$causeOfMiss, w1noComply$mother, useNA = "ifany")

# unknown ones
lostInTrans = w1noComply[is.na(w1noComply$file) & is.na(w1noComply$causeOfMiss),] # add into the lost in upload/backup box
unique(lostInTrans$SubjectID)
table(lostInTrans$mother)

unknown = w1noComply[is.na(w1noComply$causeOfMiss) & !is.na(w1noComply$file),]
# protocol change (mother before 2014-09-24, kid before 2014-09-26)
for (i in 1:nrow(unknown)) {
    if (unknown$mother[i]==1) {
        unknown$protocol[i]=ifelse(unknown$Date[i]<"2014-09-24", 1, 0)
    } else {
        unknown$protocol[i]=ifelse(unknown$Date[i]<"2014-09-26", 1, 0)
    }
}
table(unknown$protocol)
table(unknown$protocol, unknown$mother)

################
### prompted ###
################
w1comply = w1raw[!is.na(w1raw$COMPLY), 1:19]
length(unique(w1comply$SubjectID))
sum(unique(w1comply$SubjectID)>12000)
nrow(w1comply)
table(w1comply$mother)

# insufficent sample n<3
sample = aggregate(Date~SubjectID, data=w1comply, length)
sample[sample$Date<3,]

w1comply = w1comply[!w1comply$SubjectID %in% c("11157", "12048"), ]
nrow(w1comply)
table(w1comply$mother)
length(unique(w1comply$SubjectID))
sum(unique(w1comply$SubjectID)>12000)
# unanswered
unanswered = w1comply[w1comply$COMPLY==0,]
nrow(unanswered)
table(unanswered$mother)

# answered
answered = w1comply[w1comply$COMPLY==1,]
nrow(answered)
table(answered$mother)
length(unique(answered$SubjectID))
sum(unique(answered$SubjectID)>12000)
w1comply$SubjectID[!w1comply$SubjectID %in% answered$SubjectID]
length(unique(substring(answered$SubjectID, 3, 5))) # valid dyads
sum(duplicated(substring(unique(answered$SubjectID), 3, 5))) # effective dyads
nrow(answered[substring(answered$SubjectID, 3, 5) %in% substring(unique(answered$SubjectID), 3, 5)[duplicated(substring(unique(answered$SubjectID), 3, 5))],])  


# complete/incomplete EMA prompts
incomplete = answered[answered$COMPLETE == 0, ]
nrow(incomplete)
table(incomplete$mother)
complete = answered[answered$COMPLETE == 1, ]
nrow(complete)
table(complete$mother)

