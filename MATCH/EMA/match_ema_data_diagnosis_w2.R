work_dir="C:/Users/wangjink/Documents/REACH/MATCH/Dataset/MATCH_EMA/Wave2/Beta20160714"
setwd(work_dir)

# load necessary functions
source("C:/Users/wangjink/Documents/Bitbucket/reach/MATCH/EMA/backend/match_ema_diagnosis.R")

w2raw = readRDS("MATCH_EMA_W2_2016-07-14.rds")
##################
### unprompted ###
##################
w2noComply = w2raw[is.na(w2raw$COMPLY), 1:19]
w2noComply = ema.diagnosis(w2noComply, emaDir = "C:/Users/wangjink/Documents/REACH/MATCH/EMA_Clean_RDS/W2", emaList = "MATCH_EMA_List_Clean_2016-07-14.csv")

# remove indivs with no EMA data in wave 2 (11087, 11125, 12122, 12125)
sum(w2noComply$SubjectID %in% c("11087", "11125", "12122", "12125"))
sum(w2noComply$SubjectID %in% c("11087", "11125"))
sum(w2noComply$SubjectID %in% c("12122", "12125"))
w2noComply = w2noComply[!w2noComply$SubjectID %in% c("11087", "11125", "12122", "12125"),]

w2noComply = ema.reason.automatic(w2noComply)
table(w2noComply$primary, useNA = "ifany")
table(w2noComply$primary, w2noComply$mother, useNA = "ifany")

# missing prompts that need manual check
w2noComply = ema.battery(ema = w2noComply, emaDir = "C:/Users/wangjink/Documents/REACH/MATCH/EMA_Clean_RDS/W2")
w2noComply = ema.inWakeWindow(ema = w2noComply, emaDir = "C:/Users/wangjink/Documents/REACH/MATCH/EMA_Clean_RDS/W2", emaList = "MATCH_EMA_List_Clean_2016-07-14.csv")

# summarize the causes
w2noComply = ema.causeOfMiss(w2noComply)
table(w2noComply$causeOfMiss, useNA = "ifany")
table(w2noComply$causeOfMiss, w2noComply$mother, useNA = "ifany")

# unknown ones
lostInTrans = w2noComply[is.na(w2noComply$file) & is.na(w2noComply$causeOfMiss),] # add into the lost in upload/backup box
unique(lostInTrans$SubjectID)
table(lostInTrans$mother)

unknown = w2noComply[is.na(w2noComply$causeOfMiss) & !is.na(w2noComply$file),]
nrow(unknown)
table(unknown$mother)


################
### prompted ###
################
w2comply = w2raw[!is.na(w2raw$COMPLY), 1:19]
length(unique(w2comply$SubjectID))
sum(unique(w2comply$SubjectID)>12000)
nrow(w2comply)
table(w2comply$mother)

# insufficent sample n<3
sample = aggregate(Date~SubjectID, data=w2comply, length)
sample[sample$Date<3,]

w2comply = w2comply[w2comply$SubjectID != "12020", ]
# unanswered
unanswered = w2comply[w2comply$COMPLY==0,]
nrow(unanswered)
table(unanswered$mother)

# answered
answered = w2comply[w2comply$COMPLY==1,]
nrow(answered)
table(answered$mother)
length(unique(answered$SubjectID))
sum(unique(answered$SubjectID)>12000)
w2comply$SubjectID[!w2comply$SubjectID %in% answered$SubjectID] # 11155 never answered any prompts
length(unique(substring(answered$SubjectID, 3, 5))) # valid dyads
sum(duplicated(substring(unique(answered$SubjectID), 3, 5))) # effective dyads
nrow(answered[substring(answered$SubjectID, 3, 5) %in% substring(unique(answered$SubjectID), 3, 5)[duplicated(substring(unique(answered$SubjectID), 3, 5))],])  


# complete/incomplete EMA prompts
incomplete = answered[answered$COMPLETE == 0, ]
table(incomplete$mother)
complete = answered[answered$COMPLETE == 1, ]
nrow(complete)
table(complete$mother)









