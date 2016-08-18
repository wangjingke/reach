work_dir="C:/Users/wangjink/Documents/REACH/MATCH/EMA"
setwd(work_dir)

target_dir="Y:/Jing/EMA/data"
MATCHs=list.files(target_dir, full.names = TRUE)

# parallel processing
library(foreach)
library(doParallel)
triage=function(jobs, nCL) {    # Function to split a vector down to job list for each node
    splits=rep(1:nCL, length.out=length(jobs))
    return(split(jobs, splits))
}

nCL=4
cl=makeCluster(nCL)
registerDoParallel(cl)

jobList=triage(MATCHs, nCL)
checklist=foreach(m=1:nCL, .combine = rbind) %dopar% {
    job=jobList[[m]]
    checklistX=data.frame(job=job, survey=NA, stringsAsFactors = FALSE)
    for (j in 1:length(job)) {checklistX[j, "survey"]=any(grepl("survey", list.files(paste0(job[j], "/.match"))))}
    print(checklistX)
}
stopCluster(cl)

# reading in necessary functions
source("C:/Users/wangjink/Documents/Bitbucket/reach/MATCH/EMA/backend/match_ema_aggregate.R")
# limit to the ones with survey folder and exclude MATCH_0001 - MATCH_0032
surveyfolder=checklist[checklist$survey==TRUE,]
surveyfolder=surveyfolder[!grepl(paste(sprintf("MATCH_00%02d", 1:32), collapse = "|"), surveyfolder$job),]

for (i in 1:length(surveyfolder$job)) {
    dirX=tail(strsplit(surveyfolder$job[i], "/")[[1]], 1)
    path=paste0(surveyfolder$job[i], "/.match/survey")
    surveydate=grep("^20[0-9]{2}-[0-9]{2}-[0-9]{2}$", list.files(path), value = TRUE)
    for (j in 1:length(surveydate)) {
        emaX=ema.aggregate(paste0(path, "/", surveydate[j]))
        if (!all(is.na(emaX))) {
            if (!file.exists(dirX)) {dir.create(dirX)}
            emaXname=paste0(dirX, "/", paste(tail(strsplit(surveyfolder$job[i], "/")[[1]], 1), paste(emaX$id, collapse = "_"), surveydate[j], paste0("B", length(emaX$battery)),"wockets.rds", sep="_"))
            saveRDS(emaX, file = paste0("./", emaXname))
        }
    }
    print(paste0(i, "; ", round(i/length(surveyfolder$job)*100, 2), "%"))
}

# summarize
write.csv(ema.archive(work_dir), paste0("MATCH_EMA_List_Wockets_", Sys.Date(), ".csv"), row.names=FALSE, quote=FALSE)



# # diagnosis for duplicates within same folder
# emaprompts=read.csv("C:/Users/wangjink/Documents/REACH/MATCH/EMA/MATCH_EMA_LIST_WOCKETS_2016-04-14.csv", header = TRUE, stringsAsFactors = FALSE)
# samefolder=emaprompts[emaprompts$num>1,]
#
# wave1=readWorksheetFromFile("C:/Users/wangjink/Documents/Bitbucket/reach/MATCH/EMA/match_ema_schedule.xlsx", sheet="W1")
# wave1$mother=substring(wave1$Participant.ID, 1, 5)
# wave1$child=paste0("12", substring(wave1$mother, 3, 5))
# wave1$start=sapply(strsplit(wave1$Wave.1.Appointment.Date, " "), head, 1)
#
# wave2=readWorksheetFromFile("C:/Users/wangjink/Documents/Bitbucket/reach/MATCH/EMA/match_ema_schedule.xlsx", sheet="W2")
# wave2$mother=substring(wave2$Participant.ID, 1, 5)
# wave2$child=paste0("12", substring(wave2$mother, 3, 5))
# wave2$start=sapply(strsplit(wave2$Wave.2.Appointment.Date, " "), head, 1)
#
# ema=merge(wave1[c("mother", "child", "start")], wave2[c("mother", "child", "start")], by=c("mother", "child"), all = TRUE)
# ema$enterDate1=sapply(ema$start.x, ema.dateConv)
# ema$exitDate1=sapply(ema$enterDate1, ema.dayAfter, 7)
# ema$enterDate2=sapply(ema$start.y, ema.dateConv)
# ema$exitDate2=sapply(ema$enterDate2, ema.dayAfter, 7)
#
# samefolder$conflict=NA
# for (i in 1:nrow(samefolder)) {
#     dateX=strptime(samefolder$date[i], format = "%Y-%m-%d", tz="America/Los_Angeles")
#     indiv=strsplit(samefolder$subjectID[i], "_")[[1]]
#
#     loc1=max(grep(indiv[1], ema$mother), grep(indiv[1], ema$child))
#     in11=strptime(ema$enterDate1[loc1], format = "%Y-%m-%d", tz="America/Los_Angeles")
#     out11=strptime(ema$exitDate1[loc1], format = "%Y-%m-%d", tz="America/Los_Angeles")
#     in12=strptime(ema$enterDate2[loc1], format = "%Y-%m-%d", tz="America/Los_Angeles")
#     out12=strptime(ema$exitDate2[loc1], format = "%Y-%m-%d", tz="America/Los_Angeles")
#     conflict1=ifelse((in11<=dateX & dateX<=out11) | (in12<=dateX & dateX<=out12), 1, 0)
#
#     loc2=max(grep(indiv[2], ema$mother), grep(indiv[2], ema$child))
#     in21=strptime(ema$enterDate1[loc2], format = "%Y-%m-%d", tz="America/Los_Angeles")
#     out21=strptime(ema$exitDate1[loc2], format = "%Y-%m-%d", tz="America/Los_Angeles")
#     in22=strptime(ema$enterDate2[loc2], format = "%Y-%m-%d", tz="America/Los_Angeles")
#     out22=strptime(ema$exitDate2[loc2], format = "%Y-%m-%d", tz="America/Los_Angeles")
#     conflict2=ifelse((in21<=dateX & dateX<=out21) | (in22<=dateX & dateX<=out22), 1, 0)
#
#     if (conflict1==1 & conflict2==1) samefolder$conflict[i]=1
# }
#
#
# emaprompts = read.csv("C:/Users/wangjink/Documents/REACH/MATCH/EMA/MATCH_EMA_List_Wockets_2016-05-03.csv", header = TRUE, stringsAsFactors = FALSE)
# emaprompts = emaprompts[emaprompts$correctFolder == FALSE & emaprompts$num == 2 & !emaprompts$folder %in% c(sprintf("110%02d", 1:32), sprintf("120%02d", 1:32)), ]
#
#
# test = emaprompts[emaprompts$correctFolder==FALSE & grepl("12070", emaprompts$subjectID),]
# emaprompts[grepl("12118", emaprompts$subjectID),]
