# compare manual backup and auto uploads for wave 2
workDir = "D:/REACH/MATCH/EMA_Clean/Wave1"
setwd(workDir)
source("D:/GitHub/reach/MATCH/EMA/backend/match_ema_promptList.R")

library(XLConnect)
w1 = readWorksheetFromFile("D:/GitHub/reach/MATCH/EMA/backend/match_ema_keys.xlsx", sheet="W1", colTypes = "character")
w1 = w1[w1$Wave.1=="complete",]
w1$DID = ifelse(nchar(w1$DID)<3, paste0("0", w1$DID), as.character(w1$DID))
w1$enterDate = sapply(strsplit(ifelse(is.na(w1$ManualCorrectedEnter), w1$W1pickup, w1$ManualCorrectedEnter), " "), head, 1)

w1Schedule = c()
for (i in 1:nrow(w1)) {
    indivSchedule =  rbind(data.frame(subjectID=paste0("11", w1$DID[i]), date=ema.dayAfter(w1$enterDate[i], 0:7), stringsAsFactors = FALSE), data.frame(subjectID=paste0("12", w1$DID[i]), date=ema.dayAfter(w1$enterDate[i], 0:7), stringsAsFactors = FALSE))
    indivSchedule = cbind(indivSchedule, data.frame(matrix(data = NA, nrow = nrow(indivSchedule), ncol = 13, dimnames = list(c(), c("manualFile", "manualPrompts", "manualResponse", "wocketsFile", "wocketsPrompts", "wocketsResponse", "identicalPrompts", "identicalResponse", "cover", "coverManual", "coverWockets", "diffPrompts", "diffResponse"))), stringsAsFactors = FALSE))
    w1Schedule = rbind(w1Schedule, indivSchedule)
    rm(indivSchedule)
}

w1manual = read.csv("D:/REACH/MATCH/EMA_ManualRetrieve/w1/MATCH_EMA_List_Manual_2016-08-22.csv", header = TRUE, stringsAsFactors = FALSE)
w1manual$correctID = substring(w1manual$folder, 1, 5) # the manual backup folder name contains the correct ID, regardless the prompts
w1wockets = read.csv("D:/REACH/MATCH/EMA_Wockets/MATCH_EMA_List_Wockets_2016-06-20.csv", header = TRUE, stringsAsFactors = FALSE)
w1wockets = w1wockets[w1wockets$correctFolder == TRUE & grepl("W1", w1wockets$folder), ] # limit to prompts in correct folder and w1 to increase speed and accuracy, which may harm coverage

# function to return NA instead of logical zero
ema.returnNA = function(x) {
    if (length(x)==0) return(NA) else return(x)
}

# function to get manual survey folder
ema.manualSearchSurvey = function (dataDir, rdsName) {
    dirX = paste0(dataDir, "/", strsplit(rdsName, "/")[[1]][1])
    return(if (any(grepl("survey", list.files(dirX)))) {dirX} else {paste0(dirX, "/.match")})
}

for (i in 1:nrow(w1Schedule)) {
    w1Schedule$manualFile[i] = ema.returnNA(paste(w1manual$file[which(w1manual$date==w1Schedule$date[i] & w1manual$correctID==w1Schedule$subjectID[i])], collapse = ","))
    w1Schedule$wocketsFile[i] = ema.returnNA(paste(w1wockets$file[which(w1wockets$date == w1Schedule$date[i] & grepl(w1Schedule$subjectID[i], w1wockets$file))], collapse=","))
    
    if (w1Schedule$subjectID[i]>12000) {mother=0} else {mother=1}
    # manual
    if (!is.na(w1Schedule$manualFile[i]) & w1Schedule$manualFile[i]!="") {
        manualX = readRDS(paste0("D:/REACH/MATCH/EMA_ManualRetrieve/w1/", w1Schedule$manualFile[i]))
        w1Schedule$manualPrompts[i] = ema.returnNA(nrow(manualX$prompts))
        switch (as.character(mother),
                "1" = {w1Schedule$manualResponse[i] = ema.returnNA(nrow(manualX$responses_mother))},
                "0" = {w1Schedule$manualResponse[i] = ema.returnNA(nrow(manualX$responses_child))}
        )
    }
    # wockets
    if (!is.na(w1Schedule$wocketsFile[i]) & w1Schedule$wocketsFile[i]!="") {
        wocketsX = readRDS(paste0("D:/REACH/MATCH/EMA_Wockets/", w1Schedule$wocketsFile[i]))
        w1Schedule$wocketsPrompts[i] = ema.returnNA(nrow(wocketsX$prompts))
        switch (as.character(mother),
                "1" = {w1Schedule$wocketsResponse[i] = ema.returnNA(nrow(wocketsX$responses_mother))},
                "0" = {w1Schedule$wocketsResponse[i] = ema.returnNA(nrow(wocketsX$responses_child))}
        )
    }
    if (!is.na(w1Schedule$manualPrompts[i]) & !is.na(w1Schedule$wocketsPrompts[i])) {
        w1Schedule$identicalPrompts[i] = identical(manualX$prompts, wocketsX$prompts)
    }
    if (is.na(w1Schedule$manualPrompts[i]) & is.na(w1Schedule$wocketsPrompts[i])) {w1Schedule$identicalPrompts[i] = TRUE}
    if (!is.na(w1Schedule$manualResponse[i]) & !is.na(w1Schedule$wocketsResponse[i])) {
        switch (as.character(mother),
                "1" = {w1Schedule$identicalResponse[i] = identical(manualX$responses_mother, wocketsX$responses_mother)},
                "0" = {w1Schedule$identicalResponse[i] = identical(manualX$responses_child, wocketsX$responses_child)}
        )
    }
    if (is.na(w1Schedule$manualResponse[i]) & is.na(w1Schedule$wocketsResponse[i])) {w1Schedule$identicalResponse[i] = TRUE}
    
    if (exists("manualX")) rm(manualX)
    if (exists("wocketsX")) rm(wocketsX)
    rm(mother)
    if (i%%100 == 0) print(i)
}

# how many days were covered
w1Schedule$cover = ifelse(!is.na(w1Schedule$manualPrompts) | !is.na(w1Schedule$wocketsPrompts), 1, 0)
w1Schedule$coverManual = ifelse(!is.na(w1Schedule$manualPrompts), 1, 0)
w1Schedule$coverWockets = ifelse(!is.na(w1Schedule$wocketsPrompts), 1, 0)
w1Schedule$diffPrompts = w1Schedule$manualPrompts - w1Schedule$wocketsPrompts
w1Schedule$diffResponse = w1Schedule$manualResponse - w1Schedule$wocketsResponse
# Majority of the surveys (> 3 days) from the following subjects were completely missed, requiring investigation
aggregate(date~subjectID, data = w1Schedule[w1Schedule$cover==0,], FUN = function (x) {length(unique(x))})
# 11032, 11049, 11157, 11189, 11194, 11212, 11231, 12032, 12048, 12115, 12147, 12150, 12154, 12163, 12166, 12177, 12184, 12189, 12197

# how many prompt days are identical between wockets and manual
table(w1Schedule$identicalPrompts, w1Schedule$identicalResponse, useNA = "ifany")
# identicalPrompts cannot be NA when manualFile or wocketsFile != ""
w1Schedule[is.na(w1Schedule$identicalPrompts) & w1Schedule$manualFile!="" & w1Schedule$wocketsFile!="",]
w1Schedule$scenario = NA
# check for senarios
for (i in 1:nrow(w1Schedule)) {
    # senario control switch
    # senario 1, only available in manual backup
    if (w1Schedule$manualFile[i]!="" && w1Schedule$wocketsFile[i]=="") {w1Schedule$scenario[i] = "1"} 
    # senario 2, only available in wockets
    if (w1Schedule$manualFile[i]=="" && w1Schedule$wocketsFile[i]!="") {w1Schedule$scenario[i] = "2"}
    # senario 3, available in both, completely identical prompts and responses, use the wockets copy
    if (w1Schedule$manualFile[i]!="" && w1Schedule$wocketsFile[i]!="" && isTRUE(w1Schedule$identicalResponse[i]) && isTRUE(w1Schedule$identicalPrompts[i])) {w1Schedule$scenario[i] = "3"}
    # senario 4, avaiable in both, identical responses but different prompts
    if (w1Schedule$manualFile[i]!="" && w1Schedule$wocketsFile[i]!="" && isTRUE(w1Schedule$identicalResponse[i]) && !isTRUE(w1Schedule$identicalPrompts[i])) {w1Schedule$scenario[i] = "4"}
    # senario 5, not available in eith
    if (w1Schedule$manualFile[i]=="" && w1Schedule$wocketsFile[i]=="") {w1Schedule$scenario[i] = "5"}
}


# copy the identical ones and unique ones to one location
for (i in 1:nrow(w1Schedule)) {
    # actions under each senario, when no senario found, skip to next
    if (is.na(w1Schedule$scenario[i]) | w1Schedule$scenario[i]=="5") {next} # skip when no data coverage for the person-day
    targetDir = paste0(workDir, "/MATCH_", w1Schedule$subjectID[i], "W1")
    if (!dir.exists(targetDir)) {
        dir.create(paste0(targetDir, "/data/mhealth/sensors"), recursive = TRUE)
        dir.create(paste0(targetDir, "/logs"), recursive = TRUE)
        dir.create(paste0(targetDir, "/survey"), recursive = TRUE)
    }
    
    if (w1Schedule$scenario[i] == "1") {
        originDir = ema.manualSearchSurvey(dataDir = "Y:/MATCH STUDY/Main Study/Data/Manual Uploads/Phone EMA Manual Back-up/Wave 1", rdsName = w1Schedule$manualFile[i])
    } else {
        originDir = paste0("Y:/Jing/EMA/data/", strsplit(w1Schedule$wocketsFile[i], "/")[[1]][1], "/.match")
    }
    if (exists("originDir")) {
        if (dir.exists(paste0(originDir, "/data/mhealth/sensors/", w1Schedule$date[i]))) {file.copy(from = paste0(originDir, "/data/mhealth/sensors/", w1Schedule$date[i]), to = paste0(targetDir, "/data/mhealth/sensors"), recursive = TRUE)}
        if (dir.exists(paste0(originDir, "/logs/", w1Schedule$date[i]))) {file.copy(from = paste0(originDir, "/logs/", w1Schedule$date[i]), to = paste0(targetDir, "/logs"), recursive = TRUE)}
        if (dir.exists(paste0(originDir, "/survey/", w1Schedule$date[i]))) {file.copy(from = paste0(originDir, "/survey/", w1Schedule$date[i]), to = paste0(targetDir, "/survey"), recursive = TRUE)}
        # senario 4, use the responses and logs from wockets (completed above), and use the longer prompts
        if (w1Schedule$senario[i] == "4" && w1Schedule$diffPrompts[i] > 0) {
            # more prompts in manual than wockets, change the originDir to manual, and overwrite the prompts.csv file, otherwise the prompts.csv would be from wockets
            originDir = ema.manualSearchSurvey(dataDir = "Y:/MATCH STUDY/Main Study/Data/Manual Uploads/Phone EMA Manual Back-up/Wave 1", rdsName = w1Schedule$manualFile[i])
            file.copy(from = paste0(originDir, "/survey/", w1Schedule$date[i], "/Prompts.csv"), to = paste0(targetDir, "/survey/", w1Schedule$date[i]), recursive = TRUE, overwrite = TRUE)
        }
    }
    if(exists("originDir")) rm(originDir)
    if(i%%10 == 0) print(paste0(i, "; ", round(i/nrow(w1Schedule)*100, 2), "%; ", Sys.time()))
}

# output questional ones that needs attention
write.csv(w1Schedule[is.na(w1Schedule$scenario),], paste0("MATCH_EMA_w1_Diagnosis_Attentions_", Sys.Date(), ".csv"), row.names = FALSE, quote = FALSE)
# output diagnosis details
write.csv(w1Schedule, paste0("MATCH_EMA_w1_Diagnosis_", Sys.Date(), ".csv"), row.names = FALSE, quote = FALSE)

