# compare manual backup and auto uploads for wave 2
workDir = "C:/Users/wangjink/Documents/REACH/MATCH/EMA_Clean/Wave2"
setwd(workDir)
source("C:/Users/wangjink/Documents/Bitbucket/reach/MATCH/EMA/backend/match_ema_promptList.R")

library(XLConnect)
w2 = readWorksheetFromFile("C:/Users/wangjink/Documents/Bitbucket/reach/MATCH/EMA/backend/match_ema_keys.xlsx", sheet="W2", colTypes = "character")
w2 = w2[w2$Wave.2=="complete",]
w2$DID = ifelse(nchar(w2$DID)<3, paste0("0", w2$DID), as.character(w2$DID))
w2$enterDate = sapply(strsplit(ifelse(is.na(w2$ManualCorrectedEnter), w2$W2pickup, w2$ManualCorrectedEnter), " "), head, 1)

w2Schedule = c()
for (i in 1:nrow(w2)) {
    indivSchedule =  rbind(data.frame(subjectID=paste0("11", w2$DID[i]), date=ema.dayAfter(w2$enterDate[i], 0:7), stringsAsFactors = FALSE), data.frame(subjectID=paste0("12", w2$DID[i]), date=ema.dayAfter(w2$enterDate[i], 0:7), stringsAsFactors = FALSE))
    indivSchedule = cbind(indivSchedule, data.frame(matrix(data = NA, nrow = nrow(indivSchedule), ncol = 13, dimnames = list(c(), c("manualFile", "manualPrompts", "manualResponse", "wocketsFile", "wocketsPrompts", "wocketsResponse", "identicalPrompts", "identicalResponse", "cover", "coverManual", "coverWockets", "diffPrompts", "diffResponse"))), stringsAsFactors = FALSE))
    w2Schedule = rbind(w2Schedule, indivSchedule)
    rm(indivSchedule)
}

w2manual = read.csv("C:/Users/wangjink/Documents/REACH/MATCH/EMA_ManualRetrieve/W2/MATCH_EMA_List_Manual_2016-07-06.csv", header = TRUE, stringsAsFactors = FALSE)
w2manual$correctID = substring(w2manual$folder, 1, 5) # the manual backup folder name contains the correct ID, regardless the prompts
w2wockets = read.csv("C:/Users/wangjink/Documents/REACH/MATCH/EMA_Wockets/MATCH_EMA_List_Wockets_2016-06-20.csv", header = TRUE, stringsAsFactors = FALSE)
w2wockets = w2wockets[w2wockets$correctFolder == TRUE & grepl("W2", w2wockets$folder), ] # limit to prompts in correct folder and W2 to increase speed and accuracy, which may harm coverage

# function to return NA instead of logical zero
ema.returnNA = function(x) {
    if (length(x)==0) return(NA) else return(x)
}

# function to get manual survey folder
ema.manualSearchSurvey = function (dataDir, rdsName) {
    dirX = paste0(dataDir, "/", strsplit(rdsName, "/")[[1]][1])
    return(if (any(grepl("survey", list.files(dirX)))) {dirX} else {paste0(dirX, "/.match")})
}

for (i in 1:nrow(w2Schedule)) {
    w2Schedule$manualFile[i] = ema.returnNA(paste(w2manual$file[which(w2manual$date==w2Schedule$date[i] & w2manual$correctID==w2Schedule$subjectID[i])], collapse = ","))
    w2Schedule$wocketsFile[i] = ema.returnNA(paste(w2wockets$file[which(w2wockets$date == w2Schedule$date[i] & grepl(w2Schedule$subjectID[i], w2wockets$file))], collapse=","))
    
    if (w2Schedule$subjectID[i]>12000) {mother=0} else {mother=1}
    # manual
    if (!is.na(w2Schedule$manualFile[i]) & w2Schedule$manualFile[i]!="") {
        manualX = readRDS(paste0("C:/Users/wangjink/Documents/REACH/MATCH/EMA_ManualRetrieve/W2/", w2Schedule$manualFile[i]))
        w2Schedule$manualPrompts[i] = ema.returnNA(nrow(manualX$prompts))
        switch (as.character(mother),
            "1" = {w2Schedule$manualResponse[i] = ema.returnNA(nrow(manualX$responses_mother))},
            "0" = {w2Schedule$manualResponse[i] = ema.returnNA(nrow(manualX$responses_child))}
        )
    }
    # wockets
    if (!is.na(w2Schedule$wocketsFile[i]) & w2Schedule$wocketsFile[i]!="") {
        wocketsX = readRDS(paste0("C:/Users/wangjink/Documents/REACH/MATCH/EMA_Wockets/", w2Schedule$wocketsFile[i]))
        w2Schedule$wocketsPrompts[i] = ema.returnNA(nrow(wocketsX$prompts))
        switch (as.character(mother),
                "1" = {w2Schedule$wocketsResponse[i] = ema.returnNA(nrow(wocketsX$responses_mother))},
                "0" = {w2Schedule$wocketsResponse[i] = ema.returnNA(nrow(wocketsX$responses_child))}
        )
    }
    if (!is.na(w2Schedule$manualPrompts[i]) & !is.na(w2Schedule$wocketsPrompts[i])) {
        w2Schedule$identicalPrompts[i] = identical(manualX$prompts, wocketsX$prompts)
    }
    if (is.na(w2Schedule$manualPrompts[i]) & is.na(w2Schedule$wocketsPrompts[i])) {w2Schedule$identicalPrompts[i] = TRUE}
    if (!is.na(w2Schedule$manualResponse[i]) & !is.na(w2Schedule$wocketsResponse[i])) {
        switch (as.character(mother),
            "1" = {w2Schedule$identicalResponse[i] = identical(manualX$responses_mother, wocketsX$responses_mother)},
            "0" = {w2Schedule$identicalResponse[i] = identical(manualX$responses_child, wocketsX$responses_child)}
        )
    }
    if (is.na(w2Schedule$manualResponse[i]) & is.na(w2Schedule$wocketsResponse[i])) {w2Schedule$identicalResponse[i] = TRUE}
    
    if (exists("manualX")) rm(manualX)
    if (exists("wocketsX")) rm(wocketsX)
    rm(mother)
    if (i%%100 == 0) print(i)
}

# how many days were covered
w2Schedule$cover = ifelse(!is.na(w2Schedule$manualPrompts) | !is.na(w2Schedule$wocketsPrompts), 1, 0)
w2Schedule$coverManual = ifelse(!is.na(w2Schedule$manualPrompts), 1, 0)
w2Schedule$coverWockets = ifelse(!is.na(w2Schedule$wocketsPrompts), 1, 0)
w2Schedule$diffPrompts = w2Schedule$manualPrompts - w2Schedule$wocketsPrompts
w2Schedule$diffResponse = w2Schedule$manualResponse - w2Schedule$wocketsResponse
# Majority of the surveys (> 3 days) from the following subjects were completely missed, requiring investigation
aggregate(date~subjectID, data = w2Schedule[w2Schedule$cover==0,], FUN = function (x) {length(unique(x))})
# 11031, 11044, 11051, 11087, 11125, 12044, 12068 (phone switched in the middle of study), 12122, 12125

# how many prompt days are identical between wockets and manual
table(w2Schedule$identicalPrompts, w2Schedule$identicalResponse, useNA = "ifany")
# identicalPrompts cannot be NA when manualFile or wocketsFile != ""
w2Schedule[is.na(w2Schedule$identicalPrompts) & w2Schedule$manualFile!="" & w2Schedule$wocketsFile!="",]
# copy the identical ones and unique ones to one location
for (i in 1:nrow(w2Schedule)) {
    if (w2Schedule$manualFile[i]=="" && w2Schedule$wocketsFile[i]=="") {next} # skip when no data coverage for the person-day
    targetDir = paste0(workDir, "/MATCH_", w2Schedule$subjectID[i], "W2")
    if (!dir.exists(targetDir)) {
        dir.create(paste0(targetDir, "/data/mhealth/sensors"), recursive = TRUE)
        dir.create(paste0(targetDir, "/logs"), recursive = TRUE)
        dir.create(paste0(targetDir, "/survey"), recursive = TRUE)
    }
    # senario control swtich
    # senario 1, only available in manual backup
    if (w2Schedule$manualFile[i]!="" && w2Schedule$wocketsFile[i]=="") {senario = "1"} 
    # senario 2, only available in wockets
    if (w2Schedule$manualFile[i]=="" && w2Schedule$wocketsFile[i]!="") {senario = "2"}
    # senario 3, available in both, completely identical prompts and responses, use the wockets copy
    if (w2Schedule$manualFile[i]!="" && w2Schedule$wocketsFile[i]!="" && isTRUE(w2Schedule$identicalResponse[i]) && isTRUE(w2Schedule$identicalPrompts[i])) {senario = "3"}
    # senario 4, avaiable in both, identical responses but different prompts
    if (w2Schedule$manualFile[i]!="" && w2Schedule$wocketsFile[i]!="" && isTRUE(w2Schedule$identicalResponse[i]) && !isTRUE(w2Schedule$identicalPrompts[i])) {senario = "4"}
    
    # actions under each senario, when no senario found, skip to next
    if (!exists("senario")) {next}
    if (senario == "1") {
        originDir = ema.manualSearchSurvey(dataDir = "Y:/MATCH STUDY/Main Study/Data/Manual Uploads/Phone EMA Manual Back-up/Wave 2", rdsName = w2Schedule$manualFile[i])
    } else {
        originDir = paste0("Y:/Jing/EMA/data/", strsplit(w2Schedule$wocketsFile[i], "/")[[1]][1], "/.match")
    }
    if (exists("originDir")) {
        if (dir.exists(paste0(originDir, "/data/mhealth/sensors/", w2Schedule$date[i]))) {file.copy(from = paste0(originDir, "/data/mhealth/sensors/", w2Schedule$date[i]), to = paste0(targetDir, "/data/mhealth/sensors"), recursive = TRUE)}
        if (dir.exists(paste0(originDir, "/logs/", w2Schedule$date[i]))) {file.copy(from = paste0(originDir, "/logs/", w2Schedule$date[i]), to = paste0(targetDir, "/logs"), recursive = TRUE)}
        if (dir.exists(paste0(originDir, "/survey/", w2Schedule$date[i]))) {file.copy(from = paste0(originDir, "/survey/", w2Schedule$date[i]), to = paste0(targetDir, "/survey"), recursive = TRUE)}
        # senario 4, use the responses and logs from wockets (completed above), and use the longer prompts
        if (senario == "4" && w2Schedule$diffPrompts[i] > 0) {
            # more prompts in manual than wockets, change the originDir to manual, and overwrite the prompts.csv file, otherwise the prompts.csv would be from wockets
            originDir = ema.manualSearchSurvey(dataDir = "Y:/MATCH STUDY/Main Study/Data/Manual Uploads/Phone EMA Manual Back-up/Wave 2", rdsName = w2Schedule$manualFile[i])
            file.copy(from = paste0(originDir, "/survey/", w2Schedule$date[i], "/Prompts.csv"), to = paste0(targetDir, "/survey/", w2Schedule$date[i]), recursive = TRUE, overwrite = TRUE)
        }
    }
    if(exists("originDir")) rm(originDir)
    if(exists("senario")) rm(senario)
    if(i%%10 == 0) print(paste0(i, "; ", round(i/nrow(w2Schedule)*100, 2), "% ;", Sys.time()))
}

# output questional ones that needs attention
write.csv(w2Schedule[!w2Schedule$identicalResponse & !is.na(w2Schedule$identicalResponse),], paste0("MATCH_EMA_W2_Diagnosis_Attentions_", Sys.Date(), ".csv"), row.names = FALSE, quote = FALSE)
# output diagnosis details
write.csv(w2Schedule, paste0("MATCH_EMA_W2_Diagnosis_", Sys.Date(), ".csv"), row.names = FALSE, quote = FALSE)

