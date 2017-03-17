work_dir <- "D:/REACH/MATCH/EMA_Clean/Wave3"
setwd(work_dir)
source("D:/GitHub/reach/MATCH/EMA/backend/match_ema_promptList.R")

library(XLConnect)
w3 <- readWorksheetFromFile("D:/GitHub/reach/MATCH/EMA/backend/match_ema_keys.xlsx", sheet = "W3", colTypes = "character")
w3 <- w3[!is.na(w3$w3_start),]
w3$DID <- ifelse(nchar(w3$DID)<3, paste0("0", w3$DID), as.character(w3$DID))
w3$enterDate <- sapply(strsplit(w3$w3pu, " "), head, 1)

w3Schedule <- c()
for (i in 1:nrow(w3)) {
    indivSchedule =  rbind(data.frame(subjectID=paste0("11", w3$DID[i]), date=ema.dayAfter(w3$enterDate[i], 0:7), stringsAsFactors = FALSE), data.frame(subjectID=paste0("12", w3$DID[i]), date=ema.dayAfter(w3$enterDate[i], 0:7), stringsAsFactors = FALSE))
    indivSchedule = cbind(indivSchedule, data.frame(matrix(data = NA, nrow = nrow(indivSchedule), ncol = 13, dimnames = list(c(), c("manualFile", "manualPrompts", "manualResponse", "wocketsFile", "wocketsPrompts", "wocketsResponse", "identicalPrompts", "identicalResponse", "cover", "coverManual", "coverWockets", "diffPrompts", "diffResponse"))), stringsAsFactors = FALSE))
    w3Schedule = rbind(w3Schedule, indivSchedule)
    rm(indivSchedule)
}

w3manual <- read.csv("D:/REACH/MATCH/EMA_ManualRetrieve/W3/MATCH_EMA_W3_List_manual_2017-03-10.csv", header = TRUE, stringsAsFactors = FALSE)
w3wockets <- read.csv("D:/REACH/MATCH/EMA_Wockets/EMA_Wockets_W3/MATCH_EMA_W3_List_Wockets_2017-03-09.csv", header = TRUE, stringsAsFactors = FALSE)

# function to return NA instead of logical zero
ema.returnNA = function(x) {
    if (length(x)==0) return(NA) else return(x)
}

for (i in 1:nrow(w3Schedule)) {
    w3Schedule$manualFile[i] <- ema.returnNA(paste(w3manual$file[which(w3manual$date == w3Schedule$date[i] & w3manual$subjectID == w3Schedule$subjectID[i])], collapse = ","))
    w3Schedule$wocketsFile[i] <- ema.returnNA(paste(w3wockets$file[which(w3wockets$date == w3Schedule$date[i] & grepl(w3Schedule$subjectID[i], w3wockets$file))], collapse = ","))
}

w3Schedule$twoFile <- grepl(",", w3Schedule$manualFile) | grepl(",", w3Schedule$wocketsFile)

for (i in 1:nrow(w3Schedule)) {
    if (w3Schedule$twoFile[i]) next
    if (w3Schedule$subjectID[i] > 12000) {mother <- 0} else {mother <- 1}
    if (!is.na(w3Schedule$manualFile[i]) & w3Schedule$manualFile[i]!="") {
        manualX <- readRDS(paste0("D:/REACH/MATCH/EMA_ManualRetrieve/W3/", w3Schedule$manualFile[i]))
        w3Schedule$manualPrompts[i] <- ema.returnNA(nrow(manualX$prompts))
        switch (as.character(mother),
            "1" = {w3Schedule$manualResponse[i] <- ema.returnNA(nrow(manualX$responses_mother))},
            "0" = {w3Schedule$manualResponse[i] <- ema.returnNA(nrow(manualX$responses_child))}
        )
    }
    if (!is.na(w3Schedule$wocketsFile[i]) & w3Schedule$wocketsFile[i] != "") {
        wocketsX <- readRDS(paste0("D:/REACH/MATCH/EMA_Wockets/EMA_Wockets_W3/", w3Schedule$wocketsFile[i]))
        w3Schedule$wocketsPrompts[i] <- ema.returnNA(nrow(wocketsX$prompts))
        switch(as.character(mother),
            "1" = {w3Schedule$wocketsResponse[i] <- ema.returnNA(nrow(wocketsX$responses_mother))},
            "0" = {w3Schedule$wocketsResponse[i] <- ema.returnNA(nrow(wocketsX$responses_child))}
        )
    }
    if (!is.na(w3Schedule$manualPrompts[i]) & !is.na(w3Schedule$wocketsPrompts[i])) {
        w3Schedule$identicalPrompts[i] = identical(manualX$prompts, wocketsX$prompts)
    }
    if (is.na(w3Schedule$manualPrompts[i]) & is.na(w3Schedule$wocketsPrompts[i])) {w3Schedule$identicalPrompts[i] = TRUE}
    if (!is.na(w3Schedule$manualResponse[i]) & !is.na(w3Schedule$wocketsResponse[i])) {
        switch (as.character(mother),
                "1" = {w3Schedule$identicalResponse[i] = identical(manualX$responses_mother, wocketsX$responses_mother)},
                "0" = {w3Schedule$identicalResponse[i] = identical(manualX$responses_child, wocketsX$responses_child)}
        )
    }
    if (is.na(w3Schedule$manualResponse[i]) & is.na(w3Schedule$wocketsResponse[i])) {w3Schedule$identicalResponse[i] = TRUE}
    
    if (exists("manualX")) rm(manualX)
    if (exists("wocketsX")) rm(wocketsX)
    rm(mother)
    if (i%%100 == 0) print(i)
}

w3Schedule$manualPrompts[is.na(w3Schedule$manualPrompts)] <- 0
w3Schedule$wocketsPrompts[is.na(w3Schedule$wocketsPrompts)] <- 0
w3Schedule$manualResponse[is.na(w3Schedule$manualResponse)] <- 0
w3Schedule$wocketsResponse[is.na(w3Schedule$wocketsResponse)] <- 0

# how many days were covered
w3Schedule$cover = ifelse(w3Schedule$manualPrompts > 0 | w3Schedule$wocketsPrompts > 0, 1, 0)
w3Schedule$coverManual = ifelse(w3Schedule$manualPrompts > 0, 1, 0)
w3Schedule$coverWockets = ifelse(w3Schedule$wocketsPrompts > 0, 1, 0)
w3Schedule$diffPrompts = w3Schedule$manualPrompts - w3Schedule$wocketsPrompts
w3Schedule$diffResponse = w3Schedule$manualResponse - w3Schedule$wocketsResponse

# Majority of the surveys (> 3 days) from the following subjects were completely missed, requiring investigation
aggregate(date~subjectID, data = w3Schedule[w3Schedule$cover==0,], FUN = function (x) {length(unique(x))})

# how many prompt days are identical between wockets and manual
table(w3Schedule$identicalPrompts, w3Schedule$identicalResponse, useNA = "ifany")
# identicalPrompts cannot be NA when manualFile or wocketsFile != ""
w3Schedule[is.na(w3Schedule$identicalPrompts) & w3Schedule$manualFile!="" & w3Schedule$wocketsFile!="",]

w3Schedule$senario <- NA
for (i in 1:nrow(w3Schedule)) {
    if (w3Schedule$twoFile[i]) next
    if (w3Schedule$manualFile[i]=="" && w3Schedule$wocketsFile[i]=="") next
    targetDir = paste0(work_dir, "/MATCH_", w3Schedule$subjectID[i], "W3")
    if (!dir.exists(targetDir)) dir.create(targetDir)
    
    # senario control swtich
    # senario 1, only available in manual backup
    if (w3Schedule$manualFile[i]!="" && w3Schedule$wocketsFile[i]=="") {senario = "1"} 
    # senario 2, only available in wockets
    if (w3Schedule$manualFile[i]=="" && w3Schedule$wocketsFile[i]!="") {senario = "2"}
    # senario 3, available in both, completely identical prompts and responses, use the wockets copy
    if (w3Schedule$manualFile[i]!="" && w3Schedule$wocketsFile[i]!="" && isTRUE(w3Schedule$identicalResponse[i]) && isTRUE(w3Schedule$identicalPrompts[i])) {senario = "3"}
    # senario 4, avaiable in both, identical responses but different prompts
    if (w3Schedule$manualFile[i]!="" && w3Schedule$wocketsFile[i]!="" && isTRUE(w3Schedule$identicalResponse[i]) && !isTRUE(w3Schedule$identicalPrompts[i])) {senario = "4"}
    
    # actions under each senario, when no senario found, skip to next
    if (!exists("senario")) next
    w3Schedule$senario[i] <- senario
    if (senario == "1") {
        originDir = paste0("D:/REACH/MATCH/EMA_W3_temp/manual/", strsplit(w3Schedule$manualFile[i], "/")[[1]][1])
    } else {
        originDir = paste0("D:/REACH/MATCH/EMA_W3_temp/wockets/", strsplit(w3Schedule$wocketsFile[i], "/")[[1]][1])
    }
    if (exists("originDir")) {
        dirsToCopy <- grep(w3Schedule$date[i], list.dirs(originDir, full.names = FALSE, recursive = TRUE), value = TRUE)
        for (j in dirsToCopy) {
            path.seg <- strsplit(j, "/")[[1]]
            targetDir.X <- paste0(targetDir, "/", paste(path.seg[2:(length(path.seg)-1)], collapse = "/"))
            # get rid of .match
            targetDir.X <- gsub(".match/", "", targetDir.X)
            if (!dir.exists(targetDir.X)) dir.create(targetDir.X, recursive = TRUE)
            file.copy(from = paste0(originDir, "/", j), to = targetDir.X, recursive = TRUE)
        }

        # senario 4, use the responses and logs from wockets (completed above), and use the longer prompts
        if (senario == "4" && w3Schedule$diffPrompts[i] > 0) {
            # more prompts in manual than wockets, change the originDir to manual, and overwrite the prompts.csv file, otherwise the prompts.csv would be from wockets
            originDir = paste0("D:/REACH/MATCH/EMA_W3_temp/manual/", strsplit(w3Schedule$manualFile[i], "/")[[1]][1])
            promptX <- grep(w3Schedule$date[i], list.files(originDir, "Prompts.csv", recursive = TRUE), value = TRUE)
            path.seg <- strsplit(promptX, "/")[[1]]
            targetDir.X <- paste0(targetDir, "/.match/", paste(path.seg[2:(length(path.seg)-1)], collapse = "/"))
            if (!dir.exists(targetDir.X)) dir.create(targetDir.X, recursive = TRUE)
            file.copy(from = paste0(originDir, "/", promptX), to = targetDir.X, recursive = TRUE)
        }
    }
    if(exists("originDir")) rm(originDir)
    if(exists("senario")) rm(senario)
    if(i%%10 == 0) print(paste0(i, "; ", round(i/nrow(w3Schedule)*100, 2), "%; ", Sys.time()))
}

for (i in 1:nrow(w3Schedule)) {
    if (w3Schedule$senario[i] == "1" & !is.na(w3Schedule$senario[i])) {
        targetDir = paste0(work_dir, "/MATCH_", w3Schedule$subjectID[i], "W3")
        if (!dir.exists(targetDir)) dir.create(targetDir)
        originDir = paste0("D:/REACH/MATCH/EMA_W3_temp/manual/", strsplit(w3Schedule$manualFile[i], "/")[[1]][1])
        if (exists("originDir")) {
            dirsToCopy <- grep(w3Schedule$date[i], list.dirs(originDir, full.names = FALSE, recursive = TRUE), value = TRUE)
            for (j in dirsToCopy) {
                path.seg <- strsplit(j, "/")[[1]]
                targetDir.X <- paste0(targetDir, "/", paste(path.seg[2:(length(path.seg)-1)], collapse = "/"))
                if (!dir.exists(targetDir.X)) dir.create(targetDir.X, recursive = TRUE)
                file.copy(from = paste0(originDir, "/", j), to = targetDir.X, recursive = TRUE)
            }
        }
        if(exists("originDir")) rm(originDir)
    }
    if(i%%10 == 0) print(paste0(i, "; ", round(i/nrow(w3Schedule)*100, 2), "%; ", Sys.time()))
}

# special cases
# two files for same day
w3Schedule[w3Schedule$twoFile == TRUE,]
w3Schedule[w3Schedule$senario==4 & !is.na(w3Schedule$senario),]
w3Schedule[w3Schedule$manualFile!="" & w3Schedule$wocketsFile!="" & w3Schedule$identicalPrompts!=TRUE,]
w3Schedule[w3Schedule$manualFile!="" & w3Schedule$wocketsFile!="" & w3Schedule$identicalResponse!=TRUE,]
w3Schedule[is.na(w3Schedule$senario) & w3Schedule$twoFile==FALSE & !is.na(w3Schedule$subjectID) & w3Schedule$cover == 1,]

# manually put senario 1 into correct folders
unique(w3Schedule$subjectID[w3Schedule$senario=="1"])

