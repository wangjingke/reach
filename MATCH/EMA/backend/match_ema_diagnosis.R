# function to add file and reason
ema.diagnosis = function(ema, emaDir, emaList) {
    ema$reason = NA_character_
    jobList = unique(ema[c("SubjectID", "Date")])
    emaList = read.csv(paste0(emaDir, "/", emaList), header = TRUE, stringsAsFactors = FALSE)
    for (i in 1:nrow(jobList)) {
        candidates=emaList[intersect(grep(jobList$Date[i], emaList$date), grep(jobList$SubjectID[i], emaList$subjectID)),]
        if (any(candidates$correctFolder)) {
            survey=head(candidates$file[candidates$correctFolder==TRUE], n=1)
        } else {
            survey=head(candidates$file[candidates$num==1], n=1)
        }
        if (length(survey)>0) {
            promptX = readRDS(paste0(emaDir, "/", survey))
            if (length(promptX$prompts)>1) {
                reasonX = promptX$prompts[promptX$prompts$Status == "Never Prompted",]
                if (nrow(reasonX)>0) {
                    reasonX$timestamp = strptime(reasonX$TimeStampPrompted, format = "%Y-%m-%d %H:%M:%S", tz = "America/Los_Angeles")
                    for (j in 1:nrow(reasonX)) {
                        ema[which(ema$SubjectID==jobList$SubjectID[i] & ema$window_start<=reasonX$timestamp[j] & reasonX$timestamp[j]<=ema$window_end), "reason"] = reasonX$Reason[j]
                    }
                    rm(reasonX)
                }
            }
            ema[which(ema$SubjectID==jobList$SubjectID[i] & ema$Date==jobList$Date[i]), "file"] = survey
            rm(survey)
        }
        if (i%%100==0) {print(paste0(i, "; ", round(i/nrow(jobList)*100, 2), "%"))}
    }
    return(ema)
}

# function to clean reason
ema.reason.automatic = function(ema) {
    grepReason = function(reason) {
        if (grepl("WithoutStartEndDateWindow", reason)) {
            return("outDate")
        } else if (grepl("WithoutBedWakeWindow", reason)) {
            return("outSleep")
        } else if (grepl("Phone off", reason)) {
            return("phoneOff")
        } else {
            return(NA_character_)
        }
    }
    ema$primary = sapply(ema$reason, grepReason)
    return(ema)
}

ema.battery=function(ema, emaDir) {
    ema$battery=NA
    for (i in 1:nrow(ema)) {
        if(is.na(ema$file[i])) {next}
        emaX=readRDS(paste0(emaDir, "/", ema$file[i]))
        # break if there is no battery record
        if (length(emaX$battery)==1) {
            ema$battery[i]=NA
        } else {
            runService=gsub(" PDT| MDT", "", emaX$battery$V4[grep("RunService", emaX$battery$V3)])
            state=as.numeric(gsub("BatteryRemaining: ([0-9]*)%", "\\1", emaX$battery$V4[grep("PhoneState", emaX$battery$V3)])) # extract the num

            if (length(runService)!=length(state)) {
                runService=c(runService, rep(NA, max(length(runService), length(state))-length(runService)))
                state=c(state, rep(NA_integer_, max(length(runService), length(state))-length(runService)))
            }
            battery=data.frame(time=strptime(runService, format = "%a %B %d %H:%M:%S %Y", tz="America/Los_Angeles"), juice=state)
            status=mean(battery$juice[battery$time>=ema$window_start[i] & battery$time<=ema$window_end[i]], na.rm = TRUE)
            ema$battery[i] = ifelse(status<=20 | is.na(status), 0, 1) # considered phone off when battery below 20% or not available
            rm(runService)
            rm(state)
            rm(battery)
            rm(status)
            rm(emaX)
        }
        if(i%%10==0) print(i)
    }
    return(ema)
}

ema.inWakeWindow = function(ema, emaDir, emaList) {
    emaList = read.csv(paste0(emaDir, "/", emaList), header = TRUE, stringsAsFactors = FALSE)
    ema$inWakeWindow = NA
    for (i in 1:nrow(ema)) {
        pos = which(emaList$subjectID == ema$SubjectID[i] & emaList$date == as.character(as.Date(ema$Date[i], format = "%Y-%m-%d", tz="America/Los_Angeles")-1))
        if (length(pos)==1) {
            emaX = readRDS(paste0(emaDir, "/", emaList$file[pos]))
            if (length(emaX$bedwake)>1) {
                wakeX = strptime(paste0(ema$Date[i], " ", emaX$bedwake$Waketime_Tomorrow[1]), format = "%Y-%m-%d %H:%M", tz = "America/Los_Angeles")
                ema$inWakeWindow[i] = ifelse(wakeX>ema$window_end[i], "out", "in")
            }
        }
        if(i%%10==0) print(i)
    }
    return(ema)
}

ema.causeOfMiss = function(ema) {
    ema$causeOfMiss = NA
    for (i in 1:nrow(ema)) {
        if ((!is.na(ema$primary[i]) & ema$primary[i]=="outDate") | ema$DayInStudy[i]==1 | ema$DayInStudy[i]==8) {
            ema$causeOfMiss[i] = "outStudyWindow"
        } else if ((!is.na(ema$primary[i]) & ema$primary[i]=="outSleep") | (!is.na(ema$inWakeWindow[i]) & ema$inWakeWindow[i]=="out")) {
            ema$causeOfMiss[i] = "outSleepWindow"
        } else if ((!is.na(ema$primary[i]) & ema$primary[i]=="phoneOff") | (!is.na(ema$battery[i]) & ema$battery[i]==0)) {
            ema$causeOfMiss[i] = "outBattery"
        } else {
            ema$causeOfMiss[i] = NA
        }
    }
    return(ema)
}
