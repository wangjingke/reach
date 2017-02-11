# this script process raw csv results from accelerometers and prepare for merging with EMA
setwd("D:/REACH/MATCH/ACC")

# reading in functions for ACC processing
source("D:/GitHub/reach/common/functions_acc_process.R")

# reading in demographics data for age
person=read.csv("D:/GitHub/reach/MATCH/ACC/match_child_dob.csv", header = TRUE, stringsAsFactors = FALSE)
person$DOB=paste(person$DOB.Y, person$DOB.M, person$DOB.D, sep = "-")
person$birthday=strptime(person$DOB, format="%Y-%m-%d", tz = "America/Los_Angeles")

# target_dir
accList1=list.files(path="Y:/MATCH STUDY/Main Study/Data/Meterplus/ACTIGRAPH", full.names = TRUE, recursive = TRUE)
accList2=list.files(path="Y:/MATCH STUDY/Main Study/Data/Manual Uploads/Actigraph Data", full.names = TRUE, recursive = TRUE)
accList=grep("^((?!DataTable).)*\\.csv$", c(accList1, accList2), perl = TRUE, value = TRUE)

# acc files
for (i in 1:length(accList)) {
    segX=strsplit(accList[i], "/")[[1]]
    
    idX=substring(tail(segX, 1), first = 1, last = 5)
    accX=try(acc.read(accList[i]), silent = FALSE)
    if (inherits(accX, "try-error")) {
        datatableX=paste0(strsplit(accList[i], "\\.csv")[[1]][1], "DataTable.csv")
        accX=try(acc.read.DT(datatableX), silent = FALSE)
        if (inherits(accX, "try-error")) {next}
    } # using DataTable file if the raw csv is missing
    
    surveyDate=strptime(accX$start_date, format = "%m/%d/%Y", tz = "America/Los_Angeles")
    if (idX<12000) {
        ageX = 18
    } else {
        ageX = as.numeric(difftime(surveyDate, person$birthday[which(person$SubjectID==idX)], units = "days")) %/% 365.25
    }
    
    if (length(ageX)>0) {
        saveRDS(acc.sum.mp(accX, id=idX, age=ageX), paste("MATCH", idX, surveyDate,"mp.rds", sep="_"))
        # saveRDS(acc.sum.reach(accX, id=idX, age=ageX), paste("MATCH", idX, wX, "reach.rds", sep="_"))
    }
    if (i%%10==0) {print(paste0(i, "; ", round(i/length(accList)*100, 2), "%"))}
}

# archive ACC
accArchive = data.frame(file = list.files(pattern = "^.*_mp.rds$"), stringsAsFactors = FALSE)
accArchive$subjectID = sapply(strsplit(accArchive$file, "_"), "[[", 2)
accArchive$date = sapply(strsplit(accArchive$file, "_"), "[[", 3)
write.csv(accArchive, paste0("MATCH_ACC_List_", Sys.Date(),".csv"), row.names = FALSE, quote = FALSE)

# summarize all the ACC
for (i in 1:nrow(accArchive)) {
    accX = readRDS(accArchive$file[i])$details
    accX$subjectID = accArchive$subjectID[i]
    write.table(accX, paste0("MATCH_ACC_Summary_", Sys.Date(), ".txt"), sep = "\t", append = TRUE, quote = FALSE, row.names = FALSE, col.names = {if(i==1) TRUE else FALSE})
    if (i%%10==0) {print(paste0(i, "; ", round(i/nrow(accArchive)*100, 2), "%"))}
}

# ACC log file Y:\Karen's folder\KAREN\Copy of missings_LOG.xlsx
