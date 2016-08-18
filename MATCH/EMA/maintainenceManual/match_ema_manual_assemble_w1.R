work_dir = "C:/Users/wangjink/Documents/REACH/MATCH/EMA_ManualRetrieve/W1"
setwd(work_dir)

# load functions
source("C:/Users/wangjink/Documents/GitHub/reach/MATCH/EMA/backend/match_ema_manual.R")
manual = list.dirs("Y:/MATCH STUDY/Main Study/Data/Manual Uploads/Phone EMA Manual Back-up/Wave 1", recursive = FALSE)
ema.manualAggregate(manual, manualDir = work_dir)
# archive records
write.csv(ema.archive(work_dir, folder = 1), paste0("MATCH_EMA_List_Manual_", Sys.Date(), ".csv"), row.names = FALSE, quote = FALSE)

# check if the subjectID matches the folder name
library(XLConnect)
w1 = readWorksheetFromFile("C:/Users/wangjink/Documents/GitHub/reach/MATCH/EMA/backend/match_ema_keys.xlsx", sheet="W1")
w1$DID=ifelse(nchar(w1$DID)<3, paste0("0", w1$DID), as.character(w1$DID))

archive  = read.csv("C:/Users/wangjink/Documents/REACH/MATCH/EMA_ManualRetrieve/W1/MATCH_EMA_List_Manual_2016-08-17.csv", header = TRUE, stringsAsFactors = FALSE)

# scenario 1, corret folder, wrong subjectID
archive$folderDID  = substring(archive$folder, 3, 5)
archive$inStudyByFolder = NA

# scenario 2, wrong folder, correct subjectID (subDID=="" in correct folder)
subDID = function(x, y) {
    # due to ema.archive setting, the first item in subjectID has to be the folder name
    return(unique(gsub(paste0(substring(x, 1, 5), "|_11", "|_12"), "", y)))
}
archive$subDID = mapply(subDID, x = archive$folder, y = archive$subjectID)
archive$inStudyByID = NA

# define inStudy by folder name or ID from surveys
for (i in 1:nrow(archive)) {
    inStudyByFolder = archive$date[i]>=w1$W1pickup[which(w1$DID==archive$folderDID[i])] & archive$date[i]<=w1$W1dropoff[which(w1$DID==archive$folderDID[i])]
    archive$inStudyByFolder[i] = if(length(inStudyByFolder)>0) inStudyByFolder else NA
    rm(inStudyByFolder)
    
    inStudyByID = archive$date[i]>=w1$W1pickup[which(w1$DID==archive$subDID[i])] & archive$date[i]<=w1$W1dropoff[which(w1$DID==archive$subDID[i])]
    # archive$inStudyByID[i] = if(length(inStudyByID)>0) inStudyByID else NA
    if (isTRUE(inStudyByID)) {
        idX = tail(strsplit(archive$subjectID[i], "_")[[1]], 1)
        posX = which(archive$subjectID==idX & archive$correctFolder == TRUE & archive$date == archive$date[i])
        if (length(posX)>0) {
            archive$inStudyByID[i] = FALSE # not unique 
        } else {
            archive$inStudyByID[i] = TRUE # unique existence in other folder
        }
    } else {
        archive$inStudyByID[i] = FALSE
    }
    rm(inStudyByID)
}

scenario1 = archive[archive$inStudyByFolder==TRUE & !is.na(archive$inStudyByFolder) & archive$correctFolder==FALSE,] # correct folder, wrong ID
scenario2 = archive[archive$inStudyByID==TRUE & !is.na(archive$inStudyByID),] # correct ID, wrong folder
