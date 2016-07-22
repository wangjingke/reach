work_dir = "C:/Users/wangjink/Documents/REACH/MATCH/EMA_ManualRetrieve/W2"
setwd(work_dir)

# load functions
source("C:/Users/wangjink/Documents/Bitbucket/reach/MATCH/EMA/backend/match_ema_manual.R")
manual = list.dirs("Y:/MATCH STUDY/Main Study/Data/Manual Uploads/Phone EMA Manual Back-up/Wave 2", recursive = FALSE)
ema.manualAggregate(manual, manualDir = work_dir)
# archive records
write.csv(ema.archive(work_dir, folder = 1), paste0("MATCH_EMA_List_Manual_", Sys.Date(), ".csv"), row.names = FALSE, quote = FALSE)

# check if the subjectID matches the folder name
library(XLConnect)
w2 = readWorksheetFromFile("C:/Users/wangjink/Documents/Bitbucket/reach/MATCH/EMA/backend/match_ema_keys.xlsx", sheet="W2")
w2$DID=ifelse(nchar(w2$DID)<3, paste0("0", w2$DID), as.character(w2$DID))

archive = read.csv("C:/Users/wangjink/Documents/REACH/MATCH/EMA_ManualRetrieve/W2/MATCH_EMA_List_Manual_2016-07-06.csv", header = TRUE, stringsAsFactors = FALSE)

# senario 1, correct folder, wrong subjectID
archive$folderDID = substring(archive$folder, 3, 5)
archive$inStudyByFolder = NA

# senario 2, wrong folder, correct subjectID
subDID = function(x, y) {
    # due to ema.archive setting, the first item in subjectID has to be the folder name
    return(unique(gsub(paste0(substring(x, 1, 5), "|_11", "|_12"), "", y)))
}
archive$subDID = mapply(subDID, x = archive$folder, y = archive$subjectID)
archive$inStudyByID = NA

# define inStudy by folder name or ID from surveys
for (i in 1:nrow(archive)) {
    inStudyByFolder = archive$date[i]>=w2$W2pickup[which(w2$DID==archive$folderDID[i])] & archive$date[i]<=w2$W2dropoff[which(w2$DID==archive$folderDID[i])]
    archive$inStudyByFolder[i] = if(length(inStudyByFolder)>0) inStudyByFolder else NA
    rm(inStudyByFolder)
    
    inStudyByID = archive$date[i]>=w2$W2pickup[which(w2$DID==archive$subDID[i])] & archive$date[i]<=w2$W2dropoff[which(w2$DID==archive$subDID[i])]
    archive$inStudyByID[i] = if(length(inStudyByID)>0) inStudyByID else NA
    rm(inStudyByID)
}

archive[archive$inStudyByFolder==TRUE & !is.na(archive$inStudyByFolder) & archive$correctFolder==FALSE,] # correct folder, wrong ID
archive[archive$inStudyByID==TRUE & !is.na(archive$inStudyByID),] # correct ID, wrong folder

# 11028, 12104, and 12112 were mislabeled. Use their folder name as their ID
# 11056 was in 11046 folder
# 11027 was in 11053 folder
# 11083 was in 11088 folder
# 11184 was in 11192 folder
# 12070 was in 12046 folder
# 12053 was in 12051 folder
# 12043 duplicated in 12061 folder
# 12023 was in 12076 folder
# 12045 was in 12076 folder
# 12054 was in 12076 folder
# 12028 duplicated in 12079 folder
# 12056 was in 12086 folder
# 12020 was in 12087 folder (This 12020 falls in the time window of the W2, but with very little valid data. Another 11/12020W2 was used as their W3 due to time distance from W1)
# 12049 was in 12087 folder
# 12069 was in 12088 folder
# 12031 duplicated in 12090 folder
# 12065 was in 12090 folder

# after cleaning, redo assembles
manual = list.dirs("Y:/MATCH STUDY/Main Study/Data/Manual Uploads/Phone EMA Manual Back-up/Wave 2", recursive = FALSE)
ema.manualAggregate(manual, manualDir = work_dir)
# after changing the folder names, redo archive
write.csv(ema.archive(work_dir, folder = 1), paste0("MATCH_EMA_List_Manual_", Sys.Date(), ".csv"), row.names = FALSE, quote = FALSE)
# repeat the examination above, this time only the mislabeled and duplicates remains
