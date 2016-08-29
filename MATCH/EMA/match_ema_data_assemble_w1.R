setwd("D:/REACH/MATCH/Dataset/MATCH_EMA/Wave1/Beta20160829")
# loading functions
source("D:/GitHub/reach/MATCH/EMA/backend/match_ema_functions.R")
# construct EMA skeletons
# remember to update schedule sheets for ones whose startDates were not coherent with their enterDate
library(XLConnect)
w1 = readWorksheetFromFile("D:/GitHub/reach/MATCH/EMA/backend/match_ema_keys.xlsx", sheet="W1", colTypes = "character")
w1 = w1[w1$Wave.1=="complete",]
w1$DID = ifelse(nchar(w1$DID)<3, paste0("0", w1$DID), as.character(w1$DID))
w1$enterDate = sapply(strsplit(ifelse(is.na(w1$ManualCorrectedEnter), w1$W1pickup, w1$ManualCorrectedEnter), " "), head, 1)
w1$mother = paste0("11", w1$DID)
w1$child = paste0("12", w1$DID)

W1EMA=ema.promptList(w1, start = "enterDate")
W1EMA=ema.fetchPrompt(ema=W1EMA, emaList="MATCH_EMA_List_Clean_2016-08-24.csv", emaDir="D:/REACH/MATCH/EMA_Clean_RDS/W1", warning="MATCH_W1_warnings.txt")

# search for low compliance
compliance = ema.compliance(W1EMA)

# output low compliance subjects
saveRDS(compliance, paste0("MATCH_W1_EMA_Compliance_", Sys.Date(), ".rds"))
# 11185/12185 dropped out
# deleting prompts with obviously wrong answers (multiple answers for binary variables)
sapply(W1EMA[c(22:119, 122:156)], table)

# change TMRWAKETIME to today's WAKETIME
W1EMA=ema.waketime(W1EMA)
# change some variables to factors
W1EMA=ema.factor(W1EMA)

# integrating ACC data
W1EMA=ema.attachACC(W1EMA)
W1EMA=ema.attachACC(W1EMA, prefix = "O")
W1EMA=ema.ACC(ema=W1EMA, missing_output=paste0("MATCH_EMA_ACC_Missing_", Sys.Date(), ".csv"), accDir = "D:/REACH/MATCH/ACC", accList="MATCH_ACC_List_2016-08-29.csv")

# save the unanchored set
saveRDS(W1EMA, paste0("MATCH_EMA_W1_preanchored_", Sys.Date(), ".rds"))

# creating anchored measurements
library(foreach)
library(doParallel)
nCL=4
cl=makeCluster(nCL)
registerDoParallel(cl) # for the doParallel library
clusterExport(cl, c("W1EMA"))

jobList=ema.triage(W1EMA, nCL)
W1EMA_anchored=foreach (m=1:nCL, .combine = rbind) %dopar% {
    emaSegX=jobList[[m]]
    emaX=ema.anchor.parallel(emaSeg = emaSegX, ema=W1EMA, node = m, checkpoint = paste0("W1EMA_checkpoints_", Sys.Date(),".txt"))
    print(emaX)
}
stopCluster(cl)
saveRDS(W1EMA_anchored, paste0("MATCH_EMA_W1_anchored_", Sys.Date(), ".rds"))

# adding daily average physical activity
matchW1=ema.dailyPA(W1EMA_anchored, AccSummary="D:/REACH/MATCH/ACC/MATCH_ACC_Summary_2016-08-29.txt")
saveRDS(matchW1, paste0("MATCH_EMA_W1_", Sys.Date(), ".rds"))

# create STATA script to label variables
ema.stata(paste0("MATCH_EMA_W1_", Sys.Date(), ".do"))

# save workplace for transformation to STATA
matchW1=readRDS(paste0("MATCH_EMA_W1_", Sys.Date(), ".rds"))
matchW1clean = matchW1[!is.na(matchW1$COMPLY),]
matchW1clean[is.na(matchW1clean)]=99999
# get rid of anchored OMVPA variables
for (i in grep("^(OMVPA|OSED|OLIGHT|OMOD|OVIG|OVALID|ONONVALID).*_(1P1|1P2|1M1|1M2|0C|0P1|0P2|0M1|0M2)$", names(matchW1), value = TRUE)) {
    matchW1clean[,i]=NULL
}
save(list=c("matchW1clean"), file=paste0("MATCH_EMA_W1_", Sys.Date(),".RData"))

