setwd("D:/REACH/MATCH/Dataset/MATCH_EMA/Wave2/Beta20160907")
# loading functions
source("D:/GitHub/reach/MATCH/EMA/backend/match_ema_functions.R")
# construct EMA skeletons
# remember to update schedule sheets for ones whose startDates were not coherent with their enterDate
library(XLConnect)
w2 = readWorksheetFromFile("D:/GitHub/reach/MATCH/EMA/backend/match_ema_keys.xlsx", sheet="W2", colTypes = "character")
w2 = w2[w2$Wave.2=="complete",]
w2$DID = ifelse(nchar(w2$DID)<3, paste0("0", w2$DID), as.character(w2$DID))
w2$enterDate = sapply(strsplit(ifelse(is.na(w2$ManualCorrectedEnter), w2$W2pickup, w2$ManualCorrectedEnter), " "), head, 1)
w2$mother = paste0("11", w2$DID)
w2$child = paste0("12", w2$DID)

W2EMA=ema.promptList(w2, start = "enterDate")
W2EMA=ema.fetchPrompt(ema=W2EMA, emaList="MATCH_EMA_List_Clean_2016-08-24.csv", emaDir="D:/REACH/MATCH/EMA_Clean_RDS/W2", warning="MATCH_W2_warnings.txt")

# search for low compliance
compliance = ema.compliance(W2EMA)

# output low compliance subjects
saveRDS(compliance, paste0("MATCH_W2_EMA_Compliance_", Sys.Date(), ".rds"))
# 11185/12185 dropped out
# deleting prompts with obviously wrong answers (multiple answers for binary variables)
sapply(W2EMA[c(22:119, 122:156)], table)
W2EMA[W2EMA$CHILD_Sad==6 & !is.na(W2EMA$CHILD_Sad), c("CHILD_Sad")]=NA

# change TMRWAKETIME to today's WAKETIME
W2EMA=ema.waketime(W2EMA)

# integrating ACC data
W2EMA=ema.attachACC(W2EMA)
W2EMA=ema.attachACC(W2EMA, prefix = "o")
W2EMA=ema.ACC(ema=W2EMA, missing_output=paste0("MATCH_EMA_ACC_Missing_", Sys.Date(), ".csv"), accDir = "D:/REACH/MATCH/ACC", accList="MATCH_ACC_List_2016-09-07.csv")

# save the unanchored set
saveRDS(W2EMA, paste0("MATCH_EMA_W2_preanchored_", Sys.Date(), ".rds"))

# creating anchored measurements
library(foreach)
library(doParallel)
nCL=4
cl=makeCluster(nCL)
registerDoParallel(cl) # for the doParallel library
clusterExport(cl, c("W2EMA"))

jobList=ema.triage(W2EMA, nCL)
W2EMA_anchored=foreach (m=1:nCL, .combine = rbind) %dopar% {
    emaSegX=jobList[[m]]
    emaX=ema.anchor.parallel(emaSeg = emaSegX, ema=W2EMA, node = m, checkpoint = paste0("W2EMA_checkpoints_", Sys.Date(),".txt"))
    print(emaX)
}
stopCluster(cl)
saveRDS(W2EMA_anchored, paste0("MATCH_EMA_W2_anchored_", Sys.Date(), ".rds"))

# adding daily average physical activity
matchW2 = ema.dailyPA(W2EMA_anchored, AccSummary="D:/REACH/MATCH/ACC/MATCH_ACC_Summary_2016-09-07.txt")
# cleaning up
matchW2 = ema.cleanup(matchW2, wave = 2)
saveRDS(matchW2, paste0("MATCH_EMA_W2_", Sys.Date(), ".rds"))

# save workplace for transformation to STATA
matchW2raw=readRDS(paste0("MATCH_EMA_W2_", Sys.Date(), ".rds"))
# change some variables to factors
matchW2clean = ema.factor(matchW2raw)

matchW2clean = matchW2clean[!is.na(matchW2clean$comply),]
matchW2clean[is.na(matchW2clean)]=99999

save(list=c("matchW2clean"), file=paste0("MATCH_EMA_W2_", Sys.Date(),".RData"))
    
# create STATA script to label variables
ema.stata(paste0("MATCH_EMA_W2_", Sys.Date(), ".do"))
