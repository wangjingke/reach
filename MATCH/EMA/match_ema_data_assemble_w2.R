setwd("C:/Users/wangjink/Documents/REACH/MATCH/Dataset/MATCH_EMA/Wave2/Beta20160802")
# loading functions
source("C:/Users/wangjink/Documents/GitHub/reach/MATCH/EMA/backend/match_ema_functions.R")
# construct EMA skeletons
# remember to update schedule sheets for ones whose startDates were not coherent with their enterDate
library(XLConnect)
w2 = readWorksheetFromFile("C:/Users/wangjink/Documents/GitHub/reach/MATCH/EMA/backend/match_ema_keys.xlsx", sheet="W2", colTypes = "character")
w2 = w2[w2$Wave.2=="complete",]
w2$DID = ifelse(nchar(w2$DID)<3, paste0("0", w2$DID), as.character(w2$DID))
w2$enterDate = sapply(strsplit(ifelse(is.na(w2$ManualCorrectedEnter), w2$W2pickup, w2$ManualCorrectedEnter), " "), head, 1)
w2$mother = paste0("11", w2$DID)
w2$child = paste0("12", w2$DID)

W2EMA=ema.promptList(w2, start = "enterDate")
W2EMA=ema.fetchPrompt(ema=W2EMA, emaList="MATCH_EMA_List_Clean_2016-07-14.csv", emaDir="C:/Users/wangjink/Documents/REACH/MATCH/EMA_Clean_RDS/W2", warning="MATCH_W2_warnings.txt")

# search for low compliance
compliance = ema.compliance(W2EMA)

# output low compliance subjects
saveRDS(compliance, paste0("MATCH_W2_EMA_Compliance_", Sys.Date(), ".rds"))
# 11185/12185 dropped out
# deleting prompts with obviously wrong answers (multiple answers for binary variables)
sapply(W2EMA[c(22:119, 122:156)], table)
W2EMA[W2EMA$CHILD_MANAGE %in% c(2,3) & !is.na(W2EMA$CHILD_MANAGE), c("CHILD_PLANNED", "CHILD_MANAGE")]=NA
W2EMA[W2EMA$CHILD_STRESS==7 & !is.na(W2EMA$CHILD_STRESS), c("CHILD_STRESS")]=NA
W2EMA[W2EMA$CHILD_SAD==6 & !is.na(W2EMA$CHILD_SAD), c("CHILD_SAD")]=NA

# change TMRWAKETIME to today's WAKETIME
W2EMA=ema.waketime(W2EMA)
# change some variables to factors
W2EMA=ema.factor(W2EMA)

# integrating ACC data
W2EMA=ema.attachACC(W2EMA)
W2EMA=ema.attachACC(W2EMA, prefix = "O")
W2EMA=ema.ACC(ema=W2EMA, missing_output=paste0("MATCH_EMA_ACC_Missing_", Sys.Date(), ".csv"), accDir = "C:/Users/wangjink/Documents/REACH/MATCH/ACC", accList="MATCH_ACC_List_2016-07-14.csv")

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
matchW2=ema.dailyPA(W2EMA_anchored, AccSummary="C:/Users/wangjink/Documents/REACH/MATCH/ACC/MATCH_ACC_Summary_2016-07-14.txt")
saveRDS(matchW2, paste0("MATCH_EMA_W2_", Sys.Date(), ".rds"))

# create STATA script to label variables
ema.stata(paste0("MATCH_EMA_W2_", Sys.Date(), ".do"))

# save workplace for transformation to STATA
matchW2=readRDS(paste0("MATCH_EMA_W2_", Sys.Date(), ".rds"))
matchW2clean = matchW2[!is.na(matchW2$COMPLY),]
matchW2clean[is.na(matchW2clean)]=99999
# get rid of anchored OMVPA variables
for (i in grep("^(OMVPA|OSED|OLIGHT|OMOD|OVIG|OVALID|ONONVALID).*_(1P1|1P2|1M1|1M2|0C|0P1|0P2|0M1|0M2)$", names(matchW2), value = TRUE)) {
    matchW2clean[,i]=NULL
}
save(list=c("matchW2clean"), file=paste0("MATCH_EMA_W2_", Sys.Date(),".RData"))
    
