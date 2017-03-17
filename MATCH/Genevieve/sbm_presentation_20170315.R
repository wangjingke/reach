setwd("Y:/Jing/Genevieve/20160802_SBM")
library(foreign)
library(memisc)

ema <- readRDS("D:/REACH/MATCH/Dataset/MATCH_EMA/Combined/V21/MATCH_EMA_V21.rds")
varlist.parenting <- c("mAskTV", "mAskJunkfood", "mLimitTV", "mLimitJunkfood", "mGoPlay", "mTakePlay", "mGoFresh", "mCookFresh", "mWithChild")
varlist.stress <- c("mDeal", "mHandle", "mStress_homewk", "mStress_job", "mStress_demands", "mStress_cowrkr", "mStress_spouse", "mStress_child", "mStress_else", "mStressed")

ema <- ema[ema$mother==1, c("id", "wave", "date", "promptStart", "promptEnd", "comply", "weekend", "tod", "mother", varlist.parenting, paste0(varlist.parenting, "_1p1"), varlist.stress)]

descriptives <- data.frame(as.data.set(spss.system.file("D:/REACH/MATCH/Project MATCH/MATCH_PERSON_V16_11152016.sav", to.lower = FALSE)))
var.demograph <- c("W1_mID", "W1_mBi_BornUS", "W1_mBi_CC_Fred", "W1_mCC_Grandparent", "W1_mCC_program", "W1_mAge", "W1_ChildAge", "W1_mHouseholdSize", "W1_mNchildren", "W1_mBi_SingleParent", "W1_mBi_married", "W1_mBi_college", "W1_mHispanic", "W1_cHispanic", "W1_mIncomeQ", "W1_mBi_Fulltime", "W1_mBi_Work", "W1_Child_Gender", "W1_mBMI", "W1_mBMIcat")
descriptivesSub <- descriptives[var.demograph]
names(descriptivesSub) <- gsub("W1_", "", names(descriptivesSub))

sbm <- merge(ema, descriptivesSub, by.x = "id", by.y = "mID", all.x = TRUE, sort = FALSE)

# recode
recode = function (x, before, after) {
    if (is.na(x)) {
        return (x)
    } else {
        return (after[grep(x, before)])
    }
}

sbm$rmAskTV <- unlist(sapply(as.numeric(sbm$mAskTV), recode, c(0, 1, 2, 3, 4), c(0, 0, 1, 0, NA)))
sbm$rmLimitTV <- unlist(sapply(as.numeric(sbm$mLimitTV), recode, c(0, 1), c(0, 0.5)))
sbm$rmLimitTV <- ifelse(!is.na(sbm$mAskTV) & as.numeric(sbm$mAskTV)<4 & is.na(sbm$mLimitTV), 0, sbm$rmLimitTV)

sbm$rmAskJunkfood <- unlist(sapply(as.numeric(sbm$mAskJunkfood), recode, c(0, 1, 2, 3, 4), c(0, 0, 1, 0, NA)))
sbm$rmLimitJunkfood <- unlist(sapply(as.numeric(sbm$mLimitJunkfood), recode, c(0, 1), c(0, 0.5)))
sbm$rmLimitJunkfood <- ifelse(!is.na(sbm$mAskJunkfood) & as.numeric(sbm$mAskJunkfood)<4 & is.na(sbm$mLimitJunkfood), 0, sbm$rmLimitJunkfood)

sbm$rmDeal <- unlist(sapply(as.numeric(sbm$mDeal), recode, c(0, 1, 2, 3), c(4, 3, 2, 1)))
sbm$rmHandle <- unlist(sapply(as.numeric(sbm$mHandle), recode, c(0, 1, 2, 3), c(4, 3, 2, 1)))

sbm$rmStress_homewk <- ifelse(is.na(sbm$mStress_homewk) & sbm$comply == 1, 0, sbm$mStress_homewk)
sbm$rmStress_job <- ifelse(is.na(sbm$mStress_job) & sbm$comply == 1, 0, sbm$mStress_job)
sbm$rmStress_demands <- ifelse(is.na(sbm$mStress_demands) & sbm$comply == 1, 0, sbm$mStress_demands)
sbm$rmStress_cowrkr <- ifelse(is.na(sbm$mStress_cowrkr) & sbm$comply == 1, 0, sbm$mStress_cowrkr)
sbm$rmStress_spouse <- ifelse(is.na(sbm$mStress_spouse) & sbm$comply == 1, 0, sbm$mStress_spouse)
sbm$rmStress_child <- ifelse(is.na(sbm$mStress_child) & sbm$comply == 1, 0, sbm$mStress_child)
sbm$rmStress_else <- ifelse(is.na(sbm$mStress_else) & sbm$comply == 1, 0, sbm$mStress_else)

# create summary variables
sbm$totalActivityParent <- as.numeric(sbm$mGoPlay) + as.numeric(sbm$mTakePlay) + sbm$rmAskTV + sbm$rmLimitTV
sbm$totalPhysicalActivityParent <- as.numeric(sbm$mGoPlay) + as.numeric(sbm$mTakePlay)
sbm$totalSedentaryActivityParent <- sbm$rmAskTV + sbm$rmLimitTV

sbm$totalEatingParent <- as.numeric(sbm$mGoFresh) + as.numeric(sbm$mCookFresh) + sbm$rmAskJunkfood + sbm$rmLimitJunkfood
sbm$totalHealthyEatingParent <- as.numeric(sbm$mGoFresh) + as.numeric(sbm$mCookFresh)
sbm$totalUnhealthyEatingParent <- sbm$rmAskJunkfood + sbm$rmLimitJunkfood

sbm$totalPerceivedStress <- (sbm$rmDeal + sbm$rmHandle)*0.5
sbm$totalStressors <- sbm$rmStress_homewk + sbm$rmStress_job + sbm$rmStress_demands + sbm$rmStress_cowrkr + sbm$rmStress_spouse + sbm$rmStress_child + sbm$rmStress_else

# convert to z score
zScore = function (pool) {
    avg <- mean(pool, na.rm = TRUE)
    std <- sd(pool, na.rm = TRUE)
    return ((pool-avg)/std)
}
sbm$zmStressed <- zScore(as.numeric(sbm$mStressed))
sbm$zTotalPerceivedStress <- zScore(sbm$totalPerceivedStress)
sbm$zTotalStressor <- zScore(sbm$totalStressors)

sbm$totalStress <- sbm$zmStressed + sbm$zTotalPerceivedStress + sbm$zTotalStressor

# recode the 1p1 variables
sbm$rmAskTV_1p1 <- unlist(sapply(as.numeric(sbm$mAskTV_1p1), recode, c(0, 1, 2, 3, 4), c(0, 0, 1, 0, NA)))
sbm$rmLimitTV_1p1 <- unlist(sapply(as.numeric(sbm$mLimitTV_1p1), recode, c(0, 1), c(0, 0.5)))
sbm$rmLimitTV_1p1 <- ifelse(!is.na(sbm$mAskTV_1p1) & as.numeric(sbm$mAskTV_1p1)<4 & is.na(sbm$mLimitTV_1p1), 0, sbm$rmLimitTV_1p1)

sbm$rmAskJunkfood_1p1 <- unlist(sapply(as.numeric(sbm$mAskJunkfood_1p1), recode, c(0, 1, 2, 3, 4), c(0, 0, 1, 0, NA)))
sbm$rmLimitJunkfood_1p1 <- unlist(sapply(as.numeric(sbm$mLimitJunkfood_1p1), recode, c(0, 1), c(0, 0.5)))
sbm$rmLimitJunkfood_1p1 <- ifelse(!is.na(sbm$mAskJunkfood_1p1) & as.numeric(sbm$mAskJunkfood_1p1)<4 & is.na(sbm$mLimitJunkfood_1p1), 0, sbm$rmLimitJunkfood_1p1)

sbm$rmDeal_1p1 <- unlist(sapply(as.numeric(sbm$mDeal_1p1), recode, c(0, 1, 2, 3), c(4, 3, 2, 1)))
sbm$rmHandle_1p1 <- unlist(sapply(as.numeric(sbm$mHandle_1p1), recode, c(0, 1, 2, 3), c(4, 3, 2, 1)))

# create summary variables
sbm$totalActivityParent_1p1 <- as.numeric(sbm$mGoPlay_1p1) + as.numeric(sbm$mTakePlay_1p1) + sbm$rmAskTV_1p1 + sbm$rmLimitTV_1p1
sbm$totalPhysicalActivityParent_1p1 <- as.numeric(sbm$mGoPlay_1p1) + as.numeric(sbm$mTakePlay_1p1)
sbm$totalSedentaryActivityParent_1p1 <- sbm$rmAskTV_1p1 + sbm$rmLimitTV_1p1

sbm$totalEatingParent_1p1 <- as.numeric(sbm$mGoFresh_1p1) + as.numeric(sbm$mCookFresh_1p1) + sbm$rmAskJunkfood_1p1 + sbm$rmLimitJunkfood_1p1
sbm$totalHealthyEatingParent_1p1 <- as.numeric(sbm$mGoFresh_1p1) + as.numeric(sbm$mCookFresh_1p1)
sbm$totalUnhealthyEatingParent_1p1 <- sbm$rmAskJunkfood_1p1 + sbm$rmLimitJunkfood_1p1

# create BS/WS variable
wsbs = function(data, var) {
    indivMean = setNames(aggregate(eval(substitute(y~id, list(y = as.name(var)))), data = data, mean, na.rm = TRUE), c('id', 'indivMean'))
    grandMean = mean(indivMean$indivMean, na.rm = TRUE)
    indivMean[paste0(var, '_bs')] = indivMean$indivMean - grandMean
    
    wsbs = merge(data[c('id', 'promptStart', var)], indivMean, by.x = 'id', by.y = 'id', all.x = TRUE, sort = FALSE)
    wsbs[paste0(var, '_ws')] = wsbs[,var] - wsbs$indivMean
    
    data = merge(data, wsbs[c('id', 'promptStart', paste0(var, '_bs'), paste0(var, '_ws'))], by = c('id', 'promptStart'), all.x = TRUE, sort = FALSE)
    
    return (data)
}

predictors <- c("totalPerceivedStress", "totalStress", "totalStressors", "mStressed", "rmStress_homewk", "rmStress_job", "rmStress_demands", "rmStress_cowrkr", "rmStress_spouse", "rmStress_child", "rmStress_else")
for (i in predictors) {
    sbm <- wsbs(sbm, i)
}

# exclusions
fewMain <- aggregate(date ~ id, data = sbm, length)
sbmMain <- sbm[sbm$id %in% fewMain$id[fewMain$date >3],]
sbmMain <- sbmMain[!is.na(sbmMain$mWithChild) & as.numeric(sbmMain$mWithChild)==1,]

# function to report summary statitics
sumContinuous = function(x) {
    print(paste0("mean=", mean(x, na.rm = TRUE)))
    print(paste0("sd=", sd(x, na.rm = TRUE)))
}
sumCategory = function(x) {
    print(table(x, useNA = "ifany"))
    print(prop.table(table(x, useNA = "ifany"))*100)
}

demographicsMain = unique(sbmMain[c("id", "mBi_BornUS", "mBi_CC_Fred", "mCC_Grandparent", "mCC_program", "mAge", "ChildAge", "mHouseholdSize", "mNchildren", "mBi_SingleParent", "mBi_married", "mBi_college", "mHispanic", "cHispanic", "mIncomeQ", "mBi_Fulltime", "mBi_Work", "Child_Gender", "mBMI", "mBMIcat")])
varCat <- c("mBi_BornUS", "mBi_CC_Fred", "mCC_Grandparent", "mCC_program", "mBi_SingleParent", "mBi_married", "mBi_college", "mHispanic", "cHispanic", "mIncomeQ", "mBi_Fulltime", "mBi_Work", "Child_Gender", "mBMIcat")
varCon <- c("mAge", "ChildAge", "mHouseholdSize", "mNchildren", "mBMI")
# categorical demographic variable summary
for (i in varCat) {
    print(i)
    sumCategory(demographicsMain[i])
}
# continuous demographic variable summary
for (i in varCon) {
    print(i)
    sumContinuous(unlist(demographicsMain[i]))
}

# table 2
# average number of prompts per mother
sumContinuous(aggregate(date~id, data = sbmMain, length)$date)
# compliance rate
sumContinuous(aggregate(comply~id, data = sbm, mean)$comply)

for (i in c("rmStress_homewk", "rmStress_job", "rmStress_demands", "rmStress_cowrkr", "rmStress_spouse", "rmStress_child", "rmStress_else", "mGoPlay", "mTakePlay", "rmAskTV", "rmLimitTV", "rmDeal", "rmHandle", "mStressed", "mGoFresh", "mCookFresh", "rmAskJunkfood", "rmLimitJunkfood")) {
    print(i)
    sumCategory(sbmMain[i])
}

for (i in c("totalActivityParent", "totalPhysicalActivityParent", "totalSedentaryActivityParent", "totalEatingParent", "totalHealthyEatingParent", "totalUnhealthyEatingParent", "totalPerceivedStress", "totalStressors", "totalStress")) {
    print(i)
    sumContinuous(unlist(sbmMain[i]))
}
# cronbach's alpha
library(psych)
alpha(sbmMain[c("totalPerceivedStress", "rmDeal", "rmHandle")])
alpha(sbmMain[c("zmStressed", "zTotalPerceivedStress", "zTotalStressor")])

write.dta(sbmMain, "sbm_20170316_main.dta")

###### Ancillary ######
fewAnc <- aggregate(date~id, data = sbm, length)
sbmAnc <- sbm[sbm$id %in% fewAnc$id[fewAnc$date>3],]
sbmAnc <- sbm[!is.na(sbmAnc$mWithChild_1p1) & as.numeric(sbmAnc$mWithChild_1p1)==1,]
nrow(sbmAnc)
length(unique(sbmAnc$id))

demographicsAnc <- unique(sbmAnc[c("id", "mBi_BornUS", "mBi_CC_Fred", "mCC_Grandparent", "mCC_program", "mAge", "ChildAge", "mHouseholdSize", "mNchildren", "mBi_SingleParent", "mBi_married", "mBi_college", "mHispanic", "cHispanic", "mIncomeQ", "mBi_Fulltime", "mBi_Work", "Child_Gender", "mBMI", "mBMIcat")])

for (i in varCat) {
    print(i)
    sumCategory(demographicsAnc[i])
}
# continuous demographic variable summary
for (i in varCon) {
    print(i)
    sumContinuous(unlist(demographicsAnc[i]))
}

# table 2
# average number of prompts per mother
sumContinuous(aggregate(date~id, data = sbmAnc, length)$date)
for (i in c("rmStress_homewk", "rmStress_job", "rmStress_demands", "rmStress_cowrkr", "rmStress_spouse", "rmStress_child", "rmStress_else", "mGoPlay_1p1", "mTakePlay_1p1", "rmAskTV_1p1", "rmLimitTV_1p1", "rmDeal", "rmHandle", "mStressed", "mGoFresh_1p1", "mCookFresh_1p1", "rmAskJunkfood_1p1", "rmLimitJunkfood_1p1")) {
    print(i)
    sumCategory(sbmAnc[i])
}

for (i in c("totalActivityParent_1p1", "totalPhysicalActivityParent_1p1", "totalSedentaryActivityParent_1p1", "totalEatingParent_1p1", "totalHealthyEatingParent_1p1", "totalUnhealthyEatingParent_1p1", "totalPerceivedStress", "totalStressors", "totalStress")) {
    print(i)
    sumContinuous(unlist(sbmAnc[i]))
}

library(psych)
alpha(sbmAnc[c("totalPerceivedStress", "rmDeal", "rmHandle")])
alpha(sbmAnc[c("zmStressed", "zTotalPerceivedStress", "zTotalStressor")])

write.dta(sbmAnc, "sbm_20170316_anc.dta")












