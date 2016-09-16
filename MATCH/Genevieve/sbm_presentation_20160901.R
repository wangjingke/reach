library(foreign)
library(memisc)
setwd("Y:/Jing/Genevieve/20160802_SBM")

load("D:/REACH/MATCH/Dataset/MATCH_EMA/Wave1/Beta20160829/MATCH_EMA_W1_2016-08-29.RData")
w1Sub = matchW1clean[matchW1clean$mother==1, c("SubjectID", "Date", "prompt_start","COMPLY","weekend", "TOD", "mother", "MOTHER_ASKTV", "MOTHER_ASKJUNKFOOD", "MOTHER_LIMITTV", "MOTHER_LIMITJUNKFOOD", "MOTHER_DEAL", "MOTHER_HANDLE", "MOTHER_STRESS_HOMEWK", "MOTHER_STRESS_JOB", "MOTHER_STRESS_DEMANDS", "MOTHER_STRESS_COWRKR", "MOTHER_STRESS_SPOUSE", "MOTHER_STRESS_CHILD", "MOTHER_STRESS_ELSE", "MOTHER_GOPLAY", "MOTHER_TAKEPLAY", "MOTHER_GOFRESH", "MOTHER_COOKFRESH", "MOTHER_STRESSED", "MOTHER_WITHCHILD", "MOTHER_ASKTV_1P1", "MOTHER_ASKJUNKFOOD_1P1", "MOTHER_LIMITTV_1P1", "MOTHER_LIMITJUNKFOOD_1P1", "MOTHER_DEAL_1P1", "MOTHER_HANDLE_1P1", "MOTHER_STRESS_HOMEWK_1P1", "MOTHER_STRESS_JOB_1P1", "MOTHER_STRESS_DEMANDS_1P1", "MOTHER_STRESS_COWRKR_1P1", "MOTHER_STRESS_SPOUSE_1P1", "MOTHER_STRESS_CHILD_1P1", "MOTHER_STRESS_ELSE_1P1", "MOTHER_GOPLAY_1P1", "MOTHER_TAKEPLAY_1P1", "MOTHER_GOFRESH_1P1", "MOTHER_COOKFRESH_1P1", "MOTHER_STRESSED_1P1", "MOTHER_WITHCHILD_1P1")]

descriptives=data.frame(as.data.set(spss.system.file("Y:/MATCH STUDY/Main Study/Data/Surveys and Anthropometric Data/Wave 1 Data Survey/SUMMARY VARIABLES_RELEASE/MATCH_Person_V15_06302016.sav", to.lower = FALSE)))
descriptivesSub = descriptives[c("Mother_ID", "Bi_BornUS", "Bi_CC_Fred", "CC_Grandparent", "CC_program", "Age", "Childage", "HouseholdSize", "Nchildren", "Bi_singleparent", "Bi_married", "Bi_college", "Hispanic", "cHispanic", "incomeQ", "Bi_Fulltime", "Bi_Work", "Child_Gender", "mBMI", "mBMIcat")]

sbm = merge(w1Sub, descriptivesSub, by.x="SubjectID", by.y="Mother_ID", all.x = TRUE, sort = FALSE)
sbm$DID = substr(sbm$SubjectID, 3, 5)
sbm[sbm==99999]=NA

# recodes
recode = function (x, before, after) {
    if (is.na(x)) {
        return (x)
    } else {
        return (after[grep(x, before)])
    }
}

sbm$rMOTHER_ASKTV = sapply(as.numeric(sbm$MOTHER_ASKTV), recode, c(1, 2, 3, 4, 5), c(0, 0, 1, 0, NA))
sbm$rMOTHER_LIMITTV = sapply(as.numeric(sbm$MOTHER_LIMITTV), recode, c(0, 1), c(0, 0.5))
sbm$rMOTHER_LIMITTV = ifelse((!is.na(sbm$MOTHER_ASKTV) & as.numeric(sbm$MOTHER_ASKTV)<5) & (is.na(sbm$MOTHER_LIMITTV)), 0, sbm$rMOTHER_LIMITTV)

sbm$rMOTHER_ASKJUNKFOOD = sapply(as.numeric(sbm$MOTHER_ASKJUNKFOOD), recode, c(1, 2, 3, 4, 5), c(0, 0, 1, 0, NA))
sbm$rMOTHER_LIMITJUNKFOOD = sapply(as.numeric(sbm$MOTHER_LIMITJUNKFOOD), recode, c(0, 1), c(0, 0.5))
sbm$rMOTHER_LIMITJUNKFOOD = ifelse((!is.na(sbm$MOTHER_ASKJUNKFOOD) & as.numeric(sbm$MOTHER_ASKJUNKFOOD)<5) & is.na(sbm$MOTHER_LIMITJUNKFOOD), 0, sbm$rMOTHER_LIMITJUNKFOOD)

sbm$rMOTHER_DEAL = sapply(as.numeric(sbm$MOTHER_DEAL), recode, c(1, 2, 3, 4), c(4, 3, 2, 1))
sbm$rMOTHER_HANDLE = sapply(as.numeric(sbm$MOTHER_HANDLE), recode, c(1, 2, 3, 4), c(4, 3, 2, 1))

sbm$rMOTHER_STRESS_HOMEWK = ifelse(is.na(sbm$MOTHER_STRESS_HOMEWK) & sbm$COMPLY==1, 0, sbm$MOTHER_STRESS_HOMEWK)
sbm$rMOTHER_STRESS_JOB = ifelse(is.na(sbm$MOTHER_STRESS_JOB) & sbm$COMPLY==1, 0, sbm$MOTHER_STRESS_JOB)
sbm$rMOTHER_STRESS_DEMANDS = ifelse(is.na(sbm$MOTHER_STRESS_DEMANDS) & sbm$COMPLY==1, 0, sbm$MOTHER_STRESS_DEMANDS)
sbm$rMOTHER_STRESS_COWRKR = ifelse(is.na(sbm$MOTHER_STRESS_COWRKR) & sbm$COMPLY==1, 0, sbm$MOTHER_STRESS_COWRKR)
sbm$rMOTHER_STRESS_SPOUSE = ifelse(is.na(sbm$MOTHER_STRESS_SPOUSE) & sbm$COMPLY==1, 0, sbm$MOTHER_STRESS_SPOUSE)
sbm$rMOTHER_STRESS_CHILD = ifelse(is.na(sbm$MOTHER_STRESS_CHILD) & sbm$COMPLY==1, 0, sbm$MOTHER_STRESS_CHILD)
sbm$rMOTHER_STRESS_ELSE = ifelse(is.na(sbm$MOTHER_STRESS_ELSE) & sbm$COMPLY==1, 0, sbm$MOTHER_STRESS_ELSE)

# create summary variables
sbm$totalActivityParent = as.numeric(sbm$MOTHER_GOPLAY) + as.numeric(sbm$MOTHER_TAKEPLAY) + sbm$rMOTHER_ASKTV + sbm$rMOTHER_LIMITTV
sbm$totalPhysicalActivityParent = as.numeric(sbm$MOTHER_GOPLAY) + as.numeric(sbm$MOTHER_TAKEPLAY)
sbm$totalSedentaryActivityParent = sbm$rMOTHER_ASKTV + sbm$rMOTHER_LIMITTV

sbm$totalEatingParent = as.numeric(sbm$MOTHER_GOFRESH) + as.numeric(sbm$MOTHER_COOKFRESH) + sbm$rMOTHER_ASKJUNKFOOD + sbm$rMOTHER_LIMITJUNKFOOD
sbm$totalHealthyEatingParent = as.numeric(sbm$MOTHER_GOFRESH) + as.numeric(sbm$MOTHER_COOKFRESH)
sbm$totalUnhealthyEatingParent = sbm$rMOTHER_ASKJUNKFOOD + sbm$rMOTHER_LIMITJUNKFOOD

sbm$totalPerceivedStress = (sbm$rMOTHER_DEAL + sbm$rMOTHER_HANDLE)*0.5
sbm$totalStressors = sbm$rMOTHER_STRESS_HOMEWK + sbm$rMOTHER_STRESS_JOB + sbm$rMOTHER_STRESS_DEMANDS + sbm$rMOTHER_STRESS_COWRKR + sbm$rMOTHER_STRESS_SPOUSE + sbm$rMOTHER_STRESS_CHILD + sbm$rMOTHER_STRESS_ELSE

# convert to z score
zScore = function (pool) {
    return (sapply(pool, FUN = function(x) {(x-mean(pool, na.rm = TRUE))/sd(pool, na.rm = TRUE)}))
}
sbm$zMOTHER_STRESSED = zScore(as.numeric(sbm$MOTHER_STRESSED))
sbm$zTotalPerceivedStress = zScore(sbm$totalPerceivedStress)
sbm$zTotalStressors = zScore(sbm$totalStressors)

sbm$totalStress = sbm$zMOTHER_STRESSED + sbm$zTotalPerceivedStress + sbm$zTotalStressors

# recode the 1P1 parent variables
sbm$rMOTHER_ASKTV_1P1 = sapply(as.numeric(sbm$MOTHER_ASKTV_1P1), recode, c(1, 2, 3, 4, 5), c(0, 0, 1, 0, NA))
sbm$rMOTHER_LIMITTV_1P1 = sapply(as.numeric(sbm$MOTHER_LIMITTV_1P1), recode, c(0, 1), c(0, 0.5))
sbm$rMOTHER_LIMITTV_1P1 = ifelse(!is.na(sbm$MOTHER_ASKTV_1P1) & is.na(sbm$MOTHER_LIMITTV_1P1), 0, sbm$rMOTHER_LIMITTV_1P1)

sbm$rMOTHER_ASKJUNKFOOD_1P1 = sapply(as.numeric(sbm$MOTHER_ASKJUNKFOOD_1P1), recode, c(1, 2, 3, 4, 5), c(0, 0, 1, 0, NA))
sbm$rMOTHER_LIMITJUNKFOOD_1P1 = sapply(as.numeric(sbm$MOTHER_LIMITJUNKFOOD_1P1), recode, c(0, 1), c(0, 0.5))
sbm$rMOTHER_LIMITJUNKFOOD_1P1 = ifelse(!is.na(sbm$MOTHER_ASKJUNKFOOD_1P1) & is.na(sbm$MOTHER_LIMITJUNKFOOD_1P1), 0, sbm$rMOTHER_LIMITJUNKFOOD_1P1)

sbm$totalActivityParent_1P1 = as.numeric(sbm$MOTHER_GOPLAY_1P1) + as.numeric(sbm$MOTHER_TAKEPLAY_1P1) + sbm$rMOTHER_ASKTV_1P1 + sbm$rMOTHER_LIMITTV_1P1
sbm$totalPhysicalActivityParent_1P1 = as.numeric(sbm$MOTHER_GOPLAY_1P1) + as.numeric(sbm$MOTHER_TAKEPLAY_1P1)
sbm$totalSedentaryActivityParent_1P1 = sbm$rMOTHER_ASKTV_1P1 + sbm$rMOTHER_LIMITTV_1P1

sbm$totalEatingParent_1P1 = as.numeric(sbm$MOTHER_GOFRESH_1P1) + as.numeric(sbm$MOTHER_COOKFRESH_1P1) + sbm$rMOTHER_ASKJUNKFOOD_1P1 + sbm$rMOTHER_LIMITJUNKFOOD_1P1
sbm$totalHealthyEatingParent_1P1 = as.numeric(sbm$MOTHER_GOFRESH_1P1) + as.numeric(sbm$MOTHER_COOKFRESH_1P1)
sbm$totalUnhealthyEatingParent_1P1 = sbm$rMOTHER_ASKJUNKFOOD_1P1 + sbm$rMOTHER_LIMITJUNKFOOD_1P1

# creating BS/WS variables
bsWs = function (var, data) {
    data$BS = NA
    data$WS = NA 
    for (i in 1:nrow(data)) {
        data[i, "BS"] = data[i, var] - mean(unlist(data[var]), na.rm = TRUE)
        data[i, "WS"] = data[i, var] - mean(unlist(data[data$ID==data$ID[i], var]), na.rm = TRUE)
    }
    return(data)
}

predictors = c("totalPerceivedStress", "totalStress", "totalStressors", "rMOTHER_STRESS_HOMEWK", "rMOTHER_STRESS_JOB", "rMOTHER_STRESS_DEMANDS", "rMOTHER_STRESS_COWRKR", "rMOTHER_STRESS_SPOUSE", "rMOTHER_STRESS_CHILD", "rMOTHER_STRESS_ELSE")
for (i in predictors) {
    for (j in 1:nrow(sbm)) {
        sbm[j, paste0(i, "_BS")] = sbm[j, i] - mean(unlist(sbm[i]), na.rm = TRUE)
        sbm[j, paste0(i, "_WS")] = sbm[j, i] - mean(unlist(sbm[sbm$SubjectID==sbm$SubjectID[j], i]), na.rm = TRUE)
    }
    print(i)
}


# exclusions
fewMain = aggregate(Date~SubjectID, data = sbm, length)
sbmMain = sbm[sbm$SubjectID %in% fewMain$SubjectID[fewMain$Date>3],]
sbmMain = sbmMain[!is.na(sbmMain$MOTHER_WITHCHILD) & as.numeric(sbmMain$MOTHER_WITHCHILD)==1, ]

# function to report summary statitics
sumContinuous = function(x) {
    print(paste0("mean=", mean(x, na.rm = TRUE)))
    print(paste0("sd=", sd(x, na.rm = TRUE)))
}
sumCategory = function(x) {
    print(table(x, useNA = "ifany"))
    print(prop.table(table(x, useNA = "ifany"))*100)
}

# Table 1 demographics (MAIN)
demographicsMain = unique(sbmMain[c("SubjectID", "Bi_BornUS", "Bi_CC_Fred", "CC_Grandparent", "CC_program", "Age", "Childage", "HouseholdSize", "Nchildren", "Bi_singleparent", "Bi_married", "Bi_college", "Hispanic", "cHispanic", "incomeQ", "Bi_Fulltime", "Bi_Work", "Child_Gender", "mBMI", "mBMIcat")])
varCat = c("Bi_BornUS", "Bi_CC_Fred", "CC_Grandparent", "CC_program", "Bi_singleparent", "Bi_married", "Bi_college", "Hispanic", "cHispanic", "incomeQ", "Bi_Fulltime", "Bi_Work", "Child_Gender", "mBMIcat")
varCon = c("Age", "Childage", "HouseholdSize", "Nchildren", "mBMI")
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

# Table 2 descriptive statistics (MAIN)
# number of mothers
length(unique(demographicsMain$SubjectID))
# number of total prompts
nrow(sbmMain)
# average number of prompts per mother
sumContinuous(aggregate(Date~SubjectID, data = sbmMain, length)$Date)
# mother ema compliance rate
sumContinuous(aggregate(COMPLY~SubjectID, data = sbmMain, mean)$COMPLY)

for (i in c("rMOTHER_STRESS_HOMEWK", "rMOTHER_STRESS_JOB", "rMOTHER_STRESS_DEMANDS", "rMOTHER_STRESS_COWRKR", "rMOTHER_STRESS_SPOUSE", "rMOTHER_STRESS_CHILD", "rMOTHER_STRESS_ELSE", "MOTHER_GOPLAY", "MOTHER_TAKEPLAY", "rMOTHER_ASKTV", "rMOTHER_LIMITTV", "MOTHER_GOFRESH", "MOTHER_COOKFRESH", "rMOTHER_ASKJUNKFOOD", "rMOTHER_LIMITJUNKFOOD", "rMOTHER_DEAL", "rMOTHER_HANDLE")) {
    print(i)
    sumCategory(sbmMain[i])
}

for (i in c("totalPhysicalActivityParent", "totalHealthyEatingParent", "totalPerceivedStress", "totalStressors", "totalStress")) {
    print(i)
    sumContinuous(unlist(sbmMain[i]))
}
# cronbach's alpha
library(psych)
alpha(sbmMain[c("totalPerceivedStress", "rMOTHER_DEAL", "rMOTHER_HANDLE")])
alpha(sbmMain[c("zMOTHER_STRESSED", "zTotalPerceivedStress", "zTotalStressors")])

write.dta(sbmMain, "sbm_20160901_main.dta")

### Ancillary, exclusion ###
fewAnc = aggregate(Date~SubjectID, data = sbm, length)
sbmAnc = sbm[sbm$SubjectID %in% fewAnc$SubjectID[fewAnc$Date>3],]
# summary
nrow(sbmAnc)
length(unique(sbmAnc$SubjectID))
# prompt summmary, compliance rate
table(sbmAnc$COMPLY)
round(prop.table(table(sbmAnc$COMPLY))*100, 2)
# exclusion by withchild
sbmAnc = sbmAnc[!is.na(sbmAnc$MOTHER_WITHCHILD_1P1) & as.numeric(sbmAnc$MOTHER_WITHCHILD_1P1)==1, ]
# summary
nrow(sbmAnc)
length(unique(sbmAnc$SubjectID))

# Table 1 demographics (Ancillary)
demographicsAnc = unique(sbmAnc[c("SubjectID", "Bi_BornUS", "Bi_CC_Fred", "CC_Grandparent", "CC_program", "Age", "Childage", "HouseholdSize", "Nchildren", "Bi_singleparent", "Bi_married", "Bi_college", "Hispanic", "cHispanic", "incomeQ", "Bi_Fulltime", "Bi_Work", "Child_Gender", "mBMI", "mBMIcat")])
varCat = c("Bi_BornUS", "Bi_CC_Fred", "CC_Grandparent", "CC_program", "Bi_singleparent", "Bi_married", "Bi_college", "Hispanic", "cHispanic", "incomeQ", "Bi_Fulltime", "Bi_Work", "Child_Gender", "mBMIcat")
varCon = c("Age", "Childage", "HouseholdSize", "Nchildren", "mBMI")
# categorical demographic variable summary
for (i in varCat) {
    print(i)
    sumCategory(demographicsAnc[i])
}
# continuous demographic variable summary
for (i in varCon) {
    print(i)
    sumContinuous(unlist(demographicsAnc[i]))
}

# Table 2 descriptive statistics (Ancillary)
# number of mothers
length(unique(demographicsAnc$SubjectID))
# number of total prompts
nrow(sbmAnc)
# average number of prompts per mother
sumContinuous(aggregate(Date~SubjectID, data = sbmAnc, length)$Date)
# mother ema compliance rate
sumContinuous(aggregate(COMPLY~SubjectID, data = sbmAnc, mean)$COMPLY)

for (i in c("rMOTHER_STRESS_HOMEWK", "rMOTHER_STRESS_JOB", "rMOTHER_STRESS_DEMANDS", "rMOTHER_STRESS_COWRKR", "rMOTHER_STRESS_SPOUSE", "rMOTHER_STRESS_CHILD", "rMOTHER_STRESS_ELSE", "MOTHER_GOPLAY", "MOTHER_TAKEPLAY", "rMOTHER_ASKTV", "rMOTHER_LIMITTV", "MOTHER_GOFRESH", "MOTHER_COOKFRESH", "rMOTHER_ASKJUNKFOOD", "rMOTHER_LIMITJUNKFOOD")) {
    print(i)
    sumCategory(sbmAnc[i])
}

for (i in c("totalActivityParent", "totalPhysicalActivityParent", "totalSedentaryActivityParent", "totalEatingParent", "totalHealthyEatingParent", "totalUnhealthyEatingParent", "totalPerceivedStress", "totalStressors", "zMOTHER_STRESSED", "zTotalPerceivedStress", "zTotalStressors", "totalStress")) {
    print(i)
    sumContinuous(unlist(sbmAnc[i]))
}

library(psych)

alpha(sbmAnc[c("rMOTHER_DEAL", "rMOTHER_HANDLE")]) # total perceived stress
alpha(sbmAnc[c("zMOTHER_STRESSED", "zTotalPerceivedStress", "zTotalStressors")]) # total stress
alpha(data.frame(as.numeric(sbmAnc$MOTHER_GOPLAY), as.numeric(sbmAnc$MOTHER_TAKEPLAY))) # physical activity parenting
alpha(data.frame(as.numeric(sbmAnc$MOTHER_GOFRESH), as.numeric(sbmAnc$MOTHER_COOKFRESH))) # healthy eating parenting

# Table 3 covariate screening
library(lme4)
library(nlme)

covariates = list()
for (i in c("totalActivityParent", "totalPhysicalActivityParent", "totalSedentaryActivityParent", "totalEatingParent", "totalHealthyEatingParent", "totalUnhealthyEatingParent")) {
    for (j in c("Bi_BornUS", "Bi_CC_Fred", "CC_Grandparent", "CC_program", "Bi_singleparent", "Bi_married", "Bi_college", "Hispanic", "cHispanic", "incomeQ", "Bi_Fulltime", "Bi_Work", "Child_Gender", "mBMIcat", "Age", "Childage", "HouseholdSize", "Nchildren", "TOD", "weekend")) {
        lme.X = lme(eval(substitute(y ~ x, list(y = as.name(i), x = as.name(j)))), random = ~1|SubjectID, data = sbm[!is.na(sbm[i]) & !is.na(sbm[j]),])
        print(i)
        print(j)
        print(summary(lme.X))
        
        if (summary(lme.X)$tTable[2, 5]<=0.06) {
            covariates[[length(covariates)+1]] = c(i, j, summary(lme.X)$tTable[2, 5])
        }
    }
}

write.dta(sbmAnc, "sbm_20160908_ancillary.dta")















