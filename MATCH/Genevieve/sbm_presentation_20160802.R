setwd("Y:/Jing/Genevieve/20160802_SBM")

library(foreign)
library(memisc)
w1 = read.dta("C:/Users/wangjink/Documents/REACH/MATCH/Project MATCH/MATCH_EMA_V14.dta", warn.missing.labels = FALSE)
w1Sub = w1[w1$MOTHER==1, c("ID", "WAVE", "DID", "DAY", "WINDOW_MOTHER", "WINDOW", "DATE", "STRINGTIME","COMPLY","WEEKEND", "TOD", "MOTHER", "CHILD", "MOTHER_ASKTV", "MOTHER_ASKJUNKFOOD", "MOTHER_LIMITTV", "MOTHER_LIMITJUNKFOOD", "MOTHER_DEAL", "MOTHER_HANDLE", "MOTHER_STRESS_HOMEWK", "MOTHER_STRESS_JOB", "MOTHER_STRESS_DEMANDS", "MOTHER_STRESS_COWRKR", "MOTHER_STRESS_SPOUSE", "MOTHER_STRESS_CHILD", "MOTHER_STRESS_ELSE", "MOTHER_GOPLAY", "MOTHER_TAKEPLAY", "MOTHER_GOFRESH", "MOTHER_COOKFRESH", "MOTHER_STRESSED", "MOTHER_WITHCHILD", "MOTHER_ASKTV_1P1", "MOTHER_ASKJUNKFOOD_1P1", "MOTHER_LIMITTV_1P1", "MOTHER_LIMITJUNKFOOD_1P1", "MOTHER_DEAL_1P1", "MOTHER_HANDLE_1P1", "MOTHER_STRESS_HOMEWK_1P1", "MOTHER_STRESS_JOB_1P1", "MOTHER_STRESS_DEMANDS_1P1", "MOTHER_STRESS_COWRKR_1P1", "MOTHER_STRESS_SPOUSE_1P1", "MOTHER_STRESS_CHILD_1P1", "MOTHER_STRESS_ELSE_1P1", "MOTHER_GOPLAY_1P1", "MOTHER_TAKEPLAY_1P1", "MOTHER_GOFRESH_1P1", "MOTHER_COOKFRESH_1P1", "MOTHER_STRESSED_1P1", "MOTHER_WITHCHILD_1P1")]

descriptives=data.frame(as.data.set(spss.system.file("Y:/MATCH STUDY/Main Study/Data/Surveys and Anthropometric Data/Wave 1 Data Survey/SUMMARY VARIABLES_RELEASE/MATCH_Person_V15_06302016.sav", to.lower = FALSE)))
descriptivesSub = descriptives[c("Mother_ID", "Bi_BornUS", "Bi_CC_Fred", "CC_Grandparent", "CC_program", "Age", "Childage", "HouseholdSize", "Nchildren", "Bi_singleparent", "Bi_married", "Bi_college", "Hispanic", "cHispanic", "incomeQ", "Bi_Fulltime", "Bi_Work", "Child_Gender", "mBMI", "mBMIcat")]

sbm = merge(w1Sub, descriptivesSub, by.x="ID", by.y="Mother_ID", all.x = TRUE, sort = FALSE)

# recodes
recode = function (x, before, after) {
    if (is.na(x)) {
        return (x)
    } else {
        return (after[grep(x, before)])
    }
}
sbm$rMOTHER_ASKTV = sapply(as.numeric(sbm$MOTHER_ASKTV), recode, c(1, 2, 3, 4, 5), c(0, 0, 1, 0, NA))
sbm$rMOTHER_LIMITTV = sapply(as.numeric(sbm$MOTHER_LIMITTV), recode, c(1, 2), c(0, 0.5))
sbm$rMOTHER_LIMITTV = ifelse((!is.na(sbm$MOTHER_ASKTV)) & (is.na(sbm$MOTHER_LIMITTV)), 0, sbm$rMOTHER_LIMITTV)

sbm$rMOTHER_ASKJUNKFOOD = sapply(as.numeric(sbm$MOTHER_ASKJUNKFOOD), recode, c(1, 2, 3, 4, 5), c(0, 0, 1, 0, NA))
sbm$rMOTHER_LIMITJUNKFOOD = sapply(as.numeric(sbm$MOTHER_LIMITJUNKFOOD), recode, c(1, 2), c(0, 0.5))
sbm$rMOTHER_LIMITJUNKFOOD = ifelse((!is.na(sbm$MOTHER_ASKJUNKFOOD)) & is.na(sbm$MOTHER_LIMITJUNKFOOD), 0, sbm$rMOTHER_LIMITJUNKFOOD)

sbm$rMOTHER_DEAL = sapply(as.numeric(sbm$MOTHER_DEAL), recode, c(1, 2, 3, 4), c(4, 3, 2, 1))
sbm$rMOTHER_HANDLE = sapply(as.numeric(sbm$MOTHER_HANDLE), recode, c(1, 2, 3, 4), c(4, 3, 2, 1))

sbm$rMOTHER_STRESS_HOMEWK = ifelse(is.na(sbm$MOTHER_STRESS_HOMEWK) & sbm$COMPLY==1, 0, sbm$MOTHER_STRESS_HOMEWK)
sbm$rMOTHER_STRESS_JOB = ifelse(is.na(sbm$MOTHER_STRESS_JOB) & sbm$COMPLY==1, 0, sbm$MOTHER_STRESS_JOB)
sbm$rMOTHER_STRESS_DEMANDS = ifelse(is.na(sbm$MOTHER_STRESS_DEMANDS) & sbm$COMPLY==1, 0, sbm$MOTHER_STRESS_DEMANDS)
sbm$rMOTHER_STRESS_COWRKR = ifelse(is.na(sbm$MOTHER_STRESS_COWRKR) & sbm$COMPLY==1, 0, sbm$MOTHER_STRESS_COWRKR)
sbm$rMOTHER_STRESS_SPOUSE = ifelse(is.na(sbm$MOTHER_STRESS_SPOUSE) & sbm$COMPLY==1, 0, sbm$MOTHER_STRESS_SPOUSE)
sbm$rMOTHER_STRESS_CHILD = ifelse(is.na(sbm$MOTHER_STRESS_CHILD) & sbm$COMPLY==1, 0, sbm$MOTHER_STRESS_CHILD)
sbm$rMOTHER_STRESS_ELSE = ifelse(is.na(sbm$MOTHER_STRESS_ELSE) & sbm$COMPLY==1, 0, sbm$MOTHER_STRESS_ELSE)

# change 1,2 to 0,1
sbm$MOTHER_GOPLAY = sapply(as.numeric(sbm$MOTHER_GOPLAY), recode, c(1, 2), c(0, 1))
sbm$MOTHER_TAKEPLAY = sapply(as.numeric(sbm$MOTHER_TAKEPLAY), recode, c(1, 2), c(0, 1))
sbm$MOTHER_GOFRESH = sapply(as.numeric(sbm$MOTHER_GOFRESH), recode, c(1, 2), c(0, 1))
sbm$MOTHER_COOKFRESH = sapply(as.numeric(sbm$MOTHER_COOKFRESH), recode, c(1, 2), c(0, 1))

# summary variables
sbm$totalPhysicalActivityParenting = sbm$MOTHER_GOPLAY + sbm$MOTHER_TAKEPLAY + sbm$rMOTHER_ASKTV + sbm$rMOTHER_LIMITTV
sbm$totalHealthyEatingParenting = sbm$MOTHER_GOFRESH + sbm$MOTHER_COOKFRESH + sbm$rMOTHER_ASKJUNKFOOD + sbm$rMOTHER_LIMITJUNKFOOD
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

# recode the 1P1 parenting variables
sbm$rMOTHER_ASKTV_1P1 = sapply(as.numeric(sbm$MOTHER_ASKTV_1P1), recode, c(0, 1, 2, 3, 4), c(0, 0, 1, 0, NA))
sbm$rMOTHER_LIMITTV_1P1 = sapply(as.numeric(sbm$MOTHER_LIMITTV_1P1), recode, c(0, 1), c(0, 0.5))
sbm$rMOTHER_LIMITTV_1P1 = ifelse(!is.na(sbm$MOTHER_ASKTV_1P1) & is.na(sbm$MOTHER_LIMITTV_1P1), 0, sbm$rMOTHER_LIMITTV_1P1)

sbm$rMOTHER_ASKJUNKFOOD_1P1 = sapply(as.numeric(sbm$MOTHER_ASKJUNKFOOD_1P1), recode, c(0, 1, 2, 3, 4), c(0, 0, 1, 0, NA))
sbm$rMOTHER_LIMITJUNKFOOD_1P1 = sapply(as.numeric(sbm$MOTHER_LIMITJUNKFOOD_1P1), recode, c(0, 1), c(0, 0.5))
sbm$rMOTHER_LIMITJUNKFOOD_1P1 = ifelse(!is.na(sbm$MOTHER_ASKJUNKFOOD_1P1) & is.na(sbm$MOTHER_LIMITJUNKFOOD_1P1), 0, sbm$rMOTHER_LIMITJUNKFOOD_1P1)

sbm$totalPhysicalActivityParenting_1P1 = sbm$MOTHER_GOPLAY_1P1 + sbm$MOTHER_TAKEPLAY_1P1 + sbm$rMOTHER_ASKTV_1P1 + sbm$rMOTHER_LIMITTV_1P1
sbm$totalHealthyEatingParenting_1P1 = sbm$MOTHER_GOFRESH_1P1 + sbm$MOTHER_COOKFRESH_1P1 + sbm$rMOTHER_ASKJUNKFOOD_1P1 + sbm$rMOTHER_LIMITJUNKFOOD_1P1

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
        sbm[j, paste0(i, "_WS")] = sbm[j, i] - mean(unlist(sbm[sbm$ID==sbm$ID[j], i]), na.rm = TRUE)
    }
    print(i)
}

# exclusions
sbmMain = sbm[!is.na(sbm$MOTHER_WITHCHILD) & as.numeric(sbm$MOTHER_WITHCHILD)==2, ]
fewMain = aggregate(WINDOW_MOTHER~ID, data = sbmMain, length)
sbmMain = sbmMain[sbmMain$ID %in% fewMain$ID[fewMain$WINDOW_MOTHER>3],]


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
demographicsMain = unique(sbmMain[c("ID", "Bi_BornUS", "Bi_CC_Fred", "CC_Grandparent", "CC_program", "Age", "Childage", "HouseholdSize", "Nchildren", "Bi_singleparent", "Bi_married", "Bi_college", "Hispanic", "cHispanic", "incomeQ", "Bi_Fulltime", "Bi_Work", "Child_Gender", "mBMI", "mBMIcat")])
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
length(unique(demographicsMain$ID))
# number of total prompts
nrow(sbmMain)
# average number of prompts per mother
sumContinuous(aggregate(WINDOW~ID, data = sbmMain, length)$WINDOW)
# mother ema compliance rate
sumContinuous(aggregate(COMPLY~ID, data = sbmMain, mean)$COMPLY)

for (i in c("rMOTHER_STRESS_HOMEWK", "rMOTHER_STRESS_JOB", "rMOTHER_STRESS_DEMANDS", "rMOTHER_STRESS_COWRKR", "rMOTHER_STRESS_SPOUSE", "rMOTHER_STRESS_CHILD", "rMOTHER_STRESS_ELSE", "MOTHER_GOPLAY", "MOTHER_TAKEPLAY", "rMOTHER_ASKTV", "rMOTHER_LIMITTV", "MOTHER_GOFRESH", "MOTHER_COOKFRESH", "rMOTHER_ASKJUNKFOOD", "rMOTHER_LIMITJUNKFOOD", "rMOTHER_DEAL", "rMOTHER_HANDLE")) {
    print(i)
    sumCategory(sbmMain[i])
}

for (i in c("totalPhysicalActivityParenting", "totalHealthyEatingParenting", "totalPerceivedStress", "totalStressors", "totalStress")) {
    print(i)
    sumContinuous(unlist(sbmMain[i]))
}
# cronbach's alpha
library(psych)
alpha(sbmMain[c("totalPerceivedStress", "rMOTHER_DEAL", "rMOTHER_HANDLE")])
alpha(sbmMain[c("zMOTHER_STRESSED", "zTotalPerceivedStress", "zTotalStressors")])

### Ancillary, exclusion ###
sbmAnc = sbm[!is.na(sbm$MOTHER_WITHCHILD_1P1) & as.numeric(sbm$MOTHER_WITHCHILD_1P1)==1, ]
fewAnc = aggregate(WINDOW_MOTHER~ID, data = sbmAnc, length)
sbmAnc = sbmAnc[sbmAnc$ID %in% fewAnc$ID[fewAnc$WINDOW_MOTHER>3],]

# Table 1 demographics (Ancillary)
demographicsAnc = unique(sbmAnc[c("ID", "Bi_BornUS", "Bi_CC_Fred", "CC_Grandparent", "CC_program", "Age", "Childage", "HouseholdSize", "Nchildren", "Bi_singleparent", "Bi_married", "Bi_college", "Hispanic", "cHispanic", "incomeQ", "Bi_Fulltime", "Bi_Work", "Child_Gender", "mBMI", "mBMIcat")])
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
length(unique(demographicsAnc$ID))
# number of total prompts
nrow(sbmAnc)
# average number of prompts per mother
sumContinuous(aggregate(WINDOW~ID, data = sbmAnc, length)$WINDOW)
# mother ema compliance rate
sumContinuous(aggregate(COMPLY~ID, data = sbmAnc, mean)$COMPLY)

for (i in c("rMOTHER_STRESS_HOMEWK", "rMOTHER_STRESS_JOB", "rMOTHER_STRESS_DEMANDS", "rMOTHER_STRESS_COWRKR", "rMOTHER_STRESS_SPOUSE", "rMOTHER_STRESS_CHILD", "rMOTHER_STRESS_ELSE", "MOTHER_GOPLAY", "MOTHER_TAKEPLAY", "rMOTHER_ASKTV", "rMOTHER_LIMITTV", "MOTHER_GOFRESH", "MOTHER_COOKFRESH", "rMOTHER_ASKJUNKFOOD", "rMOTHER_LIMITJUNKFOOD")) {
    print(i)
    sumCategory(sbmAnc[i])
}

for (i in c("totalPhysicalActivityParenting", "totalHealthyEatingParenting", "totalPerceivedStress", "totalStressors", "zMOTHER_STRESSED", "zTotalPerceivedStress", "zTotalStressors", "totalStress")) {
    print(i)
    sumContinuous(unlist(sbmAnc[i]))
}

library(psych)
alpha(sbmAnc[c("totalPerceivedStress", "rMOTHER_DEAL", "rMOTHER_HANDLE")])
alpha(sbmAnc[c("zMOTHER_STRESSED", "zTotalPerceivedStress", "zTotalStressors")])

# Table 3 covariate screening
library(lme4)
library(nlme)

covariates = list()
for (i in c("MOTHER_GOPLAY", "MOTHER_TAKEPLAY", "rMOTHER_ASKTV", "rMOTHER_LIMITTV", "MOTHER_GOFRESH", "MOTHER_COOKFRESH", "rMOTHER_ASKJUNKFOOD", "totalPhysicalActivityParenting", "totalHealthyEatingParenting")) {
    for (j in c("Bi_BornUS", "Bi_CC_Fred", "CC_Grandparent", "CC_program", "Bi_singleparent", "Bi_married", "Bi_college", "Hispanic", "cHispanic", "incomeQ", "Bi_Fulltime", "Bi_Work", "Child_Gender", "mBMIcat", "Age", "Childage", "HouseholdSize", "Nchildren", "TOD", "WEEKEND")) {
        lme.X = lme(eval(substitute(y ~ x, list(y = as.name(i), x = as.name(j)))), random = ~1|ID, data = sbm[!is.na(sbm[i]) & !is.na(sbm[j]),])
        print(i)
        print(j)
        print(summary(lme.X))
        
        if (summary(lme.X)$tTable[2, 5]<=0.06) {
            covariates[[length(covariates)+1]] = c(i, j, summary(lme.X)$tTable[2, 5])
        }
    }
}

write.dta(sbmMain, "sbm_20160808_main.dta")
write.dta(sbmAnc, "sbm_20160808_ancillary.dta")


# # Table 4
# # checked for normality of outcomes, and no need for transformation
# for (i in c("totalPhysicalActivityParenting", "totalHealthyEatingParenting")) {
#     for (j in c("totalPerceivedStress", "totalStressors", "totalStress")) {
#         dataX = sbmMain[!is.na(sbmMain[i]) & !is.na(sbmMain[j]) & !is.na(sbmMain$Bi_college) & !is.na(sbmMain$Nchildren) & !is.na(sbmMain$TOD) & !is.na(sbmMain$Bi_CC_Fred) & !is.na(sbmMain$Hispanic) & !is.na(sbmMain$cHispanic) & !is.na(sbmMain$HouseholdSize) & !is.na(sbmMain$Child_Gender) & !is.na(sbmMain$Age) & !is.na(sbmMain$WEEKEND) & !is.na(sbmMain$Bi_Fulltime) & !is.na(sbmMain$Childage) & !is.na(sbmMain$Bi_BornUS),]
#         dataX = bsWs(j, dataX)
# 
#         lme.X = lme(eval(substitute(y ~ BS+WS+Bi_college+Nchildren+TOD+Bi_CC_Fred+Hispanic+cHispanic+HouseholdSize+Child_Gender+Age+WEEKEND+Bi_Fulltime+Childage+Bi_BornUS, list(y = as.name(i)))), random = ~1|ID, data = dataX)
#         print(i)
#         print(j)
#         print(summary(lme.X))
#     }
# }
# 
# # Table 5
# for (i in c("totalPhysicalActivityParenting", "totalHealthyEatingParenting")) {
#     for (j in c("rMOTHER_STRESS_HOMEWK", "rMOTHER_STRESS_JOB", "rMOTHER_STRESS_DEMANDS", "rMOTHER_STRESS_COWRKR", "rMOTHER_STRESS_SPOUSE")) {
#         dataX = sbmMain[!is.na(sbmMain[i]) & !is.na(sbmMain[j]) & !is.na(sbmMain$Bi_college) & !is.na(sbmMain$Nchildren) & !is.na(sbmMain$TOD) & !is.na(sbmMain$Bi_CC_Fred) & !is.na(sbmMain$Hispanic) & !is.na(sbmMain$cHispanic) & !is.na(sbmMain$HouseholdSize) & !is.na(sbmMain$Child_Gender) & !is.na(sbmMain$Age) & !is.na(sbmMain$WEEKEND) & !is.na(sbmMain$Bi_Fulltime) & !is.na(sbmMain$Childage) & !is.na(sbm$Bi_BornUS),]
#         dataX = bsWs(j, dataX)
# 
#         lme.X = lme(eval(substitute(y ~ BS+WS+Bi_college+Nchildren+TOD+Bi_CC_Fred+Hispanic+cHispanic+HouseholdSize+Child_Gender+Age+WEEKEND+Bi_Fulltime+Childage+Bi_BornUS, list(y = as.name(i)))), random = ~1|ID, data = dataX)
#         print(i)
#         print(j)
#         print(summary(lme.X))
#     }
# }

