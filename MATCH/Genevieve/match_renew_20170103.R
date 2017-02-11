setwd("Y:/Jing/Genevieve/20170103_matchRenew")

dob = read.csv('Y:/Jing/Genevieve/20170103_matchRenew/basic_info.csv', header = TRUE, stringsAsFactors = FALSE)[c('did', 'dob_child', 'dob_mother')]
dob$mDOB = as.Date('1970-01-01 00:00:00') + dob$dob_mother/86400

raw = read.csv("D:/REACH/MATCH/Project MATCH/W1-W5_Anthro_017916_DOM_cleaned_KR.csv", header = TRUE, stringsAsFactors = FALSE)
# assign waves to the missing ones
raw$Wave = ifelse(is.na(raw$Wave), 2, raw$Wave)
# harmonize children age
raw$dom = strptime(raw$cDOM, format = '%m/%d/%Y', tz = 'America/Los_Angeles')
raw$dob = strptime(raw$cDOB, format = '%m/%d/%Y', tz = 'America/Los_Angeles')
raw$age_child = as.numeric(difftime(raw$dom, raw$dob, units = 'days')/365.25)
raw$age_child = ifelse(is.na(raw$age_child), raw$cAge, raw$age_child)
# calculate child waist/height ratio
raw$cWHR = raw$cWaist/raw$cHeight

# harmonize children gender
for (i in 1:nrow(raw)) {
    if (is.na(raw$cGender[i])) {
        raw$cGender[i] = unique(raw$cGender[raw$cID == raw$cID[i] & !is.na(raw$cGender)])
    }
}
# 0 = girl, 1 = boy
raw = merge(raw, dob[c('did', 'mDOB')], by.x = 'DID', by.y = 'did', all.x = TRUE, sort = FALSE)
raw$age_mom = as.numeric(difftime(raw$dom, strptime(raw$mDOB, '%Y-%m-%d', tz = 'America/Los_Angeles'), units = 'days'))/365.25
# raw$age_mom = ifelse(is.na(raw$age_mom), raw$mAge, raw$age_mom)
raw = raw[order(raw$DID, raw$Wave),]
write.csv(raw, 'match_renew_anthro_20170123.csv', row.names = FALSE, quote = FALSE)

##########
# BMI and WC 
renewPlot = function(data, id, var, byVar, xlab, ylab, title) {
    subject = unique(data[id])
    xMin = floor(min(data[byVar], na.rm = TRUE))
    xMax = ceiling(max(data[byVar], na.rm = TRUE))
    yMin = min(data[var], na.rm = TRUE)
    yMax = max(data[var], na.rm = TRUE)
    for (i in 1:nrow(subject)) {
        if (i==1){
            plot(data[data[id]==subject[i ,1], byVar], data[data[id]==subject[i, 1], var], col=i, type='l', xlab=xlab, ylab = ylab, ylim=c(yMin, yMax), xlim = c(xMin, xMax), main = title)
        } else {
            points(data[data[id]==subject[i, 1], byVar], data[data[id]==subject[i, 1], var], col=i, type='l')
        }
    }
}

# plots for mother
png('mWc.png', width = 800, height = 600, type = 'cairo')
renewPlot(raw, 'mID', 'mWaist', 'age_mom', 'Age', 'Waist Circumference', 'MATCH Mothers Waist Circumference vs. Age')
dev.off()
png('mBmi.png', width = 800, height = 600, type = 'cairo')
renewPlot(raw, 'mID', 'mBMI', 'age_mom', 'Age', 'BMI', 'MATCH Mothers BMI vs. Age')
dev.off()
# unadjusted plots for boys and girls
png('cGirlWHR.png', width = 800, height = 600, type = 'cairo')
renewPlot(raw[raw$cGender==0,], 'cID', 'cWHR', 'age_child', 'Age', 'Waist Height Ratio', 'MATCH Girls Waist Height Ratio vs. Age')
dev.off()
png('cGirlBmiUnadjusted.png', width = 800, height = 600, type = 'cairo')
renewPlot(raw[raw$cGender==0,], 'cID', 'cBMI', 'age_child', 'Age', 'BMI', 'MATCH Girls BMI vs. Age')
dev.off()
png('cBoyWHR.png', width = 800, height = 600, type = 'cairo')
renewPlot(raw[raw$cGender==1,], 'cID', 'cWHR', 'age_child', 'Age', 'Waist Height Ratio', 'MATCH Boys Waist height Ratio vs. Age')
dev.off()
png('cBoyBmiUnadjusted.png', width = 800, height = 600, type = 'cairo')
renewPlot(raw[raw$cGender==1,], 'cID', 'cBMI', 'age_child', 'Age', 'BMI', 'MATCH Boys BMI vs. Age')
dev.off()
# adjusted plots for boys and girls
png('cGirlBmiAdjusted.png', width = 800, height = 600, type = 'cairo')
renewPlot(raw[raw$cGender==0,], 'cID', 'cBMIpct', 'age_child', 'Age', 'BMI Percentile', 'MATCH Girls BMI Percentile vs. Age')
dev.off()
png('cBoyBmiAdjusted.png', width = 800, height = 600, type = 'cairo')
renewPlot(raw[raw$cGender==1,], 'cID', 'cBMIpct', 'age_child', 'Age', 'BMI Percentile', 'MATCH Boys BMI Percentile vs. Age')
dev.off()


# Mental 
library(memisc)
mental = data.frame(as.data.set(spss.system.file("D:/REACH/MATCH/Project MATCH/MATCH_PERSON_V16_11152016.sav", to.lower = FALSE)))
# self-estime W1_mRSES_Sum W1_c_RSES_SumChild
# perceived stress W1_mPSS W1_cSiC
# depression W1_mCESD  W1_cRCADS_MDD
# life satisfaction W1_mSWLS W1_cHappiness
# anxiety W1_mSTAI_Y2 W1_cRCADS_GAD

mMental = c('W1_mRSES_Sum', 'W1_mPSS', 'W1_mCESD', 'W1_mSWLS', 'W1_mSTAI_Y2')
cMental = c('W1_c_RSES_SumChild', 'W1_cSiC', 'W1_cRCADS_MDD', 'W1_cHappiness', 'W1_cRCADS_GAD')

mental = mental[c('W1_m_ID', 'W1_mID', 'W1_Child_ID', 'W1_DID', 'W1_mRSES_Sum', 'W1_c_RSES_SumChild', 'W1_mCESD', 'W1_cRCADS_MDD', 'W1_mPSS', 'W1_cSiC', 'W1_mSWLS', 'W1_cHappiness', 'W1_mSTAI_Y2', 'W1_cRCADS_GAD')]

renew = merge(raw, mental, by.x='DID', by.y='W1_DID', all.x = TRUE, sort = FALSE)
# limit to people completed wave 1-3 123, 1234, 1235
w3 = aggregate(Wave~DID, data = raw, paste0, collapse = '')
w3 = w3[w3$Wave %in% c('13', '123', '1234', '1235', '12345'),]
renew = renew[renew$DID %in% w3$DID,]

# function to report summary statitics
sumContinuous = function(x) {
    print(paste0("mean=", signif(mean(x, na.rm = TRUE), 4)))
    print(paste0("sd=", signif(sd(x, na.rm = TRUE), 4)))
}
sumCategory = function(x) {
    print(table(x, useNA = "ifany"))
    print(prop.table(table(x, useNA = "ifany"))*100)
}

demographics = mental
for (i in 1:nrow(demographics)){
    for (j in 1:5) {
        mBMI = renew[renew$DID == demographics$W1_DID[i] & renew$Wave == j, 'mBMI']
        demographics[i, paste0('mBMI_w', j)] = ifelse(length(mBMI)>0, mBMI, NA)
        rm(mBMI)
        
        mWC = renew[renew$DID == demographics$W1_DID[i] & renew$Wave == j, 'mWaist']
        demographics[i, paste0('mWC_w', j)] = ifelse(length(mWC)>0, mWC, NA)
        rm(mWC)
        
        cBMI = renew[renew$DID == demographics$W1_DID[i] & renew$Wave == j, 'cBMI']
        demographics[i, paste0('cBMI_w', j)] = ifelse(length(cBMI)>0, cBMI, NA)
        rm(cBMI)
        
        cBMIz = renew[renew$DID == demographics$W1_DID[i] & renew$Wave == j, 'cBMIz']
        demographics[i, paste0('cBMIz_w', j)] = ifelse(length(cBMIz)>0, cBMIz, NA)
        rm(cBMIz)
        
        cBMIpct = renew[renew$DID == demographics$W1_DID[i] & renew$Wave == j, 'cBMIpct']
        demographics[i, paste0('cBMIpct_w', j)] = ifelse(length(cBMIpct)>0, cBMIpct, NA)
        rm(cBMIpct)
        
        cWC = renew[renew$DID == demographics$W1_DID[i] & renew$Wave == j, 'cWaist']
        demographics[i, paste0('cWC_w', j)] = ifelse(length(cWC)>0, cWC, NA)
        rm(cWC)
        
        cWHR = renew[renew$DID == demographics$W1_DID[i] & renew$Wave == j, 'cWHR']
        demographics[i, paste0('cWHR_w', j)] = ifelse(length(cWHR)>0, cWHR, NA)
        rm(cWHR)
    }
}
names(demographics[5:ncol(demographics)])
for (i in names(demographics)[5:ncol(demographics)]){
    print(i)
    sumContinuous(unlist(demographics[i]))
}

library(corrplot)
names(demographics)[5:14] = c('mSelf_Esteem', 'cSelf_Esteem', 'mDepression', 'cDepression', 'mPerceived_Stress', 'cPerceived_Stress', 'mLife_Satisfaction', 'cHappiness', 'mAnxiety', 'cAnxiety')

M = cor(demographics[5:ncol(demographics)], use = 'pairwise.complete.obs')
png('CorrMatrixLarge.png', height = 2048, width = 2048, type = 'cairo', res = 300)
corrplot(M, method = 'ellipse', type = 'lower', tl.cex = 0.5)
dev.off()

N = cor(demographics[5:21], use = 'pairwise.complete.obs')
png('CorrMatrixSmall.png', height = 2048, width = 2048, type = 'cairo', res = 200)
corrplot(N, method = 'number', type = 'lower', tl.cex = 0.8)
dev.off()

# write to clipboard
write.table(N, 'clipboard', sep = '\t')

# # center BMI and WC on wave 3
# center = function(x, data, did, var) {
#     var3 = data[data$DID == did & data$Wave == 3, var]
#     return(x-var3)
# }
center = function(x, data, var) {
    return((x - mean(unlist(data[var]), na.rm = TRUE))/sd(unlist(data[var]), na.rm = TRUE))
}

renew = renew[renew$Wave <= 3,]
renew$ctWave = renew$Wave-3

library(nlme)
# mother predicting mother
for (i in mMental) {
    for (j in c('mBMI', 'mWaist')) {
        lme.X = lme(eval(substitute(y ~ ctWave*x, list(y = as.name(j), x = as.name(i)))), random = ~ctWave|DID, data = renew, subset = !is.na(renew[i]) & !is.na(renew[j]))
        print(i)
        print(j)
        print(summary(lme.X))
    }
}
# child predicting child
for (i in cMental) {
    for (j in c('cBMIpct', 'cWHR')) {
        if (!(i %in% c('W1_cSiC') & j %in% c('cWHR'))){
            lme.X = lme(eval(substitute(y ~ ctWave*x, list(y = as.name(j), x = as.name(i)))), random = ~ctWave|DID, data = renew, subset = !is.na(renew[i]) & !is.na(renew[j]))
            print(i)
            print(j)
            print(summary(lme.X))
        }
    }
}
# mother predicting child
for (i in mMental) {
    for (j in c('cBMIpct', 'cWHR')) {
        if (!(i %in% c('W1_mRSES_Sum', 'W1_mPSS') & j %in% c('cWHR'))) {
            lme.X = lme(eval(substitute(y ~ ctWave*x, list(y = as.name(j), x = as.name(i)))), random = ~ctWave|DID, data = renew, subset = !is.na(renew[i]) & !is.na(renew[j]))
            print(i)
            print(j)
            print(summary(lme.X))
        }
    }
}
# child predicting mother
for (i in cMental) {
    for (j in c('mBMI', 'mWaist')) {
        lme.X = lme(eval(substitute(y ~ ctWave*x, list(y = as.name(j), x = as.name(i)))), random = ~ctWave|DID, data = renew, subset = !is.na(renew[i]) & !is.na(renew[j]))
        print(i)
        print(j)
        print(summary(lme.X))
    }
}

# power analysis
# with-in M, SD
sumContinuous(renew$mBMI)
sumContinuous(renew$cBMIz)
# between M, SD
between = aggregate(mBMI~DID, data = renew, mean, na.rm = TRUE)
sumContinuous(between$mBMI)
between = aggregate(cBMIz~DID, data = renew, mean, na.rm = TRUE)
sumContinuous(between$cBMIz)
between = aggregate(W1_mPSS~DID, data = renew, mean, na.rm = TRUE)
sumContinuous(between$W1_mPSS)
between = aggregate(W1_cSiC~DID, data = renew, mean, na.rm = TRUE)
sumContinuous(between$W1_cSiC)
# ICC
library(psych)
library(irr)
ICC(demographics[c('mBMI_w1', 'mBMI_w2', 'mBMI_w3')], missing = FALSE)
icc(demographics[c('mBMI_w1', 'mBMI_w2', 'mBMI_w3')], model = 'oneway', unit = 'single')
ICC(demographics[c('cBMIz_w1', 'cBMIz_w2', 'cBMIz_w3')], missing = FALSE)
icc(demographics[c('cBMIz_w1', 'cBMIz_w2', 'cBMIz_w3')], model = 'oneway', unit = 'single')

library(memisc)
w2 = data.frame(as.data.set(spss.system.file("D:/REACH/MATCH/Project MATCH/MATCH_PERSON_V20_Long_Tentative_011716_KR.sav", to.lower = FALSE)))[c('W_mID', 'DID', 'Wave', 'mPSS', 'cSiC')]
w2$Wave = ifelse(is.na(w2$Wave), 2, w2$Wave)

demo = raw[c('DID', 'Wave', 'mBMI', 'cBMIz')]
sas = merge(w2, demo, by.x = c('DID', 'Wave'), by.y = c('DID', 'Wave'), all.x = TRUE, sort = TRUE)

wsbs = function(data, var) {
    indivMean = setNames(aggregate(eval(substitute(y~DID, list(y = as.name(var)))), data = data, mean, na.rm = TRUE), c('DID', 'indivMean'))
    grandMean = mean(indivMean$indivMean, na.rm = TRUE)
    indivMean[paste0(var, '_bs')] = indivMean$indivMean - grandMean
    
    wsbs = merge(data[c('DID', 'Wave', var)], indivMean, by.x = 'DID', by.y = 'DID', all.x = TRUE, sort = FALSE)
    wsbs[paste0(var, '_ws')] = wsbs[,var] - wsbs$indivMean
    
    data = merge(data, wsbs[c('DID', 'Wave', paste0(var, '_bs'), paste0(var, '_ws'))], by = c('DID', 'Wave'), all.x = TRUE, sort = FALSE)
    
    return (data)
}

sas = wsbs(sas, 'mPSS')
sas = wsbs(sas, 'cSiC')

standardize = function(x) {
    x = as.numeric(x)
    return ((x - mean(x, na.rm = TRUE))/sd(x, na.rm = TRUE))
}
sas$mBMI_st = standardize(sas$mBMI)
sas$cBMIz_st = standardize(sas$cBMIz)
sas$mPSS_bs_st = standardize(sas$mPSS_bs)
sas$mPSS_ws_st = standardize(sas$mPSS_ws)
sas$cSiC_bs_st = standardize(sas$cSiC_bs)
sas$cSiC_ws_st = standardize(sas$cSiC_ws)
sas[is.na(sas)]=''
write.csv(sas, 'match_renew_20170123_long.csv', row.names = FALSE, quote = FALSE)

# transform data
did = unique(sas$DID)
sasTname = c('DID', outer(names(sas)[4:ncol(sas)], c('_w1', '_w2'), paste0))
sasT = data.frame(matrix(ncol = length(sasTname), nrow = length(did), dimnames = list(c(), sasTname)))
sasT$DID = did
for (i in 1:nrow(sas)) {
    lineX = grep(sas$DID[i], sasT$DID)
    for (j in 4:length(names(sas))) {
        sasT[lineX, paste0(names(sas)[j], '_w', sas$Wave[i])] = sas[i, names(sas)[j]]
    }
}
write.csv(sasT, 'match_renew_20170123_wide.csv', row.names = FALSE, quote = FALSE)


# only bs from w1-w3 with w1 pss and sic
sas2 = renew[c('DID', 'Wave', 'mBMI', 'cBMIz', 'W1_mPSS', 'W1_cSiC')]
bs = function(data, var) {
    indivMean = setNames(aggregate(eval(substitute(y~DID, list(y = as.name(var)))), data = data, mean, na.rm = TRUE), c('DID', 'indivMean'))
    grandMean = mean(indivMean$indivMean, na.rm = TRUE)
    indivMean[paste0(var, '_bs')] = indivMean$indivMean - grandMean
    
    data = merge(data, indivMean[c('DID', paste0(var, '_bs'))], by = 'DID', all.x = TRUE, sort = FALSE)
    return (data)
}

sas2 = bs(sas2, 'W1_mPSS')
sas2 = bs(sas2, 'W1_cSiC')

sas2$mBMI_st = standardize(sas2$mBMI)
sas2$cBMIz_st = standardize(sas2$cBMIz)
sas2$W1_mPSS_bs_st = standardize(sas2$W1_mPSS_bs)
sas2$W1_cSiC_bs_st = standardize(sas2$W1_cSiC_bs)
sas2[is.na(sas2)] = ''
write.csv(sas2, './power_analysis/match_renew_w1_20170123_long.csv', row.names = FALSE, quote = FALSE)
# transform data
did = unique(sas2$DID)
sas2Tname = c('DID', outer(names(sas2)[3:ncol(sas2)], c('_w1', '_w2', '_w3'), paste0))
sas2T = data.frame(matrix(ncol = length(sas2Tname), nrow = length(did), dimnames = list(c(), sas2Tname)))
sas2T$DID = did
for (i in 1:nrow(sas2)) {
    lineX = grep(sas2$DID[i], sas2T$DID)
    for (j in 3:ncol(sas2)) {
        sas2T[lineX, paste0(names(sas2)[j], '_w', sas2$Wave[i])] = sas2[i, names(sas2)[j]]
    }
}
write.csv(sas2T, './power_analysis/match_renew_w1_20170123_wide.csv', row.names = FALSE, quote = FALSE)



