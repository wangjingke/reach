setwd("Y:/Jing/Genevieve/20170203_matchRenew")


raw = read.csv("D:/REACH/MATCH/Project MATCH/W1-W5_Anthro_010916_cleaned_KR.csv", header = TRUE, stringsAsFactors = FALSE)
raw = raw[order(raw$Wave),]
# assign waves to the missing ones
raw$Wave = ifelse(is.na(raw$Wave), 2, raw$Wave)
# harmonize children age
raw$dom = strptime(raw$cDOM, format = '%m/%d/%Y', tz = 'America/Los_Angeles')
raw$dob = strptime(raw$cDOB, format = '%m/%d/%Y', tz = 'America/Los_Angeles')
raw$age_child = as.numeric(difftime(raw$dom, raw$dob, units = 'days')/365.25)
raw$age_child = ifelse(is.na(raw$age_child), raw$cAge, raw$age_child)

# harmonize children gender
for (i in 1:nrow(raw)) {
    if (is.na(raw$cGender[i])) {
        raw$cGender[i] = unique(raw$cGender[raw$cID == raw$cID[i] & !is.na(raw$cGender)])
    }
}
# 0 = girl, 1 = boy

##########
# BMI and WC 
ageNormal = function(x, age, gender) {
    if (is.na(x) | is.na(age) | is.na(gender)) {
        return (NA)
    } else {
        age = paste0('age', age)
        if (gender == 1) {
            switch(age,
                   'age7' = {mean = 60.6; sd = 0.66*sqrt(226)},
                   'age8' = {mean = 62.8; sd = 0.61*sqrt(219)},
                   'age9' = {mean = 64.4; sd = 0.80*sqrt(213)},
                   'age10' = {mean = 69.1; sd = 1.25*sqrt(199)},
                   'age11' = {mean = 74.9; sd = 1.4*sqrt(182)},
                   'age12' = {mean = 73.9; sd = 1.37*sqrt(172)},
                   'age13' = {mean = 79.5; sd = 1.56*sqrt(169)},
                   'age14' = {mean = 79.8; sd = 1.26*sqrt(176)},
                   'age15' = {mean = 82.3; sd = 1.53*sqrt(152)},
                   'age16' = {mean = 84.8; sd = 1.09*sqrt(174)}
            )
        } else {
            switch(age,
                   'age7' = {mean = 59.1; sd = 0.78*sqrt(204)},
                   'age8' = {mean = 63.7; sd = 0.85*sqrt(190)},
                   'age9' = {mean = 68.0; sd = 1.05*sqrt(198)},
                   'age10' = {mean = 69.5; sd = 1.29*sqrt(182)},
                   'age11' = {mean = 73.5; sd = 1.16*sqrt(223)},
                   'age12' = {mean = 78.0; sd = 1.58*sqrt(163)},
                   'age13' = {mean = 77.1; sd = 1.32*sqrt(155)},
                   'age14' = {mean = 79.6; sd = 1.30*sqrt(163)},
                   'age15' = {mean = 82.2; sd = 1.79*sqrt(146)},
                   'age16' = {mean = 83.5; sd = 2.28*sqrt(180)}
            ) 
        }
        return(round(pnorm((x-mean)/sd)*100, 1))
    }
}

ageStand = function(x, age, gender) {
    if (gender == 'boy') {
        
    }
}

renewPlot = function(data, id, var, byVar, ylab, title) {
    subject = unique(data[id])
    xMin = floor(min(data[byVar], na.rm = TRUE))
    xMax = ceiling(max(data[byVar], na.rm = TRUE))
    yMin = floor(min(data[var], na.rm = TRUE)) - 5
    yMax = ceiling(max(data[var], na.rm = TRUE)) + 5
    for (i in 1:nrow(subject)) {
        if (i==1){
            plot(data[data[id]==subject[i ,1], byVar], data[data[id]==subject[i, 1], var], col=i, type='l', xlab='Wave', ylab = ylab, ylim=c(yMin, yMax), xlim = c(xMin, xMax), main = title)
        } else {
            points(data[data[id]==subject[i, 1], byVar], data[data[id]==subject[i, 1], var], col=i, type='l')
        }
    }
}

for (i in 1:nrow(raw)) {
    raw$cWaist_st[i] = ageNormal(raw$cWaist[i], floor(raw$age_child[i]), raw$cGender[i])
}

write.csv(raw, 'match_renew_anthro_20170103.csv', row.names = FALSE, quote = FALSE)

# plots for mother
png('mWc.png', width = 800, height = 600, type = 'cairo')
renewPlot(raw, 'mID', 'mWaist', 'Wave', 'Waist Circumference', 'MATCH Mothers Waist Circumference vs. Wave')
dev.off()
png('mBmi.png', width = 800, height = 600, type = 'cairo')
renewPlot(raw, 'mID', 'mBMI', 'Wave', 'BMI', 'MATCH Mothers BMI vs. Wave')
dev.off()
# unadjusted plots for boys and girls
png('cGirlWcUnadjusted.png', width = 800, height = 600, type = 'cairo')
renewPlot(raw[raw$cGender==0,], 'cID', 'cWaist', 'age_child', 'Waist Circumference', 'MATCH Girls Unadjusted Waist Circumference vs. Age')
dev.off()
png('cGirlBmiUnadjusted.png', width = 800, height = 600, type = 'cairo')
renewPlot(raw[raw$cGender==0,], 'cID', 'cBMI', 'age_child', 'BMI', 'MATCH Girls Unadjusted BMI vs. Age')
dev.off()
png('cBoyWcUnadjusted.png', width = 800, height = 600, type = 'cairo')
renewPlot(raw[raw$cGender==1,], 'cID', 'cWaist', 'age_child', 'Waist Circumference', 'MATCH Boys Unadjusted Waist Circumference vs. Age')
dev.off()
png('cBoyBmiUnadjusted.png', width = 800, height = 600, type = 'cairo')
renewPlot(raw[raw$cGender==1,], 'cID', 'cBMI', 'age_child', 'BMI', 'MATCH Boys Unadjusted BMI vs. Age')
dev.off()
# adjusted plots for boys and girls
png('cGirlWcAdjusted.png', width = 800, height = 600, type = 'cairo')
renewPlot(raw[raw$cGender==0,], 'cID', 'cWaist_st', 'age_child', 'Waist Circumference', 'MATCH Girls Adjusted Waist Circumference vs. Age')
dev.off()
png('cGirlBmiAdjusted.png', width = 800, height = 600, type = 'cairo')
renewPlot(raw[raw$cGender==0,], 'cID', 'cBMIpct', 'age_child', 'BMI Percentile', 'MATCH Girls Adjusted BMI vs. Age')
dev.off()
png('cBoyWcAdjusted.png', width = 800, height = 600, type = 'cairo')
renewPlot(raw[raw$cGender==1,], 'cID', 'cWaist_st', 'age_child', 'Waist Circumference', 'MATCH Boys Adjusted Waist Circumference vs. Age')
dev.off()
png('cBoyBmiAdjusted.png', width = 800, height = 600, type = 'cairo')
renewPlot(raw[raw$cGender==1,], 'cID', 'cBMIpct', 'age_child', 'BMI Percentile', 'MATCH Boys Adjusted BMI vs. Age')
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
renew = renew[renew$DID %in% w3$DID & renew$Wave<=3,]

# function to report summary statitics
sumContinuous = function(x) {
    print(paste0("mean=", mean(x, na.rm = TRUE)))
    print(paste0("sd=", sd(x, na.rm = TRUE)))
}
sumCategory = function(x) {
    print(table(x, useNA = "ifany"))
    print(prop.table(table(x, useNA = "ifany"))*100)
}

demographics = mental
for (i in 1:nrow(demographics)){
    for (j in 1:5) {
        mBMI = renew[renew$DID == demographics$W1_DID[i] & renew$Wave == j, 'mBMI']
        demographics[i, paste0('mBMIw', j)] = ifelse(length(mBMI)>0, mBMI, NA)
        rm(mBMI)
        
        mWC = renew[renew$DID == demographics$W1_DID[i] & renew$Wave == j, 'mWaist']
        demographics[i, paste0('mWC', j)] = ifelse(length(mWC)>0, mWC, NA)
        rm(mWC)
        
        cBMI = renew[renew$DID == demographics$W1_DID[i] & renew$Wave == j, 'cBMI']
        demographics[i, paste0('cBMI', j)] = ifelse(length(cBMI)>0, cBMI, NA)
        rm(cBMI)
        
        cBMIpct = renew[renew$DID == demographics$W1_DID[i] & renew$Wave == j, 'cBMIpct']
        demographics[i, paste0('cBMIpct', j)] = ifelse(length(cBMIpct)>0, cBMIpct, NA)
        rm(cBMIpct)
        
        cWC = renew[renew$DID == demographics$W1_DID[i] & renew$Wave == j, 'cWaist']
        demographics[i, paste0('cWC', j)] = ifelse(length(cWC)>0, cWC, NA)
        rm(cWC)
        
    }
}


# # center BMI and WC on wave 3
# center = function(x, data, did, var) {
#     var3 = data[data$DID == did & data$Wave == 3, var]
#     return(x-var3)
# }
center = function(x, data, var) {
    return((x - mean(unlist(data[var]), na.rm = TRUE))/sd(unlist(data[var]), na.rm = TRUE))
}
# 
# for (i in 1:nrow(renew)) {
#     for (j in c('cBMIpct', 'cWaist', 'mBMI', 'mWaist')) {
#         renew[i, paste0(j, '_center')] = unique(center(renew[i, j], renew, renew[i, 'DID'], j))
#     }
# }
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
    for (j in c('cBMIpct', 'cWaist')) {
        lme.X = lme(eval(substitute(y ~ ctWave*x, list(y = as.name(j), x = as.name(i)))), random = ~ctWave|DID, data = renew, subset = !is.na(renew[i]) & !is.na(renew[j]))
        print(i)
        print(j)
        print(summary(lme.X))
    }
}
# mother predicting child
for (i in mMental) {
    for (j in c('cBMIpct', 'cWaist')) {
        lme.X = lme(eval(substitute(y ~ ctWave*x, list(y = as.name(j), x = as.name(i)))), random = ~ctWave|DID, data = renew, subset = !is.na(renew[i]) & !is.na(renew[j]))
        print(i)
        print(j)
        print(summary(lme.X))
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




# library(foreign)
# write.dta(renew, 'match_renew_20170102.dta')
# 
# standardize = function(x) {
#     return((x-mean(x, na.rm = TRUE))/sd(x, na.rm = TRUE))
# }
# test$mPSS = standardize(test$W1_mPSS)
