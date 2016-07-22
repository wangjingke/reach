library(foreign)
library(memisc)
w1=read.dta("Y:\\MATCH STUDY\\Main Study\\Data\\Surveys and Anthropometric Data\\Stata Clean Data\\MATCH_EMA_V12.dta", warn.missing.labels = FALSE)
descriptives=data.frame(as.data.set(spss.system.file("Y:\\MATCH STUDY\\Main Study\\Data\\Surveys and Anthropometric Data\\Wave 1 Data Survey\\SUMMARY VARIABLES_RELEASE\\MATCH_PERSON_V13.sav", to.lower = FALSE)))

setwd("Y:\\Jing\\Genevieve\\sbm_20160229")

sbm=w1[c("ID", "WAVE", "DID", "DAY", "WINDOW_CHILD", "WINDOW_MOTHER", "WINDOW", "DATE", "STRINGTIME","COMPLY","WEEKEND", "MOTHER", "CHILD", "MOTHER_ID", "CHILD_ID", "MOTHER_AGE", "CHILD_AGE", "GENDER", "MOTHER_BMI", "MOTHER_BMI_CATEGORY", "CHILD_BMI", "CHILD_ZBMI", "CHILD_BMI_CATEGORY", "MOTHER_EDUCATION", "MARITAL", "HOUSEHOLD_SIZE", "MOTHER_HISPANIC", "CHILD_HISPANIC", "MOTHER_DONE_TV", "CHILD_DONE_TV", "SED_120_BEFORE", "NONVALID_120_BEFORE", "MOTHER_SOCIAL_TV_ALONE", "MOTHER_SOCIAL_TV_CHILD", "MOTHER_SOCIAL_TV_SPOUSE", "MOTHER_SOCIAL_TV_OTHER", "CHILD_DONE_CHIPS", "CHILD_DONE_PASTRY", "CHILD_DONE_FASTFOOD", "CHILD_DONE_SODA", "MOTHER_DONE_CHIPS", "MOTHER_DONE_PASTRY", "MOTHER_DONE_FASTFOOD", "MOTHER_DONE_SODA", "TOD")]
sbm=merge(sbm, descriptives[c("Mother_ID", "incomeQ", "Bi_Fulltime")], by.x="MOTHER_ID", by.y="Mother_ID", all.x=TRUE, sort=FALSE)
sbm=merge(sbm, descriptives[c("Child_ID", "cPA1_Sports")], by.x="CHILD_ID", by.y="Child_ID", all.x=TRUE, sort=FALSE)
sbm$sed120=ifelse(sbm$NONVALID_120_BEFORE==0, sbm$SED_120_BEFORE, NA)

##########
# slide 11, availability and compliance
sub11=sbm[c("ID", "WAVE", "DID", "MOTHER", "CHILD", "COMPLY")]
indiv.comp=setNames(aggregate(sub11$COMPLY, by=list(sub11$ID), mean), c("ID", "compliance"))
indiv.comp$child=as.numeric(substring(indiv.comp$ID, 2, 2))-1
# people without ema
indiv.comp[indiv.comp$compliance==0,]

## exclude people without EMA
sbm=sbm[!sbm$ID %in% c("11068", "11124", "11157", "12048", "12124"),]
indiv.comp=indiv.comp[!indiv.comp$ID %in% c("11068", "11124", "11157", "12048", "12124"),]

# catenate marital status and team sports
sbm$married=ifelse(sbm$MARITAL==2, 1, 0)
sbm$team=ifelse(sbm$cPA1_Sports %in% c("4 days", "5 or more days"), 1, ifelse(sbm$cPA1_Sports %in% c("0 day", "1 day", "2 days", "3 days"), 0, NA))

# mean compliance
setNames(aggregate(indiv.comp$compliance, by=list(indiv.comp$child), mean), c("child", "mean"))
# median
setNames(aggregate(indiv.comp$compliance, by=list(indiv.comp$child), median), c("child", "median"))
# range
setNames(aggregate(indiv.comp$compliance, by=list(indiv.comp$child), range), c("child", "range"))

##########
# slide 12, demographics
demographics=unique(sbm[c("ID", "WAVE", "DID", "MOTHER", "CHILD", "MOTHER_AGE", "CHILD_AGE", "GENDER", "MOTHER_BMI", "MOTHER_BMI_CATEGORY", "CHILD_BMI", "CHILD_ZBMI", "CHILD_BMI_CATEGORY", "MOTHER_EDUCATION", "MARITAL", "HOUSEHOLD_SIZE", "MOTHER_HISPANIC", "CHILD_HISPANIC", "incomeQ", "Bi_Fulltime", "cPA1_Sports",  "married", "team")])

mother=demographics[demographics$MOTHER==1,]
child=demographics[demographics$MOTHER==0,]

# sample size
table(demographics$MOTHER)
# age
mean(mother$MOTHER_AGE, na.rm = TRUE)
sd(mother$MOTHER_AGE, na.rm = TRUE)
mean(child$CHILD_AGE, na.rm = TRUE)
sd(child$CHILD_AGE, na.rm = TRUE)
# gender
table(child$GENDER)
prop.table(table(child$GENDER))
# Hispanic
table(mother$MOTHER_HISPANIC, useNA="ifany")
prop.table(table(mother$MOTHER_HISPANIC, useNA="ifany"))
table(child$CHILD_HISPANIC, useNA="ifany")
prop.table(table(child$CHILD_HISPANIC, useNA="ifany"))
# SES
table(mother$incomeQ, useNA = "ifany")
prop.table(table(mother$incomeQ, useNA = "ifany"))
table(child$incomeQ, useNA = "ifany")
prop.table(table(child$incomeQ, useNA = "ifany"))
# Married
table(mother$MARITAL, useNA = "ifany")
prop.table(table(mother$MARITAL, useNA = "ifany"))
table(mother$married, useNA="ifany")
prop.table(table(mother$married, useNA="ifany"))
# fulltime work
table(mother$Bi_Fulltime, useNA = "ifany")
prop.table(table(mother$Bi_Fulltime, useNA = "ifany"))
# teamsports
table(child$cPA1_Sports, useNA = "ifany")
prop.table(table(child$cPA1_Sports, useNA = "ifany"))
table(child$team, useNA = "ifany")
prop.table(table(child$team, useNA = "ifany"))
# BMI
table(mother$MOTHER_BMI_CATEGORY, useNA = "ifany")
prop.table(table(mother$MOTHER_BMI_CATEGORY, useNA = "ifany"))
table(child$CHILD_BMI_CATEGORY, useNA="ifany")
prop.table(table(child$CHILD_BMI_CATEGORY, useNA="ifany"))

##########
# slide 13, sedentary behaviors
library(lme4)
library(nlme)
sbm.m=sbm[sbm$MOTHER==1,]
sbm.c=sbm[sbm$MOTHER==0,]
sbm.m$tv=ifelse((sbm.m$MOTHER_DONE_TV==1 & sbm.m$COMPLY==1), 1, ifelse((sbm.m$MOTHER_DONE_TV==0 & sbm.m$COMPLY==1), 0, NA))
sbm.c$tv=ifelse((sbm.c$CHILD_DONE_TV==1 & sbm.c$COMPLY==1), 1, ifelse((sbm.c$CHILD_DONE_TV==0 & sbm.c$COMPLY==1), 0, NA))

mother_tv=aggregate(tv~MOTHER_ID, data=sbm.m, function(x) {100*mean(x, na.rm = TRUE)})
mean(mother_tv$tv)
sd(mother_tv$tv)

mother_tv_week=aggregate(tv~MOTHER_ID+WEEKEND, data=sbm.m, function(x) {100*mean(x, na.rm = TRUE)})
aggregate(tv~WEEKEND, data=mother_tv_week, function(x) {c(mean(x), sd(x))})
mother_tv_morning=aggregate(tv~MOTHER_ID+TOD, data=sbm.m, function(x) {100*mean(x, na.rm = TRUE)})
aggregate(tv~TOD, data=mother_tv_morning, function(x) {c(mean(x), sd(x))})

lme.m.wk=lme(tv~WEEKEND, random=~1|MOTHER_ID, data=sbm.m[!is.na(sbm.m$tv),])
summary(lme.m.wk)
lme.m.tod=lme(tv~TOD, random=~1|MOTHER_ID, data=sbm.m[!is.na(sbm.m$tv),])
summary(lme.m.tod)

child_tv=aggregate(tv~CHILD_ID, data=sbm.c, function(x) {100*mean(x, na.rm=TRUE)})
mean(child_tv$tv)
sd(child_tv$tv)
child_tv_week=aggregate(tv~CHILD_ID+WEEKEND, data=sbm.c, function(x) {100*mean(x, na.rm = TRUE)})
aggregate(tv~WEEKEND, data=child_tv_week, function(x) {c(mean(x), sd(x))})
child_tv_morning=aggregate(tv~CHILD_ID+TOD, data=sbm.c, function(x) {100*mean(x, na.rm = TRUE)})
aggregate(tv~TOD, data=child_tv_morning, function(x) {c(mean(x), sd(x))})

lme.c.wk=lme(tv~WEEKEND, random=~1|CHILD_ID, data=sbm.c[!is.na(sbm.c$tv),])
summary(lme.c.wk)
lme.c.tod=lme(tv~TOD, random=~1|CHILD_ID, data=sbm.c[!is.na(sbm.c$tv),])
summary(lme.c.tod)

##########
# slide 14, by demographics
aggregate(tv~MOTHER_HISPANIC, aggregate(tv~MOTHER_HISPANIC+MOTHER_ID, data=sbm.m[!is.na(sbm.m$tv),], mean), function(x) {c(mean(x), sd(x))})
lme.m.hispanic=lme(tv~MOTHER_HISPANIC, random=~1|MOTHER_ID, data=sbm.m[!is.na(sbm.m$tv),])
summary(lme.m.hispanic)

aggregate(tv~incomeQ, aggregate(tv~incomeQ+MOTHER_ID, data=sbm.m[!is.na(sbm.m$tv),], mean), function(x) {c(mean(x), sd(x))})
lme.m.income=lme(tv~incomeQ, random=~1|MOTHER_ID, data=sbm.m[!is.na(sbm.m$tv) & !is.na(sbm.m$incomeQ),])
summary(lme.m.income)

sbm.m$MARITAL=as.factor(sbm.m$MARITAL)
aggregate(tv~MARITAL, aggregate(tv~MARITAL+MOTHER_ID, data=sbm.m[!is.na(sbm.m$tv),], mean), function(x) {c(mean(x), sd(x))})
lme.m.marital=lme(tv~MARITAL, random=~1|MOTHER_ID, data=sbm.m[!is.na(sbm.m$tv),])
summary(lme.m.marital)

aggregate(tv~married, aggregate(tv~married+MOTHER_ID, data=sbm.m[!is.na(sbm.m$tv),], mean), function(x) {c(mean(x), sd(x))})
lme.m.married=lme(tv~married, random=~1|MOTHER_ID, data=sbm.m[!is.na(sbm.m$tv),])
summary(lme.m.married)

aggregate(tv~Bi_Fulltime, aggregate(tv~Bi_Fulltime+MOTHER_ID, data=sbm.m, mean), function(x) {c(mean(x), sd(x))})
lme.m.work=lme(tv~Bi_Fulltime, random=~1|MOTHER_ID, data=sbm.m[!is.na(sbm.m$tv) & !is.na(sbm.m$Bi_Fulltime),])
summary(lme.m.work)

aggregate(tv~MOTHER_BMI_CATEGORY, aggregate(tv~MOTHER_BMI_CATEGORY+MOTHER_ID, data=sbm.m, mean), function(x) {c(mean(x), sd(x))})
lme.m.bmi=lme(tv~MOTHER_BMI_CATEGORY, random=~1|MOTHER_ID, data=sbm.m[!is.na(sbm.m$tv) & !is.na(sbm.m$MOTHER_BMI_CATEGORY),])
summary(lme.m.bmi)

aggregate(tv~GENDER, aggregate(tv~GENDER+CHILD_ID, data=sbm.c, mean), function(x) {c(mean(x), sd(x))})
lme.c.sex=lme(tv~GENDER, random=~1|CHILD_ID, data=sbm.c[!is.na(sbm.c$tv),])
summary(lme.c.sex)

aggregate(tv~CHILD_HISPANIC, aggregate(tv~CHILD_HISPANIC+CHILD_ID, data=sbm.c, mean), function(x) {c(mean(x), sd(x))})
lme.c.hispanic=lme(tv~CHILD_HISPANIC, random=~1|CHILD_ID, data=sbm.c[!is.na(sbm.c$tv),])
summary(lme.c.hispanic)

aggregate(tv~incomeQ, aggregate(tv~incomeQ+CHILD_ID, data=sbm.c, mean), function(x) {c(mean(x), sd(x))})
lme.c.income=lme(tv~incomeQ, random=~1|CHILD_ID, data=sbm.c[!is.na(sbm.c$tv) & !is.na(sbm.c$incomeQ),])
summary(lme.c.income)

aggregate(tv~cPA1_Sports, aggregate(tv~cPA1_Sports+CHILD_ID, data=sbm.c, mean), function(x) {c(mean(x), sd(x))})
lme.c.activity=lme(tv~cPA1_Sports, random=~1|CHILD_ID, data=sbm.c[!is.na(sbm.c$tv) & !is.na(sbm.c$cPA1_Sports),])
summary(lme.c.activity)

aggregate(tv~team, aggregate(tv~team+CHILD+ID, data=sbm.c, mean), function(x) {c(mean(x), sd(x))})
lme.c.team=lme(tv~team, random=~1|CHILD_ID, data=sbm.c[!is.na(sbm.c$tv) & !is.na(sbm.c$cPA1_Sports),])
summary(lme.c.team)

aggregate(tv~CHILD_BMI_CATEGORY, aggregate(tv~CHILD_BMI_CATEGORY+CHILD_ID, data=sbm.c, mean), function(x) {c(mean(x), sd(x))})
sbm.c$CHILD_BMI_CAT.re=relevel(sbm.c$CHILD_BMI_CATEGORY, ref=4) # use normal weight as reference
lme.c.bmi=lme(tv~CHILD_BMI_CAT.re, random=~1|CHILD_ID, data=sbm.c[!is.na(sbm.c$tv) & !is.na(sbm.c$CHILD_BMI_CAT.re),])
summary(lme.c.bmi)

##########
# slide 15, sed120
aggregate(SED_120_BEFORE~tv, data=sbm.m, function(x) {c(mean(x), sd(x))})
lme.m.sed=lme(SED_120_BEFORE~tv, random=~1|MOTHER_ID, data=sbm.m[!is.na(sbm.m$tv),])
summary(lme.m.sed)

aggregate(sed120~tv, data=sbm.m, function(x) {c(mean(x), sd(x))})
lme.m.120=lme(sed120~tv, random=~1|MOTHER_ID, data=sbm.m[!is.na(sbm.m$tv) & !is.na(sbm.m$sed120),])
summary(lme.m.120)

aggregate(SED_120_BEFORE~tv, data=sbm.c, function(x) {c(mean(x), sd(x))})
lme.c.sed=lme(SED_120_BEFORE~tv, random=~1|CHILD_ID, data=sbm.c[!is.na(sbm.c$tv),])
summary(lme.c.sed)

aggregate(sed120~tv, data=sbm.c, function(x) {c(mean(x), sd(x))})
lme.c.120=lme(sed120~tv, random=~1|CHILD_ID, data=sbm.c[!is.na(sbm.c$tv) & !is.na(sbm.c$sed120),])
summary(lme.c.120)

#########
# slide 16, pts of companions
# only use data whose MOTHER_SOCIAL are not missing, and tv==1
sub16=sbm.m[!is.na(sbm.m$MOTHER_SOCIAL_TV_ALONE) & sbm.m$tv==1,]
prop.table(table(sub16$MOTHER_SOCIAL_TV_ALONE, useNA = "ifany"))
prop.table(table(sub16$MOTHER_SOCIAL_TV_CHILD, useNA = "ifany"))
prop.table(table(sub16$MOTHER_SOCIAL_TV_SPOUSE, useNA = "ifany"))
prop.table(table(sub16$MOTHER_SOCIAL_TV_OTHER, useNA = "ifany"))

##########
# slide 17, junk food
# mother chips
aggregate(MOTHER_DONE_CHIPS~tv, aggregate(MOTHER_DONE_CHIPS~tv+MOTHER_ID, data=sbm.m[!is.na(sbm.m$tv),], mean), function(x) {c(mean(x), sd(x))})
lme.m.chips=lme(MOTHER_DONE_CHIPS~tv, random=~1|MOTHER_ID, data=sbm.m[!is.na(sbm.m$tv),])
summary(lme.m.chips)
# mother pastry
aggregate(MOTHER_DONE_PASTRY~tv, aggregate(MOTHER_DONE_PASTRY~tv+MOTHER_ID, data=sbm.m[!is.na(sbm.m$tv),], mean), function(x) {c(mean(x), sd(x))})
lme.m.pastry=lme(MOTHER_DONE_PASTRY~tv, random=~1|MOTHER_ID, data=sbm.m[!is.na(sbm.m$tv),])
summary(lme.m.pastry)
# mother fastfood
aggregate(MOTHER_DONE_FASTFOOD~tv, aggregate(MOTHER_DONE_FASTFOOD~tv+MOTHER_ID, data=sbm.m[!is.na(sbm.m$tv),], mean), function(x) {c(mean(x), sd(x))})
lme.m.fast=lme(MOTHER_DONE_FASTFOOD~tv, random=~1|MOTHER_ID, data=sbm.m[!is.na(sbm.m$tv),])
summary(lme.m.fast)
# mother soda 
aggregate(MOTHER_DONE_SODA~tv, aggregate(MOTHER_DONE_SODA~tv+MOTHER_ID, data=sbm.m[!is.na(sbm.m$tv),], mean), function(x) {c(mean(x), sd(x))})
lme.m.soda=lme(MOTHER_DONE_SODA~tv, random=~1|MOTHER_ID, data=sbm.m[!is.na(sbm.m$tv),])
summary(lme.m.soda)
# child chips
aggregate(CHILD_DONE_CHIPS~tv, aggregate(CHILD_DONE_CHIPS~tv+CHILD_ID, data=sbm.c[!is.na(sbm.c$tv),], mean), function(x) {c(mean(x), sd(x))})
lme.c.chips=lme(CHILD_DONE_CHIPS~tv, random=~1|CHILD_ID, data=sbm.c[!is.na(sbm.c$tv),])
summary(lme.c.chips)
# child pastry
aggregate(CHILD_DONE_PASTRY~tv, aggregate(CHILD_DONE_PASTRY~tv+CHILD_ID, data=sbm.c[!is.na(sbm.c$tv),], mean), function(x) {c(mean(x), sd(x))})
lme.c.pastry=lme(CHILD_DONE_PASTRY~tv, random=~1|CHILD_ID, data=sbm.c[!is.na(sbm.c$tv),])
summary(lme.c.pastry)
# child fastfood
aggregate(CHILD_DONE_FASTFOOD~tv, aggregate(CHILD_DONE_FASTFOOD~tv+CHILD_ID, data=sbm.c[!is.na(sbm.c$tv),], mean), function(x) {c(mean(x), sd(x))})
lme.c.fast=lme(CHILD_DONE_FASTFOOD~tv, random=~1|CHILD_ID, data=sbm.c[!is.na(sbm.c$tv),])
summary(lme.c.fast)
# child soda
aggregate(CHILD_DONE_SODA~tv, aggregate(CHILD_DONE_SODA~tv+CHILD_ID, data=sbm.c[!is.na(sbm.c$tv),], mean), function(x) {c(mean(x), sd(x))})
lme.c.soda=lme(CHILD_DONE_SODA~tv, random=~1|CHILD_ID, data=sbm.c[!is.na(sbm.c$tv),])
summary(lme.c.soda)
