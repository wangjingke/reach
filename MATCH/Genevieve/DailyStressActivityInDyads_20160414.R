setwd("Y:/Jing/Genevieve/20160418_DailyStressActivityInDyads")

load("C:/Users/wangjink/Documents/REACH/MATCH/Project MATCH/MATCH_EMA_V14.RData")
activity=emaW1[emaW1$COMPLY==1, c("ID", "WAVE", "DID", "MOTHER", "COMPLY", "MOTHER_WITHCHILD", "MOTHER_WITHCHILD_0C","MOTHER_STRESSED", "CHILD_STRESSED", "MOTHER_STRESSED_0C", "CHILD_STRESSED_0C", "MVPA_120_BEFORE", "SED_120_BEFORE","OMVPA_120_BEFORE", "OSED_120_BEFORE", "MVPA_120_AFTER", "OMVPA_120_AFTER", "SED_120_AFTER", "OSED_120_AFTER", "NONVALID_120_BEFORE", "NONVALID_120_AFTER", "ONONVALID_120_BEFORE", "ONONVALID_120_AFTER")]
# screen for staying togher
activity$together=ifelse(activity$MOTHER_WITHCHILD==1 | activity$MOTHER_WITHCHILD_0C==1, 1, 0)
activity=activity[activity$together==1 & !is.na(activity$together),]

# log transform MVPA
activity$logMVPA_120_BEFORE=log(activity$MVPA_120_BEFORE+1)
activity$logOMVPA_120_BEFORE=log(activity$OMVPA_120_BEFORE+1)
activity$logMVPA_120_AFTER=log(activity$MVPA_120_AFTER+1)
activity$logOMVPA_120_AFTER=log(activity$OMVPA_120_AFTER+1)

# activity[activity==Inf | activity==-Inf]=NA # after adding 1 to log transformation, this line is not needed

for (i in c("MOTHER_STRESSED", "CHILD_STRESSED", "MOTHER_STRESSED_0C", "CHILD_STRESSED_0C", "MVPA_120_BEFORE","logMVPA_120_BEFORE", "SED_120_BEFORE", "OMVPA_120_BEFORE","logOMVPA_120_BEFORE", "OSED_120_BEFORE", "MVPA_120_AFTER", "logMVPA_120_AFTER", "OMVPA_120_AFTER", "logOMVPA_120_AFTER", "SED_120_AFTER", "OSED_120_AFTER")) {
    for (j in 1:nrow(activity)) {
        activity[j, paste0(i, "_BS")]=ifelse(activity[j, "MOTHER"]==1, activity[j, i]-mean(activity[activity$MOTHER==1, i], na.rm=TRUE), activity[j, i]-mean(activity[activity$MOTHER==0, i], na.rm=TRUE))
        activity[j, paste0(i, "_WS")]=activity[j, i]-mean(activity[activity$ID==activity[j, "ID"], i], na.rm = TRUE)
    }
}

library(foreign)
write.dta(activity, "activity_20160426.dta")


# # seperating child and mother population
# mother=activity[activity$MOTHER==1,]
# child=activity[activity$MOTHER!=1,]

# library(lme4)
# child_stress=lmer(CHILD_STRESSED~MOTHER_STRESSED_0C+logOMVPA_120_BEFORE+logMVPA_120_BEFORE+OSED_120_BEFORE+SED_120_BEFORE+(1+MOTHER_STRESSED_0C+OMVPA_120_BEFORE+MVPA_120_BEFORE+OSED_120_BEFORE+SED_120_BEFORE|DID), data=child[!is.na(child$CHILD_STRESSED) & !is.na(child$MOTHER_STRESSED_0C) & !is.na(child$logOMVPA_120_BEFORE) & !is.na(child$logMVPA_120_BEFORE) & !is.na(child$OSED_120_BEFORE) & !is.na(child$SED_120_BEFORE),])
# summary(child_stress)








