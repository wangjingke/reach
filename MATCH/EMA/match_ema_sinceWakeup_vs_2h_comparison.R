setwd("D:/Temp")

ema = readRDS("D:/REACH/MATCH/Dataset/MATCH_EMA/Combined/V21/MATCH_EMA_V21.rds")
# subsetting
ema = ema[, !grepl("_0c|_0p1|_1p1|_0p2|_1p2|_0m1|_1m1|_0m2|_1m2", names(ema))]

weekday = ema[ema$weekend==0,]
weekday$first = ifelse(weekday$winSeq == 5, "first", "other")

mvarlist = c("mStress", "mStress_homewk", "mStress_job", "mStress_demands", "mStress_cowrkr", "mStress_spouse", "mStress_child", "mStress_none", "mStress_else", "mDone_tv", "mDone_sports", "mDone_chips", "mDone_pastry", "mDone_fastfood", "mDone_fruit", "mDone_soda", "mDone_none", "mWithChild", "mAskTV", "mLimitTV", "mGoPlay", "mTakePlay", "mAskJunkfood", "mLimitJunkfood", "mAskFastfood", "mControlFastfood", "mGoFresh", "mCookFresh", "mAskSoda", "mLimitSoda", "mRules_familyMeal", "mRules_tv", "mRules_carMeal", "mRules_tvReward", "mRules_foodReward", "mRules_none", "mTimeuse_errands", "mTimeuse_lesson", "mTimeuse_chores", "mTimeuse_work", "mTimeuse_childcare", "mTimeuse_none")

cvarlist = c("cStress", "cStress_hw", "cStress_notWell", "cStress_tease", "cStress_someone", "cStress_parent", "cStress_todo", "cStress_none", "cDone_tv", "cDone_sports", "cDone_chips", "cDone_pastry", "cDone_fastfood", "cDone_fruit", "cDone_soda", "cDone_none")


freqTable = function(x) {
    round(prop.table(table(x, weekday$winSeq, useNA = "ifany"), 1)*100, 2)
}
lapply(weekday[mvarlist], freqTable)
lapply(weekday[cvarlist], freqTable)
