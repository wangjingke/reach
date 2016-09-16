ema.factor = function(ema) {
    # change the following variable to factors
    scaleTodoList = setNames(data.frame(rbind(
        c("mTimesWokeup", "scaleWU"),
        c("mSleepQuality", "scaleWell"),
        c("mHappy", "scale4"),
        c("mAngry", "scale4"),
        c("mStressed", "scale4"),
        c("mCalm", "scale4"),
        c("mSadDepressed", "scale4"),
        c("mDeal", "scale4"),
        c("mHandle", "scale4"),
        c("mAskTV", "scale5"),
        c("mAskJunkfood", "scale5"),
        c("mAskFastfood", "scale5"),
        c("mAskSoda", "scale5"),
        c("cTimesWokeup", "scaleWU"),
        c("cSleepQuality", "scaleWell"),
        c("cHappy", "scale4"),
        c("cJoyful", "scale4"),
        c("cStressed", "scale4"),
        c("cMad", "scale4"),
        c("cSad", "scale4")
    ), stringsAsFactors = FALSE), c("variable", "scale"))

    # scales
    scale4=rbind(
        scale=c("1. Not at all", "2. A little", "3. Quite a bit", "4. Extremely"),
        level=c(1:4)
    )
    scale5=rbind(
        scale=c("1. Yes, and I allowed it", "2. Yes, and my spouse/partner allowed it", "3. Yes, but I/we dit NOT allow it", "4. No, but did so WITHOUT my permission", "5. No, has not asked"),
        level=c(1:5)
    )
    scaleWU=rbind(
        scale=c("1. 0", "2. 1", "3. 2", "4. 3", "5. 4", "6. 5-8", "7. 9+"),
        level=c(1:7)
    )
    scaleWell=rbind(
        scale=c("1. Much worse than usual", "2. A little worse than usual", "3. About the same as usual", "4. A little better than usual", "5. Much better than usual"),
        level=c(1:5)
    )

    # change the variable in ema to factors
    for (i in 1:nrow(scaleTodoList)) {
        scaleX=get(scaleTodoList$scale[i])
        varX = grep(scaleTodoList$variable[i], names(ema), value = TRUE)
        for (j in 1:length(varX)) {
            ema[,varX[j]]=factor(ema[,varX[j]]+1, levels=scaleX["level",], labels=scaleX["scale",]) # add 1 since scales starts with 0 in the phone
        }
    }
    ema$tod = factor(ema$tod, levels=c(1:3), labels=c("Morning", "Afternoon", "Evening"))
    return(ema)
}
