ema.factor = function(ema) {
    # change the following variable to factors
    scaleTodoList = setNames(data.frame(rbind(
        c("MOTHER_TIMESWOKEUP", "scaleWU"),
        c("MOTHER_SLEEPQUALITY", "scaleWell"),
        c("MOTHER_HAPPY", "scale4"),
        c("MOTHER_ANGRY", "scale4"),
        c("MOTHER_STRESSED", "scale4"),
        c("MOTHER_CALM", "scale4"),
        c("MOTHER_SAD_DEPRESSED", "scale4"),
        c("MOTHER_DEAL", "scale4"),
        c("MOTHER_HANDLE", "scale4"),
        c("MOTHER_ASKTV", "scale5"),
        c("MOTHER_ASKJUNKFOOD", "scale5"),
        c("MOTHER_ASKFASTFOOD", "scale5"),
        c("MOTHER_ASKSODA", "scale5"),
        c("CHILD_TIMESWOKEUP", "scaleWU"),
        c("CHILD_SLEEPQUALITY", "scaleWell"),
        c("CHILD_HAPPY", "scale4"),
        c("CHILD_JOYFUL", "scale4"),
        c("CHILD_STRESSED", "scale4"),
        c("CHILD_MAD", "scale4"),
        c("CHILD_SAD", "scale4")
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
        scale=c("1. 0", "2. 1", "3. 2", "4. 3", "5. 4", "6. 5", "7. 6+"),
        level=c(1:7)
    )
    scaleWell=rbind(
        scale=c("1. Much worse than usual", "2. A little worse than usual", "3. About the same as usual", "4. A little better than usual", "5. Much better than usual"),
        level=c(1:5)
    )
    
    # change the variable in ema to factors
    for (i in 1:nrow(scaleTodoList)) {
        scaleX=get(scaleTodoList$scale[i])
        ema[,scaleTodoList$variable[i]]=factor(ema[,scaleTodoList$variable[i]]+1, levels=scaleX["level",], labels=scaleX["scale",]) # add 1 since scales starts with 0 in the phone
    }
    return(ema)
}





