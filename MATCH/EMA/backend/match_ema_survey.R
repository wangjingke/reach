# function to process ema survey
# add function to output warnings about start!=end
ema.survey=function(survey) {
    emaX=readRDS(survey)
    # break if the surveys are empty
    if (length(emaX$surveys)==1) {return(NA)}

    mother=ifelse(emaX$id<12000, 1, 0)
    keyX=if(mother) {keys.mother} else {keys.child}
    # read start/end surveys
    startSurvey=grep("Start Survey", emaX$surveys$V3)
    endSurvey=grep("End Survey", emaX$surveys$V3)

    if (length(startSurvey)==0) {print(paste0("Warning: ", survey, " startSurvey=0")); return(NA)} # check whether there is a start survey
    if (length(endSurvey)==0) {endSurvey=nrow(emaX$surveys)} # check whether there is an end survey

    # when two start/end survey were close (<30 min), treat them as one survey
    startSurvey=startSurvey[c(TRUE, as.numeric(diff(emaX$surveys$time[startSurvey]), units="secs")>1800)] # keep the first start when conflict
    endSurvey=endSurvey[rev(c(TRUE, abs(as.numeric(diff(rev(emaX$surveys$time[endSurvey])), units="secs"))>1800))] # keep the last end when conflict

    # check the length of start and end survey
    if (length(startSurvey)!=length(endSurvey)) {
        assign("EMAwarningX", paste0("Warning: ", survey, " startSurvey != endSurvey"), envir = .GlobalEnv)
        endSurvey=c(startSurvey[2:length(startSurvey)]-1, nrow(emaX$surveys))
    }

    # unify output variable name
    emaColNames=c("COMPLY", "COMPLETE","prompt_start", "prompt_end", "reprompt", "BEDTIME", "TMRWAKETIME", keyX$variable)
    answer=c() # initiate output
    for (i in 1:length(startSurvey)) {
        surveyX=emaX$surveys[startSurvey[i]:endSurvey[i],]
        # survey skeleton
        ansX=data.frame(matrix(NA, nrow=1, ncol=length(emaColNames), dimnames = list(c(), emaColNames)), stringsAsFactors = FALSE)
        ansX$prompt_start=surveyX$time[1]
        ansX$prompt_end=tail(surveyX$time, n=1)
        ansX$reprompt=length(grep("Reprompt", surveyX$V3))

        if(length(grep("QuesIndex", surveyX$V4))==0) {
            ansX$COMPLY=0
        } else {
            ansX$COMPLY=1

            for (j in grep("Question", surveyX$V3)) {
                qX=strsplit(surveyX$V5[j], "_[0-9]")[[1]] # account for question name ending with _num
                if (is.na(keyX[keyX$question==qX, "options"][1])) {
                    k=1
                    while (!identical(surveyX$V3[j+k], "Question") & j+k<=nrow(surveyX)) { # cannot use != here since j+k may point to NA
                        ansX[keyX[keyX$question==qX, "variable"]]=ifelse(grepl("Y", surveyX$V6[j+k]), as.numeric(strsplit(surveyX$V4[j+k], ": ")[[1]][2]), NA)
                        k=k+1
                    }
                } else if (keyX[keyX$question==qX, "options"][1]=="b") {
                    # for binary varibles
                    k=1
                    while (!identical(surveyX$V3[j+k], "Question") & j+k<=nrow(surveyX)) { # cannot use != here since j+k may point to NA
                        ansX[keyX[keyX$question==qX, "variable"]]=ifelse(grepl("Y", surveyX$V6[j+k]), as.numeric(strsplit(surveyX$V4[j+k], ": ")[[1]][2]), NA)
                        k=k+1
                    }
                    # by the app setting, ans[0] = "yes", ans[1] = "no"
                    yesNoSwtich = function (x) {
                        if (is.na(x)) {
                            return(NA)
                        } else if (x=="1") {
                            return("0")
                        } else if (x=="0") {
                            return("1")
                        } else {
                            return(NA)
                        }
                    }
                    ansX[keyX[keyX$question==qX, "variable"]] = yesNoSwtich(ansX[keyX[keyX$question==qX, "variable"]])
                } else {
                    ansX[keyX$variable[keyX$question==qX]]=0
                    k=1
                    while (!identical(surveyX$V3[j+k], "Question") & j+k<=endSurvey[i]) {
                        keyNum=which(keyX$question==qX & keyX$options==as.numeric(strsplit(surveyX$V4[j+k], ": ")[[1]][2]))
                        ansX[keyX$variable[keyNum]]=ifelse(grepl("Y", surveyX$V6[j+k]), 1, 0)
                        k=k+1
                    }
                }
            }
            ansX$COMPLETE=ifelse(any(grepl("SocialContext", surveyX$V5)), 1, 0) # survey completed when last social context question asked
        }
        answer=rbind(answer, ansX)
    }
    # fetch yesterday sleep and wakeup time
    if (mother==1 && length(emaX$responses_mother)>1 && length(emaX$responses_mother$Q0_b_sleepTime)>0) {
        answer$SLEEPTIME=emaX$responses_mother$Q0_b_sleepTime[1]
        answer$WAKEUPTIME=emaX$responses_mother$Q0_c_wakeUpTime[1]
    }
    if (mother==0 && length(emaX$responses_child)>1 && length(emaX$responses_child$Q0_b_sleepTime>0)) {
        answer$SLEEPTIME=emaX$responses_child$Q0_b_sleepTime[1]
        answer$WAKEUPTIME=emaX$responses_child$Q0_c_wakeUpTime[1]
    }
    # fetch sleep and wakeup time predicted for today
    # function to detect number of digits in the hr/min and add 0 if it is 1
    digit = function(timepoint) {
        inputtime = strsplit(timepoint, ":")[[1]]
        outputtime = paste0(paste(ifelse(lapply(inputtime, nchar) == 1, paste0("0", inputtime), inputtime), collapse = ":"), ":00")
        return(outputtime)
    }
    if (length(emaX$bedwake)>1) {
        answer$BEDTIME=digit(tail(emaX$bedwake$Bedtime_Tonight, 1))
        answer$TMRWAKETIME=digit(tail(emaX$bedwake$Waketime_Tomorrow, 1))
    }

    colnames(answer)=c(names(answer)[1:7], paste0(ifelse(mother, "MOTHER_", "CHILD_"), names(answer[8:ncol(answer)])))
    answer$file=survey
    return(answer)
}
