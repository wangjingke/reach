ema.fetchPrompt = function (ema, emaList, emaDir, warning) {
    jobList=unique(ema[c("SubjectID", "Date")])
    emaList=read.csv(paste0(emaDir, "/", emaList), header = TRUE, stringsAsFactors = FALSE)

    # matching survey with prompt windows
    for (i in 1:nrow(jobList)) {
        idX=jobList$SubjectID[i]
        dateX=jobList$Date[i]
        EMAwarningX = c()

        candidates=emaList[intersect(grep(dateX, emaList$date), grep(idX, emaList$subjectID)),]
        if (any(candidates$correctFolder)) {
            survey=head(candidates$file[candidates$correctFolder==TRUE], n=1)
        } else {
            survey=head(candidates$file[candidates$num==1], n=1)
        }
        if (length(survey)>0) {
            answerX=ema.survey(paste0(emaDir, "/", survey))
            if (length(answerX)==1) {next} # break when the surveys are empty
            for (j in 1:nrow(answerX)) {
                lineInEMA=which(ema$SubjectID==idX & ema$Date==dateX & ema$window_start<=answerX$prompt_start[j] & ema$window_end>=answerX$prompt_start[j])
                answerX$prompt_start=as.character(answerX$prompt_start)
                answerX$prompt_end=as.character(answerX$prompt_end)
                ema[lineInEMA, names(answerX)]=answerX[j,]
            }
        }
        if (i%%100==0) {print(paste0(i, "; ", round(i/nrow(jobList)*100, 2), "%"))}
        # add direction to output warnings
        if (length(EMAwarningX)>0) {write(EMAwarningX, file = warning, append = TRUE)}
    }
    return(ema)
}

ema.compliance = function(ema) {
    compliance = setNames(as.data.frame.matrix(round(prop.table(table(ema$SubjectID, ema$COMPLY, useNA = "ifany"), 1)*100, 2)), c("Skipped", "Complete", "Missing"))
    compliance$SubjectID=row.names(compliance)
    compliance$dydID=substring(compliance$SubjectID, 3, 5)
    compliance=compliance[order(compliance$dydID, compliance$SubjectID),]

    missed = compliance[compliance$Missing>=30,]
    missed$SubjectID=row.names(missed)
    missed = missed[order(missed$SubjectID),]

    return(list(full = compliance, attention = missed))
}

ema.waketime = function (ema) {
    waketime=aggregate(TMRWAKETIME~SubjectID+DayInStudy, data=ema, unique)
    waketime=waketime[order(waketime$SubjectID, waketime$DayInStudy),]
    waketime$WAKETIME=NA
    for (i in 1:nrow(waketime)) {
        X=waketime$TMRWAKETIME[which(waketime$DayInStudy==waketime$DayInStudy[i]-1 & waketime$SubjectID==waketime$SubjectID[i])]
        waketime$WAKETIME[i]=ifelse(length(X)!=0, X, NA)
    }
    ema=merge(ema, waketime, by=intersect(names(ema), names(waketime)), all.x=TRUE, sort = FALSE)
    return(ema)
}

ema.triage=function(jobs, nCL) {    # Function to split a data frame down to job list for each node
    splits=rep(1:nCL, length.out=nrow(jobs))
    return(split(jobs, splits))
}
