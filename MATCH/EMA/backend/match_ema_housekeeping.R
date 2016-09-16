ema.fetchPrompt = function (ema, emaList, emaDir, warning) {
    jobList=unique(ema[c("subjectID", "date")])
    emaList=read.csv(paste0(emaDir, "/", emaList), header = TRUE, stringsAsFactors = FALSE)

    # matching survey with prompt windows
    for (i in 1:nrow(jobList)) {
        idX=jobList$subjectID[i]
        dateX=jobList$date[i]
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
                lineInEMA=which(ema$subjectID==idX & ema$date==dateX & ema$windowStart<=answerX$promptStart[j] & ema$windowEnd>=answerX$promptStart[j])
                answerX$promptStart=as.character(answerX$promptStart)
                answerX$promptEnd=as.character(answerX$promptEnd)
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
    compliance = setNames(as.data.frame.matrix(round(prop.table(table(ema$subjectID, ema$comply, useNA = "ifany"), 1)*100, 2)), c("skipped", "complete", "missing"))
    compliance$subjectID=row.names(compliance)
    compliance$dydID=substring(compliance$subjectID, 3, 5)
    compliance=compliance[order(compliance$dydID, compliance$subjectID),]

    missed = compliance[compliance$missing>=30,]
    missed$subjectID=row.names(missed)
    missed = missed[order(missed$subjectID),]

    return(list(full = compliance, attention = missed))
}

ema.waketime = function (ema) {
    waketime=aggregate(tmrwaketime~subjectID+dayInStudy, data=ema, unique)
    waketime=waketime[order(waketime$subjectID, waketime$dayInStudy),]
    waketime$waketime=NA
    for (i in 1:nrow(waketime)) {
        X=waketime$tmrwaketime[which(waketime$dayInStudy==waketime$dayInStudy[i]-1 & waketime$subjectID==waketime$subjectID[i])]
        waketime$waketime[i]=ifelse(length(X)!=0, X, NA)
    }
    ema=merge(ema, waketime, by=intersect(names(ema), names(waketime)), all.x=TRUE, sort = FALSE)
    return(ema)
}

ema.cleanup = function (ema, wave) {
    # generate wave
    ema$wave = wave
    # generate DID
    ema$did = substring(ema$subjectID, 3, 5)
    # move wave and did to the beginning
    posWave = grep("wave", names(ema))
    posDid = grep("did", names(ema))
    ema = ema[,c(posWave, posDid, 1:(posWave-1))]
    # rename all the mother child variables
    momVar = grep("MOTHER_", names(ema), value = TRUE)
    for (i in 1:length(momVar)) {colnames(ema)[colnames(ema) == momVar[i]] = gsub("MOTHER_", "m", momVar[i])}
    kidVar = grep("CHILD_", names(ema), value = TRUE)
    for (i in 1:length(kidVar)) {colnames(ema)[colnames(ema) == kidVar[i]] = gsub("CHILD_", "c", kidVar[i])}
    # rename subjectID to id
    colnames(ema)[colnames(ema) == "subjectID"] = "id"

    return(ema)
}


# ema.triage=function(jobs, nCL) {    # Function to split a data frame down to job list for each node
#     splits=rep(1:nCL, length.out=nrow(jobs))
#     return(split(jobs, splits))
# }
