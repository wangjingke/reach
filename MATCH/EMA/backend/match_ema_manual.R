source("C:/Users/wangjink/Documents/Bitbucket/reach/MATCH/EMA/backend/match_ema_aggregate.R")
ema.manualAggregate = function (manual, manualDir) {
    for (i in 1:length(manual)) {
        dirX = paste0(manualDir, "/", tail(strsplit(manual[i], "/")[[1]], 1))
        if (any(grepl("survey", list.files(manual[i])))) {
            path = paste0(manual[i], "/survey")
        } else {
            path = paste0(manual[i], "/.match/survey")
        }
        surveydate=grep("^20[0-9]{2}-[0-9]{2}-[0-9]{2}$", list.files(path), value = TRUE)
        if(length(surveydate)==0) {next}
        for (j in 1:length(surveydate)) {
            idX = substring(tail(strsplit(manual[i], "/")[[1]], 1), 1, 5)
            emaX=ema.aggregate(paste0(path, "/", surveydate[j]))
            if (!all(is.na(emaX))) {
                if (!file.exists(dirX)) {dir.create(dirX)}
                emaXname=paste0(dirX, "/",paste("MATCH", idX, paste(emaX$id, collapse = "_"), surveydate[j], paste0("B", length(emaX$battery)),"manual.rds", sep="_"))
                saveRDS(emaX, file = emaXname)
            }
        }
    }
}

ema.manualFetch = function (ema, manualDir) {
    manualEmaList = list.files(path = manualDir, pattern = "^.*\\.rds$", full.names = TRUE, recursive = TRUE)
    manualJobList=unique(ema[ema$SubjectID %in% compliance$attention$SubjectID, c("SubjectID", "Date")])
    for (i in 1:nrow(manualJobList)) {
        idX=manualJobList$SubjectID[i]
        dateX=manualJobList$Date[i]

        candidates=manualEmaList[intersect(grep(dateX, manualEmaList), grep(idX, manualEmaList))]
        survey = candidates[1]
        if (length(survey)>0 & !is.na(survey)) {
            answerX=ema.survey(survey)
            if (length(answerX)==1) {next} # break when the surveys are empty
            for (j in 1:nrow(answerX)) {
                lineInEMA=which(ema$SubjectID==idX & ema$Date==dateX & ema$window_start<=answerX$prompt_start[j] & ema$window_end>=answerX$prompt_start[j])
                answerX$prompt_start=as.character(answerX$prompt_start)
                answerX$prompt_end=as.character(answerX$prompt_end)
                ema[lineInEMA, names(answerX)]=answerX[j,]
            }
        }
        print(paste0(i, "; ", round(i/nrow(manualJobList)*100, 2), "%"))
    }
    return(ema)
}
