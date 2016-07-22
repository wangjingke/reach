library(plyr)
ema.aggregate = function(path, clean = FALSE) {
    # function to read files when non-empty, and return NA otherwise
    wockets.read=function(target, header = TRUE) {
        output=try(read.csv(target, header = header, stringsAsFactors = FALSE, skipNul = TRUE, encoding = "UTF-8"), silent = TRUE)
        if (inherits(output, "try-error")) return(NA) else return(output)
    }

    filelist=list.files(path, full.names = TRUE)

    pos_prompt=grep("Prompts.csv", filelist)
    prompts=wockets.read(filelist[pos_prompt])

    pos_bed=grep("Bed_Wake_Times.csv", filelist)
    bedwake=wockets.read(filelist[pos_bed])

    pos_res_mother=grep("PromptResponses_Mother.csv", filelist)
    responses_mother=wockets.read(filelist[pos_res_mother])

    pos_res_child=grep("PromptResponses_Child.csv", filelist)
    responses_child=wockets.read(filelist[pos_res_child])

    pos_sali=grep("SalivaResponses.csv", filelist)
    saliva=wockets.read(filelist[pos_sali])

    pos_sur=grep("^.*Survey_[0-9]{2}_[0-9]{2}\\.csv$", filelist)
    if (length(pos_sur)>0) {
        surveys=c()
        for (i in 1:length(pos_sur)) {
            surveyX=read.csv(filelist[pos_sur[i]], header = FALSE, quote = '\"', encoding = "UTF-8", stringsAsFactors = FALSE, skipNul = TRUE, col.names = sprintf("V%d", 1:10))
            surveyX=surveyX[grepl("[0-9]{13}", surveyX$V1), 2:6]
            surveyX$V6=ifelse(grepl("Selected", surveyX$V6), surveyX$V6, "")
            surveys=rbind.fill(surveys, surveyX)
        }
        surveys=surveys[!surveys$V3 %in% c("Deliberation", "BackPressed", "Backpressed", ""),]
        surveys$time=strptime(surveys$V2, format = "%Y-%m-%d %H:%M:%S", tz = "America/Los_Angeles")
        surveys=surveys[order(surveys$time),]
    }

    # extract battery info
    battery=wockets.read(target=paste0(gsub("survey", "logs", path), "/BluetoothSensorService.output.csv"), header = FALSE)

    if (clean) {
        id=grep("[0-9]{5}", unlist(strsplit(path, "[^0-9]+")), value = TRUE)
    } else {
        id=unique(grep("[0-9]{5}", unlist(strsplit(as.character(c(prompts[1], responses_mother[1], responses_child[1])), "[^0-9]+")), value = TRUE))
    }

    if (length(id)>0) {
        return(list(
            "id"=id,
            "prompts"=prompts,
            "bedwake"=bedwake,
            "saliva"=saliva,
            "responses_mother"=responses_mother,
            "responses_child"=responses_child,
            "surveys"=if (exists("surveys")) surveys else NA,
            "battery"=battery
        ))
    } else return(NA)
}


# function to organize and summarize the wockets survey data
ema.archive=function(path, folder=2) {
    filelist=data.frame(file=list.files(path, pattern="^.*\\.rds$", recursive = TRUE), date=NA, folder=NA, subjectID=NA, correctFolder=NA, num=NA, stringsAsFactors = FALSE)
    filelist$folder=sapply(strsplit(filelist$file, "_|/"), "[[", folder)
    filelist$date=sapply(strsplit(filelist$file, "_|/"), FUN = function(x) {head(tail(x, 3), 1)})
    filelist$subjectID=sapply(strsplit(filelist$file, "_|/"), FUN = function(x) {paste(grep("^[0-9]{5}$", unique(unlist(x)), value = TRUE), collapse = "_")})
    filelist$correctFolder=mapply(function(x, y) {grepl(x, substring(y, 1, 5))}, filelist$subjectID, filelist$folder)
    filelist$num=sapply(filelist$subjectID, FUN = function(x) {length(grep("_", strsplit(x, "")))+1})
    return(filelist)
}


# ema.manualArchive = function (path) {
#     filelist=data.frame(file=list.files(path, pattern="^.*\\.rds$", recursive = TRUE), date=NA, folder=NA, subjectID=NA, correctFolder=NA, num=NA, stringsAsFactors = FALSE)
#     filelist$folder=sapply(strsplit(filelist$file, "_|/"), "[[", 1)
#     filelist$date=sapply(strsplit(filelist$file, "_|/"), FUN = function(x) {head(tail(x, 3), 1)})
#     filelist$subjectID=sapply(strsplit(filelist$file, "_|/"), FUN = {paste(grep("^[0-9]{5}$", unlist(x), value = TRUE), collapse = "_")})
#     filelist$correctFolder=mapply(function(x, y) {grepl(x, substring(y, 1, 5))}, filelist$subjectID, filelist$folder)
#     filelist$num=sapply(filelist$subjectID, FUN = function(x) {length(grep("_", strsplit(x, "")))+1})
#     return(filelist)
# }
