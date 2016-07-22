# rank the surveys based on timeline
library(plyr)
surveys=data.frame(file=list.files(path=work_dir, pattern="^.*Survey_"), stringsAsFactors = FALSE)
surveys$hr=sapply(strsplit(surveys$file, "_|\\."), "[[", 2)
surveys$min=sapply(strsplit(surveys$file, "_|\\."), "[[", 3)
surveys=surveys[order(surveys$hr, surveys$min),]
surveys$order=1:nrow(surveys)

# stack all surveys into one
survey=c()
for (i in 1:nrow(surveys)) {
  surveyX=read.csv(surveys$file[i], header=FALSE, quote="\"")
  survey=rbind.fill(survey, surveyX)
}
survey=survey[!survey$V3 %in% c("Deliberation", "BackPressed", "Backpressed", ""), 2:6]
# append files
do.call("rbind",lapply(BedWake$files, FUN=function(files){read.csv(files,header=TRUE, stringsAsFactors = FALSE)}))
# or
for (file in file_list){
  # if the merged dataset doesn't exist, create it
  if (!exists("dataset")){
    dataset <- read.table(file, header=TRUE, sep="\t")
  }
  # if the merged dataset does exist, append to it
  if (exists("dataset")){
    temp_dataset <-read.table(file, header=TRUE, sep="\t")
    dataset<-rbind(dataset, temp_dataset)
    rm(temp_dataset)
  }
}

# reading in dates of surveys from one participant
ema.date = function(directory) {
    days = data.frame(date = list.dirs(directory, full.names = FALSE, recursive = FALSE), stringsAsFactors = FALSE)
    days$year = sapply(strsplit(days$date, "-"), "[[", 1)
    days$mon = sapply(strsplit(days$date, "-"), "[[", 2)
    days$day = sapply(strsplit(days$date, "-"), "[[", 3)
    days = days[order(days$year, days$mon, days$day),]
    days$ordnung = 1:nrow(days)

    BedWake = data.frame(files = list.files(pattern = "^Bed_Wake_Times.csv$", path = directory, full.names = TRUE, recursive =TRUE), stringsAsFactors = FALSE)
    BedWake$date = sub("^.*survey/(.*?)/Bed_Wake.*", "\\1", BedWake$files)
    BedWake$BedTime = NA
    BedWake$WakeTime = NA

    for (i in 1:nrow(BedWake)) {
        BedWakeX = read.csv(BedWake$files[i], header = TRUE, stringsAsFactors = FALSE)
        BedWake$BedTime[i] = digit(tail(BedWakeX$Bedtime_Tonight, 1)) # using the last input
        BedWake$WakeTime[i] = digit(tail(BedWakeX$Waketime_Tomorrow, 1))
    }

    days = merge(days, BedWake, by = "date", all.x = TRUE, sort = FALSE)
    days$survey.date = paste(days$mon, days$day, days$year, sep = "/")
    # this is the input bedtime and waketime of that day, the actual bed/wake time should be determined chronogically
    days = days[order(days$ordnung), c("date", "year", "mon", "day", "ordnung", "BedTime", "WakeTime")]
    return(days)
}


# invoke system shell command to search for bed/wake files
ema.shell = function(directory) {
    days = data.frame(date = list.dirs(directory, full.names = FALSE, recursive = FALSE), stringsAsFactors = FALSE)
    days$year = sapply(strsplit(days$date, "-"), "[[", 1)
    days$mon = sapply(strsplit(days$date, "-"), "[[", 2)
    days$day = sapply(strsplit(days$date, "-"), "[[", 3)
    days = days[order(days$year, days$mon, days$day),]
    days$ordnung = 1:nrow(days)

    BedWakeList = shell(paste0('dir /s/b "', gsub("/","\\\\",directory), '.\\Bed_Wake_Times.csv"'), translate = FALSE, intern = TRUE)
    BedWake = data.frame(files = gsub("\\\\", "/", BedWakeList), stringsAsFactors =FALSE)
    BedWake$date = sub("^.*survey/(.*?)/Bed_Wake.*", "\\1", BedWake$files)
    BedWake$BedTime = NA
    BedWake$WakeTime = NA

    for (i in 1:nrow(BedWake)) {
        BedWakeX = read.csv(BedWake$files[i], header = TRUE, stringsAsFactors = FALSE)
        BedWake$BedTime[i] = digitC(tail(BedWakeX$Bedtime_Tonight, 1)) # using the last input
        BedWake$WakeTime[i] = digitC(tail(BedWakeX$Waketime_Tomorrow, 1))
    }

    days = merge(days, BedWake, by = "date", all.x = TRUE, sort = FALSE)
    days$survey.date = paste(days$mon, days$day, days$year, sep = "/")
    # this is the input bedtime and waketime of that day, the actual bed/wake time should be determined chronogically
    days = days[order(days$ordnung), c("date", "year", "mon", "day", "ordnung", "BedTime", "WakeTime")]
    return(days)
}

# need to make sure the questions align with the varaible names
survey.skeleton = function(row_num) {
    # responses table names
    survey.demographics.names = c("Subject_ID", "PromptID", "PromptType", "Status", "PromptDate", "PromptTime", "WAKETIME", "BEDTIME", "WEEKEND")
    skeleton.demographics = data.frame(matrix(character(0), nrow = row_num, ncol = length(survey.demographics.names), dimnames = list(c(), survey.demographics.names)), stringsAsFactors = FALSE)
    # skeleton data frame
    skeleton.answer = data.frame(matrix(character(0), nrow = row_num, ncol = length(survey.variable.names), dimnames = list(c(), survey.variable.names)), stringsAsFactors = FALSE)
    return(cbind(skeleton.demographics, skeleton.answer))
}

# reading in survey data
ema.survey = function(data_dir) {
    data_dir = paste0(data_dir, "/.match/survey")
    # reading in days of surveys and bed/wake time for each day
    days = ema.shell(data_dir)

    # empty survey results
    EMA_result = c()
    for (i in 1:nrow(days)) {
        target_dir = paste(data_dir, "/", days$date[i], sep = "")
        fileX = list.files(target_dir)

        # responses
        if (any(grepl("PromptResponses", fileX))) {
            responseX = read.csv(paste(target_dir, "/PromptResponses_Mother.csv", sep = ""), header = TRUE, stringsAsFactor = FALSE, encoding = "UTF-8")
            resultX = survey.skeleton(row_num = nrow(responseX))
            resultX[1:6] = responseX[1:6]

            for (j in 1:nrow(index)) {
                if (index$key[j] %in% c("format_2", "format_4", "format_5")) {
                    formatX = get(index$key[j])
                    ansX = rep("", nrow(responseX))
                    varX = index[j, "variable"]
                    quesX = index[j, "question"]

                    for (k in 1:ncol(formatX)) {
                        keyX = formatX["key", k]
                        posX = grep(keyX, responseX[, quesX], ignore.case = TRUE)
                        ansX[posX] = formatX["num", k]
                    }
                    resultX[varX] = ansX
                } else {
                    keyX = index[j, "key"]
                    varX = index[j, "variable"]
                    quesX = index[j, "question"]
                    posX = grep(keyX, responseX[, quesX], ignore.case = TRUE)
                    resultX[posX, varX] = 1
                }
            }

            # add in bed/wake time, the current date is days$date[i]
            resultX$BEDTIME = days$BedTime[i]
            resultX$WAKETIME = ifelse(i == 1, "NA", days$WakeTime[i - 1])

            # Prompts
            promptX = read.csv(paste0(target_dir, "/Prompts.csv"), header = TRUE, stringsAsFactors = FALSE, encoding = "UTF-8")[c("Subject_ID", "TimeStampStarted", "TimeStampCompleted", "PromptType", "NumReprompt")]
            colnames(promptX) = c("Subject_ID", "START_TIME", "STOP_TIME", "PromptID", "REPROMPTS")

            resultX = merge(promptX, resultX, by = c("Subject_ID", "PromptID"), all.y = TRUE, sort = FALSE)
            EMA_result = rbind(EMA_result, resultX)
        }
    }
    return(EMA_result)
}
