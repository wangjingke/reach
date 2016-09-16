# add day after the enter date
ema.dayAfter=function(x, N) {
    day=as.Date(x, format = "%Y-%m-%d", tz = "America/Los_Angeles")
    return(as.character(day+N))
}
# convert manual input date
ema.dateConv=function(x) {
    if (is.na(x)) {
        return(NA)
    } else {
        if (grepl("/", x)) {
            seg=strsplit(x, "/")[[1]]
            year=ifelse(nchar(seg[3])<4, paste0("20", seg[3]), seg[3])
            mon=ifelse(nchar(seg[1])<2, paste0("0", seg[1]), seg[1])
            day=ifelse(nchar(seg[2])<2, paste0("0", seg[2]), seg[2])
        } else {
            seg=strsplit(x, "-")[[1]]
            year=ifelse(nchar(seg[1])<4, paste0("20", seg[1]), seg[1])
            mon=ifelse(nchar(seg[2])<2, paste0("0", seg[2]), seg[2])
            day=ifelse(nchar(seg[3])<2, paste0("0", seg[3]), seg[3])
        }
        return(paste(year, mon, day, sep="-"))
    }
}

# indiv prompt list
ema.indivPromptList = function (idX, enterDateX) {
    win=rbind(
        names=c("7-8am", "9-10am", "11am-12pm", "1-2pm", "3-4pm", "5-6pm", "7-8pm", "9-9:30pm"),
        winSeq=c(1:8),
        start=c("07:00:00", "09:00:00", "11:00:00", "13:00:00", "15:00:00", "17:00:00","19:00:00", "21:00:00"),
        end=c("08:00:00", "10:00:00", "12:00:00", "14:00:00", "16:00:00", "18:00:00", "20:00:00", "21:30:00")
    )
    # add time of the day
    tod = function(x) {
        if (x %in% c("7-8am", "9-10am", "11am-12pm")) {
            return(1)
        } else if (x %in% c("1-2pm", "3-4pm")) {
            return(2)
        } else {
            return(3)
        }
    }

    mother=ifelse(idX<12000, 1, 0) # determine whether the participant is a mother
    promptList=c()
    for (i in 0:7) {
        currentDate=ema.dayAfter(enterDateX, i)
        weekday=weekdays(strptime(currentDate, format = "%Y-%m-%d", tz = "America/Los_Angeles"), abbreviate = TRUE)
        weekend=ifelse(weekday %in% c("Sat", "Sun"), 1, 0)
        winX=switch(as.character(mother),
                    "0"={switch(as.character(weekend), "0"=win["names", 5:7], "1"=win["names", 1:7])},
                    "1"={switch(as.character(weekend), "0"=win["names", 5:8], "1"=win["names", 1:8])}
        )
        # for the first and last day, cutting data off by 7 pm pick up and 5 pm drop-off
        if (i==0) {winX=winX[winX %in% c("7-8pm", "9-9:30pm")]}
        if (i==7) {winX=winX[winX %in% c("7-8am", "9-10am", "11am-12pm", "1-2pm", "3-4pm")]}
        promptListX=data.frame(
            subjectID=idX,
            mother=mother,
            date=currentDate,
            dayInStudy=i+1,
            weekday=weekday,
            weekend=weekend,
            window=winX,
            windowStart=strptime(paste0(currentDate, " ", win["start",][sapply(winX, grep, win["names",])]), format = "%Y-%m-%d %H:%M:%S", tz = "America/Los_Angeles"),
            windowEnd=strptime(paste0(currentDate, " ", win["end",][sapply(winX, grep, win["names",])]), format = "%Y-%m-%d %H:%M:%S", tz = "America/Los_Angeles"),
            winSeq=win["winSeq",][sapply(winX, grep, win["names",])],
            comply=NA_integer_,
            complete=NA_integer_,
            promptStart=NA,
            promptEnd=NA,
            reprompt=NA_integer_,
            bedtime=NA_character_,
            tmrwaketime=NA_character_,
            file=NA_character_,
            tod=NA,
            stringsAsFactors = FALSE)
        promptListX[c(paste0("MOTHER_", keys.mother$variable), paste0("CHILD_", keys.child$variable))]=NA
        promptList=rbind(promptList, promptListX)
    }
    promptList$tod=sapply(promptList$window, tod)
    return(promptList)
}

# function to create prompt windows based on ID and date
ema.promptList=function(wave, start) {
    EMA=c()
    input=rbind(setNames(wave[,c("mother", start)], c("ID", "EnterDate")), setNames(wave[,c("child", start)], c("ID", "EnterDate")))
    for (j in 1:nrow(input)) {
        promptList = ema.indivPromptList(idX = input$ID[j], enterDateX = input$EnterDate[j])
        EMA=rbind(EMA, promptList)
    }
    return(EMA)
}
