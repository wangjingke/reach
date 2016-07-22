# this function package contains functions used for conversion/unification of date and time

# function to detect number of digits in the minutes and add 0 if it is 1
digit = function(timepoint) {
    inputtime = strsplit(timepoint, ":")[[1]]
    outputtime = paste0(paste(ifelse(lapply(inputtime, nchar) == 1, paste0("0", inputtime), inputtime), collapse = ":"), ":00")
    return(outputtime)
}

# calculating time windows by hrs (hrs to sec)
hrs = function(u) {return(u*3600)}

# convert time to numeric seconds
toSec = function(time) {
    if (!is.character(time)) stop("x must be a character string of the form H:M:S")
    if (length(time) <= 0) return(x)
    sepX = as.numeric(strsplit(time, ":")[[1]])
    return(ifelse(length(sepX) == 3, sepX[1] * 3600 + sepX[2] * 60 + sepX[3], ifelse(length(sepX) == 2, sepX[1] * 60 + sepX[2], sepX[1])))
}

# function to uniform time in log and saliva
timeSplit=function(x) {
    segments=strsplit(x, " ")[[1]]

    if (length(segments)>1) {
        timepoint=segments[2]
    } else {
        timepoint=segments[1]
    }

    if (grepl("AM|PM|am|pm", timepoint)) {
        if (grepl("AM|am", timepoint)) {
            hr=strsplit(timepoint, "AM|am|:")[[1]][1]
            if (nchar(hr)==1) {hr=paste("0", hr, sep="")}
            min=strsplit(timepoint, "AM|am|:")[[1]][2]
            return(paste(hr, min, "00", sep=":"))
        } else {
            hr=as.numeric(strsplit(timepoint, "PM|pm|:")[[1]][1])
            hr=ifelse(hr==12, hr, hr+12)
            min=strsplit(timepoint, "PM|pm|:")[[1]][2]
            return(paste(hr, min, "00", sep=":"))
        }
    } else {
        return(timepoint)
    }
}

# function to uniform date in saliva lab results
dateSplit=function(x) {
    segments=strsplit(x, " ")[[1]][1]

    if (grepl("\\.", segments)) {
        year=strsplit(segments, "\\.")[[1]][3]
        mon=strsplit(segments, "\\.")[[1]][1]
        day=strsplit(segments, "\\.")[[1]][2]
        return(paste(year, mon, day, sep="-"))
    } else {
        return(segments)
    }
}

# convert manual input date
DateConv=function(x) {
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

# add day after the enter date
DayAfter=function(x, N) {
    day=strptime(x, format = "%Y-%m-%d", tz = "America/Los_Angeles")
    return(as.character(day+N*24*60*60))
}
