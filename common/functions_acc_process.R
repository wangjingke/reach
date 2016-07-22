# convert time to numeric seconds
toSec = function(time) {
    if (!is.character(time)) stop("x must be a character string of the form H:M:S")
    if (length(time) <= 0) return(x)
    sepX = as.numeric(strsplit(time, ":")[[1]])
    return(ifelse(length(sepX) == 3, sepX[1] * 3600 + sepX[2] * 60 + sepX[3], ifelse(length(sepX) == 2, sepX[1] * 60 + sepX[2], sepX[1])))
}

# Freedson MVPA cut points
# freedson=data.frame(
#   type=c("sedentary", "light", "moderate", "vigorous", "very vigorous"),
#   adult=c(0, 100, 1952, 5725, 9499),
#   child=c(0, 150, 500, 4000, 7600),
# stringsAsFactors = FALSE)

# Troiano MVPA cut points
troiano = data.frame(
    type = c("sedentary", "light", "moderate", "vigorous"),
    age6 = c(0, 100, 1400, 3758),
    age7 = c(0, 100, 1515, 3947),
    age8 = c(0, 100, 1638, 4147),
    age9 = c(0, 100, 1770, 4360),
    age10 = c(0, 100, 1910, 4588),
    age11 = c(0, 100, 2059, 4832),
    age12 = c(0, 100, 2220, 5094),
    age13 = c(0, 100, 2393, 5375),
    age14 = c(0, 100, 2580, 5679),
    age15 = c(0, 100, 2781, 6007),
    age16 = c(0, 100, 3000, 6363),
    age17 = c(0, 100, 3239, 6751),
    adult = c(0, 100, 2020, 5999),
    stringsAsFactors = FALSE
)

# function of reading ACC files
acc.read = function(fileX, epoch = 60, ...) {
    # reading header
    header = read.csv(fileX, nrow = 10, header = FALSE, stringsAsFactors = FALSE)
    serial = strsplit(header[2, 1], " ")[[1]][3]
    start_time = strsplit(header[3, 1], " ")[[1]][3]
    start_date = strsplit(header[4, 1], " ")[[1]][3]
    epo = toSec(strsplit(header[5, 1], " ")[[1]][4])
    voltage = strsplit(header[9, 1], " ")[[1]][4]
    # reading observations
    col.num = max(count.fields(fileX, skip = 10, sep = ","))
    if (col.num>=4) {
        data = read.csv(fileX, skip = 10, header = FALSE, colClasses = c(rep("integer", 4), rep("NULL", col.num - 4)))
        colnames(data) = c("y", "x", "z", "steps")
    } else {
        data = read.csv(fileX, skip = 10, header = FALSE, colClasses = "integer")
        colnames(data)[1] = "y"
    }
    
    epo.int = epoch / epo
    min.total = nrow(data) %/% epo.int
    data = head(data, min.total * epo.int)
    
    records = data.frame(y = as.integer(tapply(data$y, (seq_along(data$y) - 1) %/% epo.int, sum, na.rm = TRUE)), stamp=NA, stringsAsFactors = FALSE)
    # records$VectorMagnitude=sqrt(records$x^2+records$y^2+records$z^2)
    # time stamp
    stamp = strptime(paste(start_date, start_time, sep = " "), format = "%m/%d/%Y %H:%M:%S", tz = "America/Los_Angeles")
    stamp_epoch = as.difftime(epoch, units = "secs") # convert epoch to time
    records$stamp = seq(from = stamp, to = stamp + (nrow(records) - 1) * stamp_epoch, by = stamp_epoch) # time seq
    # returning output
    return(list("serial" = serial, "start_time" = start_time, "start_date" = start_date, "epoch" = epoch, "voltage" = voltage, "data" = records))
}

# function of reading ACC files using the DataTable file
acc.read.DT = function(fileX, epoch = 60, ...) {
    # reading header
    header = read.csv(fileX, nrow = 10, header = FALSE, stringsAsFactors = FALSE)
    serial = strsplit(header[2, 1], " ")[[1]][3]
    start_time = strsplit(header[3, 1], " ")[[1]][3]
    start_date = strsplit(header[4, 1], " ")[[1]][3]
    epo = toSec(strsplit(header[5, 1], " ")[[1]][4])
    voltage = strsplit(header[9, 1], " ")[[1]][4]
    # reading observations
    data = read.csv(fileX, skip = 10, header = TRUE, colClasses = "character")["Axis1"]
    colnames(data) = "y"
    
    epo.int = epoch / epo
    min.total = nrow(data) %/% epo.int
    data = head(data, min.total * epo.int)
    
    records = data.frame(y = as.integer(tapply(as.numeric(data$y), (seq_along(data$y) - 1) %/% epo.int, sum, na.rm = TRUE)))
    # records$VectorMagnitude=sqrt(records$x^2+records$y^2+records$z^2)
    # time stamp
    stamp = strptime(paste(start_date, start_time, sep = " "), format = "%m/%d/%Y %H:%M:%S", tz = "America/Los_Angeles")
    stamp_epoch = as.difftime(epoch, units = "secs") # convert epoch to time
    records$stamp = seq(from = stamp, to = stamp + (nrow(records) - 1) * stamp_epoch, by = stamp_epoch) # time seq
    # returning output
    return(list("serial" = serial, "start_time" = start_time, "start_date" = start_date, "epoch" = epoch, "voltage" = voltage, "data" = records))
}

# function of aggregating ACC data
# accX is the file name, threshold is the number of consecutive hrs defining nonwear
# following needed to add: malfunction identification 1. repeating 32767, 2. constaly repeating 3. counts>16000

# the sum.acc.mp function collapses the accelerometer data under meterplus criteria of nonwear (consecutive 60 min 0 count), a valid day is defined as over 10 hr device wearing.
acc.sum.mp=function(accX, id, age, VldDay_cutoff=10, ...) {
    chunk=data.frame(values=rle(accX$data$y)$values, lengths=rle(accX$data$y)$lengths)
    chunk$nonvalid=ifelse(chunk$lengths>=3600/accX$epoch & chunk$values==0, 1, 0)
    accX$data$nonvalid=rep(chunk$nonvalid, chunk$lengths)
    accX$data$date=as.Date(accX$data$stamp, tz="America/Los_Angeles")
    # using troiano cut points for MVPA
    type=c("nonvalid", troiano$type)
    if (age<6) {age=6} # for children under 6, use age6 criteria for MVPA
    age_group=ifelse(age>=18, "adult", grep(age, names(troiano), value=TRUE))
    accX$data$MVPA=ifelse(accX$data$nonvalid==1, type[1], as.character(cut(accX$data$y, c(troiano[,age_group], Inf), labels=type[2:5], right=FALSE)))
    accX$data$min=accX$epoch/60
    
    # function to calculate nonwear in mins
    minlyview=function(x) {round(accX$epoch*sum(x)/60,3)}
    duration=function(x) {difftime(max(x)+60, min(x), units = "min")} # add 1 min for the difference
    details=aggregate(list(duration=accX$data$stamp), by=list(accX$data$date), duration)
    
    specs=reshape(aggregate(accX$data$min, by=list(accX$data$date, accX$data$MVPA), minlyview), idvar="Group.1", timevar="Group.2", direction="wide")
    details=merge(details, specs, by="Group.1", all.x=TRUE, sort=FALSE)
    colnames(details)=gsub("x.", "",names(details))
    missing=type[!type %in% names(details)]
    if (length(missing>0)) details[missing]=NA
    details[is.na(details)]=0
    details$mvpa=details$moderate+details$vigorous
    details$valid=details$duration-details$nonvalid
    
    details$VldHrs=details$valid/60
    details$VldDay=ifelse(details$VldHrs>=VldDay_cutoff, 1, 0)
    names(details)[1]="Date"
    abstract=data.frame(SN=as.character(), ID=as.character(), Date=as.character(), TotDays=as.numeric(), VldDays=as.numeric(), stringsAsFactors = FALSE)
    abstract[1,]=c(accX$serial, id, as.character(details$Date[1]), nrow(details), sum(details$VldDay))
    return(list("abstract"=abstract, "details"=details[c("Date", "VldDay", "VldHrs", "duration", "nonvalid", "valid", "sedentary", "light", "moderate", "vigorous", "mvpa")], "data"=accX$data))
}


# the acc.sum function collapse the accelerometer data under REACH criteria of nonwear (consecutive 60 min 0 count with max 2 min allowance of counts < 100 in 1 min)
acc.sum.reach=function(accX, id, age, VldDay_cutoff=10, ...) {
    vert=ifelse(accX$data$y>0 & accX$data$y<100*accX$epoch/60, 1, accX$data$y) # change all interrupts to 1
    chunk=data.frame(values=rle(vert)$values, lengths=rle(vert)$lengths) # cat consecutive 0 and interrupts
    chunk$filtered=ifelse(chunk$values==1 & chunk$lengths<=120/accX$epoch, 0, chunk$values) # change <2 min interrupts to 0
    rleX=rle(chunk$filtered) # extract smoothed movement
    chunk$indicator=rep(seq_along(rleX$lengths), rleX$lengths) # assign new class to each move patterns
    # compress consecutive zeros accounting for minor interrupt
    compress=data.frame(values=rleX$values, aggregate(chunk$lengths, by=list(c(chunk$indicator)), FUN=sum, na.rm=TRUE))
    compress$nonwear=ifelse(compress$values==0 & compress$x>=3600/accX$epoch, 1, 0) # defining nonwears
    accX$data$nonwear=rep(compress$nonwear, compress$x) # assign nonwears
    accX$data$date=as.Date(accX$data$stamp, tz="America/Los_Angeles")
    # using troiano cut points for MVPA
    type=c("nonwear", troiano$type)
    age_group=ifelse(age>=18, "adult", grep(age, names(troiano), value=TRUE))
    accX$data$MVPA=ifelse(accX$data$nonwear==1, type[1], as.character(cut(accX$data$y, c(troiano[,age_group], Inf), labels=type[2:5], right=FALSE)))
    accX$data$min=accX$epoch/60
    
    # function to calculate nonwear in hrs
    hrlyview=function(x) {round(accX$epoch*sum(x)/3600, 3)}
    duration=function(x) {max(x)-min(x)}
    details=aggregate(list(duration=accX$data$stamp), by=list(accX$data$date), duration)
    
    specs=reshape(aggregate(accX$data$min, by=list(accX$data$date, accX$data$MVPA), hrlyview), idvar="Group.1", timevar="Group.2", direction="wide")
    details=merge(details, specs, by="Group.1", all.x=TRUE, sort=FALSE)
    colnames(details)=gsub("x.", "",names(details))
    missing=type[!type %in% names(details)]
    if (length(missing>0)) details[missing]=NA
    
    details$VldHrs=details$duration-details$nonwear
    details$VldDay=ifelse(details$VldHrs>=VldDay_cutoff, 1, 0)
    names(details)[1]="Date"
    
    abstract=data.frame(SN=as.character(), ID=as.character(), Date=as.character(), TotDays=as.numeric(), VldDays=as.numeric(), stringsAsFactors = FALSE)
    abstract[1,]=c(accX$serial, id, as.character(details$Date[1]), nrow(details), sum(details$VldDay))
    return(list("abstract"=abstract, "details"=details[c("Date", "duration", "VldHrs", "VldDay", "nonwear", "light", "moderate", "sedentary", "vigorous")], "data"=accX$data))
}

# function to subset by time, and aggregate MVPA (template)
acc.mvpa = function(accX, start, end) {
    skeleton = data.frame(MVPA = c("light", "moderate", "nonwear", "sedentary", "vigorous"))
    accX.seq = accX$data[accX$data$stamp > start & accX$data$stamp <= end, ]
    mvpa = aggregate(min ~ MVPA, data = accX.seq, sum, na.rm = TRUE)
    result = merge(skeleton, mvpa, by = "MVPA", all.x = TRUE, sort = FALSE)
    result[is.na(result)] = 0
    return(result)
}
