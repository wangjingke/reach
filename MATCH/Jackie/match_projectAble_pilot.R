setwd("Y:/Jing/Jackie")

fillPa = function(ema, activ) {
    attachPa = function(ema, pa) {
        pa_point = c(outer(outer(pa, c("_15", "_30", "_60", "_120"), paste0), c("_before", "_after"), paste0))
        pa_win = paste0(c(outer(pa, c("_30", "_60", "_120", "_240"), paste0)), "_window")
        return(cbind(
            ema, 
            data.frame(matrix(NA, nrow=nrow(ema), ncol=length(pa_point)+length(pa_win), dimnames = list(c(), c(pa_point, pa_win))))
        ))
    }
    
    extractActiv = function(activ, id, start, end) {
        activX = activ[activ$subjectID == id & activ$timestamp >= start & activ$timestamp <= end,]
        sedentary = sum(activX$Sedentary.Time..s., na.rm = TRUE)
        upright = sum(activX$Upright.Time..s., na.rm = TRUE)
        stepping = sum(activX$Stepping.Time..s., na.rm = TRUE)
        stepCount = sum(activX$StepCount, na.rm = TRUE)
        standToSit = sum(activX$Sedentary.to.Upright.Movements, na.rm = TRUE)
        sitToStand = sum(activX$Upright.to.Sedentary.Movements, na.rm = TRUE)
        return(list("sedentary" = sedentary, "upright" = upright, "stepping" = stepping, "stepCount" = stepCount, "standToSit" = standToSit, "sitToStand" = sitToStand))
    }
    
    # pa variables
    pa = c("sedentary", "upright", "stepping", "stepCount", "standToSit", "sitToStand")
    able = attachPa(ema, pa)
    
    for (i in 1:nrow(able)) {
        for (j in c(15, 30, 60, 120)) {
            beforeX = extractActiv(activ, able$subjectID[i], able$timestamp[i] - j*60, able$timestamp[i])
            afterX = extractActiv(activ, able$subjectID[i], able$timestamp[i], able$timestamp[i] + j*60)
            for (k in pa) {
                able[i, paste(k, j, "before", sep = "_")] = beforeX[[k]]
                able[i, paste(k, j, "after", sep = "_")] = afterX[[k]]
                able[i, paste(k, j*2, "window", sep = "_")] = able[i, paste(k, j, "before", sep = "_")] + able[i, paste(k, j, "after", sep = "_")]
            }
        }
        print(i)
    }
    
    return(able)
}

ema = read.csv("Y:/Project ABLE/Data - Equipment Test/EMA Test/Project ABLE_20160922T205958+0200.csv.txt", header = TRUE, encoding = "UTF-8", stringsAsFactors = FALSE, sep = "\t", skipNul = TRUE)

# change the number of decimals in display
options(digits = 12)
activ = read.csv("Y:/Project ABLE/Data - Equipment Test/ActivPAL Test/002/002-AP477996 19Aug16 08-46am for 10d 0m by 15s epoch.csv", header = TRUE, stringsAsFactors = FALSE)

activ = c()
for (i in c("002", "005", "006", "007", "008")) {
    activX = read.csv(list.files(paste0("Y:/Project ABLE/Data - Equipment Test/ActivPAL Test/", i), pattern = "15s epoch.csv", full.names = TRUE), header = TRUE, stringsAsFactors = FALSE)
    activX$timestamp = strptime("1899-12-29 23:00:01", format = "%Y-%m-%d %H:%M:%S", tz = "America/Los_Angeles")+activX$time*24*60*60
    activX$subjectID = i
    activ = rbind(activ, activX)
    rm(activX)
}


ema$subjectID = sapply(ema$X.ff..fe.Participant, FUN = function(x){paste(c(rep("0", 3-nchar(x)), x), collapse = "")})
ema$timestamp = strptime(paste(ema$Form_start_date, ema$Form_start_time), format = "%m/%d/%Y %H:%M:%S", tz = "America/Los_Angeles")

able = fillPa(ema, activ)

# daily total
dateConvert = function(x) {
    segX = strsplit(x, "/")[[1]]
    add0 = function(y) {
        if (nchar(y)>1) {
            return(y)
        } else {
            return(paste0("0", y))
        }
    }
    return(paste(segX[3], add0(segX[1]), add0(segX[2]), sep = "-"))
}
able$date = sapply(able$Form_start_date, dateConvert)

activ$date = sapply(strsplit(as.character(activ$timestamp), " "), "[[", 1)

dailyTotal = function(data){
    stepCount = aggregate(StepCount~subjectID+date, data=data, sum, na.rm = TRUE)
    sedentary = aggregate(Sedentary.Time..s.~subjectID+date, data=data, sum, na.rm = TRUE)
    upright = aggregate(Upright.Time..s.~subjectID+date, data=data, sum, na.rm = TRUE)
    stepping = aggregate(Stepping.Time..s.~subjectID+date, data=data, sum, na.rm = TRUE)
    standToSit = aggregate(Sedentary.to.Upright.Movements~subjectID+date, data=data, sum, na.rm = TRUE)
    sitToStand = aggregate(Upright.to.Sedentary.Movements~subjectID+date, data=data, sum, na.rm = TRUE)
    duration = aggregate(timestamp~subjectID+date, data=data, FUN = function(x) {difftime(max(x), min(x), units = "sec")})
    
    output = Reduce(function(...) merge(..., by=c("subjectID", "date"), all=TRUE), list(stepCount, sedentary, upright, stepping, standToSit, sitToStand, duration))
    return(setNames(output, c("subjectID", "date", "stepCount_day", "sedentary_day", "upright_day", "stepping_day", "standToSit_day", "sitToStand_day", "duration_day")))
}

daily = dailyTotal(activ)
able = merge(able, daily, by=c("subjectID", "date"), all.x = TRUE, sort = FALSE)

write.csv(able, "projectAble_20161031.csv", row.names = FALSE, quote = FALSE, na = "")

