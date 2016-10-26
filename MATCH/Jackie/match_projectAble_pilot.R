setwd("Y:/Jing/Jackie")

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
}


ema$subjectID = sapply(ema$X.ff..fe.Participant, FUN = function(x){paste(c(rep("0", 3-nchar(x)), x), collapse = "")})
ema$timestamp = strptime(paste(ema$Form_start_date, ema$Form_start_time), format = "%m/%d/%Y %H:%M:%S", tz = "America/Los_Angeles")

able = fillPa(ema, activ)

write.csv(able, "projectAble_20161024.csv", row.names = FALSE, quote = FALSE, na = "")


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

# daily total



