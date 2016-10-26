setwd("X:/Maternal and Child Health Study/Pilot/Jing/Saliva")
library(XLConnect)

# load in data
results = readWorksheetFromFile("CortisolAssayResults 13 07 2016_Dresden_JC.xlsx", sheet = "Tabelle1", header = TRUE, startRow = 6, endCol = 7, colTypes="character")
log1 = readWorksheetFromFile("EMA_Saliva_Excel_Import.xlsx", sheet = "Sheet1", colTypes="character")
log2 = readWorksheetFromFile("EMA_Saliva_N0236_N0237.xlsx", sheet = "N0236", colTypes="character")
log3 = readWorksheetFromFile("EMA_Saliva_N0236_N0237.xlsx", sheet = "N0237", colTypes="character")

log = rbind(
    setNames(log1[c("Name", "Participant.ID", "Sample.ID.", "Dresden.ID", "Date.Collected.", "Time.Collected", "Wake.Time", "Did.you.eat.drink.brush.teeth.smoke.exercise.in.the.last.30.mins.")], c("label", "subjectID", "barcode", "numID", "date", "time", "waketime", "interrupt30")),
    setNames(log2[c("name", "Participant.ID", "Sample.ID", "Dresden.ID", "Date.Collected", "Time.of.Collection", "Wake.Time", "Eat.drink.brush.smoke.exercise.in.last.30.min.")], c("label", "subjectID", "barcode", "numID", "date", "time", "waketime", "interrupt30")),
    setNames(log3[c("name", "Participant.ID", "Sample.ID", "Dresden.ID", "Date.Collected", "Time.of.Collection", "Wake.Time", "Eat.drink.brush.smoke.exercise.in.last.30.min.")], c("label", "subjectID", "barcode", "numID", "date", "time", "waketime", "interrupt30"))
)

log$date = sapply(strsplit(log$date, " "), "[[", 1)
log$time = sapply(strsplit(log$time, " "), tail, 1)
log$waketime = sapply(strsplit(log$waketime, " "), tail, 1)
log$label = gsub(" ", "", log$label)

# correlation between repliations
cor(as.numeric(results$Cortisol.nmol.l[!is.na(results$Cortisol.nmol.l.1)]), as.numeric(results$Cortisol.nmol.l.1[!is.na(results$Cortisol.nmol.l.1)]))

# there are duplications in the barcode in results, compare the dups one by one for discrepancies
dupID = results$Barcode.ID[duplicated(results$Barcode.ID)] # duplicate barcode in results
for (i in 1:length(dupID)) {
    print(results[results$Barcode.ID==dupID[i] & !is.na(results$Barcode.ID) & !results$Col1%in%c("N0236", "N0237"),])
}

# empty measurements
log$cortisol = NA
for (i in 1:nrow(log)) {
    if (!log$subjectID[i] %in% c("N0236", "N0237")) {
        log$cortisol[i] = unique(results$Cortisol.nmol.l[results$Col1==log$subjectID[i] & results$Barcode.ID==log$barcode[i] & as.numeric(results$sample.no)<=191 & !is.na(results$Col1)])
    } else {
        log$cortisol[i] = results$Cortisol.nmol.l[results$sample.no==log$numID[i]]
    }
}
log$cortisol = as.numeric(log$cortisol)

# add time seq to each day
log$timeTaken = sapply(log$time, splitTime)
splitTime = function(timepoint) {
    if (timepoint=="N/A") {
        return(NA)
    } else {
        seg = strsplit(timepoint, ":")[[1]]
        if (seg[1]=="00"){seg[1]="24"}
        return(round(as.numeric(seg[1])+as.numeric(seg[2])/60, 2))
    }
}

salivaPlot = function(data, id) {
    days = unique(data$date)
    minTime = min(data$timeTaken, na.rm = TRUE)  - 0.5
    maxCor = max(data$cortisol, na.rm = TRUE) + 0.5
    for (i in 1:length(days)) {
        if (i==1) {
            plot(data$timeTaken[data$date==days[i]], data$cortisol[data$date==days[i]], xlab = "Time", ylab = "nmol/l", xlim=c(minTime, 24), ylim=c(0, maxCor), col=i, type="l", main = id)
        } else {
            points(data$timeTaken[data$date==days[i]], data$cortisol[data$date==days[i]], col=i, type="l")
        }
        legend("topright", days, col = 1:i, bty = "n", lty = 1, cex = 0.8)
    }
}


subjectList = sort(unique(log$subjectID))
for (i in 1:length(subjectList)) {
    logX = log[log$subjectID==subjectList[i],]
    salivaPlot(logX[order(logX$date, logX$timeTaken),], subjectList[i])
}

# interaction.plot(test$timeTaken, test$date, as.numeric(test$cortisol), col=c(1:4))



