setwd("X:/MADRES/Project 2/Jing/Genevieve/20170218_advisoryCommittee")

### diet recall ###
asa.14 <- read.csv("X:/MADRES/ASA24 Diet Recall/Raw Data Files/2014 Version - FINAL FILES/MAD1_61323_TNMYPHEI.csv", header = TRUE, stringsAsFactors = FALSE)
# change asa14 variable name to align with asa 16
colnames(total14)[names(total14)=="G_WHL"] <- "G_WHOLE"
colnames(total14)[names(total14)=="ADD_SUG"] <- "ADD_SUGARS"

total16.1 <- read.csv("X:/MADRES/ASA24 Diet Recall/Raw Data Files/2016 Version 093016/MMAD1_15670_Totals.csv", header = TRUE, stringsAsFactors = FALSE)
total16.2 <- read.csv("X:/MADRES/ASA24 Diet Recall/Raw Data Files/2016 Version 093016/MAD2_15664_Totals.csv", header = TRUE, stringsAsFactors = FALSE)
asa <- rbind(total14[common], total16.1[common], total16.2[common])
# subjectID
extractID <- function(x) {
    temp <- substring(gsub("[[:alpha:]]", "", x), 4)
    return(substring(temp, 1, nchar(temp)-2))
}

asa$subjectID <- extractID(asa$UserName)
duedate <- read.csv("X:/MADRES/Project 2/Jing/Genevieve/20170218_advisoryCommittee/madres_subjects_duedate.csv", header = TRUE, stringsAsFactors = FALSE)
asa <- merge(asa, duedate[c("subjectID", "due_date")], by = "subjectID", all.x = TRUE, sort = FALSE)
asa$due_date <- strptime(asa$due_date, format = "%m/%d/%Y")
asa$intake_date <- strptime(sapply(strsplit(asa$IntakeStartDateTime, " "), "[[", 1), format = "%m/%d/%Y")
asa$counter <- as.numeric(difftime(asa$due_date, asa$intake_date, units = "days"))
trimester <- function(period) {
  if (period<0) {
    return(paste0("B", abs(period)))
  } else if (period >=0 & period < 94) {
    return("T3")
  } else if (period >=94 & period < 186) {
    return("T2")
  } else {
    return("T1")
  }
}
asa$trimester <- sapply(asa$counter, trimester)

basic <- c("KCAL", "PROT", "TFAT", "VC", "BCAR")
varlist = c("F_TOTAL", "V_TOTAL", "KCAL", "VC", "SFAT", "FIBE", "TFAT", "CARB", "PROT", "SUGR", "G_WHOLE", "ADD_SUGARS")


### Accelerometer ###
source("D:/GitHub/ActigraphAccelerometer/actigraph/R/acc.read.R")
source("D:/GitHub/ActigraphAccelerometer/actigraph/R/acc.summarize.R")

accList <- list.files("X:/Maternal and Child Health Study/Pilot/Jing/Accelerometer Data/", pattern = "^.*\\.csv$", recursive = TRUE, full.names = TRUE)
accList.p2 <- list.files("X:/MADRES/Project 2/Data/Accelerometer/", pattern = "^.*\\.csv$", recursive = TRUE, full.names = TRUE)

accFile <- accList[!grepl("DataTable", accList)]
pilot.acc <- c()
for (i in accFile) {
  idX <- gsub("^.*(N[0-9]{4}).*$", "\\1", i)
  accX <- acc.read(i)
  sumX <- acc.summarize(accX, age = 18, id = idX)$details
  sumX$subjectID <- idX
  pilot.acc <- rbind(pilot.acc, sumX)
}


### cortisol ###
library(XLConnect)
# load in data
cortisol = readWorksheetFromFile("X:/Maternal and Child Health Study/Pilot/Jing/Saliva/CortisolAssayResults 13 07 2016_Dresden_JC.xlsx", sheet = "Tabelle1", header = TRUE, startRow = 6, endCol = 7, colTypes="character")
log1 = readWorksheetFromFile("X:/Maternal and Child Health Study/Pilot/Jing/Saliva/EMA_Saliva_Excel_Import.xlsx", sheet = "Sheet1", colTypes="character")
log2 = readWorksheetFromFile("X:/Maternal and Child Health Study/Pilot/Jing/Saliva/EMA_Saliva_N0236_N0237.xlsx", sheet = "N0236", colTypes="character")
log3 = readWorksheetFromFile("X:/Maternal and Child Health Study/Pilot/Jing/Saliva/EMA_Saliva_N0236_N0237.xlsx", sheet = "N0237", colTypes="character")

log = rbind(
    setNames(log1[c("Name", "Participant.ID", "Sample.ID.", "Dresden.ID", "Date.Collected.", "Time.Collected", "Wake.Time", "Did.you.eat.drink.brush.teeth.smoke.exercise.in.the.last.30.mins.")], c("label", "subjectID", "barcode", "numID", "date", "time", "waketime", "interrupt30")),
    setNames(log2[c("name", "Participant.ID", "Sample.ID", "Dresden.ID", "Date.Collected", "Time.of.Collection", "Wake.Time", "Eat.drink.brush.smoke.exercise.in.last.30.min.")], c("label", "subjectID", "barcode", "numID", "date", "time", "waketime", "interrupt30")),
    setNames(log3[c("name", "Participant.ID", "Sample.ID", "Dresden.ID", "Date.Collected", "Time.of.Collection", "Wake.Time", "Eat.drink.brush.smoke.exercise.in.last.30.min.")], c("label", "subjectID", "barcode", "numID", "date", "time", "waketime", "interrupt30"))
)

log$date = sapply(strsplit(log$date, " "), "[[", 1)
log$time = sapply(strsplit(log$time, " "), tail, 1)
log$waketime = sapply(strsplit(log$waketime, " "), tail, 1)
log$label = gsub(" ", "", log$label)

# correlation between replications
cor(as.numeric(cortisol$Cortisol.nmol.l[!is.na(cortisol$Cortisol.nmol.l.1)]), as.numeric(cortisol$Cortisol.nmol.l.1[!is.na(cortisol$Cortisol.nmol.l.1)]))

# there are duplications in the barcode in results, compare the dups one by one for discrepancies
dupID = cortisol$Barcode.ID[duplicated(cortisol$Barcode.ID)] # duplicate barcode in results
for (i in 1:length(dupID)) {
    print(cortisol[cortisol$Barcode.ID==dupID[i] & !is.na(cortisol$Barcode.ID) & !cortisol$Col1%in%c("N0236", "N0237"),])
}

# empty measurements
log$cortisol = NA
for (i in 1:nrow(log)) {
    if (!log$subjectID[i] %in% c("N0236", "N0237")) {
        log$cortisol[i] = unique(cortisol$Cortisol.nmol.l[cortisol$Col1==log$subjectID[i] & cortisol$Barcode.ID==log$barcode[i] & as.numeric(cortisol$sample.no)<=191 & !is.na(cortisol$Col1)])
    } else {
        log$cortisol[i] = cortisol$Cortisol.nmol.l[cortisol$sample.no==log$numID[i]]
    }
}
log$cortisol = as.numeric(log$cortisol)

# add time seq to each day
splitTime = function(timepoint) {
    if (timepoint=="N/A") {
        return(NA)
    } else {
        seg = strsplit(timepoint, ":")[[1]]
        if (seg[1]=="00"){seg[1]="24"}
        return(round(as.numeric(seg[1])+as.numeric(seg[2])/60, 2))
    }
}
log$timeTaken = sapply(log$time, splitTime)



