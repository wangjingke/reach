library(XLConnect)

work_dir="X:/Accelerometer Data"
data_dir=work_dir
result_dir=work_dir
setwd(work_dir)

# create workbook to store results
compliance=loadWorkbook("../madres_pilot_acc_compliance_012816.xlsx", create=TRUE)
setStyleAction(compliance, XLC$"STYLE_ACTION.DATATYPE")
datetime = createCellStyle(compliance, name = "NoTimeForDate")
setDataFormat(datetime, format = "yyyy-mm-dd")
setCellStyleForType(compliance, style = datetime, type = XLC$"DATA_TYPE.DATETIME")
# create worksheets for 14hr and 10hr thresholds
createSheet(compliance, name="14hr")
createSheet(compliance, name="10hr")

# reading in functions for ACC processing
source("C:/Users/Wangjing Ke/Documents/Bitbucket/reach/common/acc_process.R")

# search csv files containing the accelerometer data ending with "sec.csv"
files=list.files(path=data_dir, pattern="^.*EMAP\\.csv$", all.files=TRUE, include.dirs = FALSE, recursive = TRUE, full.names = FALSE)

result=data.frame(matrix(vector(), length(files), 7, dimnames = list(c(), c("PilotID","SN","ID", "Date", "TotDays", "VldDays", "QualifyForPayment"))), stringsAsFactors=FALSE)

hrs=c(14, 10)
for (j in 1:2) {
    for (i in 1:length(files)) {
        fileX=files[i]
        # extract ID from file name
        segments=strsplit(strsplit(fileX, "/")[[1]][1], "_")[[1]]
        N_idX=unique(grep("^.*N[[:digit:]]{4}.*$", segments, value=TRUE))
        P_idX=unique(grep("^.*Pilot[[:digit:]]{2}.*$", segments, value=TRUE))
        resultX=read.acc(fileX)
        resultX.sum=sum.acc.mp(resultX, id=N_idX, VldDay_cutoff = hrs[j])
        result[i,1]=P_idX
        result[i,2:6]=resultX.sum$abstract
        result[i, 7]=ifelse(result[i, 6]>=3, "yes", "no")
        result=result[order(result$PilotID),]
    }
    print(result)
    writeWorksheet(object=compliance, data=result, sheet=j, startRow=1, startCol=1)
}

saveWorkbook(compliance)


# this script read in EMA data and determine compliance
work_dir="X:/EMA Data"
data_dir=work_dir
result_dir=work_dir
setwd(work_dir)
library(XLConnect)

files=list.files(path=data_dir, all.files=TRUE, include.dirs = FALSE, recursive = TRUE, full.names = FALSE)

compliance=data.frame(matrix(vector(), length(files), 5, dimnames = list(c(), c("ID","totPrompt", "completed", "ignored", "incomplete"))), stringsAsFactors=FALSE)

for (i in 1:length(files)){
    segmentX=strsplit(files[i], "\\.|_|/")[[1]] # split by dot or underscore
    idX=unique(grep("^.*N[[:digit:]]{4}.*$", segmentX, value=TRUE))
    extenX=tail(segmentX, n=1)
    if (extenX=="xlsx") {
        fileX=readWorksheetFromFile(files[i], sheet=1, header=TRUE)
    } else {
        fileX=read.csv(files[i], stringsAsFactors = FALSE)
    }
    
    qestX=fileX[grepl("^.*Random Time", fileX$Trigger),] # extract only the questionaires
    
    compliance[i,]=c(idX, nrow(qestX), sum(is.na(qestX$Missing)), sum(fileX$Missing=="Ignored", na.rm=TRUE), sum(fileX$Missing=="Incomplete", na.rm=TRUE))
}

compliance[2:5]=sapply(compliance[2:5], as.numeric)
comp_agg=aggregate(compliance[2:5], by=list(c(compliance$ID)), sum)
colnames(comp_agg)[1]="ID"

comp_agg$compliance=round(comp_agg$completed/comp_agg$totPrompt*100, 2)

comp_agg$QualifyForPayment=ifelse(comp_agg$compliance>=80, "yes", "no")

writeWorksheetToFile(file="../madres_pilot_ema_compliance_012816.xlsx", data=comp_agg, sheet="pilot ema compliance", clearSheets = TRUE)




# Year 1 progress report for MADRES Project 2
# summarize compliance for EMA, saliva, and accelerometer for the pilots

library(XLConnect)

setwd("X:\\Jing\\Genevieve\\madres_20160302_year1_report")

ema=readWorksheetFromFile("../../madres_pilot_ema_compliance_012816.xlsx", sheet=1, header=TRUE)
acc=readWorksheetFromFile("../../madres_pilot_acc_compliance_012816.xlsx", sheet="10hr", header=TRUE) # 10hr cutoff for valid datys
saliva=readWorksheetFromFile("../../EMA Saliva Log 030916.xlsx", sheet=1, header=TRUE, colTypes=rep("character", 6))[1:4]
saliva[saliva=="N/A"]=NA
saliva=saliva[!is.na(saliva$Date.Collected.),]
saliva.comp=aggregate(saliva$Time.Collected, by=list(saliva$Participant.ID), length)
saliva.comp$pts=round(saliva.comp$x/16*100, 2)

# average and range of ema
sum(ema$completed)/sum(ema$totPrompt)*100
# average saliva
sum(saliva.comp$x)/(16*nrow(saliva.comp))
# average acc
mean(as.numeric(acc$VldDays))
