setwd("Y:/Jing/Karen")
source("D:/GitHub/reach/common/functions_acc_process.R")

pa_dir = "D:/REACH/MATCH/ACC"

roster = list.files(path = pa_dir, pattern = "^.*\\.rds", full.names = TRUE)

paSum = c()
for (i in 1:length(roster)) {
    paX = readRDS(roster[i])
    calendar = aggregate(stamp~date, data = paX$data, max)
    
    output = c()
    for (j in 1:nrow(calendar)) {
        if (paste0(calendar$date[j], " 15:00:00") < calendar$stamp[j]) {
            calendarX = acc.mvpa(paX, paste0(calendar$date[j], " 15:00:00"), calendar$stamp[j])
            outputX = setNames(data.frame(t(calendarX), stringsAsFactors = FALSE), calendarX$MVPA)[2,]
            outputX$date = calendar$date[j]
            output = rbind(output, outputX)
        }
    }
    output$subjectID = paX$abstract$ID
    
    paSum = rbind(paSum, output)
    
    if(i%%10==0) {print(round(i/length(roster)*100, 2))}
}

paSum[,1:5] = sapply(paSum[,1:5], as.numeric)


paSum$valid = paSum$light + paSum$moderate + paSum$sedentary + paSum$vigorous
paSum$total = paSum$nonvalid + paSum$valid
paSum$date = as.character(paSum$date)

write.csv(paSum[c(7,6,1:5,8,9)], "match_physical_activity_halfday_20161004.csv", row.names = FALSE, quote = FALSE)
