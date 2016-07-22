work_dir="X:/Maternal and Child Health Study/Pilot/Jing/Genevieve/20160422_ExternalAdvisory"
setwd(work_dir)

# reading in descriptives
library(XLConnect)
demographics=readWorksheetFromFile("X:/Maternal and Child Health Study/Pilot/Jing/EMA Pilot Dem Phases 030216.xlsx", sheet="DEMOGRAPHICS", header=TRUE, startCol=2, startRow=3, endCol=5, endRow=13)
anthropmetrics=readWorksheetFromFile("X:/Maternal and Child Health Study/Pilot/Jing/Anthropometrics/Anthropometrics.xlsx", sheet=1, header=TRUE)
anthropmetrics$ht=(anthropmetrics$Height.cm.+anthropmetrics$Height..cm.)/2
anthropmetrics$wt=(anthropmetrics$Weight.kg.+anthropmetrics$Weight.kg..1)/2

descriptive=merge(demographics[c("Participant", "Baby.DOB", "Mom.DOB", "Language.")], anthropmetrics[c("Participant", "Date", "ht", "wt")], byd="Participant", all=TRUE, sort = FALSE)
descriptive$bmi=descriptive$wt/(descriptive$ht/100)^2
descriptive$age=floor(as.numeric(difftime(strptime(descriptive$Date, format="%Y-%m-%d", tz="America/Los_Angeles"), strptime(descriptive$Mom.DOB, format="%Y-%m-%d", tz="America/Los_Angeles"), units = "days"))/365)
descriptive$postpartum=floor(as.numeric(difftime(strptime(descriptive$Date, format="%Y-%m-%d", tz="America/Los_Angeles"), strptime(descriptive$Baby.DOB, format="%Y-%m-%d", tz="America/Los_Angeles"), units = "days"))/30)

mean(descriptive$age)
sd(descriptive$age)
mean(descriptive$bmi)
sd(descriptive$bmi)
mean(descriptive$postpartum)
sd(descriptive$postpartum)


load("X:/Maternal and Child Health Study/Pilot/Jing/processed/madres_pilot_ema_20160415.RData")
ema$seq=1:nrow(ema)
table(ema$Missing, useNA = "ifany")
validEMA=ema[ema$Missing=="Incomplete" | is.na(ema$Missing), c(1:61, ncol(validEMA))]
write.csv(validEMA, "madres_pilot_ema.csv", row.names = FALSE, quote = FALSE)

# stress
table(validEMA$feeling_stressed, useNA = "ifany")
mean(as.numeric(validEMA$feeling_stressed), na.rm = TRUE)
sd(as.numeric(validEMA$feeling_stressed), na.rm = TRUE)
# physical content
table(validEMA$physical_context, useNA = "ifany")
# stressors
sapply(validEMA[grep("daily_stressors", names(validEMA), value = TRUE)], table, useNA="ifany")
# behaviors
sapply(validEMA[grep("behavior_done", names(validEMA), value = TRUE)], table, useNA="ifany")
