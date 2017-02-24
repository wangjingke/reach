setwd("Y:/Jing/Britni")
library(plyr)
source("D:/GitHub/ActigraphAccelerometer/actigraph/R/acc.describe.R")
library(XLConnect)
w1 <- readWorksheetFromFile("D:/GitHub/reach/MATCH/EMA/backend/match_ema_keys.xlsx", sheet="W1", colTypes = "character")
w1 <- w1[w1$Wave.1=="complete",]
w1$DID <- ifelse(nchar(w1$DID)<3, paste0("0", w1$DID), as.character(w1$DID))
w1$enterDate <- sapply(strsplit(ifelse(is.na(w1$ManualCorrectedEnter), w1$W1pickup, w1$ManualCorrectedEnter), " "), head, 1)
w1$mother <- paste0("11", w1$DID)
w1$child <- paste0("12", w1$DID)

acc.list <- read.csv("D:/REACH/MATCH/ACC/MATCH_ACC_List_2017-02-23.csv", header = TRUE, stringsAsFactors = FALSE)

output <- c()
for (i in 1:nrow(w1)) {
  dyad <- c(w1$mother[i], w1$child[i])
  for (j in dyad) {
    acc.list.x <- acc.list[acc.list$subjectID == j,]
    posX <- which(abs(difftime(acc.list.x$date, w1$enterDate[i], units = "days")) <= 10)
    if (length(posX) > 0){
      accX <- acc.describe(readRDS(paste0("D:/REACH/MATCH/ACC/", acc.list.x$file[posX])))$full
      accX$subjectID <- j
      output <- rbind.fill(output, accX)
      # write.table(accX, paste0("MATCH_ACC_ActivPAL_", Sys.Date(), ".txt"), sep = "\t", append = TRUE, quote = FALSE, row.names = FALSE, col.names = {if(i==1) TRUE else FALSE})
      rm(posX)
      rm(accX)
    }
  }
  if (i%%10==0) {print(i)}
}
write.table(output, paste0("MATCH_ACC_ActivPAL_", Sys.Date(), ".txt"), sep = "\t", quote = FALSE, row.names = FALSE, col.names = TRUE)