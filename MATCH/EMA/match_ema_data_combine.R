setwd("D:/REACH/MATCH/Dataset/MATCH_EMA/Combined/V21")

w1 = readRDS("D:/REACH/MATCH/Dataset/MATCH_EMA/Wave1/Beta20160907/MATCH_EMA_W1_2016-09-07.rds")
w2 = readRDS("D:/REACH/MATCH/Dataset/MATCH_EMA/Wave2/Beta20160907/MATCH_EMA_W2_2016-09-07.rds")

combined = rbind(w1, w2)

combinedClean = combined[!is.na(combined$comply),]

saveRDS(combinedClean, "D:/REACH/MATCH/Dataset/MATCH_EMA/Combined/V21/MATCH_EMA_V21.rds")
