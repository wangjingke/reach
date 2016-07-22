setwd("C:/Users/wangjink/Documents/Bitbucket/reach/MATCH/Wockets")

wocketlist=read.csv("C:/Users/wangjink/Documents/REACH/MATCH/EMA_Wockets/MATCH_EMA_List_Wockets_2016-06-20.csv", header = TRUE, stringsAsFactors = FALSE)

wocketlist$file=NULL
wocketlist$folder=paste0("MATCH_", wocketlist$folder)
wocketlist=wocketlist[!wocketlist$folder %in% paste0("MATCH_", c(sprintf("00%02d", 1:32), sprintf("110%02d", 1:16), sprintf("120%02d", 1:16), "92055", "11209", "11034")),]

wrongFolder = wocketlist[!wocketlist$correctFolder & wocketlist$date>"2015_09_01",]
wrongFolder = wrongFolder[order(wrongFolder$date),]
wrongFolder = wrongFolder[order(wrongFolder$subjectID, wrongFolder$date),]

cuckoos = wrongFolder[wrongFolder$num>1,]
cuckoos = cuckoos[order(cuckoos$date),]

write.csv(wrongFolder, "MATCH_wockets_wrong_folder.csv", row.names = FALSE, quote = FALSE)
